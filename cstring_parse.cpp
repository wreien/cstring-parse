#include <algorithm>
#include <array>
#include <charconv>
#include <concepts>
#include <iostream>
#include <optional>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

#include "constexpr_vector.hpp"
#include "static_string.hpp"
#include "type_set.hpp"

/// UTILITIES

template <static_string S>
struct parser {
  static_assert([]{ return false; }(),
                "Unknown type encountered when parsing");
  void operator()(std::string_view) const {}
};

template <> struct parser<"int"> {
  std::optional<int> operator()(std::string_view arg) const {
    if (arg.empty())
      return std::nullopt;
    int result;
    if (auto [p, _] = std::from_chars(arg.begin(), arg.end(), result); p == arg.end())
      return result;
    return std::nullopt;
  }
};

template <> struct parser<"string"> {
  std::optional<std::string_view> operator()(std::string_view arg) const {
    if (arg.empty())
      return std::nullopt;
    return { arg };
  }
};

template <auto X>
struct constant {
  using value_type = decltype(X);
  static constexpr auto value = X;
  constexpr operator value_type() const noexcept { return value; }
  constexpr value_type operator()() const noexcept { return value; }
};

template <std::size_t I>
using ic = constant<I>;

// TOKENISING

constexpr bool is_whitespace(char c) noexcept {
  return c == ' ' or c == '\t' or c == '\n' or c == '\0';
}

constexpr char to_upper(char c) noexcept {
  return c >= 'a' and c <= 'z' ? c - 'a' + 'A' : c;
}

template <bool get_values, std::size_t N = 0>
constexpr auto lexer_impl(std::string_view str) {
  constexpr_vector<std::string_view> results;
  auto start = std::ranges::find_if_not(str, is_whitespace);
  while (start != str.end()) {
    auto end = std::ranges::find_if(start, str.end(), is_whitespace);
    results.emplace_back(std::string_view(start, end));
    start = std::ranges::find_if_not(end, str.end(), is_whitespace);
  }
  // are we counting tokens or returning the tokens?
  if constexpr (get_values) {
    std::array<std::string_view, N> real_results;
    std::ranges::copy(results, real_results.begin());
    return real_results;
  } else {
    return results.size();
  }
}

template <static_string str>
constexpr auto lexer() {
  using namespace std::literals;
  constexpr auto found = std::ranges::search(str, "USAGE:"sv, {}, to_upper);
  constexpr auto real_str = std::string_view{ str }.substr(
    found.empty() ? 0 : std::ranges::distance(str.begin(), found.end()));

  constexpr auto num_tokens = lexer_impl<false>(real_str);
  constexpr auto results = lexer_impl<true, num_tokens>(real_str);

  return results;
}


/// ARGUMENT PARSING

template <static_string Name, typename T>
struct arg {
  static constexpr auto name = Name;
  using type = T;
  T value = {};
};

template <typename Lhs, typename Rhs>
struct arg_keyfn : std::bool_constant<Lhs::name == Rhs::name> {};

template <typename Lhs, typename Rhs>
struct shortarg_keyfn :
  std::bool_constant<Lhs::name == Rhs::name
                  or Lhs::name == Rhs::short_name
                  or Lhs::short_name == Rhs::name
                  or (Lhs::short_name != '\0' and Lhs::short_name == Rhs::short_name)>
{};

template <typename Arg, typename T>
concept arg_value = requires(Arg a, T t) { a.value = t; t = a.value; };

template <typename TypeSet>
class parse_result {
public:
  parse_result(TypeSet&& args, std::vector<std::string_view> remaining)
    : args(std::move(args)), remaining(std::move(remaining))
  {}

  template <static_string S>
  auto get() const {
    constexpr auto name = S;
    return args.template get<arg<name, std::nullptr_t>>().value;
  }

  decltype(auto) unparsed() const {
    return remaining;
  }

private:
  TypeSet args;
  std::vector<std::string_view> remaining;
};

template <typename PArgs, typename Flags, typename Keys>
class arg_parser {
public:
  constexpr arg_parser(
    std::string_view help_str, PArgs&& positional, Flags&& flags, Keys&& keys)
    : help_str(help_str)
    , positional(std::move(positional))
    , flags(std::move(flags))
    , keys(std::move(keys))
  {}

  auto operator()(int argc, char** argv) const {
    return parser_state{ this, prepare_type_set(), argc, argv }();
  }

private:
  std::string_view help_str;
  PArgs positional;
  Flags flags;
  Keys keys;

  // creates a result type set with all values default-constructed
  constexpr auto prepare_type_set() const {
    using namespace std::literals;

    constexpr auto map_flags = []<typename F>(const F&) {
      constexpr auto name = F::name;  // work around GCC bug
      return arg<name, bool>{};
    };
    constexpr auto map_keys = []<typename K>(const K&) {
      return decltype(std::declval<K>().parse(""sv)){};
    };

    return make_type_set<arg_keyfn>(positional).map(map_keys)
      .merge(flags.template map<arg_keyfn>(map_flags))
      .merge(keys.template map<arg_keyfn>(map_keys));
  }

  template <typename TypeSet>
  class parser_state {
  public:
    parser_state(const arg_parser* self, TypeSet&& result, int argc, char** argv)
      : self(self), result(std::move(result)), argc(argc), argv(argv)
    {}

    auto operator()() {
      return parse_args<0>();
    }

  private:
    auto get_arg(int arg) const -> std::string_view {
      if (arg >= argc)
        return {};
      else
        return argv[arg];
    }

    auto get_current_arg() const -> std::string_view {
      return get_arg(current_arg);
    }

    auto get_next_arg() -> std::string_view {
      return get_arg(++current_arg);
    }

    void print_help() const {
      // TODO: prettify help string somewhat?
      std::cout << self->help_str;
    }

    void parse_longname(std::string_view arg) {
      const auto equals = arg.find('=');
      bool has_equals = equals != std::string_view::npos;
      auto name = arg.substr(0, equals);

      const auto key = [name]<typename T>(const T&) { return T::name == name; };
      if (not has_equals and self->flags.contains(key)) {
        result.inspect(key, [](arg_value<bool> auto& x) { x.value = true; });
      } else {
        auto value = has_equals ? arg.substr(equals + 1) : get_next_arg();
        bool was_found = self->keys.inspect(key, [=, this](auto&& t) {
          using V = decltype(t.parse(value));
          result.inspect(key, [&t, value](std::same_as<V> auto& x) {
            x = t.parse(value);
          });
        });
        if (not was_found) {
          std::cout << "error: unknown flag: --" << name << "\n\n";  // TODO: proper exception management
          print_help();
          std::exit(1);
        }
      }
    }

    void parse_shortnames(std::string_view arg) {
      const auto key = [](char c) {
        return [c]<typename T>(const T&) { return T::short_name == c; };
      };

      // get all flags
      auto it = arg.begin();
      bool was_flag = true;
      for (; it != arg.end() and was_flag; ++it) {
        was_flag = self->flags.inspect(key(*it), [this]<typename T>(const T&) {
          const auto fkey = []<typename U>(const U&) { return T::name == U::name; };
          result.inspect(fkey, [](arg_value<bool> auto& x) { x.value = true; });
        });
      }

      // we ran into a key that wasn't a flag, parse it as a key-value
      if (not was_flag) {
        auto c = *(it - 1);
        auto remainder = std::string_view(it, arg.end());
        auto value = remainder.starts_with('=') ? remainder.substr(1)
                   : not remainder.empty() ? remainder : get_next_arg();

        bool was_found = self->keys.inspect(key(c), [=, this]<typename T>(const T& t) {
          using V = decltype(t.parse(value));
          const auto fkey = []<typename U>(const U&) { return T::name == U::name; };
          result.inspect(fkey, [&t, value](std::same_as<V> auto& x) {
            x = t.parse(value);
          });
        });

        if (not was_found) {
          std::cout << "error: unknown flag: -" << c << "\n\n";
          print_help();
          std::exit(1);
        }
      }
    }

    void parse_nonpos_args() {
      using namespace std::literals;

      while (has_more_flags and current_arg < argc) {
        std::string_view arg = get_current_arg();
        if (arg == "--"sv) {
          // we use the flag "--" to determine the end of non-positional args
          has_more_flags = false;
        } else if (arg == "--help"sv or arg == "-h"sv) {
          // special-case to print help and exit
          print_help();
          std::exit(0);
        } else if (arg.starts_with("--"sv)) {
          // a single long-name argument
          parse_longname(arg.substr(2));
        } else if (arg.starts_with("-"sv)) {
          // a short-name flag; possibly many embedded in this one argument
          parse_shortnames(arg.substr(1));
        } else {
          // not a flag at all, we're done... for now
          break;
        }

        ++current_arg;
      }
    }

    template <std::size_t P>
    auto parse_args() -> parse_result<TypeSet> {
      parse_nonpos_args();
      if constexpr (P < std::tuple_size_v<PArgs>) {
        // deal with named positional args
        result.template get<std::tuple_element_t<P, PArgs>>() =
          std::get<P>(self->positional).parse(get_arg(current_arg++));
        return parse_args<P + 1>();
      } else {
        // finished with named positional args, all other positional args
        // should get stick in a vector as the 'leftovers'
        std::vector<std::string_view> remaining_posargs;
        while (current_arg < argc) {
          remaining_posargs.push_back(get_arg(current_arg++));
          parse_nonpos_args();
        }
        return parse_result{ std::move(result), std::move(remaining_posargs) };
      }
    }

    const arg_parser* self = nullptr;
    TypeSet result = {};
    int argc = 0;
    char** argv = nullptr;

    bool has_more_flags = true;
    int current_arg = 1;
  };
};


/// STRING TO ARGUMENT PARSER

template <static_string Name, static_string Type>
struct positional_arg {
  static constexpr auto name = Name;
  static constexpr auto type = Type;

  auto parse(std::string_view s) const {
    using parse_type = decltype(parser<type>{}(s));
    if constexpr (not std::is_void_v<parse_type>)
      return arg<name, parse_type>{ parser<type>{}(s) };
  }
};

template <static_string Name, char ShortName, static_string Type>
struct key_arg {
  static constexpr auto name = Name;
  static constexpr auto type = Type;
  static constexpr auto short_name = ShortName;

  auto parse(std::string_view s) const {
    using parse_type = decltype(parser<type>{}(s));
    if constexpr (not std::is_void_v<parse_type>)
      return arg<name, parse_type>{ parser<type>{}(s) };
  }
};

template <static_string Name, char ShortName>
struct flag_arg {
  static constexpr auto name = Name;
  static constexpr auto short_name = ShortName;
};

template <static_string str>
class str_to_arg_parser {
public:
  constexpr str_to_arg_parser() = default;

  constexpr auto operator()() const noexcept {
    auto&& [pos, flag, key] = parse_arg<0>(
      std::tuple{}, type_set<shortarg_keyfn>{}, type_set<shortarg_keyfn>{});
    static_assert(decltype(flag.merge(key))::is_valid(),
                  "flags and keys cannot share identifiers or short names");
    return arg_parser{ str, std::move(pos), std::move(flag), std::move(key) };
  }

private:
  static constexpr auto tokens = lexer<str>();

  template <std::size_t I>
  constexpr auto parse_arg(auto&& ps, auto&& fs, auto&& ks) const noexcept {
    using namespace std::literals;

    if constexpr (I == tokens.size()) {
      return std::tuple{ ps, fs, ks };
    } else if constexpr (tokens[I].starts_with("-"sv)) {
      constexpr auto data = []{
        if constexpr (tokens[I].starts_with("--"sv))
          return std::tuple{ tokens[I].substr(2), '\0' };
        else {
          // short name first
          constexpr char sname = tokens[I][1];
          constexpr auto offset = std::min("-?,--"sv.size(), tokens[I].size());
          return std::tuple{ tokens[I].substr(offset), sname };
        }
      }();

      constexpr auto tok = std::get<0>(data);
      constexpr auto shortname = std::get<1>(data);
      constexpr auto npos = std::string_view::npos;

      if constexpr (constexpr auto equals = tok.find('='); equals != npos) {
        // key
        constexpr auto name_sv = tok.substr(0, equals);
        static_assert(name_sv.size() != 0, "long name cannot be empty");
        constexpr auto name = static_string<name_sv.size() + 1>(name_sv);
        constexpr auto type_sv = tok.substr(equals + 1);
        constexpr auto type = static_string<type_sv.size() + 1>(type_sv);
        auto arg = key_arg<name, shortname, type>{};
        return parse_arg<I + 1>(ps, fs, std::move(ks).insert(arg));
      } else {
        // flag arg
        static_assert(tok.size() != 0, "long name cannot be empty");
        constexpr auto name = static_string<tok.size() + 1>(tok);
        auto arg = flag_arg<name, shortname>{};
        return parse_arg<I + 1>(ps, std::move(fs).insert(arg), ks);
      }
    } else {
      // positional arg
      constexpr auto delim = tokens[I].find(':');
      constexpr auto name_sv = tokens[I].substr(0, delim);
      constexpr auto name = static_string<name_sv.size() + 1>(name_sv);
      constexpr auto type_sv = tokens[I].substr(delim + 1);
      constexpr auto type = static_string<type_sv.size() + 1>(type_sv);
      auto arg = std::tuple{ positional_arg<name, type>{} };
      return parse_arg<I + 1>(std::tuple_cat(ps, arg), fs, ks);
    }
  }
};

template <static_string str>
constexpr auto operator ""_parse() {
  constexpr auto real_str = str;  // avoid GCC bug
  return str_to_arg_parser<real_str>{}();
}

int main(int argc, char** argv) try {
  constexpr auto parser = R"(arg_parse v0.1

A demo application for my dodgy argparse generator.

Usage:
  --flag1 --flag2 -s,--short
  --key=string --other=int -b,--bar=string
  inf:string count:int outf:string
)"_parse;

  std::cout << "parsing args...\n";
  auto result = parser(argc, argv);

  std::cout << "---\n";

  // boolean flags
  std::cout << std::boolalpha;
  std::cout << "  flag1 = " << result.get<"flag1">() << '\n';
  std::cout << "  flag2 = " << result.get<"flag2">() << '\n';
  std::cout << "  short = " << result.get<"short">() << '\n';

  std::cout << "---\n";

  // key-value flags
  if (auto key = result.get<"key">())
    std::cout << "  key   = " << *key << '\n';
  if (auto other = result.get<"other">())
    std::cout << "  other = " << *other << '\n';
  if (auto bar = result.get<"bar">())
    std::cout << "  bar   = " << *bar << '\n';

  std::cout << "---\n";

  // positionals
  if (auto inf = result.get<"inf">())
    std::cout << "  inf   = " << *inf << '\n';
  if (auto count = result.get<"count">())
    std::cout << "  count = " << *count << '\n';
  if (auto outf = result.get<"outf">())
    std::cout << "  outf  = " << *outf << '\n';

  std::cout << "---\n";

  // leftover positionals
  std::cout << "  remainder = ";
  for (auto&& x : result.unparsed()) std::cout << x << ' ';
  std::cout << '\n';
} catch (const char* err) {
  std::cout << "caught exception: " << err << '\n';
}
