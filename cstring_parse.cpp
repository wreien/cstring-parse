#include <algorithm>
#include <array>
#include <charconv>
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
  T value;
};

template <typename Lhs, typename Rhs>
struct arg_keyfn : std::bool_constant<Lhs::name == Rhs::name> {};

template <typename Lhs, typename Rhs>
struct shortarg_keyfn :
  std::bool_constant<Lhs::name == Rhs::name
                  or Lhs::name == Rhs::short_name
                  or Lhs::short_name == Rhs::name
                  or (Lhs::short_name != "" and Lhs::short_name == Rhs::short_name)>
{};

template <typename TypeSet>
class parse_result {
public:
  constexpr parse_result(TypeSet&& args) : args(std::move(args)) {}

  template <static_string S>
  auto get() const {
    constexpr auto name = S;
    return args.template get<arg<name, std::nullptr_t>>().value;
  }

private:
  TypeSet args;
};

template <typename PArgs, typename FArgs, typename KArgs>
class arg_parser {
public:
  constexpr arg_parser(PArgs&& positional, FArgs&& flags, KArgs&& keys)
    : positional(std::move(positional))
    , flags(std::move(flags))
    , keys(std::move(keys))
  {}

  auto operator()(int argc, char** argv) const {
    auto result = prepare_type_set();
    auto get_arg = [argv, argc](int arg) -> std::string_view {
      if (arg >= argc) return {}; else return argv[arg];
    };
    return parse_args<0>(std::move(result), 1, get_arg);
  }

private:
  PArgs positional;
  FArgs flags;
  KArgs keys;

  // creates a result type set with all values default-constructed
  // uses a key of both short/long names
  constexpr auto prepare_type_set() const {
    using namespace std::literals;

    constexpr auto map_flags = []<typename F>(const F&) {
      constexpr auto name = F::name;  // work around GCC bug
      return arg<name, bool>{};
    };
    constexpr auto map_keys = []<typename K>(const K&) {
      return decltype(std::declval<K>().parse(""sv)){};
    };

    return make_type_set<shortarg_keyfn>(positional).template map<arg_keyfn>(map_keys)
      .merge(flags.template map<arg_keyfn>(map_flags))
      .merge(keys.template map<arg_keyfn>(map_keys));
  }

  template <std::size_t P>
  constexpr auto parse_args(auto&& result, int argno, auto&& get_arg) const {
    if constexpr (P == std::tuple_size_v<PArgs>)
      return parse_result{ std::move(result) };
    else {
      result.template get<std::tuple_element_t<P, PArgs>>() =
        std::get<P>(positional).parse(get_arg(argno));
      return parse_args<P + 1>(std::move(result), argno + 1, get_arg);
    }
  }
};


/// STRING TO ARGUMENT PARSER

template <static_string Name, static_string Type>
struct positional_arg {
  static constexpr auto name = Name;
  static constexpr auto type = Type;
  static constexpr auto short_name = static_string{""};

  auto parse(std::string_view s) const {
    using parse_type = decltype(parser<type>{}(s));
    if constexpr (not std::is_void_v<parse_type>)
      return arg<name, parse_type>{ parser<type>{}(s) };
  }
};

template <static_string Name, static_string ShortName, static_string Type>
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

template <static_string Name, static_string ShortName>
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
    return arg_parser{ std::move(pos), std::move(flag), std::move(key) };
  }

private:
  static constexpr auto tokens = lexer<str>();

  template <std::size_t I>
  constexpr auto parse_arg(auto&& ps, auto&& fs, auto&& ks) const noexcept {
    using namespace std::literals;

    if constexpr (I == tokens.size()) {
      return std::tuple{ ps, fs, ks };
    } else if constexpr (tokens[I].starts_with("--"sv)) {
      constexpr auto start = 2;
      constexpr auto npos = std::string_view::npos;
      if constexpr (constexpr auto equals = tokens[I].find('='); equals != npos) {
        // key
        static_assert(equals != npos, "not yet implemented");
      } else {
        // flag arg
        constexpr auto name_sv = tokens[I].substr(start);
        constexpr auto name = static_string<name_sv.size() + 1>(name_sv);
        auto arg = flag_arg<name, static_string{""}>{};
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

int main(int argc, char** argv) {
  constexpr auto parser = R"xyz(
A demo application for my dodgy argparse generator.

Usage:
  --flag1 --flag2
  infile:string count:int outfile:string

)xyz"_parse;

  std::cout << "parsing args:\n";
  auto result = parser(argc, argv);
  if (auto inf = result.get<"infile">())
    std::cout << "  a = " << *inf << '\n';
  if (auto cnt = result.get<"count">())
    std::cout << "  b = " << *cnt << '\n';
  if (auto out = result.get<"outfile">())
    std::cout << "  c = " << *out << '\n';
}
