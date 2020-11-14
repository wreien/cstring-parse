#include <algorithm>
#include <array>
#include <charconv>
#include <concepts>
#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

#include "constexpr_vector.hpp"
#include "type_set.hpp"

/// UTILITIES

template <std::size_t N>
struct static_string {
  char m_str[N] {};

  template <std::size_t... Is>
  constexpr static_string(const char(&str)[N], std::index_sequence<Is...>)
    : m_str{ str[Is]... }
  {}

  constexpr static_string(const char(&str)[N])
    : static_string(str, std::make_index_sequence<N>{})
  {}

  constexpr static_string(std::string_view str) {
    std::ranges::copy(str, m_str);
  }

  constexpr bool operator==(const static_string& other) const noexcept {
    return std::ranges::equal(m_str, m_str + N, other.m_str, other.m_str + N);
  }

  template <std::size_t M>
  constexpr bool operator==(const static_string<M>&) const noexcept {
    return false;
  }

  constexpr operator std::string_view() const noexcept {
    return std::string_view(m_str, N);
  }
};

template <static_string S>
struct parser {
  static_assert([]{ return false; }(),
                "Unknown type encountered when parsing");
  void operator()(std::string_view) const {}
};

template <> struct parser<"int"> {
  std::optional<int> operator()(std::string_view arg) const {
    int result;
    if (auto [p, _] = std::from_chars(arg.begin(), arg.end(), result); p == arg.end())
      return result;
    return std::nullopt;
  }
};

template <> struct parser<"string"> {
  std::optional<std::string_view> operator()(std::string_view arg) const {
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
  constexpr auto num_tokens = lexer_impl<false>(str);
  return lexer_impl<true, num_tokens>(str);
}


/// ARGUMENT PARSING

template <typename Lhs, typename Rhs>
struct arg_keyfn {
  static constexpr bool value = (Lhs::name == Rhs::name);
};

template <static_string Name, typename T>
struct arg {
  static constexpr auto name = Name;
  using type = T;
  T value;
};

template <typename TypeSet>
struct parse_result {
  constexpr parse_result(TypeSet&& args) : args(std::move(args)) {}

  template <static_string S>
  auto get() const {
    constexpr auto name = S;
    return args.template get<arg<name, std::nullptr_t>>().value;
  }

  TypeSet args;
};

template <static_string Type, static_string Name>
struct positional_arg {
  static constexpr auto name = Name;
  static constexpr auto type = Type;
  auto parse(std::string_view s) const {
    using parse_type = decltype(parser<type>{}(s));
    if constexpr (not std::is_void_v<parse_type>)
      return arg<name, parse_type>{ parser<type>()(s) };
  }
};

template <typename... Args>
struct arg_parser {
  std::tuple<Args...> args;
  constexpr arg_parser(std::tuple<Args...>&& args) : args(std::move(args)) {}

  auto operator()(int argc, char** argv) const {
    auto parse = [&](auto&& handler, int argno) {
      using ret = decltype(handler.parse(argv[argno]));
      if constexpr (not std::is_void_v<ret>) {
        if (argno >= argc)
          return handler.parse(argv[argc]); // guaranteed nullptr
        return handler.parse(argv[argno]);
      }
    };

    auto parse_loop = [&]<std::size_t I>(auto&& self, auto&& result, ic<I>) {
      if constexpr (I == sizeof...(Args))
        return std::move(result);
      else {
        auto x = parse(std::get<I>(args), I + 1);
        return self(self, std::move(result).insert(x), ic<I + 1>{});
      }
    };

    return parse_result{ parse_loop(parse_loop, type_set<arg_keyfn>{}, ic<0>{}) };
  }
};


/// STRING TO ARGUMENT PARSER

template <static_string str>
inline constexpr auto make_parser = []{
  using namespace std::literals;

  constexpr auto tokens = lexer<str>();
  constexpr auto str_to_arg = [tokens]<std::size_t I>(ic<I>) {
    constexpr auto delim = tokens[I].find(':');
    constexpr auto name_sv = tokens[I].substr(0, delim);
    constexpr auto name = static_string<name_sv.size() + 1>(name_sv);
    constexpr auto type_sv = tokens[I].substr(delim+1);
    constexpr auto type = static_string<type_sv.size() + 1>(type_sv);
    return positional_arg<type, name>{};
  };

  constexpr auto make = [str_to_arg]<std::size_t... Is>(std::index_sequence<Is...>) {
    return arg_parser{ std::tuple{ str_to_arg(ic<Is>{})... }};
  };

  return make(std::make_index_sequence<tokens.size()>{});
}();

template <static_string str>
constexpr auto operator ""_parse() {
  return make_parser<str>;
}

int main(int argc, char** argv) {
  std::cout << "parsing args:\n";
  auto result = "c:string a:int b:string"_parse(argc, argv);
  if (auto a = result.get<"a">()) std::cout << "  a = " << *a << '\n';
  if (auto b = result.get<"b">()) std::cout << "  b = " << *b << '\n';
  if (auto c = result.get<"c">()) std::cout << "  c = " << *c << '\n';
}
