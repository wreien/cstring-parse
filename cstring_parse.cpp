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

/// UTILITIES

template <std::size_t N>
struct static_string {
  std::array<char, N> m_str {};

  constexpr static_string(const char(&str)[N])
    : m_str{ std::to_array(str) }
  {}

  constexpr static_string(std::string_view s) {
    std::ranges::copy(s, m_str.begin());
  }

  constexpr bool operator==(const static_string&) const noexcept = default;

  template <std::size_t M>
  constexpr bool operator==(const static_string<M>&) const noexcept {
    return false;
  }

  constexpr operator std::string_view() const noexcept {
    return std::string_view(m_str.data(), m_str.size());
  }
};

template <static_string S>
struct parser {
  static_assert([]{ return false; }(),
                "Unknown type encountered when parsing");
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
  std::optional<std::string> operator()(std::string_view arg) const {
    return std::string(arg);
  }
};

template <std::size_t I>
using ic = std::integral_constant<std::size_t, I>;

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

template <static_string Name, typename T>
struct arg {
  static constexpr auto name = Name;
  using type = T;
  T value;
};

template <typename... Args>
struct parse_result {
  std::tuple<Args...> args;
  constexpr parse_result(std::tuple<Args...>&& args) : args(std::move(args)) {}

private:
  template <static_string S>
  static constexpr std::size_t indexof() {
    constexpr auto loop = []<std::size_t I>(auto&& self, ic<I>) {
      static_assert(I < sizeof...(Args), "arg not found");
      if constexpr (I < sizeof...(Args)) {
        if constexpr (std::tuple_element_t<I, std::tuple<Args...>>::name == S)
          return I;
        else
          return self(self, ic<I+1>{});
      }
    };
    return loop(loop, ic<0>{});
  }

public:
  template <static_string S>
  auto get() const {
    return std::get<parse_result::indexof<S>()>(args).value;
  }
};

template <static_string Type, static_string Name>
struct positional_arg {
  static constexpr auto name = Name;
  static constexpr auto type = Type;
  auto parse(std::string_view s) const {
    using parse_type = decltype(parser<type>{}(s));
    return arg<name, parse_type>{ parser<type>()(s) };
  }
};

template <typename... Args>
struct arg_parser {
  std::tuple<Args...> args;
  constexpr arg_parser(std::tuple<Args...>&& args) : args(std::move(args)) {}

  auto operator()(int argc, char** argv) const {
    auto parse = [&](auto&& handler, int argno) {
      if (argno >= argc)
        return decltype(handler.parse(argv[0])){};
      return handler.parse(argv[argno]);
    };

    auto pos_parse = [&]<std::size_t... Is>(std::index_sequence<Is...>) {
      return std::tuple{ parse(std::get<Is>(args), Is + 1)... };
    };

    return parse_result{ pos_parse(std::index_sequence_for<Args...>{}) };
  }
};


/// STRING TO ARGUMENT PARSER

template <static_string str>
constexpr auto operator ""_parse() {
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
}

int main(int argc, char** argv) {
  std::cout << "parsing args:\n";
  auto result = "c:string a:int b:string"_parse(argc, argv);
  if (auto a = result.get<"a">()) std::cout << "  a = " << *a << '\n';
  if (auto b = result.get<"b">()) std::cout << "  b = " << *b << '\n';
  if (auto c = result.get<"c">()) std::cout << "  c = " << *c << '\n';
}
