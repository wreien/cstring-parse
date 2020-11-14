#ifndef STATIC_STRING_HPP_INCLUDED
#define STATIC_STRING_HPP_INCLUDED

#include <algorithm>
#include <cstdint>
#include <string_view>
#include <utility>

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

  // container interface
  constexpr auto begin() noexcept {
    return m_str;
  }
  constexpr auto begin() const noexcept {
    return m_str;
  }
  constexpr auto end() noexcept {
    return m_str + N;
  }
  constexpr auto end() const noexcept {
    return m_str + N;
  }
  constexpr std::size_t size() const noexcept {
    return N;
  }
  constexpr char* data() noexcept {
    return m_str;
  }
  constexpr const char* data() const noexcept {
    return m_str;
  }
};

#endif // STATIC_STRING_HPP_INCLUDED
