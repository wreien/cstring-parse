#ifndef TYPE_MAP_HPP_INCLUDED
#define TYPE_MAP_HPP_INCLUDED

#include <cstdint>
#include <type_traits>
#include <tuple>

// A type-level set, for building up a set of unique types.
// Uses 'IsEqual' metafunction for type equality testing.
template <template <typename, typename> typename IsEqual, typename... Args>
class type_set {
public:
  constexpr type_set() = default;

private:
  constexpr type_set(std::tuple<Args...>&& data) : m_data(std::move(data)) {}

  template <template <typename, typename> typename, typename...>
  friend class type_set;

  std::tuple<Args...> m_data;

  template <std::size_t I>
  using ic = std::integral_constant<std::size_t, I>;

  static constexpr std::size_t npos = -1;

  template <typename Key>
  static constexpr std::size_t indexof() {
    constexpr auto loop = []<std::size_t I>(auto&& self, ic<I>) {
      if constexpr (I < sizeof...(Args)) {
        using Elem = std::tuple_element_t<I, std::tuple<Args...>>;
        if constexpr (IsEqual<Elem, Key>::value)
          return I;
        else
          return self(self, ic<I+1>{});
      } else {
        return npos;
      }
    };
    return loop(loop, ic<0>{});
  }

public:
  template <typename Key>
  constexpr decltype(auto) get() {
    constexpr auto I = indexof<Key>();
    static_assert(I != npos, "key not found");
    if constexpr (I != npos)
      return std::get<indexof<Key>()>(m_data);
  }

  template <typename Key>
  constexpr decltype(auto) get() const {
    constexpr auto I = indexof<Key>();
    static_assert(I != npos, "key not found");
    if constexpr (I != npos)
      return std::get<indexof<Key>()>(m_data);
  }

  template <typename T>
  constexpr auto insert(T&& t) && noexcept {
    using NewKey = std::remove_cvref_t<T>;
    constexpr auto I = indexof<NewKey>();
    static_assert(I == npos, "key already exists");
    if constexpr (I == npos)
      return type_set<IsEqual, Args..., NewKey>(
        std::tuple_cat(std::move(m_data), std::tuple<NewKey>(std::forward<T>(t))));
    else
      return *this;
  }

  template <typename T>
  constexpr auto insert(T&& t) const& {
    using NewKey = std::remove_cvref_t<T>;
    constexpr auto I = indexof<NewKey>();
    static_assert(I == npos, "key already exists");
    if constexpr (I == npos)
      return type_set<IsEqual, Args..., NewKey>(
        std::tuple_cat(m_data, std::tuple<NewKey>(std::forward<T>(t))));
    else
      return *this;
  }
};

#endif // TYPE_MAP_HPP_INCLUDED
