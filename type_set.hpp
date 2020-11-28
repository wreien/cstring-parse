#ifndef TYPE_MAP_HPP_INCLUDED
#define TYPE_MAP_HPP_INCLUDED

#include <cstddef>
#include <concepts>
#include <functional>
#include <type_traits>
#include <tuple>

namespace detail {
  struct prechecked_tag {};

  template <typename T, template <typename...> typename Template>
  struct is_specialisation : std::false_type {};
  template <template <typename...> typename Template, typename... Args>
  struct is_specialisation<Template<Args...>, Template> : std::true_type {};
  template <typename T, template <typename...> typename Template>
  inline constexpr bool is_specialisation_v = is_specialisation<T, Template>::value;
}

// A type-level set, for building up a set of unique types.
// Uses 'IsEqual' metafunction for type equality testing.
template <template <typename, typename> typename IsEqual, typename Data = std::tuple<>>
class type_set {
public:
  constexpr type_set() = default;
  constexpr type_set(Data&& data) : m_data(std::move(data)) {
    static_assert(is_valid(), "provided values do not fulfil set invariant");
  }
  constexpr type_set(const Data& data) : m_data(data) {
    static_assert(is_valid(), "provided values do not fulfil set invariant");
  }

private:
  constexpr type_set(detail::prechecked_tag, Data&& data)
    : m_data(std::move(data))
  {}

  template <template <typename, typename> typename, typename>
  friend class type_set;

  Data m_data;

  template <std::size_t I>
  using ic = std::integral_constant<std::size_t, I>;

  static constexpr std::size_t npos = -1;

  template <typename Key>
  static constexpr std::size_t indexof() {
    constexpr auto loop = []<std::size_t I>(auto&& self, ic<I>) {
      if constexpr (I < std::tuple_size_v<Data>) {
        using Elem = std::tuple_element_t<I, Data>;
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
  // full check to ensure that the type is valid
  static constexpr bool is_valid() {
    constexpr auto loop = []<std::size_t I>(auto&& self, ic<I>) {
      if constexpr (I + 1 < std::tuple_size_v<Data>) {
        using Lhs = std::tuple_element_t<I, Data>;
        constexpr auto inner = []<std::size_t J>(auto&& self, ic<J>) {
          if constexpr (J < std::tuple_size_v<Data>) {
            using Rhs = std::tuple_element_t<J, Data>;
            if constexpr (IsEqual<Lhs, Rhs>::value)
              return false;
            else
              return self(self, ic<J + 1>{});
          } else {
            return true;
          }
        };
        if constexpr (inner(inner, ic<I + 1>{}))
          return self(self, ic<I + 1>{});
        else
          return false;
      } else {
        return true;
      }
    };
    return loop(loop, ic<0>{});
  }

  template <typename Key>
  constexpr decltype(auto) get() {
    constexpr auto I = indexof<Key>();
    static_assert(I != npos, "key not found");
    if constexpr (I != npos) {
      using std::get;
      return get<indexof<Key>()>(m_data);
    }
  }

  template <typename Key>
  constexpr decltype(auto) get() const {
    constexpr auto I = indexof<Key>();
    static_assert(I != npos, "key not found");
    if constexpr (I != npos) {
      using std::get;
      return get<indexof<Key>()>(m_data);
    }
  }

  template <typename T>
    requires detail::is_specialisation_v<Data, std::tuple>
  constexpr auto insert(T&& t) && noexcept {
    using NewKey = std::remove_cvref_t<T>;
    constexpr auto I = indexof<NewKey>();
    static_assert(I == npos, "key already exists");
    if constexpr (I == npos) {
      auto x = std::tuple_cat(std::move(m_data), std::tuple<NewKey>(std::forward<T>(t)));
      return type_set<IsEqual, decltype(x)>(detail::prechecked_tag{}, std::move(x));
    } else {
      return *this;
    }
  }

  template <typename T>
    requires detail::is_specialisation_v<Data, std::tuple>
  constexpr auto insert(T&& t) const& {
    using NewKey = std::remove_cvref_t<T>;
    constexpr auto I = indexof<NewKey>();
    static_assert(I == npos, "key already exists");
    if constexpr (I == npos) {
      auto x = std::tuple_cat(m_data, std::tuple<NewKey>(std::forward<T>(t)));
      return type_set<IsEqual, decltype(x)>(detail::prechecked_tag{}, std::move(x));
    } else {
      return *this;
    }
  }

  // Access a the first element as specified by the given `Compare`,
  // calling `Function` with the result (which should be a generic functor).
  // Returns `true` on success, or `false` if no such key exists.
  template <typename Compare, typename Function>
  constexpr bool inspect(Compare&& key, Function&& func) {
    auto loop = [&]<std::size_t I>(auto&& self, ic<I>) {
      if constexpr (I == std::tuple_size_v<Data>) {
        return false;
      } else {
        using elem_t = std::tuple_element_t<I, Data>;
        if constexpr (std::regular_invocable<Compare, const elem_t&>
                  and std::invocable<Function, elem_t&>)
        {
          using std::get;
          if (std::invoke(key, get<I>(m_data))) {
            std::invoke(std::forward<Function>(func), get<I>(m_data));
            return true;
          }
        }
        return self(self, ic<I + 1>{});
      }
    };
    return loop(loop, ic<0>{});
  }

  template <typename Compare, typename Function>
  constexpr bool inspect(Compare&& key, Function&& func) const {
    return cinspect(std::forward<Compare>(key), std::forward<Function>(func));
  }

  // Identical to `inspect` but without possibility of mutability
  // (sad code duplication life)
  template <typename Compare, typename Function>
  constexpr bool cinspect(Compare&& key, Function&& func) const {
    auto loop = [&]<std::size_t I>(auto&& self, ic<I>) {
      if constexpr (I == std::tuple_size_v<Data>) {
        return false;
      } else {
        using elem_t = std::tuple_element_t<I, Data>;
        if constexpr (std::regular_invocable<Compare, const elem_t&>
                  and std::invocable<Function, const elem_t&>)
        {
          using std::get;
          if (std::invoke(key, get<I>(m_data))) {
            std::invoke(std::forward<Function>(func), get<I>(m_data));
            return true;
          }
        }
        return self(self, ic<I + 1>{});
      }
    };
    return loop(loop, ic<0>{});
  }

  // Runtime check if the given key exists
  template <typename Compare>
  constexpr bool contains(Compare&& key) const {
    return inspect(std::forward<Compare>(key), [](auto&&){});
  }

  // Convert this type set to use a different comparison function,
  // verifying that such a translation is valid
  template <template <typename, typename> typename NewIsEqual>
  constexpr type_set<NewIsEqual, Data> map() const {
    static_assert(type_set<NewIsEqual, Data>::is_valid(),
                  "new key does not maintain set invariants");
    return { detail::prechecked_tag{}, m_data };
  }

  // Convert this type set to use a different comparison function,
  // mutating the elements as we go as appropriate,
  // verifying that such a translation is valid
  template <template <typename, typename> typename NewIsEqual = IsEqual,
            typename Function>
    requires detail::is_specialisation_v<Data, std::tuple>
  constexpr auto map(Function&& mapper) const {
    auto transformed = [this, &mapper]<std::size_t... Is>(std::index_sequence<Is...>) {
      return std::tuple{ mapper(std::get<Is>(m_data))... };
    };
    auto values = transformed(std::make_index_sequence<std::tuple_size_v<Data>>{});
    return type_set<NewIsEqual, decltype(values)>(values);
  }

  // Merge two type sets together,
  // erroring on mismatch
  template <typename Other>
    requires detail::is_specialisation_v<Data, std::tuple>
  constexpr auto merge(const type_set<IsEqual, Other>& other) const {
    auto combined = std::tuple_cat(m_data, other.m_data);
    using new_type = type_set<IsEqual, decltype(combined)>;
    static_assert(new_type::is_valid(), "the sets must not share keys");
    return new_type{ detail::prechecked_tag{}, std::move(combined) };
  }

  // TODO: other ref-qualifiers for map and merge
};

#endif // TYPE_MAP_HPP_INCLUDED
