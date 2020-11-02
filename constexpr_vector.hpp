#ifndef CONSTEXPR_VECTOR_HPP_INCLUDED
#define CONSTEXPR_VECTOR_HPP_INCLUDED

#include <algorithm>
#include <concepts>
#include <compare>
#include <cstddef>
#include <iterator>
#include <initializer_list>
#include <memory>
#include <stdexcept>
#include <type_traits>
#include <utility>

namespace detail {
  // workaround for [[no_unique_address]] bug
  template <typename T, typename Allocator>
  class constexpr_vector_base {
  public:
    constexpr constexpr_vector_base() noexcept(noexcept(Allocator()))
      : constexpr_vector_base(Allocator())
    {}
    constexpr constexpr_vector_base(const Allocator& alloc) noexcept
      : m_alloc(alloc)
    {}
  protected:
    Allocator m_alloc;  // GCC bugs on [[no_unique_address]]
  };

  // constexpr variants of uninitialized memory algorithms
  template <std::forward_iterator I, std::sentinel_for<I> S>
    requires std::default_initializable<std::iter_value_t<I>>
  constexpr I uninitialized_value_construct(I first, S last) {
    auto current = first;
    try {
      for (; current != last; ++current)
        std::construct_at(std::addressof(*current));
      return current;
    } catch (...) {
      std::destroy(first, current);
      throw;
    }
  }

  template <std::input_iterator I, std::sentinel_for<I> S1,
            std::forward_iterator O, std::sentinel_for<O> S2>
    requires std::constructible_from<std::iter_value_t<O>,
                                     std::iter_reference_t<I>>
  constexpr O uninitialized_copy(I first, S1 last, O d_first, S2 d_last) {
    auto current = d_first;
    try {
      for (; first != last and current != d_last; ++first, (void)++current)
        std::construct_at(std::addressof(*current), *first);
      return current;
    } catch (...) {
      std::destroy(d_first, current);
      throw;
    }
  }

  template <std::input_iterator I, std::sentinel_for<I> S1,
            std::forward_iterator O, std::sentinel_for<O> S2>
    requires std::constructible_from<std::iter_value_t<O>,
                                     std::iter_rvalue_reference_t<I>>
  constexpr O uninitialized_move(I first, S1 last, O d_first, S2 d_last) {
    auto current = d_first;
    try {
      for (; first != last and current != d_last; ++first, (void)++current)
        std::construct_at(std::addressof(*current), std::move(*first));
      return current;
    } catch (...) {
      std::destroy(d_first, current);
      throw;
    }
  }

  template <std::forward_iterator I, std::sentinel_for<I> S, typename T>
    requires std::constructible_from<std::iter_value_t<I>, const T&>
  constexpr I uninitialized_fill(I first, S last, const T& value) {
    auto current = first;
    try {
      for (; current != last; ++current)
        std::construct_at(std::addressof(*current), value);
      return current;
    } catch (...) {
      std::destroy(first, current);
      throw;
    }
  }

  template <std::forward_iterator I, typename T>
    requires std::constructible_from<std::iter_value_t<I>, const T&>
  constexpr I uninitialized_fill_n(I first, std::iter_difference_t<I> n, const T& value) {
    return uninitialized_fill(std::counted_iterator(first, n),
                              std::default_sentinel, value).base();
  }
}

// TODO: add relevant requires clausing for SFINAE usage
template <typename T, typename Allocator = std::allocator<T>>
  requires std::is_nothrow_move_constructible_v<T>
class constexpr_vector : detail::constexpr_vector_base<T, Allocator> {
public:
  ///////////
  // types //
  ///////////

  using value_type = T;
  using allocator_type = Allocator;
  using pointer = typename std::allocator_traits<Allocator>::pointer;
  using const_pointer = typename std::allocator_traits<Allocator>::const_pointer;
  using reference = value_type&;
  using const_reference = const value_type&;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;
  using iterator = T*;
  using const_iterator = const T*;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  ///////////////////
  // [vector.cons] //
  ///////////////////

  constexpr constexpr_vector() noexcept(noexcept(Allocator()))
    : constexpr_vector(Allocator())
  {}

  constexpr explicit constexpr_vector(const Allocator& alloc) noexcept
    : detail::constexpr_vector_base<T, Allocator>(alloc)
  {}

  constexpr explicit constexpr_vector(size_type n, const Allocator& alloc = Allocator())
    : constexpr_vector(n, value_type(), alloc)
  {}

  constexpr constexpr_vector(size_type n, const T& value,
                             const Allocator& alloc = Allocator())
    : constexpr_vector(alloc)
  {
    assign(n, value);
  }

  template <std::input_iterator It>
  constexpr constexpr_vector(It first, It last, const Allocator& alloc = Allocator())
    : constexpr_vector(alloc)
  {
    assign(first, last);
  }

  constexpr constexpr_vector(const constexpr_vector& other)
    : constexpr_vector(other.begin(), other.end(), other.m_alloc)
  {}

  constexpr constexpr_vector(constexpr_vector&& other) noexcept
    : detail::constexpr_vector_base<T, Allocator>(std::move(other.m_alloc))
    , m_data(std::exchange(other.m_data, nullptr))
    , m_size(std::exchange(other.m_size, 0))
    , m_capacity(std::exchange(other.m_capacity, 0))
  {}

  constexpr constexpr_vector(const constexpr_vector& other, const Allocator& alloc)
    : constexpr_vector(other.begin(), other.end(), alloc)
  {}

  // FIXME: not correct if `other` doesn't use the same allocator as `alloc`
  constexpr constexpr_vector(constexpr_vector&& other, const Allocator& alloc)
    : detail::constexpr_vector_base<T, Allocator>(alloc)
    , m_data(std::exchange(other.m_data, nullptr))
    , m_size(std::exchange(other.m_size, 0))
    , m_capacity(std::exchange(other.m_capacity, 0))
  {}

  constexpr constexpr_vector(std::initializer_list<T> il,
                             const Allocator& alloc = Allocator())
    : constexpr_vector(il.begin(), il.end(), alloc)
  {}

  constexpr ~constexpr_vector() { clear(); }

  // FIXME: not entirely correct on allocator correctness,
  // but I'm being lazy here
  constexpr constexpr_vector& operator=(const constexpr_vector& other) {
    if constexpr (std::allocator_traits<Allocator>::
                       propagate_on_container_copy_assignment) {
      constexpr_vector(other).swap(*this);
    } else {
      constexpr_vector(other, this->m_alloc).swap(*this);
    }
    return *this;
  }

  constexpr constexpr_vector& operator=(constexpr_vector&& other)
    noexcept(std::allocator_traits<Allocator>::
                  propagate_on_container_move_assignment::value
          or std::allocator_traits<Allocator>::is_always_equal::value)
  {
    constexpr auto prop =
      std::allocator_traits<Allocator>::propagate_on_container_move_assignment::value;
    clear();
    if (prop or this->m_alloc == other.m_alloc) {
      m_data = std::exchange(other.m_data, nullptr);
      m_size = std::exchange(other.m_size, 0);
      m_capacity = std::exchange(other.m_capacity, 0);
      if constexpr (prop) {
        this->m_alloc = std::move(other.m_alloc);
      }
    } else {
      // FIXME: being lazy here again :(
      constexpr_vector(std::move(other), this->m_alloc).swap(*this);
    }
    return *this;
  }

  constexpr constexpr_vector& operator=(std::initializer_list<T> il) {
    constexpr_vector(il).swap(*this);
    return *this;
  }

  constexpr void swap(constexpr_vector& other)
    noexcept(std::allocator_traits<Allocator>::propagate_on_container_swap::value
          or std::allocator_traits<Allocator>::is_always_equal::value)
  {
    constexpr bool prop =
      std::allocator_traits<Allocator>::propagate_on_container_swap::value;
    if (prop or this->m_alloc == other.m_alloc) {
      std::ranges::swap(m_data, other.m_data);
      std::ranges::swap(m_size, other.m_size);
      std::ranges::swap(m_capacity, other.m_capacity);
      if constexpr (prop)
        std::ranges::swap(this->m_alloc, other.m_alloc);
    } else {
      // FIXME: hella lazy, but this iterator stuff is nonsense
      throw "unimplemented";
    }
  }

  friend constexpr void swap(constexpr_vector& lhs, constexpr_vector& rhs)
    noexcept(noexcept(lhs.swap(rhs)))
  {
    lhs.swap(rhs);
  }

  template <std::input_iterator It>
  constexpr void assign(It first, It last) {
    if (m_data) {
      std::destroy(m_data, m_data + m_size);
      m_size = 0;
    }
    for (; first != last; ++first) push_back(*first);
  }

  template <std::forward_iterator It>
  constexpr void assign(It first, It last) {
    if (m_data) {
      std::destroy(m_data, m_data + m_size);
      m_size = 0;
    }
    reserve(std::ranges::distance(first, last));
    detail::uninitialized_copy(first, last, m_data, m_capacity);
    m_size = m_capacity;
  }

  constexpr void assign(size_type n, const T& value) {
    if (m_data) {
      std::destroy(m_data, m_data + m_size);
      m_size = 0;
    }
    reserve(n);
    detail::uninitialized_fill_n(m_data, n, value);
    m_size = n;
  }

  constexpr void assign(std::initializer_list<T> il) {
    assign(il.begin(), il.end());
  }

  constexpr allocator_type get_allocator() const noexcept {
    return this->m_alloc;
  }

  ///////////////
  // iterators //
  ///////////////

  constexpr iterator                begin() noexcept { return m_data; }
  constexpr const_iterator          begin() const noexcept { return m_data; }
  constexpr iterator                end() noexcept { return m_data + m_size; }
  constexpr const_iterator          end() const noexcept { return m_data + m_size; }

  constexpr reverse_iterator        rbegin() noexcept { return end(); }
  constexpr const_reverse_iterator  rbegin() const noexcept { return end(); }
  constexpr reverse_iterator        rend() noexcept { return begin(); }
  constexpr const_reverse_iterator  rend() const noexcept { return begin(); }

  constexpr const_iterator          cbegin() const noexcept { return begin(); }
  constexpr const_iterator          cend() const noexcept { return end(); }
  constexpr const_reverse_iterator  crbegin() const noexcept { return rbegin(); }
  constexpr const_reverse_iterator  crend() const noexcept { return rend(); }

  ///////////////////////
  // [vector.capacity] //
  ///////////////////////

  [[nodiscard]] constexpr bool empty() const noexcept { return m_size == 0; }
  [[nodiscard]] constexpr size_type size() const noexcept { return m_size; }
  [[nodiscard]] constexpr size_type max_size() const noexcept { return -1; }
  [[nodiscard]] constexpr size_type capacity() const noexcept { return m_capacity; }

  constexpr void resize(size_type sz) {
    if (sz < m_size) {
      std::destroy(m_data + sz, m_data + m_size);
    } else if (sz > m_size) {
      if (sz < m_capacity)
        resize_storage(sz);
      detail::uninitialized_value_construct(m_data + m_size, m_data + sz);
    }
    m_size = sz;
  }

  constexpr void resize(size_type sz, const T& value) {
    if (sz < m_size) {
      std::destroy(m_data + sz, m_data + m_size);
    } else if (sz > m_size) {
      if (sz < m_capacity)
        resize_storage(sz);
      detail::uninitialized_fill(m_data + m_size, m_data + sz, value);
    }
    m_size = sz;
  }

  constexpr void reserve(size_type n) {
    if (m_capacity < n)
      resize_storage(n);
  }

  constexpr void shrink_to_fit() {
    if (m_size == 0)
      clear();
    else if (m_capacity != m_size and std::is_nothrow_move_constructible_v<T>)
      resize_storage(m_size);
  }

  ////////////////////
  // element access //
  ////////////////////

  constexpr reference operator[](size_type n) noexcept {
    return m_data[n];
  }

  constexpr const_reference operator[](size_type n) const noexcept {
    return m_data[n];
  }

  constexpr reference at(size_type n) {
    if (n >= m_size) throw std::out_of_range("constexpr_vector::at");
    return m_data[n];
  }

  constexpr const_reference at(size_type n) const {
    if (n >= m_size) throw std::out_of_range("constexpr_vector::at const");
    return m_data[n];
  }

  constexpr reference front() noexcept { return m_data[0]; }
  constexpr const_reference front() const noexcept { return m_data[0]; }
  constexpr reference back() noexcept { return m_data[m_size - 1]; }
  constexpr const_reference back() const noexcept { return m_data[m_size - 1]; }

  /////////////////
  // comparisons //
  /////////////////

  constexpr auto operator==(const constexpr_vector& other) const
    requires std::equality_comparable<T>
  {
    // a reasonable implementation should short-circuit
    return std::ranges::equal(*this, other);
  }

  // TODO: handle objects with operator< but no operator<=>
  constexpr auto operator<=>(const constexpr_vector& other) const
    requires std::three_way_comparable<T>
  {
    return std::lexicographical_compare_three_way(
      begin(), end(), other.begin(), other.end());
  }

  /////////////////
  // data access //
  /////////////////

  constexpr T* data() noexcept { return m_data; }
  constexpr const T* data() const noexcept { return m_data; }

  ////////////////////////
  // [vector.modifiers] //
  ////////////////////////

  // TODO: [sequence.reqmts]/4 note 2
  template <typename... Args>
  constexpr reference emplace_back(Args&&... args) {
    if (m_size == m_capacity)
      resize_storage(1 + m_capacity * 2);
    std::construct_at(m_data + m_size, std::forward<Args>(args)...);
    ++m_size;
    return back();
  }

  constexpr void push_back(const T& value) { emplace_back(value); }
  constexpr void push_back(T&& value) { emplace_back(std::move(value)); }

  constexpr void pop_back() {
    std::destroy_at(m_data + m_size - 1);
    --m_size;
  }

  /* TODO
  template <typename... Args>
  constexpr iterator emplace(const_iterator position, Args&&... args) {
    const auto index = std::ranges::distance(begin(), position);
    emplace_back(std::forward<Args>(args)...);
  }
  */

  // TODO: half-dozen `insert` overloads

  constexpr iterator erase(const_iterator position) {
    return erase(position, std::ranges::next(position));
  }

  constexpr iterator erase(const_iterator first, const_iterator last) {
    const auto d_first = std::ranges::next(begin(), first);
    const auto d_last = std::ranges::next(begin(), last);
    const auto new_end = std::move(d_last, end(), d_first);
    std::destroy(new_end, end());
    m_size -= std::ranges::distance(new_end, end());
  }

  constexpr void clear() noexcept {
    if (m_data) {
      std::destroy(m_data, m_data + m_size);
      this->m_alloc.deallocate(m_data, m_capacity);
      m_data = nullptr;
      m_size = 0;
      m_capacity = 0;
    }
  }

private:
  constexpr void resize_storage(size_type new_capacity) {
    T* new_data = this->m_alloc.allocate(new_capacity);

    // TODO: handle non-noexcept moveable types correctly
    detail::uninitialized_move(m_data, m_data + m_size,
                               new_data, new_data + new_capacity);

    if (m_data) {
      std::destroy(m_data, m_data + m_size);
      this->m_alloc.deallocate(m_data, m_capacity);
    }
    m_data = new_data;
    m_capacity = new_capacity;
  }

  T* m_data = nullptr;
  size_type m_size = 0;
  size_type m_capacity = 0;
};


#endif // CONSTEXPR_VECTOR_HPP_INCLUDED
