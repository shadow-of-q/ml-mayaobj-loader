#pragma once
#include "tools.hpp"
#include <new>

namespace cube {
namespace internal {
template<typename T>
void copy_n(const T* first, size_t n, T* result, int_to_type<false>) {
  const T* last = first + n;
  switch (n & 0x3) {
    case 0:
    while (first != last) {
      *result++ = *first++;
    case 3: *result++ = *first++;
    case 2: *result++ = *first++;
    case 1: *result++ = *first++;
    }
  }
}
template<typename T>
void copy_n(const T* first, size_t n, T* result, int_to_type<true>) {
  ASSERT(result >= first + n || result < first);
  memcpy(result, first, n * sizeof(T));
}

template<typename T>
void copy(const T* first, const T* last, T* result, int_to_type<false>) {
  T *localResult = result;
  while (first != last)
    *localResult++ = *first++;
}
template<typename T>
void copy(const T* first, const T* last, T* result, int_to_type<true>) {
  const size_t n = reinterpret_cast<const char*>(last) - reinterpret_cast<const char*>(first);
  memcpy(result, first, n);
}

template<typename T>
INLINE void move_n(const T* from, size_t n, T* result, int_to_type<false>) {
  for (int i = int(n) - 1; i >= 0; --i)
    result[i] = from[i];
}
template<typename T>
INLINE void move_n(const T* first, size_t n, T* result, int_to_type<true>) {
  memmove(result, first, n * sizeof(T));
}

template<typename T>
INLINE void move(const T* first, const T* last, T* result, int_to_type<false>) {
  result += (last - first);
  while (--last >= first)
    *(--result) = *last;
}

template<typename T>
INLINE void move(const T* first, const T* last, T* result, int_to_type<true>) {
  const size_t n = reinterpret_cast<uintptr>(last) - reinterpret_cast<uintptr>(first);
  memmove(result, first, n);
}

template<typename T>
void copy_construct_n(const T* first, size_t n, T* result, int_to_type<false>) {
  for (size_t i = 0; i < n; ++i)
    new (result + i) T(first[i]);
}
template<typename T>
void copy_construct_n(const T* first, size_t n, T* result, int_to_type<true>) {
  ASSERT(result >= first + n || result < first);
  memcpy(result, first, n * sizeof(T));
}

template<typename T>
void destruct_n(T* first, size_t n, int_to_type<false>) {
  for (size_t i = 0; i < n; ++i)
    (first + i)->~T();
}
template<typename T>
INLINE void destruct_n(T*, size_t, int_to_type<true>) {}

template<typename T>
INLINE void destruct(T* mem, int_to_type<false>) { mem->~T(); }
template<typename T>
INLINE void destruct(T*, int_to_type<true>) {}

template<typename T>
void construct(T* mem, int_to_type<false>) { new (mem) T(); }
template<typename T>
INLINE void construct(T*, int_to_type<true>) {}

template<typename T>
INLINE void copy_construct(T* mem, const T& orig, int_to_type<false>) {
  new (mem) T(orig);
}
template<typename T>
INLINE void copy_construct(T* mem, const T& orig, int_to_type<true>) {
  mem[0] = orig;
}

template<typename T>
void construct_n(T* to, size_t count, int_to_type<false>) {
  sizeof(to);
  for (size_t i = 0; i < count; ++i)
    new (to + i) T();
}
template<typename T>
INLINE void construct_n(T*, int, int_to_type<true>) {}

// tests if all elements in range are ordered according to pred.
template<class TIter, class TPred>
#if STL_DEBUG
void test_ordering(TIter first, TIter last, const TPred& pred) {
  if (first != last) {
    TIter next = first;
    if (++next != last) {
      ASSERT(pred(*first, *next));
      first = next;
    }
  }
}
#else
void test_ordering(TIter first, TIter last, const TPred& pred) {}
#endif // STL_DEBUG

template<typename T1, typename T2, class TPred> inline
bool debug_pred(const TPred& pred, const T1& a, const T2& b) {
#if STL_DEBUG
  if (pred(a, b)) {
    ASSERT(!pred(b, a));
    return true;
  } else
    return false;
#else
  return pred(a, b);
#endif // STL_DEBUG
}
} // namespace internal
} // namespace cube

