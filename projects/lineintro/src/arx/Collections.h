/** @file ArX Collections Library.
 *
 * Implementation of several container classes which can be pretty useful in 
 * some cases.
 */
#ifndef __ARX_COLLECTIONS_H__
#define __ARX_COLLECTIONS_H__

#include "config.h"
#include <map>
#include <set>
#include <vector>
#include <iterator>
#include <exception>
#include "smart_ptr.h"
#include "static_assert.h"
#include "Mpl.h"
#include "Memory.h"

#ifdef ARX_USE_BOOST
#  include <boost/array.hpp>
namespace arx {
  using boost::array;
}
#else
namespace arx {
// -------------------------------------------------------------------------- //
// array
// -------------------------------------------------------------------------- //
  /**
   * array template provides stl interface to c arrays.
   */
  template<class T, std::size_t N>
  class array {
  public:
    T elems[N];

  public:
    typedef T              value_type;
    typedef T*             iterator;
    typedef const T*       const_iterator;
    typedef T&             reference;
    typedef const T&       const_reference;
    typedef std::size_t    size_type;
    typedef std::ptrdiff_t difference_type;

    iterator begin()             { return elems; }
    const_iterator begin() const { return elems; }
    iterator end()               { return elems + N; }
    const_iterator end() const   { return elems + N; }

    typedef std::reverse_iterator<iterator> reverse_iterator;
    typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

    reverse_iterator rbegin()             { return reverse_iterator(end()); }
    const_reverse_iterator rbegin() const { return const_reverse_iterator(end()); }
    reverse_iterator rend()               { return reverse_iterator(begin()); }
    const_reverse_iterator rend() const   { return const_reverse_iterator(begin()); }

    reference operator[](size_type i) { 
      assert(i < N); 
      return elems[i];
    }

    const_reference operator[](size_type i) const {
      assert(i < N); 
      return elems[i];
    }

    reference at(size_type i) { rangecheck(i); return elems[i]; }
    const_reference at(size_type i) const { rangecheck(i); return elems[i]; }

    reference front()             { return elems[0]; }
    const_reference front() const { return elems[0]; }
    reference back()              { return elems[N - 1]; }
    const_reference back() const  { return elems[N - 1]; }

    static size_type size()     { return N; }
    static bool empty()         { return false; }
    static size_type max_size() { return N; }
    enum  { static_size = N };

    void swap (array<T, N>& y) {
      std::swap_ranges(begin(), end(), y.begin());
    }

    const T* data() const { return elems; }
    T* data()             { return elems; }
    T* c_array()          { return elems; }

    template <typename T2>
    array<T, N>& operator= (const array<T2, N>& r) {
      std::copy(r.begin(), r.end(), begin());
      return *this;
    }

    void assign(const T& value) {
      std::fill_n(begin(), size(), value);
    }

    static void rangecheck(size_type i) {
      if(i >= size())
        throw std::range_error("array<>: index out of range");
    }
  };

  template<class T>
  class array<T, 0> {
  public:
    typedef T              value_type;
    typedef T*             iterator;
    typedef const T*       const_iterator;
    typedef T&             reference;
    typedef const T&       const_reference;
    typedef std::size_t    size_type;
    typedef std::ptrdiff_t difference_type;

    iterator begin()             { return iterator(reinterpret_cast<T*>(this)); }
    const_iterator begin() const { return const_iterator(reinterpret_cast<const T*>(this)); }
    iterator end()               { return begin(); }
    const_iterator end() const   { return begin(); }

    typedef std::reverse_iterator<iterator> reverse_iterator;
    typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

    reverse_iterator rbegin()             { return reverse_iterator(end()); }
    const_reverse_iterator rbegin() const { return const_reverse_iterator(end()); }
    reverse_iterator rend()               { return reverse_iterator(begin()); }
    const_reverse_iterator rend() const   { return const_reverse_iterator(begin()); }

    reference operator[](size_type i)             { return failed_rangecheck(); }
    const_reference operator[](size_type i) const { return failed_rangecheck(); }
    reference at(size_type i)                     { return failed_rangecheck(); }
    const_reference at(size_type i) const         { return failed_rangecheck(); }
    reference front()                             { return failed_rangecheck(); }
    const_reference front() const                 { return failed_rangecheck(); }
    reference back()                              { return failed_rangecheck(); }
    const_reference back() const                  { return failed_rangecheck(); }

    static size_type size() { return 0; }
    static bool empty() { return true; }
    static size_type max_size() { return 0; }
    enum { static_size = 0 };

    void swap (array<T,0>& y) {}

    const T* data() const { return 0; }
    T* data()             { return 0; }
    T* c_array()          { return 0; }

    template <typename T2>
    array<T, 0>& operator= (const array<T2, 0>& ) { 
      return *this;
    }

    void assign (const T& ) {}

    static reference failed_rangecheck () {
      std::range_error e("attempt to access element of an empty array");
      throw e;
    }
  };

  template<class T, std::size_t N>
  bool operator== (const array<T, N>& x, const array<T, N>& y) {
    return std::equal(x.begin(), x.end(), y.begin());
  }
  template<class T, std::size_t N>
  bool operator< (const array<T, N>& x, const array<T, N>& y) {
    return std::lexicographical_compare(x.begin(),x.end(),y.begin(),y.end());
  }
  template<class T, std::size_t N>
  bool operator!= (const array<T, N>& x, const array<T, N>& y) {
    return !(x == y);
  }
  template<class T, std::size_t N>
  bool operator> (const array<T, N>& x, const array<T, N>& y) {
    return y < x;
  }
  template<class T, std::size_t N>
  bool operator<= (const array<T, N>& x, const array<T, N>& y) {
    return !(y < x);
  }
  template<class T, std::size_t N>
  bool operator>= (const array<T, N>& x, const array<T, N>& y) {
    return !(x < y);
  }

  template<class T, std::size_t N>
  inline void swap (array<T, N>& x, array<T, N>& y) {
    x.swap(y);
  }
} // namespace arx
#endif // ARX_USE_BOOST

namespace arx {
// -------------------------------------------------------------------------- //
// ArrayList
// -------------------------------------------------------------------------- //
  /**
   * ArrayList is a Java-inspired std::vector wrapper with a reference-counted 
   * pointer semantics.
   */
  template<class Type, class Allocator = std::allocator<Type> > class ArrayList {
  private:
    typedef std::vector<Type, Allocator> container_type;
    shared_ptr<container_type> vec;
  public:
    typedef typename container_type::allocator_type allocator_type;
    typedef typename container_type::const_pointer const_pointer;
    typedef typename container_type::const_reference const_reference;
    typedef typename container_type::pointer pointer;
    typedef typename container_type::reference reference;
    typedef typename container_type::value_type value_type;
    typedef typename container_type::size_type size_type;
    typedef typename container_type::difference_type difference_type;
    
    typedef typename container_type::const_iterator const_iterator;
    typedef typename container_type::const_reverse_iterator const_reverse_iterator;
    typedef typename container_type::iterator iterator;
    typedef typename container_type::reverse_iterator reverse_iterator;

    ArrayList(): vec(new container_type()) {}

    reference at(size_type index) {
      return vec->at(index);
    }

    const_reference at(size_type index) const {
      return vec->at(index);
    }

    const_iterator begin() const {
      return vec->begin();
    }

    iterator begin() {
      return vec->begin();
    }

    void clear() {
      vec->clear();
    }

    bool empty() const {
      return vec->empty();
    }

    iterator end() {
      return vec->end();
    }

    const_iterator end() const {
      return vec->end();
    }

    reverse_iterator rbegin() {
      return vec->rbegin();
    }

    const_reverse_iterator rbegin() const {
      return vec->rbegin();
    }

    const_reverse_iterator rend() const {
      return vec->rend();
    }

    reverse_iterator rend() {
      return vec->rend();
    }

    reference back() {
      return vec->back();
    }

    const_reference back() const {
      return vec->back();
    }

    reference front() {
      return vec->front();
    }

    const_reference front() const {
      return vec->back();
    }

    allocator_type get_allocator() const {
      return vec->get_allocator();
    }

    size_type capacity() const {
      return vec->capacity();
    }

    void reserve(size_type count) {
      vec->reserve(count);
    }

    size_type size() const {
      return vec->size();
    }

    size_type max_size() const {
      return vec->max_size();
    }

    value_type& operator[] (size_type index) {
      return vec->operator[](index);
    }

    const value_type& operator[] (size_type index) const {
      return vec->operator[](index);
    }

    void push_back(const value_type& val) {
      vec->push_back(val);
    }

    void push_back() {
      vec->push_back(value_type());
    }

    void pop_back() {
      vec->pop_back();
    }

    void add(const value_type& val) {
      this->push_back(val);
    }

    void add() {
      this->push_back();
    }

    iterator erase(iterator pos) {
      return vec->erase(pos);
    }

    iterator erase(iterator first, iterator last) {
      return vec->erase(first, last);
    }

    iterator erase(size_type pos) {
      return erase(begin() + pos);
    }

    iterator erase(size_type first, size_type last) {
      return erase(begin() + first, begin() + last);
    }

    iterator insert(iterator pos, const value_type& value) {
      return vec->insert(pos, value);
    }

    void insert(iterator pos, size_type count, const value_type& value) {
      return vec->insert(pos, count, value);
    }

    template<class Iter>
    void insert(iterator pos, Iter first, Iter last) {
      return vec->insert(pos, first, last);
    }

    void resize(size_type newSize, value_type filler) {
      vec->resize(newSize, filler);
    }

    void resize(size_type newSize) {
      vec->resize(newSize);
    }

    int indexOf(const value_type& value) {
      for(unsigned int i = 0; i < size(); i++)
        if(this->operator[](i) == value)
          return i;
      return -1;
    }

    int lastIndexOf(const value_type& value) {
      for(unsigned int i = size() - 1; i >= 0; i--)
        if(this->operator[](i) == value)
          return i;
      return -1;
    }

    bool contains(const value_type& value) {
      return indexOf(value) != -1;
    }

    value_type remove(size_type index) {
      value_type value = this->operator[](index);
      erase(index);
      return value;
    }

    bool remove(const value_type& value) {
      int index = indexOf(value);
      if(index < 0)
        return false;
      else {
        erase(index);
        return true;
      }
    }
  };


// -------------------------------------------------------------------------- //
// Map
// -------------------------------------------------------------------------- //
  /**
   * Map is a Java-inspired std::map wrapper with a reference-counted 
   * pointer semantics.
   */
  template<class KeyType, class MappedType, class Comparator = std::less<KeyType>, class Allocator = std::allocator<std::pair<const KeyType, MappedType> > > class Map {
  private:
    typedef std::map<KeyType, MappedType, Comparator, Allocator> container_type;
    shared_ptr<container_type> impl;
  public:
    typedef typename container_type::key_type key_type;
    typedef typename container_type::mapped_type mapped_type;
    typedef typename container_type::referent_type referent_type;
    typedef typename container_type::key_compare key_compare;
    typedef typename container_type::value_compare value_compare;
    typedef typename container_type::allocator_type allocator_type;
    typedef typename container_type::size_type size_type;
    typedef typename container_type::difference_type difference_type;
    typedef typename container_type::pointer pointer;
    typedef typename container_type::const_pointer const_pointer;
    typedef typename container_type::reference reference;
    typedef typename container_type::const_reference const_reference;
    typedef typename container_type::iterator iterator;
    typedef typename container_type::const_iterator const_iterator;
    typedef typename container_type::reverse_iterator reverse_iterator;
    typedef typename container_type::const_reverse_iterator const_reverse_iterator;
    typedef typename container_type::value_type value_type;

    Map(): impl(new container_type()) {}

    explicit Map(const key_compare& c): impl(new container_type(c)) {}

    Map(const key_compare& c, const allocator_type& a): impl(new container_type(c, a)) {}

    template<class Iter>
    Map(Iter first, Iter last): impl(new container_type(first, last)) {}

    template<class Iter>
    Map(Iter first, Iter last, const key_compare& comp): impl(new container_type(first, last, comp)) {}

    template<class Iter>
    Map(Iter first, Iter last, const key_compare& comp, const allocator_type& a) : impl(new container_type(first, last, comp, a)) {}

    void erase(iterator pos) {
      this->impl->erase(pos);
    }

    size_type erase(const key_type& key) {
      return this->impl->erase(key);
    }

    void erase(iterator first, iterator last) {
      return this->impl->erase(first, last);
    }

    mapped_type& operator[](const key_type& key) {
      return this->impl->operator[](key);
    }

    iterator begin() {
      return this->impl->begin();
    }

    const_iterator begin() const {
      return this->impl->begin();
    }

    iterator end() {
      return this->impl->end();
    }

    const_iterator end() const {
      return this->impl->end();
    }

    reverse_iterator rbegin() {
      return this->impl->rbegin();
    }

    const_reverse_iterator rbegin() const {
      return this->impl->rbegin();
    }

    reverse_iterator rend() {
      return this->impl->rend();
    }

    const_reverse_iterator rend() const {
      return this->impl->rend();
    }

    size_type size() const {
      return this->impl->size();
    }

    size_type max_size() const {
      return this->impl->max_size();
    }

    allocator_type get_allocator() const {
      return this->impl->get_allocator();
    }

    bool empty() const {
      return this->impl->empty();
    }

    std::pair<iterator, bool> insert(const value_type& value) {
      return this->impl->insert(value);
    }

    iterator insert(iterator pos, const value_type& value) {
      return this->impl->insert(pos, value);
    }

    template<class Iter>
    void insert(Iter first, Iter last) {
      this->impl->insert(first, last);
    }

    void clear() {
      this->impl->clear();
    }

    value_compare value_comp() const {
      return this->impl->value_comp();
    }

    size_type count(const key_type& key) const {
      return this->impl->count(key);
    }

    iterator lower_bound(const key_type& key) {
      return this->impl->lower_bound(key)
    }

    const_iterator lower_bound(const key_type& key) const {
      return this->impl->lower_bound(key)
    }

    iterator upper_bound(const key_type& key) {
      return this->impl->upper_bound(key)
    }

    const_iterator upper_bound(const key_type& key) const {
      return this->impl->upper_bound(key)
    }

    std::pair<const_iterator, const_iterator> equal_range(const key_type& key) const {
      return this->impl->equal_range(key);
    }

    std::pair<iterator, iterator> equal_range(const key_type& key) {
      return this->impl->equal_range(key);
    }

    iterator find(const key_type& key) {
      return this->impl->find(key);
    }

    const_iterator find(const key_type& key) const {
      return this->impl->find(key);
    }

    bool contains(const key_type& key) const {
      return find(key) != end();
    }
  };


// -------------------------------------------------------------------------- //
// Set
// -------------------------------------------------------------------------- //
  /**
   * Set is a Java-inspired std::set wrapper with a reference-counted 
   * pointer semantics.
   */
  template<class T, class Comparator = std::less<T>, class Allocator = std::allocator<T> > class Set {
  private:
    typedef std::set<T, Comparator, Allocator> container_type;
    shared_ptr<container_type> impl;
  public:
    typedef typename container_type::key_type key_type;
    typedef typename container_type::key_compare key_compare;
    typedef typename container_type::value_compare value_compare;
    typedef typename container_type::allocator_type allocator_type;
    typedef typename container_type::size_type size_type;
    typedef typename container_type::difference_type difference_type;
    typedef typename container_type::pointer pointer;
    typedef typename container_type::const_pointer const_pointer;
    typedef typename container_type::reference reference;
    typedef typename container_type::const_reference const_reference;
    typedef typename container_type::iterator iterator;
    typedef typename container_type::const_iterator const_iterator;
    typedef typename container_type::reverse_iterator reverse_iterator;
    typedef typename container_type::const_reverse_iterator const_reverse_iterator;
    typedef typename container_type::value_type value_type;

    Set(): impl(new container_type()) {}

    explicit Set(const key_compare& c): impl(new container_type(c)) {}

    Set(const key_compare& c, const allocator_type& a): impl(new container_type(c, a)) {}

    template<class Iter>
    Set(Iter first, Iter last): impl(new container_type(first, last)) {}

    template<class Iter>
    Set(Iter first, Iter last, const key_compare& comp): impl(new container_type(first, last, comp)) {}

    template<class Iter>
    Set(Iter first, Iter last, const key_compare& comp, const allocator_type& a) : impl(new container_type(first, last, comp, a)) {}

    void erase(iterator pos) {
      this->impl->erase(pos);
    }

    size_type erase(const key_type& key) {
      return this->impl->erase(key);
    }

    void erase(iterator first, iterator last) {
      return this->impl->erase(first, last);
    }

    iterator begin() {
      return this->impl->begin();
    }

    const_iterator begin() const {
      return this->impl->begin();
    }

    iterator end() {
      return this->impl->end();
    }

    const_iterator end() const {
      return this->impl->end();
    }

    reverse_iterator rbegin() {
      return this->impl->rbegin();
    }

    const_reverse_iterator rbegin() const {
      return this->impl->rbegin();
    }

    reverse_iterator rend() {
      return this->impl->rend();
    }

    const_reverse_iterator rend() const {
      return this->impl->rend();
    }

    size_type size() const {
      return this->impl->size();
    }

    size_type max_size() const {
      return this->impl->max_size();
    }

    allocator_type get_allocator() const {
      return this->impl->get_allocator();
    }

    bool empty() const {
      return this->impl->empty();
    }

    std::pair<iterator, bool> insert(const value_type& value) {
      return this->impl->insert(value);
    }

    iterator insert(iterator pos, const value_type& value) {
      return this->impl->insert(pos, value);
    }

    template<class Iter>
    void insert(Iter first, Iter last) {
      this->impl->insert(first, last);
    }

    void clear() {
      this->impl->clear();
    }

    value_compare value_comp() const {
      return this->impl->value_comp();
    }

    size_type count(const key_type& key) const {
      return this->impl->count(key);
    }

    iterator lower_bound(const key_type& key) {
      return this->impl->lower_bound(key)
    }

    const_iterator lower_bound(const key_type& key) const {
      return this->impl->lower_bound(key)
    }

    iterator upper_bound(const key_type& key) {
      return this->impl->upper_bound(key)
    }

    const_iterator upper_bound(const key_type& key) const {
      return this->impl->upper_bound(key)
    }

    std::pair<const_iterator, const_iterator> equal_range(const key_type& key) const {
      return this->impl->equal_range(key);
    }

    std::pair<iterator, iterator> equal_range(const key_type& key) {
      return this->impl->equal_range(key);
    }

    iterator find(const key_type& key) {
      return this->impl->find(key);
    }

    const_iterator find(const key_type& key) const {
      return this->impl->find(key);
    }

    bool contains(const key_type& key) const {
      return find(key) != end();
    }
  };


// -------------------------------------------------------------------------- //
// Generic*Traits
// -------------------------------------------------------------------------- //
  template<class Type, class PrePushBack, class DefaultConstructor, 
    class CopyConstructor, class Destructor, class Allocator = classnew_allocator<Type> > 
  struct GenericArrayBaseTraits {
    typedef Type value_type;
    typedef PrePushBack pre_push_back_type;
    typedef DefaultConstructor default_constructor_type;
    typedef CopyConstructor copy_constructor_type;
    typedef Destructor destructor_type;
    typedef Allocator allocator_type;
  };

  template<class Type, class PrePushBack, class DefaultConstructor, 
    class CopyConstructor, class Destructor, class Assigner, class Allocator = classnew_allocator<Type> >
  struct GenericArrayTraits: 
    public GenericArrayBaseTraits<Type, PrePushBack, DefaultConstructor, CopyConstructor, Destructor, Allocator> {
    typedef Assigner assigner_type;
  };


// -------------------------------------------------------------------------- //
// Pre* Checkers
// -------------------------------------------------------------------------- //
  template<int growth = 16>
  class Reserve {
  public:
    template<class VectorType>
    void operator()(VectorType& vector, int neededCapacity) {
      if(vector.capacity() < neededCapacity)
        vector.reserve(neededCapacity + growth);
    }
  };

  class NoReserve {
  public:
    template<class VectorType>
    void operator()(VectorType& vector, int neededCapacity) {
      if(vector.capacity() < neededCapacity) {
        return;
      }

      assert(vector.capacity() >= neededCapacity);
    }
  };


// -------------------------------------------------------------------------- //
// Assigners
// -------------------------------------------------------------------------- //
  class CopyAssigner {
  public:
    template<class LeftVectorType, class RightVectorType>
    void operator()(LeftVectorType& left, const RightVectorType& right) {
      left.clear();
      left.reserve(right.size());
      std::copy(right.begin(), right.end(), left.begin());
    }
  };

  class SwapAssigner {
  public:
    template<class LeftVectorType, class RightVectorType>
    void operator()(LeftVectorType& left, const RightVectorType& right) {
      swap(left, const_cast<RightVectorType&>(right));
    }
  };


// -------------------------------------------------------------------------- //
// Constructors
// -------------------------------------------------------------------------- //
  class AllocatorDefaultConstructor {
  public:
    template<class VectorType>
    void operator()(VectorType& vector, typename VectorType::pointer position) {
      vector.get_allocator().construct(position, typename VectorType::value_type());
    }
  };

  class PlacementNewDefaultConstructor {
  public:
    template<class VectorType>
    void operator()(VectorType& vector, typename VectorType::pointer position) {
      ::new (position) typename VectorType::value_type();
    }
  };

  class AllocatorCopyConstructor {
  public:
    template<class VectorType>
    void operator()(VectorType& vector, typename VectorType::pointer position, const typename VectorType::value_type& value) {
      vector.get_allocator().construct(position, value);
    }
  };

  class PlacementNewCopyConstructor {
  public:
    template<class VectorType>
    void operator()(VectorType& vector, typename VectorType::pointer position, const typename VectorType::value_type& value) {
      ::new (position) typename VectorType::value_type(value);
    }
  };

// -------------------------------------------------------------------------- //
// Destructors
// -------------------------------------------------------------------------- //
  class AllocatorDestructor {
  public:
    template<class VectorType>
    void operator()(VectorType& vector, typename VectorType::pointer position) {
      vector.get_allocator().destroy(position);
    }
  };

  class SimpleDestructor {
  public:
    template<class VectorType>
    void operator()(VectorType& vector, typename VectorType::pointer position) {
      typedef typename VectorType::value_type T;
      position->~T();
    }
  };

// -------------------------------------------------------------------------- //
// ArrayExceptions
// -------------------------------------------------------------------------- //
  class ArrayExceptions {
  public:
    static void xLen() {
      ARX_THROW(std::length_error("Array is too long"));
    }

    static void xRan() {
      ARX_THROW(std::out_of_range("invalid Array subscript"));
    }

    static void xInvarg() {
      ARX_THROW(std::invalid_argument("invalid Array argument"));
    }
  };


// -------------------------------------------------------------------------- //
// GenericArrayBase
// -------------------------------------------------------------------------- //
  /**
   * GenericArrayBase is a decomposed base class for vector-like containers. 
   * It's behavior heavily relies on the traits, provided in 
   * Derived::traits_type type.
   *
   * @param Derived derived type.
   */
  template<class Derived, class Traits>
  class GenericArrayBase: public ArrayExceptions {
  public:
    typedef Derived derived_type;
    typedef Traits traits_type;

    typedef typename traits_type::pre_push_back_type       pre_push_back_type;
    typedef typename traits_type::default_constructor_type default_constructor_type;
    typedef typename traits_type::copy_constructor_type    copy_constructor_type;
    typedef typename traits_type::destructor_type          destructor_type;
    typedef typename traits_type::allocator_type           allocator_type;
    typedef typename traits_type::value_type               value_type;

    typedef const value_type* const_pointer;
    typedef const value_type& const_reference;
    typedef value_type* pointer;
    typedef value_type& reference;
    typedef int size_type;
    typedef int difference_type;

    typedef const value_type* const_iterator;
    typedef std::reverse_iterator<const_iterator> const_reverse_iterator;
    typedef value_type* iterator;
    typedef std::reverse_iterator<iterator> reverse_iterator;

    size_type capacity() const {
      return derived().capacity();
    }

    size_type size() const {
      return derived().size();
    }

    const_pointer data() const {
      return derived().data();
    }

    pointer data() {
      return derived().data();
    }

    void reserve(size_type newCapacity) {
      derived().reserve(newCapacity);
    }

    reference at(size_type index) {
      ARX_ASSERT_OR_THROW((index >= 0 && index < size()), xRan());
      return *(data() + index);
    }

    const_reference at(size_type index) const {
      ARX_ASSERT_OR_THROW((index >= 0 && index < size()), xRan());
      return *(data() + index);
    }

    void clear() {
      for(int i = 0; i < size(); ++i)
        destructor_type()(*this, data() + i);
      setSize(0);
    }

    bool empty() const {
      return size() == 0;
    }

    iterator begin() {
      return data();
    }

    const_iterator begin() const {
      return data();
    }

    iterator end() {
      return data() + size();
    }

    const_iterator end() const {
      return data() + size();
    }

    reverse_iterator rbegin() {
      return reverse_iterator(end());
    }

    const_reverse_iterator rbegin() const {
      return const_reverse_iterator(end());
    }

    reverse_iterator rend() {
      return reverse_iterator(begin());
    }

    const_reverse_iterator rend() const {
      return const_reverse_iterator(begin());
    }

    reference back() {
      return at(size() - 1);
    }

    const_reference back() const {
      return at(size() - 1);
    }

    reference front() {
      return at(0);
    }

    const_reference front() const {
      return at(0);
    }

    allocator_type get_allocator() const {
      return allocator();
    }

    void resize(size_type newSize, const value_type& defaultValue) {
      internalResize(newSize, CopyConstructorAdapter(defaultValue))
    }

    void resize(size_type newSize) {
      internalResize(newSize, default_constructor_type());
    }

    size_type max_size() const {
      return static_cast<size_type>(allocator().max_size());
    }

    value_type& operator[] (size_type index) {
      /* We don't use ARX_ASSERT_OR_THROW here - just like in stl. */
      assert(index >= 0 && index < size());
      return *(data() + index);
    }

    const value_type& operator[] (size_type index) const {
      assert(index >= 0 && index < size());
      return *(data() + index);
    }

    void push_back(const value_type& val) {
      pre_push_back_type()(*this, size() + 1);
      copy_constructor_type()(*this, data() + size(), val);
      setSize(size() + 1);
    }

    void pop_back() {
      ARX_ASSERT_OR_THROW((!empty()), xInvarg());
      destructor_type()(*this, data() + size() - 1);
      setSize(size() - 1);
    }

    void assign(size_type count, const value_type& val) {
      clear();
      resize(count, val);
    }

    template<class InputIterator>
    void assign(InputIterator first, InputIterator last) {
      internalAssign(first, last, typename std::iterator_traits<InputIterator>::iterator_category());
    }

    iterator erase(iterator pos) {
      assert(begin <= pos && pos < end());
      std::copy(pos + 1, end(), pos);
      pop_back();
      return pos;
    }

  private:
    class CopyConstructorAdapter {
    public:
      CopyConstructorAdapter(const value_type& value): mValue(value) {}

      void operator()(GenericArrayBase& that, pointer position) {
        copy_constructor_type()(that, position, mValue);
      }
    private:
      const value_type& mValue;
    };


    allocator_type& allocator() {
      return derived().allocator();
    }

    const allocator_type& allocator() const {
      return derived().allocator();
    }

    void setSize(size_type newSize) {
      derived().setSize(newSize);
    }

    derived_type& derived() {
      return static_cast<derived_type&>(*this);
    }

    const derived_type& derived() const {
      return static_cast<const derived_type&>(*this);
    }

    template<class Constructor>
    void internalResize(size_type newSize, Constructor& c) {
      ARX_ASSERT_OR_THROW((newSize >= 0), xInvarg());
      reserve(newSize);

      if(newSize >= size()) {
        for(int i = size(); i < newSize; ++i)
          c(*this, data() + i);
      } else {
        for(int i = newSize; i < size(); ++i)
          destructor_type()(*this, data() + i);
      }
      setSize(newSize);
    }

    template<class Iterator>
    void internalAssign(Iterator first, Iterator last, std::input_iterator_tag /* category */) {
      clear();
      for(; first != last; first++)
        push_back(*first); /* TODO: checked() here. */
    }

    template<class Iterator>
    void internalAssign(Iterator first, Iterator last, std::forward_iterator_tag /* category */) {
      clear();
      reserve(std::distance(first, last));

      int pos = 0;
      for(; first != last; first++, pos++)
        copy_constructor_type()(*this, data() + pos, *first);
      setSize(pos);
    }
  };


// -------------------------------------------------------------------------- //
// GenericArray
// -------------------------------------------------------------------------- //
  /**
   * GenericArray is an analog to std::vector, implemented via 
   * GenericArrayBase.
   *
   * @param Traits traits type. Must define traits for GenericArrayBase and
   *   assigner_type type, which is used for assignment handling.
   */
  template<class Traits> 
  class GenericArray: public GenericArrayBase<GenericArray<Traits>, Traits> {
  public:
    /** Default Constructor. 
     * Constructs an empty GenericArray. */
    GenericArray(): mSize(0), mCapacity(0), mData(NULL) {}

    /** Copy Constructor. */
    GenericArray(const GenericArray& other): mSize(0), mCapacity(0), mData(NULL), 
      mAllocator(other.mAllocator) {
        assigner_type()(*this, other);
    }

    explicit GenericArray(const allocator_type& allocator): 
      mAllocator(allocator), mSize(0), mCapacity(0), mData(NULL) {}

    explicit GenericArray(size_type capacity) {
      initialize(capacity);
    }

    GenericArray(size_type capacity, const allocator_type& allocator): mAllocator(allocator) {
      initialize(capacity);
    }

    GenericArray& operator=(const GenericArray& other) {
      assigner_type()(*this, other);
      return *this;
    }

    ~GenericArray() {
      if(!mData)
        return;

      clear();
      mAllocator.deallocate(mData, mCapacity);
    }

    template<class OtherTraits>
    void swap(GenericArray<OtherTraits>& other) {
      STATIC_ASSERT((is_same<value_type, typename OtherTraits::value_type>::value));
      STATIC_ASSERT((is_same<allocator_type, typename OtherTraits::allocator_type>::value));
      using std::swap;
      if(mAllocator == other.mAllocator) {
        swap(mData, other.mData);
        swap(mSize, other.mSize);
        swap(mCapacity, other.mCapacity);
      } else {
        GenericArray<Traits> tmp = other;
        other = *this;
        *this = tmp;
      }
    }

    size_type capacity() const {
      return mCapacity;
    }

    size_type size() const {
      return mSize;
    }

    void reserve(size_type newCapacity) {
      ARX_ASSERT_OR_THROW((newCapacity < max_size()), xLen());
      ARX_ASSERT_OR_THROW((newCapacity >= 0), xInvarg());

      if(newCapacity <= mCapacity)
        return;

      pointer newData = mAllocator.allocate(newCapacity);
      ARX_TRY
        for(int i = 0; i < mSize; ++i)
          copy_constructor_type()(*this, newData + i, operator[](i));
        for(int i = 0; i < mSize; ++i)
          destructor_type()(*this, mData + i);
      ARX_CATCH_ALL
        mAllocator.deallocate(newData, newCapacity);
      ARX_RETHROW;
      ARX_TRY_END

      if(mData != NULL)
        mAllocator.deallocate(mData, mCapacity);
      mCapacity = newCapacity;
      mData = newData;
    }

    const_pointer data() const {
      return mData;
    }

    pointer data() {
      return mData;
    }

  private:
    allocator_type& allocator() {
      return mAllocator;
    }

    const allocator_type& allocator() const {
      return mAllocator;
    }

    void setSize(size_type newSize) {
      mSize = newSize;
    }

    void initialize(size_type capacity) {
      mSize = 0;
      mCapacity = capacity;
      mData = mAllocator.allocate(capacity);
    }

    typedef typename traits_type::assigner_type assigner_type;
    friend class GenericArrayBase<GenericArray<traits_type>, traits_type>;

    allocator_type mAllocator;
    size_type mSize;
    size_type mCapacity;
    pointer mData;
  };

  template<class FirstTraits, class SecondTraits> 
  inline void swap(GenericArray<FirstTraits>& a, GenericArray<SecondTraits>& b) {
    a.swap(b);
  }


// -------------------------------------------------------------------------- //
// ArrayTail
// -------------------------------------------------------------------------- //
  template<class Array>
  class ArrayTail: public GenericArrayBase<ArrayTail<Array>, 
    GenericArrayBaseTraits<typename Array::value_type, void, void, void, void, typename Array::allocator_type> > {
  public:
    typedef Array array_type;

    /** Default Constructor. 
     * Constructs an uninitialized ArrayTail. */
    ArrayTail(): mSrc(NULL), mShift(0) {}

    /* Default copy constructor is OK. */

    explicit ArrayTail(array_type& src, size_type shift): mSrc(&src), mShift(shift) {
      assert(shift >= 0);
    }

    explicit ArrayTail(ArrayTail& src, size_type shift): mSrc(src.mSrc), mShift(shift + src.mShift) {
      assert(shift >= 0);
    }

    /* Default operator= is OK. */

    /* Default destructor is OK. */

    template<class OtherTraits>
    void swap(ArrayTail& other) {
      using std::swap;
      swap(mSrc, other.mSrc);
      swap(mShift, other.mShift);
    }

    size_type capacity() const {
      return mSrc->capacity() - mShift;
    }

    /** This one can return negative value! */
    size_type size() const {
      return mSrc->size() - mShift;
    }

    void reserve(size_type newCapacity) {
      mSrc->reserve(static_cast<array_size_type>(newCapacity + mShift));
    }

    const_pointer data() const {
      return mSrc->data() + mShift;
    }

    pointer data() {
      return mSrc->data() + mShift;
    }

    /* This one is just for convenience. */
    ArrayTail tail(size_type shift) {
      return ArrayTail(*this, shift);
    }

    /* Now we have to override some of the GenericArrayBase methods
     * because we cannot provide setSize(size_type) method... */
    void clear() {
      mSrc->resize(static_cast<array_size_type>(mShift));
    }

    void resize(size_type newSize, const value_type& defaultValue) {
      mSrc->resize(static_cast<array_size_type>(newSize + mShift), defaultValue);
    }

    void resize(size_type newSize) {
      mSrc->resize(static_cast<array_size_type>(newSize + mShift));
    }

    void push_back(const value_type& val) {
      mSrc->push_back(val);
    }

    void pop_back() {
      mSrc->pop_back();
    }

    iterator erase(iterator pos) {
      return mSrc->erase(pos);
    }

  private:
    typedef typename array_type::size_type array_size_type;

    allocator_type allocator() {
      /* This approach seems to be better, since it's std-compliant. */
      return mSrc->get_allocator();
    }

    friend class GenericArrayBase<ArrayTail<array_type>, traits_type>;

    array_type* mSrc;
    size_type mShift;
  };


// -------------------------------------------------------------------------- //
// GenericStaticArray
// -------------------------------------------------------------------------- //
  template<int N, class Traits>
  class GenericStaticArray: public GenericArrayBase<GenericStaticArray<N, Traits>, Traits> {
  public:
    /** Default Constructor. 
    * Constructs an empty GenericArray. */
    GenericStaticArray(): mSize(0) {}

    ~GenericStaticArray() {
      clear();
    }

    size_type capacity() const {
      return N;
    }

    size_type size() const {
      return mSize;
    }

    void reserve(size_type newCapacity) {
      ARX_ASSERT_OR_THROW((newCapacity < max_size()), xLen());
      ARX_ASSERT_OR_THROW((newCapacity >= 0), xInvarg());
    }

    const_pointer data() const {
      return reinterpret_cast<pointer>(mData);
    }

    pointer data() {
      return reinterpret_cast<pointer>(mData);
    }

    size_type max_size() const {
      return N;
    }

  private:
    GenericStaticArray(const GenericStaticArray& other);
    GenericStaticArray& operator= (const GenericStaticArray& other);

    allocator_type& allocator() {
      return allocator_type();
    }

    const allocator_type& allocator() const {
      return allocator_type();
    }

    void setSize(size_type newSize) {
      mSize = newSize;
    }

    void initialize(size_type capacity) {
      mSize = 0;
    }

    friend class GenericArrayBase<GenericStaticArray<N, traits_type>, traits_type>;

    size_type mSize;
    unsigned char mData[N * sizeof(value_type)];
  };


// -------------------------------------------------------------------------- //
// GenericStaticArray derived classes
// -------------------------------------------------------------------------- //
  template<class Type, int N>
  class StaticFastArray: 
    public GenericStaticArray<N, GenericArrayBaseTraits<Type, NoReserve, PlacementNewDefaultConstructor, 
      PlacementNewCopyConstructor, SimpleDestructor, arx::classnew_allocator<Type> > > {
  public:
    /* */
  };


// -------------------------------------------------------------------------- //
// GenericArray derived classes
// -------------------------------------------------------------------------- //
#define ARX_GENERICARRAY_INHERIT_CONSTRUCTORS(THIS_TYPE)                        \
  THIS_TYPE() {}                                                                \
  THIS_TYPE(const THIS_TYPE& other): GenericArray(other) {}                     \
  explicit THIS_TYPE(const allocator_type& allocator): GenericArray(allocator) {} \
  explicit THIS_TYPE(size_type capacity): GenericArray(capacity) {}             \
  THIS_TYPE(size_type capacity, const allocator_type& allocator): GenericArray(capacity, allocator) {}

  /**
   * FastArray is a std::vector-like class. Key differences from std::vector are:
   * <ul>
   * <li> FastArray doesn't support automatic resizing on push_back. You should
   *      ensure that there is enough memory yourself.
   * <li> FastArray implements RAII by enforcing the semantics of strict 
   *      ownership. Therefore, operator= modifies the right-hand value. This
   *      is extremely dangerous, so think twice before assigning anything,
   *      or using FastArray in stl.
   * <li> FastArray uses nonstandard default allocator, which supports 
   *      overloaded operator new, therefore allowing to store types with 
   *      alignment.
   * <li> FastArray makes direct data manipulation possible through data()
   *      member function.
   * </ul>
   *
   * @param Type type to store in array.
   * @param Allocator allocator to use.
   */
  template<class Type, class Allocator = classnew_allocator<Type> >
  class FastArray: 
    public GenericArray<GenericArrayTraits<Type, NoReserve, PlacementNewDefaultConstructor, 
      PlacementNewCopyConstructor, SimpleDestructor, SwapAssigner, Allocator> > {
  public:
    ARX_GENERICARRAY_INHERIT_CONSTRUCTORS(FastArray)
  };


  /**
   * CheckedArray is an analog of FastArray. The only difference is that
   * CheckedArray supports automatic resizing on push_back.
   */
  template<class Type, class Allocator = classnew_allocator<Type> >
  class CheckedArray:
    public GenericArray<GenericArrayTraits<Type, Reserve<16>, PlacementNewDefaultConstructor, 
      PlacementNewCopyConstructor, SimpleDestructor, SwapAssigner, Allocator> > {
  public:
    ARX_GENERICARRAY_INHERIT_CONSTRUCTORS(CheckedArray)
  };

} // namespace arx

#endif
