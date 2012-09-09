/** @file ArX Utility Library.
 *
 * Utility Classes and more.
 */
#ifndef __ARX_UTILITY_H__
#define __ARX_UTILITY_H__

#include "config.h"
#include <functional>

#ifdef ARX_USE_BOOST
#  include <boost/utility.hpp>
namespace arx {
  using boost::addressof;
}
#else
namespace arx {
  namespace detail {
    template<class T> struct addressof_impl {
      static inline T* f(T& v, long) {
        return reinterpret_cast<T*>
          (&const_cast<char&>(reinterpret_cast<const volatile char &>(v)));
      }

      /* This magic was copied directly from boost implementation...
       * And I should admit that I don't understand, what is it for. */
      static inline T* f(T* v, int) {
        return v;
      }
    };
  } // namespace detail

  template<class T> T* addressof(T& v) {
    return detail::addressof_impl<T>::f(v, 0);
  }
} // namespace arx
#endif

namespace arx {
  namespace noncopyable_adl_protected { // protection from unintended ADL
    /**
     * noncopyable class. Subclass to hide your copy constructor and operator=.
     *
     * Note that original implementation included empty destructor, which for 
     * some strange reason was getting in the way of MSVC 8.0 optimizer, making 
     * it unable to inline and eliminate the child class's constructor. Same 
     * goes for nonassignable class. That's why even if ARX_USE_BOOST is 
     * defined, I don't use noncopyable from boost - boost implementation 
     * includes this empty destructor. It took me a lot of digging into 
     * assembly to find this bug (?).
     */
    class noncopyable {
    public:
      noncopyable() {};
    private:
      noncopyable( const noncopyable& );
      const noncopyable& operator=( const noncopyable& );
    };
  } // namespace noncopyable_adl_protected

  typedef noncopyable_adl_protected::noncopyable noncopyable;

  namespace nonassignable_adl_protected { // protection from unintended ADL
    /**
     * nonassignable class. Subclass to hide your operator=.
     *
     * Also see note on noncopyable class.
     *
     * @see noncopyable
     */
    class nonassignable {
    private:
      const nonassignable& operator=( const nonassignable& );
    };
  } // namespace nonassignable_adl_protected

  typedef nonassignable_adl_protected::nonassignable nonassignable;


  /**
   * ValueType template is used to determine the type of the result of operator[] of type T
   */
  template<class T> class ValueType {
  public:
    typedef typename T::value_type type;
  };
  template<class T> class ValueType<T*> {
  public:
    typedef T type;
  };
  template<class T, int N> class ValueType<T[N]> {
  public:
    typedef T type;
  };


  /**
   * Square template
   */
  template<class T> T sqr(T x) {
    return x * x;
  }

  /**
   * UnorderedPair stores an unordered pair of two values of the same type.
   */
  template<class T, class Comparator = std::less<T> > class UnorderedPair: private Comparator {
  public:
    T first;
    T second;

    UnorderedPair() {}

    UnorderedPair(const T& a, const T& b, const Comparator& c = Comparator()): 
      Comparator(c), first(Comparator::operator()(a, b) ? a : b), second(Comparator::operator()(a, b) ? b : a) {}

    bool operator<(const UnorderedPair& that) const {
      return Comparator::operator()(this->first, that.first) ||
        (!Comparator::operator()(that.first, this->first) && Comparator::operator()(this->second, that.second));
    }

    bool operator>(const UnorderedPair& that) const {
      return that < *this;
    }

    bool operator>=(const UnorderedPair& that) const {
      return !(*this < that);
    }

    bool operator<=(const UnorderedPair& that) const {
      return !(*this > that);
    }

    bool operator==(const UnorderedPair& that) const {
      return *this >= that && *this <= that;
    }

    bool operator!=(const UnorderedPair& that) const {
      return !(*this == that);
    }
  };

  template<class T, class Comparator> 
  UnorderedPair<T, Comparator> make_upair(const T& a, const T& b, const Comparator& c) {
    return UnorderedPair<T, Comparator>(a, b, c);
  }

  template<class T> 
  UnorderedPair<T> make_upair(const T& a, const T& b) {
    return UnorderedPair<T>(a, b);
  }

} // namespace arx

#endif // __ARX_UTILITY_H__
