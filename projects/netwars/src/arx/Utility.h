#ifndef __ARX_UTILITY_H__
#define __ARX_UTILITY_H__

#include "config.h"

#ifdef ARX_USE_BOOST
#  include <boost/noncopyable.hpp>
namespace arx {
  using boost::noncopyable;
}

#else // ARX_USE_BOOST

namespace arx {
  namespace noncopyable_adl_protected { // protection from unintended ADL
    /**
    * noncopyable class
    */
    class noncopyable {
    protected:
      noncopyable() {}
      ~noncopyable() {}
    private:
      noncopyable( const noncopyable& );
      const noncopyable& operator=( const noncopyable& );
    };
  }

  typedef noncopyable_adl_protected::noncopyable noncopyable;
}
#endif // ARX_USE_BOOST


namespace arx {
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

} // namespace arx

#endif // __ARX_UTILITY_H__