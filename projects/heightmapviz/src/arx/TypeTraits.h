#ifndef __ARX_TYPETRAITS_H__
#define __ARX_TYPETRAITS_H__

#include "config.h"
#include "Mpl.h"

#ifdef ARX_USE_BOOST
#include <boost/type_traits/is_base_and_derived.hpp>
namespace arx {
  using boost::is_same;
}
#else

namespace arx {
// -------------------------------------------------------------------------- //
// is_same
// -------------------------------------------------------------------------- //
  template<class A, class B> struct is_same {
    enum {value = false};
  };
  template<class T> struct is_same<T, T> {
    enum {value = true};
  };
} // namespace arx

#endif

namespace arx {
// -------------------------------------------------------------------------- //
// is_scalar
// -------------------------------------------------------------------------- //
#define ARX_FOREACH_SIMPLE_TYPE(MACRO)                                          \
  MACRO(char)                                                                   \
  MACRO(short)                                                                  \
  MACRO(int)                                                                    \
  MACRO(long)                                                                   \
  MACRO(long long)                                                              \
  MACRO(unsigned char)                                                          \
  MACRO(unsigned short)                                                         \
  MACRO(unsigned int)                                                           \
  MACRO(unsigned long)                                                          \
  MACRO(unsigned long long)                                                     \
  MACRO(float)                                                                  \
  MACRO(double)                                                                 \
  MACRO(long double)                                                            \
  MACRO(const char)                                                             \
  MACRO(const short)                                                            \
  MACRO(const int)                                                              \
  MACRO(const long)                                                             \
  MACRO(const long long)                                                        \
  MACRO(const unsigned char)                                                    \
  MACRO(const unsigned short)                                                   \
  MACRO(const unsigned int)                                                     \
  MACRO(const unsigned long)                                                    \
  MACRO(const unsigned long long)                                               \
  MACRO(const float)                                                            \
  MACRO(const double)                                                           \
  MACRO(const long double)                                                      \

  template<class T> struct is_scalar {
    typedef false_ type;
    enum {value = false};
  };

#define ARX_IS_SCALAR_TYPE_SPECIALIZE2(_T, _X)                                  \
  template<_T> struct is_scalar<_X> {                                           \
    typedef true_ type;                                                         \
    enum {value = true};                                                        \
  };                                                                            \

#define ARX_IS_SCALAR_TYPE_SPECIALIZE(_X)                                       \
  ARX_IS_SCALAR_TYPE_SPECIALIZE2(ARX_EMPTY(), _X)                               \

ARX_FOREACH_SIMPLE_TYPE(ARX_IS_SCALAR_TYPE_SPECIALIZE)
ARX_IS_SCALAR_TYPE_SPECIALIZE2(class T, T*)
ARX_IS_SCALAR_TYPE_SPECIALIZE2(class T, const T*)
ARX_IS_SCALAR_TYPE_SPECIALIZE2(class T, T* const)
ARX_IS_SCALAR_TYPE_SPECIALIZE2(class T, const T* const)
#undef ARX_IS_SCALAR_TYPE_SPECIALIZE
#undef ARX_IS_SCALAR_TYPE_SPECIALIZE2


// -------------------------------------------------------------------------- //
// StoreModes
// -------------------------------------------------------------------------- //
  /* TODO replace with add_reference etc... */
  template<class T> struct reference_ {
    typedef T& type;
  };
  template<class T> struct const_reference_ {
    typedef T& type;
  };
  template<class T> struct pointer_ {
    typedef T* type;
  };
  template<class T> struct identity_ {
    typedef T type;
  };

} // namespace arx

#endif // __ARX_TYPETRAITS_H__
