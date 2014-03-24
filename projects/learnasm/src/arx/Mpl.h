#ifndef __ARX_MPL_H__
#define __ARX_MPL_H__

#include "config.h"
#include "Preprocessor.h"

#ifdef ARX_USE_BOOST
#  include <boost/mpl/bool.hpp>
#  include <boost/mpl/int.hpp>
#  include <boost/mpl/long.hpp>
#  include <boost/mpl/integral_c.hpp>
#  include <boost/type_traits.hpp>
#  include <boost/mpl/if.hpp>
#  include <boost/mpl/logical.hpp>
#  include <boost/mpl/identity.hpp>

namespace arx {
  using boost::mpl::bool_;
  using boost::mpl::int_;
  using boost::mpl::long_;
  using boost::mpl::integral_c;
  using boost::mpl::true_;
  using boost::mpl::false_;

  using boost::is_same;

  using boost::mpl::if_;
  using boost::mpl::if_c;

  using boost::mpl::and_;
  using boost::mpl::or_;
  using boost::mpl::not_;

  using boost::mpl::identity;
}
#else // ARX_USE_BOOST

namespace arx {
// -------------------------------------------------------------------------- //
// Integral constant types
// -------------------------------------------------------------------------- //
#define ARX_DEFINE_INTEGRAL_CONST_TYPE2(TYPE, WRAPPER, TEMPLATE)                \
  TEMPLATE struct WRAPPER {                                                     \
    enum {value = param};                                                       \
    typedef WRAPPER type;                                                       \
    typedef TYPE value_type;                                                    \
    operator TYPE () const {return this->value;}                                \
  };                                                                            \

#define ARX_DEFINE_INTEGRAL_CONST_TYPE(TYPE, WRAPPER)                           \
  ARX_DEFINE_INTEGRAL_CONST_TYPE2(TYPE, WRAPPER, template<TYPE param>)          \

  ARX_DEFINE_INTEGRAL_CONST_TYPE(bool, bool_)
  ARX_DEFINE_INTEGRAL_CONST_TYPE(int, int_)
  ARX_DEFINE_INTEGRAL_CONST_TYPE(long, long_)
  ARX_DEFINE_INTEGRAL_CONST_TYPE2(T, integral_c, template<class T ARX_COMMA T param>)
#undef ARX_DEFINE_INTEGRAL_CONST_TYPE
#undef ARX_DEFINE_INTEGRAL_CONST_TYPE2

  typedef bool_<true> true_;
  typedef bool_<false> false_;


// -------------------------------------------------------------------------- //
// is_same
// -------------------------------------------------------------------------- //
  template<class A, class B> struct is_same {
    enum {value = false};
  };
  template<class T> struct is_same<T, T> {
    enum {value = true};
  };


// -------------------------------------------------------------------------- //
// if
// -------------------------------------------------------------------------- //
  template<bool Condition, class ThenType, class ElseType> struct if_c {
    typedef ThenType type;
  };

  template<class ThenType, class ElseType> struct if_c<false, ThenType, ElseType> {
    typedef ElseType type;
  };

  template<class Condition, class ThenType, class ElseType> struct if_ {
    typedef typename if_c<!!Condition::value, ThenType, ElseType>::type type;
  };

// -------------------------------------------------------------------------- //
// logical ops
// -------------------------------------------------------------------------- //
#define ARX_DEFINE_LOGICAL_METAFUNC(TEMPLATE, NAME, VALUE)                      \
  TEMPLATE struct NAME {                                                        \
    enum {value = VALUE};                                                       \
    typedef bool_<value> type;                                                  \
    typedef bool value_type;                                                    \
    operator bool() const {return this->value;}                                 \
  };                                                                            \

#define ARX_DEFINE_LOGICAL_METAFUNC_1(NAME, OP)                                 \
  ARX_DEFINE_LOGICAL_METAFUNC(template<class A>, NAME, OP A::value)             \

#define ARX_DEFINE_LOGICAL_METAFUNC_N(NAME, OP, DEFVALUE)                       \
  ARX_DEFINE_LOGICAL_METAFUNC(template<class A ARX_COMMA class B ARX_COMMA class C = DEFVALUE ARX_COMMA class D = DEFVALUE ARX_COMMA class E = DEFVALUE>, NAME, A::value OP B::value OP C::value OP D::value OP E::value) \

  ARX_DEFINE_LOGICAL_METAFUNC_N(and_, &&, true_)
  ARX_DEFINE_LOGICAL_METAFUNC_N(or_, ||, false_)
  ARX_DEFINE_LOGICAL_METAFUNC_1(not_, !)
#undef ARX_DEFINE_LOGICAL_METAFUNC
#undef ARX_DEFINE_LOGICAL_METAFUNC_1
#undef ARX_DEFINE_LOGICAL_METAFUNC_N

// -------------------------------------------------------------------------- //
// StoreModes
// -------------------------------------------------------------------------- //
  template<class T> struct identity {
    typedef T type;
  };
}
#endif // ARX_USE_BOOST





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

#endif