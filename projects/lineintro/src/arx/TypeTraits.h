/** @file ArX TypeTraits Library.
 * 
 * Implements some of type traits offered by Boost Type Traits Libraty.
 */
#ifndef __ARX_TYPETRAITS_H__
#define __ARX_TYPETRAITS_H__

#include "config.h"
#include "Mpl.h"

#ifdef ARX_USE_BOOST
#include <boost/type_traits/is_same.hpp>
#include <boost/type_traits/alignment_of.hpp>
#include <boost/type_traits/remove_reference.hpp>
#include <boost/type_traits/remove_pointer.hpp>
#include <boost/type_traits/add_reference.hpp>
#include <boost/type_traits/add_volatile.hpp>
#include <boost/type_traits/add_const.hpp>
#include <boost/type_traits/add_cv.hpp>
#include <boost/type_traits/add_pointer.hpp>
namespace arx {
  using boost::is_same;

  using boost::alignment_of;

  using boost::remove_reference;
  using boost::remove_pointer;

  using boost::add_reference;
  using boost::add_volatile;
  using boost::add_const;
  using boost::add_cv;
  using boost::add_pointer;
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


// -------------------------------------------------------------------------- //
// alignment_of
// -------------------------------------------------------------------------- //
  namespace detail {
#ifdef ARX_MSVC
#  pragma warning(push)
#  pragma warning(disable: 4324) /* structure was padded due to __declspec(align()) */
#endif
    template<class T>
    struct alignment_of_struct {
      char c;
      T t;
    };
#ifdef ARX_MSVC
#  pragma warning(pop)
#endif

    template<class T>
    struct alignment_of_impl {
#ifndef ARX_ALIGNMENT_OF
      enum {
        value = min_<minus<sizeof_<alignment_of_struct<T> >, sizeof_<T> >, sizeof_<T> >::value
      };
#else
      enum {
        value = ARX_ALIGNMENT_OF(T)
      };
#endif
    };
  } // namespace detail 

  template<class T> struct alignment_of: public int_<detail::alignment_of_impl<T>::value> {};

  template<class T> struct alignment_of<T&>: public alignment_of<T*> {};

  template<> struct alignment_of<void>: public int_<0> {};
  template<> struct alignment_of<void const>: public int_<0> {};
  template<> struct alignment_of<void volatile>: public int_<0> {};
  template<> struct alignment_of<void const volatile>: public int_<0> {};


#define ARX_TT_DEF(ARG, NAME, TYPE) \
  template<ARG> struct NAME { typedef TYPE type; };
#define ARX_TT_SPEC_1_1(ARG, NAME, SPEC, TYPE) \
  template<ARG> struct NAME<SPEC> { typedef TYPE type; };
#define ARX_TT_SPEC_0_1(NAME, SPEC, TYPE) \
  template<> struct NAME<SPEC> { typedef TYPE type; };
// -------------------------------------------------------------------------- //
// remove_*
// -------------------------------------------------------------------------- //
  ARX_TT_DEF(class T, remove_reference, T)
  ARX_TT_SPEC_1_1(class T, remove_reference, T&, T)

  ARX_TT_DEF(class T, remove_pointer, T)
  ARX_TT_SPEC_1_1(class T,remove_pointer, T*, T)
  ARX_TT_SPEC_1_1(class T,remove_pointer, T* const, T)
  ARX_TT_SPEC_1_1(class T,remove_pointer, T* volatile, T)
  ARX_TT_SPEC_1_1(class T,remove_pointer, T* const volatile, T)


// -------------------------------------------------------------------------- //
// add_*
// -------------------------------------------------------------------------- //
#ifdef ARX_MSVC
#   pragma warning(push)
#   pragma warning(disable: 4181) /* warning C4181: qualifier applied to reference type ignored */
#endif 

  ARX_TT_DEF(class T, add_const, T const)
  ARX_TT_SPEC_1_1(class T, add_const, T&, T&)

  ARX_TT_DEF(class T, add_cv, T const volatile)
  ARX_TT_SPEC_1_1(class T, add_cv, T&, T&)

  template<class T> struct add_pointer {
    typedef typename remove_reference<T>::type no_ref_type;
    typedef no_ref_type* type;
  };

  ARX_TT_DEF(class T, add_reference, T&)
  ARX_TT_SPEC_1_1(class T, add_reference, T&, T&)
  ARX_TT_SPEC_0_1(add_reference, void, void)
  ARX_TT_SPEC_0_1(add_reference, void const, void const)
  ARX_TT_SPEC_0_1(add_reference, void volatile, void volatile)
  ARX_TT_SPEC_0_1(add_reference, void const volatile, void const volatile)

  ARX_TT_DEF(class T, add_volatile, T volatile)
  ARX_TT_SPEC_1_1(class T, add_volatile, T&, T&)

#ifdef ARX_MSVC
#   pragma warning(pop)
#endif 

#undef ARX_TT_DEF
#undef ARX_TT_SPEC_1_1
#undef ARX_TT_SPEC_0_1

} // namespace arx

#endif

#endif // __ARX_TYPETRAITS_H__
