#ifndef __ARX_CONFIG_H__
#define __ARX_CONFIG_H__

// -------------------------------------------------------------------------- //
// Config: General & Global
// -------------------------------------------------------------------------- //
/** @def ARX_USE_BOOST
 * Define to use implementation of some features from boost library instead of hand-coded one.
 * Normally boost classes offer wider functionality and sometimes produce faster code. */

/** @def ARX_DISABLE_THREADS
 * Define to disable thread support and thread safety in ArX library. May result in
 * faster code for single-threaded applications. */

/** @def ARX_DISABLE_EXCEPTIONS
 * Define to disable exception handling in ArX Library. When defined, ArX library
 * won't throw exceptions, but also won't try handle exceptions in user code. 
 * Note that in this case all the error will become non-interceptable, since
 * error handling code will revert to assertions instead of exceptions. */


// -------------------------------------------------------------------------- //
// Config: LinearAlgebra module
// -------------------------------------------------------------------------- //
/** @def ARX_USE_EIGEN
 * Define to use Eigen library in place of ArX LinearAlgebra module. Eigen is a highly-optimized
 * library with a rich set of features, which generally produces better code that ArX LinearAlgebra. */

/** @def ARX_UNROLL_COST_LIMIT
 * Cost limit for templated cycle unrolling in LinearAlgebra module. Default value is 100. */
#ifndef ARX_UNROLL_COST_LIMIT
#  define ARX_UNROLL_COST_LIMIT 100
#endif


// -------------------------------------------------------------------------- //
// Config: Image module
// -------------------------------------------------------------------------- //
/** @def ARX_USE_OPENCV
 * Define to use OpenCV routines for image saving / loading in ArX Image Processing library. 
 * If not defined, only 24-bit bmp files will be supported. */

/** @def ARX_USE_IPPI
 * Use ippi for image processing? If not defined, hand-coded routines will be used
 * instead of ippi ones. This may lead to a significant slowdown in some cases. */

/** @def ARX_USE_CIPPIMAGE
 * Define CIppImage-compatible interfaces? */

/** @def ARX_USE_IPPIMALLOC 
 * Use ippiMalloc for image allocation? */

/** @def ARX_GAUSS_TRUNCATE
 * Truncate gaussian kernel at <tt>sigma * ARX_GAUSS_TRUNCATE</tt> pixels away from center */
#ifndef ARX_GAUSS_TRUNCATE
#  define ARX_GAUSS_TRUNCATE 4.0f
#endif


// -------------------------------------------------------------------------- //
// Guess defines - do not change
// -------------------------------------------------------------------------- //
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#  define ARX_WIN32
#elif defined(__linux__)
#  define ARX_LINUX
#endif

#if defined(__INTEL_COMPILER) || defined(__ICL) || defined(__ICC) || defined(__ECC)
#  define ARX_ICC
#elif defined __GNUC__
#  define ARX_GCC
#elif defined _MSC_VER
#  define ARX_MSVC
#endif

#if defined(_DEBUG) || !defined(NDEBUG)
#  define DEBUG
#endif


// -------------------------------------------------------------------------- //
// Derived defines
// -------------------------------------------------------------------------- //
#ifdef ARX_USE_BOOST
#  ifdef ARX_DISABLE_THREADS
#    define BOOST_DISABLE_THREADS
#  endif
#endif

#if defined(ARX_USE_CIPPIMAGE) || defined(ARX_USE_IPPIMALLOC)
#  define ARX_USE_IPPI
#endif

#ifdef ARX_DISABLE_EXCEPTIONS
#  define ARX_TRY        {{
#  define ARX_CATCH_ALL  } if(0) {
#  define ARX_CATCH(X)   } if(0) {
#  define ARX_RETHROW    
#  define ARX_THROW(X)   
#  define ARX_END_TRY    }}
#  define ARX_ASSERT_OR_THROW(CONDITION, HANDLER)                               \
  assert(CONDITION);
#else
#  define ARX_TRY        try {
#  define ARX_CATCH_ALL  } catch(...) {
#  define ARX_CATCH(X)   } catch(X) {
#  define ARX_RETHROW    throw;
#  define ARX_THROW(X)   throw(X);
#  define ARX_END_TRY    }
#  define ARX_ASSERT_OR_THROW(CONDITION, HANDLER)                               \
  if(!(CONDITION)) {                                                            \
    HANDLER;                                                                    \
  }
#endif



// -------------------------------------------------------------------------- //
// Some useful defines
// -------------------------------------------------------------------------- //
#ifdef ARX_MSVC
#  define FORCEINLINE __forceinline
# else
#  define FORCEINLINE inline
#endif


#endif
