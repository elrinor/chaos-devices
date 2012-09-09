#ifndef __ARX_MEMORY_H__
#define __ARX_MEMORY_H__

#include "config.h"
#include <malloc.h>
#include <memory>
#include <cstdlib>
#include <cstddef>
#include "static_assert.h"
#include "TypeTraits.h"

/** 
 * @def ALIGNOF
 *
 * @returns alignment in bytes of the given type. 
 */
#ifdef ARX_USE_BOOST
#  include <boost/type_traits/alignment_of.hpp>
#  define ALIGNOF(TYPE) (boost::alignment_of<TYPE>::value)
#else
#  if defined(ARX_MSVC) || defined(ARX_ICC)
#    define ALIGNOF(TYPE) __alignof(TYPE)
#  elif defined(ARX_GCC)
#    define ALIGNOF(TYPE) __alignof__(TYPE)
#  else
#    /* Not supported. */
#  endif
#endif


/**
 * @def ALIGN
 * This attribyte forces the data to be aligned.
 *
 * @param BYTES alignment.
 */
#if defined(ARX_GCC)
#  define ALIGN(BYTES) __attribute__((aligned(BYTES)))
#elif defined(ARX_MSVC) || defined(ARX_ICC)
#  define ALIGN(BYTES) __declspec(align(BYTES))
#else
#  /* Not supported. */
#endif


#ifdef ARX_LINUX
/* posix_memalign isn't always defined in the stdlib header */
extern "C" int posix_memalign(void**, size_t, size_t) throw ();
#endif


namespace arx {
  /**
   * Allocates an aligned chunk of memory.
   *
   * @param size size of memory chunk, in bytes
   * @param align alignment, in bytes
   * @returns pointer to a newly allocated aligned chunk of memory, or NULL in case of failure
   */
  inline void* aligned_malloc(std::size_t size, int align) {
#if defined(ARX_LINUX)
    void* result;
    if (posix_memalign(&result, align, size) != 0)
      return NULL;
    else
      return result;
#elif defined(ARX_WIN32)
    return _aligned_malloc(size, align);
#else
    /* generic version built on top of malloc */
    if ((align & (align - 1)) != 0)
      return NULL; /* not a power of 2 */
    if ((align & (sizeof(void*) - 1)) != 0)
      return NULL; /* not multiple of sizeof(void*) */

    char* ptr = static_cast<char*>(malloc(size + align + sizeof(char*) - 1));
    char* p = reinterpret_cast<char*>(reinterpret_cast<intptr_t>(ptr + align - 1) & -align);
    if(p - ptr < sizeof(char*))
      p += align;
    *(static_cast<char **>(p) - 1) = ptr;
    
    return p;
#endif
  }


  /** 
   * Frees memory allocated with aligned_malloc. Using this function for releasing memory 
   * not allocated by aligned_malloc may result in undefined behavior. Calling this function
   * for NULL pointer results in no action.
   *
   * @param ptr pointer to a memory block to free
   */
  inline void aligned_free(void* ptr) {
#if defined(ARX_LINUX)
    free(ptr);
#elif defined(ARX_WIN32)
    _aligned_free(ptr);
#else
    if(ptr == NULL)
      return;
    free(*(static_cast<char **>(ptr) - 1));
#endif
  }


  /**
   * This class is provided for those who want to use aligned allocation with operator new.
   * If you want your class Foo to use aligned heap allocation, then just include WithAlignedOperatorNew in
   * the list of it's parents. Use public inheritance.
   *
   * @param align desired alignment, in bytes.
   */
  template<int align>
  struct WithAlignedOperatorNew {
    STATIC_ASSERT(((align & (align - 1)) == 0)); /* is a power of 2 */
    STATIC_ASSERT(((align & (sizeof(void*) - 1)) == 0)); /* is a multiple of sizeof(void*) */

    void* operator new(std::size_t size) throw() {
      return aligned_malloc(size, align);
    }

    void* operator new[](std::size_t size) throw() {
      return aligned_malloc(size, align);
    }

    void operator delete(void* ptr) { 
      aligned_free(ptr);
    }

    void operator delete[](void* ptr) { 
      aligned_free(ptr);
    }
  };


  /** Replacement for std::allocator, which calls overloaded operator new if one
   * is present. */
  template<class T> class classnew_allocator {
  public:
    typedef T         value_type;
    typedef T*        pointer;
    typedef const T*  const_pointer;
    typedef T&        reference;
    typedef const T&  const_reference;
    typedef typename std::allocator<T>::size_type         size_type;
    typedef typename std::allocator<T>::difference_type   difference_type;

    template<typename OtherType>
    struct rebind { 
      typedef classnew_allocator<OtherType> other; 
    };

    T* address(T& ref) const { 
      return addressof(ref); 
    }

    const T* address(const T& ref) const { 
      return addressof(ref); 
    }

    T* allocate(size_t size, const void* hint = 0) { 
      return impl_type::allocate(size, hint);
    }

    void deallocate(T* ptr, size_t size) { 
      impl_type::deallocate(ptr, size);
    }
    
    size_t max_size() const { 
      return size_t(-1) / sizeof(T); 
    }

    void construct(T* ptr, const T& refObj) { 
      ::new(ptr) T(refObj); 
    }
    
    void destroy(T* ptr) { 
      ptr->~T(); 
    }

    template<class OtherT>
    bool operator==(const classnew_allocator<OtherT>&) {
      return impl_type::HAS_NEW == classnew_allocator<OtherT>::impl_type::HAS_NEW;
    }

    template<class OtherT>
    bool operator!=(const classnew_allocator<OtherT>& other) {
      return !operator==(other);
    }

  private:
    /* This one is probably buggy. */
    template<class T> struct has_operator_bracket_new {
      class yes_type { int member[128]; };
      class no_type {};
      template<void* (*func)(std::size_t)> struct nothing {};
      template<class Y> struct wrap { typedef Y type; };

      template<class Y>
      static no_type test(const wrap<Y>&, float);

      template<class Y>
      static yes_type test(const wrap<Y>&, int, nothing<&Y::operator new[]>* = 0);

      enum { value = sizeof(test(wrap<T>(), 0)) == sizeof(yes_type) };
    };

    template<bool has_new> struct allocator_impl {
      enum { HAS_NEW = true };

      static T* allocate(size_t size, const void* = 0) { 
        return static_cast<T*>(T::operator new(size * sizeof(T)));
      }

      static void deallocate(T* ptr, size_t) { 
        T::operator delete[](ptr); 
      }
    };

    template<> struct allocator_impl<false> {
      enum { HAS_NEW = false };

      static T* allocate(size_t size, const void* = 0) { 
        return static_cast<T*>(::operator new(size * sizeof(T))); 
      }

      static void deallocate(T* ptr, size_t) { 
        ::operator delete(ptr);
      }
    };

    typedef allocator_impl<has_operator_bracket_new<T>::value> impl_type;
  };

} // namespace arx

#endif // __ARX_MEMORY_H__
