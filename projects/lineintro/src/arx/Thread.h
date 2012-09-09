/** @file ArX Thread Library.
 * 
 * This is a "poor man's" threading library, poorly written and poorly tested.
 * If possible, use threading support offered by boost. And anyway, all this
 * functionality will soon be available with the C++0x threading library.
 */
#ifndef __ARX_THREAD_H__
#define __ARX_THREAD_H__

#include "config.h"
#include "Utility.h"

#ifdef ARX_USE_BOOST
#  include <boost/thread.hpp>
namespace arx {
  using boost::mutex;
  using boost::thread;
}
#else // ARX_USE_BOOST

namespace arx {
  namespace detail {
    class thread_proc_base {
    public:
      virtual void operator()() = 0;
    };

    template<class F>
    class thread_proc: public thread_proc_base {
    private:
      F f;

    public:
      thread_proc(F f): f(f) {}
      virtual void operator()() {
        f();
      }
    };
  } // namespace detail

  /** Mutex stub - for use as template argument in case thread safety isn't needed. */
  class null_mutex: noncopyable {
  public:
    null_mutex() {}

    static void lock() {}
    static void unlock() {}
  };

  /** Simple lock for the given mutex type. */
  template<typename Mutex>
  class unique_lock {
  private:
    Mutex* mMutex;
    bool mIsLocked;
    unique_lock(unique_lock&);
    unique_lock& operator=(unique_lock&);
  public:
    unique_lock(): mMutex(0), mIsLocked(false) {}

    explicit unique_lock(Mutex& mutex): mMutex(&mutex), mIsLocked(false) {
      lock();
    }

    ~unique_lock() {
      if(owns_lock()) {
        mMutex->unlock();
      }
    }

    void lock() {
      if(owns_lock())
        throw std::runtime_error("unique_lock::lock exception");
      mMutex->lock();
      mIsLocked = true;
    }

    void unlock() {
      if(!owns_lock())
        throw std::runtime_error("unique_lock::unlock exception");
      mMutex->unlock();
      mIsLocked = false;
    }

    typedef void (unique_lock::*bool_type)();

    operator bool_type() const {
      return mIsLocked ? &unique_lock::lock : 0;
    }

    bool operator!() const {
      return !owns_lock();
    }

    bool owns_lock() const {
      return mIsLocked;
    }

    Mutex* mutex() const {
      return mMutex;
    }

    Mutex* release() {
      Mutex* const result = mMutex;
      mMutex = 0;
      mIsLocked = false;
      return result;
    }
  };
}

#ifdef ARX_WIN32
#  define NOMINMAX
#  include <Windows.h>
namespace arx {
  /* TODO: this one is broken! */
  class mutex: noncopyable {
  private:
    CRITICAL_SECTION m;

  public:
    mutex() { InitializeCriticalSection(&m); }
    ~mutex() { DeleteCriticalSection(&m); }

    void lock() { EnterCriticalSection(&m); }
    void unlock() { LeaveCriticalSection(&m); }

    typedef unique_lock<mutex> scoped_lock;
  };

  namespace detail {
    inline DWORD WINAPI invoke_thread_proc(void* f) {
      /* We're already in another thread... */
      thread_proc_base* threadFunc = reinterpret_cast<thread_proc_base*>(f);
      try {
        threadFunc->operator()();
      } catch (...) {
        std::terminate();
        return 1;
      }
      delete threadFunc;
      return 0;
    }
  } // namespace detail

  class thread: noncopyable {
  private:
    HANDLE h;
    DWORD id;
    
  public:
    thread() {
      h = INVALID_HANDLE_VALUE;
    }
    
    ~thread() {
      detach();
    }

    template<class F>
    explicit thread(F f) {
      detail::thread_proc_base* threadFunc = new detail::thread_proc<F>(f);
      HANDLE h = CreateThread(0, 0, detail::invoke_thread_proc, reinterpret_cast<void*>(threadFunc), 
        CREATE_SUSPENDED, &this->id);
      if(h == NULL)
        throw std::runtime_error("Failed to create thread.");
      this->h = h;
      ResumeThread(h);
    }

    void detach() {
      this->h = INVALID_HANDLE_VALUE;
    }
      
    typedef HANDLE native_handle_type;
    native_handle_type native_handle() {
      return this->h;
    }

    static unsigned hardware_concurrency() {
      SYSTEM_INFO sInfo;
      GetSystemInfo(&sInfo);
      return sInfo.dwNumberOfProcessors;
    }
  };
}
#elif defined(_POSIX_THREADS)
#  include <pthread.h>
namespace arx {
  class mutex: noncopyable {
  private:
    pthread_mutex_t m;

  public:
    pthread_mutex() { pthread_mutex_init(&m, 0); }
    ~pthread_mutex() { pthread_mutex_destroy(&m); }

    void lock() { pthread_mutex_lock(&m); }
    void unlock() { pthread_mutex_unlock(&m); }

    typedef unique_lock<mutex> scoped_lock;
  };
}

// TODO thread class

#elif defined(ARX_DISABLE_THREADS)
namespace arx {
  typedef null_mutex mutex;
}
#else
#  error "Threads are not supported on your system, #define ARX_DISABLE_THREADS to turn off multithreading support in ArX Library."
#endif

#endif // ARX_USE_BOOST

#endif // __ARX_THREAD_H__
