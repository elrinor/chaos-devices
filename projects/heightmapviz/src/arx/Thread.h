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

#ifdef ARX_WIN32
#  define NOMINMAX
#  include <Windows.h>
namespace arx {
  class mutex: noncopyable {
  private:
    CRITICAL_SECTION m;

  public:
    mutex() { InitializeCriticalSection(&m); }
    ~mutex() { DeleteCriticalSection(&m); }

    void lock() { EnterCriticalSection(&m); }
    void unlock() { LeaveCriticalSection(&m); }
  };

  namespace detail {
    class thread_proc_base {
    public:
      virtual void operator()() = 0;
    };

    template<class F>
    class thread_proc {
    private:
      F f;

    public:
      thread_proc(F f): f(f) {}
      virtual void operator()() {
        f();
      }
    };

    DWORD WINAPI invoke_thread_proc(void* f) {
      *reinterpret_cast<thread_proc_base*>(f);
    }
  }

  class thread: noncopyable {
  private:
    HANDLE h;
    DWORD id;
    scoped_ptr<thread_proc_base> f; // not safe...
    
  public:
    thread() {
      h = INVALID_HANDLE_VALUE;
    }
    
    ~thread() {
      detach();
    }

    template<class F>
    explicit thread(F f): f(new detail::thread_proc<F>(f)) {
      HANDLE h = CreateThread(0, 0, detail::invoke_thread_proc, reinterpret_cast<void*>(f.get()), 
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
  }
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
  };
}
// TODO thread class
#elif defined(ARX_DISABLE_THREADS)
namespace arx {
  class null_mutex;
  typedef null_mutex mutex;
}
#else
#  error "Threads are not supported on your system, #define ARX_DISABLE_THREADS to turn off multithreading support in ArX Library."
#endif
#endif // ARX_USE_BOOST

namespace arx {
  /** Mutex stub - for use as template argument in case thread safety isn't needed. */
  class null_mutex: noncopyable {
  public:
    null_mutex() {}

    static void lock() {}
    static void unlock() {}
  };
}

#endif // __ARX_THREAD_H__
