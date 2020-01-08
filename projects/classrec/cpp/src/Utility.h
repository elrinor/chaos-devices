#ifndef __CREC_UTILITY_H__
#define __CREC_UTILITY_H__

#include <cassert>
#include <boost/preprocessor.hpp>

#define Unreachable() assert(!!"Unreachable");

namespace crec {
  /** Size comparison predicate */
  struct SizeCmp {
    template<class Container>
    bool operator()(const Container& l, const Container& r) {
      return l.size() < r.size();
    }
  };

  template<class InputIterator1, class InputIterator2, class Pred>
  bool has_intersection(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2, Pred pred = Pred()) {
    while(first1 != last1 && first2 != last2) {
      if(pred(*first1, *first2))
        ++first1;
      else if(pred(*first2, *first1))
        ++first2;
      else
        return true;
    }
    return false;
  }

  template<class InputIterator1, class InputIterator2>
  bool has_intersection(InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, InputIterator2 last2) {
    return has_intersection(first1, last1, first2, last2, std::less<typename InputIterator1::value_type>());
  }

} // namespace crec

#endif // __CREC_UTILITY_H__
