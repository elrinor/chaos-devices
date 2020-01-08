#ifndef __CREC_RELATIONMAP_H__
#define __CREC_RELATIONMAP_H__

#include "Relation.h"
#include <map>
#include <utility>

namespace crec {
// ------------------------------------------------------------------------- //
// RelationMap
// ------------------------------------------------------------------------- //
  template<class T>
  class RelationMap {
  public:
    Relation get(const T& l, const T& r) {
      return mMap[std::make_pair(l.getKey(), r.getKey())];
    }

    bool update(const T& l, const T& r, const Relation& rel) {
      int lKey = l.getKey(), rKey = r.getKey();

      assert(mMap[std::make_pair(lKey, rKey)] == mMap[std::make_pair(rKey, lKey)].opposite());

      mMap[std::make_pair(lKey, rKey)].update(rel);
      return mMap[std::make_pair(rKey, lKey)].update(rel.opposite());
    }

  private:
    std::map<std::pair<int, int>, Relation> mMap;  /* TODO:unordered_map? */
  };

} // namespace crec

#endif // __CREC_RELATIONMAP_H__