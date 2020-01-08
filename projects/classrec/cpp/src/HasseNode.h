#ifndef __CREC_HASSENODE_H__
#define __CREC_HASSENODE_H__

#include <set>

namespace crec {
// -------------------------------------------------------------------------- //
// HasseNode
// -------------------------------------------------------------------------- //
  class HasseNode {
  public:


  private:
    std::set<int> mSet;

    std::vector<HasseNode*> mAdjacent;
    
  };

} // namespace crec

#endif // __CREC_HASSENODE_H__
