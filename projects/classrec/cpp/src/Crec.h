#ifndef __CREC_H__
#define __CREC_H__

#include "config.h"
#include <vector>
#include <set>
#include <algorithm>
#include <iostream> // for debugging
#include "Function.h"
#include "VTable.h"
#include "Relation.h"
#include "Utility.h"

namespace crec {
// ------------------------------------------------------------------------- //
// Crec
// ------------------------------------------------------------------------- //
  class Crec {
  private:
    struct Restriction {
      int l, r;
      Relation rel;

      Restriction(int l, int r, Relation rel): l(l), r(r), rel(rel) {}
    };

    typedef std::set<int> IntSet;
    typedef std::set<IntSet> IntSetSet;

  public:
    Crec(): mPureVirtual(NULL) {}

    void updateFromFile(std::string fileName);

    void reconstructHierarchy() {
      generateRestrictions();
      buildFSets();
      subdivideHierarchy();
      
      for(std::vector<IntSet>::iterator chunk = mChunks.begin(); chunk != mChunks.end(); chunk++) {
        IntSetSet chunkFSets;
        std::vector<Restriction> chunkRestrictions;

        for(IntSetSet::iterator i = fSets.begin(); i != fSets.end(); i++)
          if(has_intersection(chunk->begin(), chunk->end(), i->begin(), i->end()))
            chunkFSets.insert(*i);

        for(unsigned int i = 0; i < mRestrictions.size(); i++)
          if(chunk->find(mRestrictions[i].l) != chunk->end() && chunk->find(mRestrictions[i].r) != chunk->end())
            chunkRestrictions.push_back(mRestrictions[i]);

        reconstructChunkHierarchy(*chunk, chunkFSets, chunkRestrictions);
      }
    }

  private:
    /**
     * Reconstructs class hierarchy inside a single chunk.
     */
    void reconstructChunkHierarchy(const IntSet& chunk, const IntSetSet& fSets, const std::vector<Restriction> restrictions) {
      IntSetSet fStar = fSets;

      fStar.insert(chunk);

      /* Add single-element sets. */
      for(IntSet::const_iterator i = chunk.begin(); i != chunk.end(); i++) {
        IntSet singleSet;
        singleSet.insert(*i);
        fStar.insert(singleSet);
      }

      /* Perform intersection closure. */
      IntSetSet fStarTmp;
      do {
        /* TODO: this is way too slow (in debug mode :P)! */
        for(IntSetSet::iterator i = fStar.begin(); i != fStar.end(); i++) {
          for(IntSetSet::iterator j = i; j != fStar.end(); j++) {
            IntSet intersection;
            set_intersection(i->begin(), i->end(), j->begin(), j->end(), inserter(intersection, intersection.begin()));
            fStarTmp.insert(intersection);
          }
        }
        std::swap(fStar, fStarTmp);
      } while(fStar.size() != fStarTmp.size());

      /* Remove empty set. */
      fStar.erase(IntSet());

      /* Flatten. */
      std::vector<IntSet> fStarFlat;
      for(IntSetSet::iterator i = fStar.begin(); i != fStar.end(); i++)
        fStarFlat.push_back(*i);

      /* Sort by size. */
      std::sort(fStarFlat.begin(), fStarFlat.end(), SizeCmp());

      /* Build nodes. */
      
      

    }

    /** 
     * Applies simple rules to generate restrictions. 
     */
    void generateRestrictions() {
      for(unsigned int i = 0; i < mVTables.size(); i++) {
        for(unsigned int j = 0; j < mVTables.size(); j++) {
          unsigned int minSize = std::min(mVTables[i]->getSize(), mVTables[j]->getSize());

          /* VTable size. */
          if(mVTables[i]->getSize() < mVTables[j]->getSize())
            mRestrictions.push_back(Restriction(i, j, Relation::LESS));

          /* Pure virtual. */
          for(unsigned int k = 0; k < minSize; k++) {
            if(mVTables[i]->getEntry(k).getFunction() == mPureVirtual && mVTables[j]->getEntry(k).getFunction() != mPureVirtual) {
              mRestrictions.push_back(Restriction(i, j, Relation::LESS));
              break;
            }
          }

          /* Param size. */
          for(unsigned int k = 0; k < minSize; k++) {
            if(mVTables[i]->getEntry(k).getFunction()->getArgSize() != mVTables[j]->getEntry(k).getFunction()->getArgSize()) {
              mRestrictions.push_back(Restriction(i, j, Relation::NOTSIM));
              break;
            }
          }
        }
      }
    }

    /** 
     * Applies f-set rule to generate f-sets. 
     */
    void buildFSets() {
      for(unsigned int i = 0; i < mVTables.size(); i++) {
        for(unsigned int k = 0; k < mVTables[i]->getSize(); k++) {
          VTableEntry entry = mVTables[i]->getEntry(k);

          if(entry.getFunction() == mPureVirtual)
            continue;

          if(entry.getFunction()->getLength() < FUNCTION_MIN_LENGTH)
            continue;

          IntSet fSet;
          for(unsigned int j = 0; j < mVTables.size(); j++)
            if(k < mVTables[j]->getSize() && mVTables[j]->getEntry(k) == entry)
              fSet.insert(j);

          fSets.insert(fSet);
        }
      }
    }

    /**
     * Subdivides hierarchy into connected chunks.
     */
    void subdivideHierarchy() {
      IntSet processed;

      for(unsigned int n = 0; n < mVTables.size(); n++) {
        if(processed.find(n) != processed.end())
          continue;

        IntSet chunk;
        chunk.insert(n);
        int lastChunkSize;
        do {
          lastChunkSize = chunk.size();

          /* Check f-sets. */
          for(IntSetSet::iterator i = fSets.begin(); i != fSets.end(); i++)
            if(has_intersection(chunk.begin(), chunk.end(), i->begin(), i->end()))
              chunk.insert(i->begin(), i->end());

          /* Check restrictions. */
          for(unsigned int i = 0; i < mRestrictions.size(); i++) {
            if(mRestrictions[i].rel.includes(Relation::SIM)) {
              if(chunk.find(mRestrictions[i].l) != chunk.end())
                chunk.insert(mRestrictions[i].r);
              if(chunk.find(mRestrictions[i].r) != chunk.end())
                chunk.insert(mRestrictions[i].l);
            }
          }
        } while(lastChunkSize != chunk.size());

        std::cout << std::endl;
        for(IntSet::iterator i = chunk.begin(); i != chunk.end(); i++)
          std::cout << "\"" << mVTables[*i]->getClassName() << "\" ";
        std::cout << std::endl;

        /* Mark all vtables in chunk as processed. */
        processed.insert(chunk.begin(), chunk.end());
        mChunks.push_back(chunk);
      }
    }

    std::vector<VTable*> mVTables;
    std::vector<Function*> mFunctions;

    std::vector<Restriction> mRestrictions;

    IntSetSet fSets;

    std::vector<IntSet> mChunks;

    Function* mPureVirtual;
  };

} // namespace crec

#endif // __CREC_H__
