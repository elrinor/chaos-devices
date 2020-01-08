#ifndef __CREC_VTABLE_H__
#define __CREC_VTABLE_H__

#include <cassert>
#include <string>
#include <vector>

namespace crec {
// -------------------------------------------------------------------------- //
// VTableEntry
// -------------------------------------------------------------------------- //
  class VTableEntry {
  public:
    VTableEntry() {
      mFunction = NULL; /* A definite crash %). */
    };

    VTableEntry(Function* function, int offset) {
      mFunction = function;
      mOffset = offset;
    }

    Function* getFunction() const {
      return mFunction;
    }

    int getOffset() const {
      return mOffset;
    }

    bool operator== (const VTableEntry& other) const {
      return mFunction == other.mFunction && mOffset == other.mOffset;
    }

  private:
    Function* mFunction;
    int mOffset;
  };


// -------------------------------------------------------------------------- //
// VTable
// -------------------------------------------------------------------------- //
  class VTable {
  public:
    VTable(intptr_t address) {
      mAddress = address;
    }

    intptr_t getAddress() const {
      return mAddress;
    }

    std::string getClassName() const {
      return mClassName;
    }

    int getIndex() const {
      return mIndex;
    }

    VTable* setClassName(const std::string& className) {
      mClassName = className;
      return this;
    }

    VTable* setIndex(int index) {
      mIndex = index;
      return this;
    }

    unsigned int getSize() const {
      return mEntries.size();
    }

    const VTableEntry& getEntry(int index) const {
      return mEntries[index];
    }

    VTable* addEntry(int index, Function* function, int offset) {
      assert(index >= 0);

      if(static_cast<unsigned int>(index) >= mEntries.size())
        mEntries.resize(index + 1);
      mEntries[index] = VTableEntry(function, offset);

      return this;
    }

  private:
    intptr_t mAddress;
    int mIndex;
    std::string mClassName;
    std::vector<VTableEntry> mEntries;
  };

} // namespace crec

#endif // __CREC_VTABLE_H__
