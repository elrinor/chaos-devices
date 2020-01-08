#ifndef __CREC_FUNCTION_H__
#define __CREC_FUNCTION_H__

#include <boost/noncopyable.hpp>

namespace crec {
// ------------------------------------------------------------------------- //
// Function
// ------------------------------------------------------------------------- //
  class Function: public boost::noncopyable {
  public:
    enum Type {
      NONE,  /**< Simple. */
      SDD    /**< Scalar Deleting Destructor. */
    };

    Function(intptr_t address): mAddress(address) {}

    intptr_t getAddress() const {
      return mAddress;
    }

    Type getType() const {
      return mType;
    }

    int getArgSize() const {
      return mArgSize;
    }

    int getLength() const {
      return mLength;
    }

    Function* setType(Type type) {
      mType = type;
      return this;
    }

    Function* setArgSize(int argSize) {
      mArgSize = argSize;
      return this;
    }

    Function* setLength(int length) {
      mLength = length;
      return this;
    }

  private:
    intptr_t mAddress;
    Type mType;
    int mArgSize;
    int mLength;
  };

} // namespace crec

#endif // __CREC_FUNCTION_H__
