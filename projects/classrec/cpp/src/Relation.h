#ifndef __CREC_RELATION_H__
#define __CREC_RELATION_H__

#include <cassert>

namespace crec {
// ------------------------------------------------------------------------- //
// Relation
// ------------------------------------------------------------------------- //
  class Relation {
  public:
    enum Type {
      UNKNOWN = 0x00,
      LESS    = 0x01,
      GREATER = 0x02,
      SIM     = 0x04,

      LHD     = LESS | SIM,
      RHD     = GREATER | SIM,
      NOTSIM  = LESS | GREATER,

      INVALID = NOTSIM | SIM,
    };

    enum {
      MASK    = INVALID
    };

    Relation(): mType(UNKNOWN) {}

    Relation(Type type): mType(type) {
      assert((type & ~MASK) == 0);
    }

    operator Type() const {
      return static_cast<Type>(mType);
    }
    
    /** Updates the type of this relation.
     * @returns true if type of this relation was changed, false otherwise. */
    bool update(const Relation& rel) {
      int oldType = mType;
      mType |= rel.mType;
      return mType != oldType;
    }

    bool includes(const Relation& other) const {
      return (mType & other.mType) != 0;
    }

    bool operator== (const Relation& other) const {
      return mType == other.mType;
    }

    Relation opposite() const {
      switch(mType) {
      case LESS:    return GREATER;
      case GREATER: return LESS;
      case LHD:     return RHD;
      case RHD:     return LHD;
      default:      return static_cast<Type>(mType);
      }
    }

  private:
    int mType;
  };

} // namespace crec

#endif // __CREC_RELATION_H__
