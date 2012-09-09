#ifndef __CONSTRAINT_H__
#define __CONSTRAINT_H__

#include "config.h"
#include "Unit.h"
#include "Tool.h"
#include "DBase.h"

enum ConstraintType {
  MUST_HAVE,
  CAN_HAVE_1,
  CANNOT_HAVE
};

class Constraint {
private:
  DBase* dBase;
  int id;
  int unitClassId;
  int toolClassId;
  ConstraintType type;
  QString message;

  friend class DBase;

  bool internalFullfilled(const Unit* unit, int count) const;

public:
  Constraint(int id, int unitClassId, int toolClassId, ConstraintType type, QString message): 
    id(id), unitClassId(unitClassId), toolClassId(toolClassId), type(type), message(message) {}

  int getId() const {return this->id;}
  UnitClass* getUnitClass() const {return this->dBase->getUnitClass(this->unitClassId);}
  ToolClass* getToolClass() const {return this->dBase->getToolClass(this->toolClassId);}
  ConstraintType getType() const {return this->type;}
  QString getMessage() const {return this->message;}

  bool isFulfilled(const Unit* unit) const;
  bool canBeAdded(const Unit* unit, const ToolType* toolType) const;

  static Constraint* deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;
};

#endif