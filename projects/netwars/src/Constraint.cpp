#include "config.h"
#include "Constraint.h"
#include "Util.h"
#include <exception>

using namespace std;

Constraint* Constraint::deserialize(std::istream& stream) {
  int id, unitClassId, toolClassId, type;
  QString message;
  ::deserialize(stream, id, unitClassId, toolClassId, type, message);
  if(stream.fail())
    throw runtime_error("Failed to deserialize Constraint from stream");
  return new Constraint(id, unitClassId, toolClassId, static_cast<ConstraintType>(type), message);
}

void Constraint::serialize(std::ostream& stream) const {
  ::serialize(stream, getId(), getUnitClass()->getId(), getToolClass()->getId(), static_cast<int>(getType()), getMessage());
}

bool Constraint::internalFullfilled(const Unit* unit, int count) const {
  assert(unit->getType()->getClass()->getId() == this->unitClassId);
  switch(this->type) {
  case MUST_HAVE:
    if(count > 0)
      return true;
    for(int i = 0; i < unit->getToolSize(); i++)
      if(unit->getTool(i)->getType()->getClass()->getId() == this->toolClassId)
        return true;
    return false;
  case CANNOT_HAVE:
    if(count > 0)
      return false;
    for(int i = 0; i < unit->getToolSize(); i++)
      if(unit->getTool(i)->getType()->getClass()->getId() == this->toolClassId)
        return false;
    return true;
  case CAN_HAVE_1:
    for(int i = 0; i < unit->getToolSize(); i++)
      if(unit->getTool(i)->getType()->getClass()->getId() == this->toolClassId && ++count > 1)
        return false;
    return true;
  default:
    assert(!"Unreachable");
    return false;
  }
}


bool Constraint::isFulfilled(const Unit* unit) const {
  return internalFullfilled(unit, 0);
}

bool Constraint::canBeAdded(const Unit* unit, const ToolType* toolType) const {
  assert(toolType->getClass()->getId() == this->toolClassId);
  return internalFullfilled(unit, 1);
}
