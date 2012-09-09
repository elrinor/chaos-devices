#include "config.h"
#include "Unit.h"
#include "Util.h"
#include "Constraint.h"

using namespace std;

// -------------------------------------------------------------------------- //
// UnitClass
// -------------------------------------------------------------------------- //
UnitClass* UnitClass::deserialize(std::istream& stream) {
  int id;
  QString name;
  ::deserialize(stream, id, name);
  if(stream.fail())
    throw runtime_error("Failed to deserialize UnitClass from stream");
  return new UnitClass(id, name);
}

void UnitClass::serialize(std::ostream& stream) const {
  ::serialize(stream, getId(), getName());
}

// -------------------------------------------------------------------------- //
// UnitType
// -------------------------------------------------------------------------- //
UnitType* UnitType::deserialize(std::istream& stream) {
  int id;
  QString name, imageName;
  int classId, cost;
  ::deserialize(stream, id, name, cost, classId, imageName);
  if(stream.fail())
    throw runtime_error("Failed to deserialize UnitType from stream");
  return new UnitType(id, name, cost, classId, imageName);
}

void UnitType::serialize(std::ostream& stream) const {
  ::serialize(stream, getId(), getName(), getCost(), getClass()->getId(), getImageName());
}

// -------------------------------------------------------------------------- //
// UnitConfiguration
// -------------------------------------------------------------------------- //
UnitConfiguration* UnitConfiguration::deserialize(std::istream& stream) {
  int id;
  QString name;
  int typeId;
  arx::ArrayList<int> toolTypeIds;
  ::deserialize(stream, id, name, typeId, toolTypeIds);
  if(stream.fail())
    throw runtime_error("Failed to deserialize UnitConfiguration from stream");
  return new UnitConfiguration(id, name, typeId, toolTypeIds);
}

void UnitConfiguration::serialize(std::ostream& stream) const {
  ::serialize(stream, getId(), getName(), getType()->getId(), this->toolTypeIds);
}

Unit* UnitConfiguration::spawnUnit(Game* target, QString name, int x, int y, bool movable, bool removable) const {
  Unit* result = new Unit(INVALID_ID, name, getId(), FuzzySet(), FuzzySet(), x, y, movable, removable);
  target->addUnit(result);

  for(int i = 0; i < getToolTypeSize(); i++)
    getToolType(i)->spawnTool(target, result->getId(), removable);

  return result;
}

int UnitConfiguration::getCost() const {
  int cost = this->getType()->getCost();
  for(int i = 0; i < this->getToolTypeSize(); i++)
    cost += this->getToolType(i)->getCost();
  return cost;
}

// -------------------------------------------------------------------------- //
// Unit
// -------------------------------------------------------------------------- //
Unit* Unit::deserialize(std::istream& stream) {
  int id;
  QString name;
  int typeId;
  FuzzySet surviveSet, strengthenSet;
  int x, y;
  bool movable, removable;
  ::deserialize(stream, id, name, typeId, surviveSet, strengthenSet, x, y, movable, removable);
  if(stream.fail())
    throw runtime_error("Failed to deserialize Unit from stream");
  return new Unit(id, name, typeId, surviveSet, strengthenSet, x, y, movable, removable);
}

void Unit::serialize(std::ostream& stream) const {
  // We don't serialize links, hardware & software 'cause they'll be added separately, on their own deserialization step
  ::serialize(stream, getId(), getName(), getType()->getId(), getSurviveSet(), getStrengthenSet(), getX(), getY(), isMovable(), isRemovable());
}

bool Unit::hasLinkTo(int unitId) const {
  for(int i = 0; i < getLinksSize(); i++)
    if(getLink(i)->getOtherEnd(getId())->getId() == unitId)
      return true;
  return false;
}

Link* Unit::linkTo(int unitId, bool removable) {
  assert(!hasLinkTo(unitId));
  Link* link = new Link(INVALID_ID, getId(), unitId, removable);
  this->game->addLink(link);
  return link;
}

int Unit::getHP() const {
  int hp = 0;
  for(int i = 0; i < getToolSize(); i++)
    hp += getTool(i)->getHP();
  return hp;
}

int Unit::getMaxHP() const {
  int maxHP = 0;
  for(int i = 0; i < getToolSize(); i++)
    maxHP += getTool(i)->getMaxHP();
  return maxHP;
}

float Unit::getCPUUsage() const {
  float produced = 0.000001f;
  float used = 0;
  for(int i = 0; i < this->getToolSize(); i++) {
    float v = this->getTool(i)->getCpuProduction();
    if(v > 0)
      produced += v;
    else
      used -= v;
  }
  return min(used / produced, 1000000.0f);
}

float Unit::getPowerUsage() const {
  float produced = 0.000001f;
  float used = 0;
  for(int i = 0; i < this->getToolSize(); i++) {
    float v = this->getTool(i)->getPowerProduction();
    if(v > 0)
      produced += v;
    else
      used -= v;
  }
  return min(used / produced, 1000000.0f);
}

int Unit::getCost() const {
  int cost = this->getEmptyCost();
  for(int i = 0; i < this->getToolSize(); i++)
    cost += this->getTool(i)->getCost();
  return cost;
}


float Unit::getMemoryUsage() const {
  float produced = 0.000001f;
  float used = 0;
  for(int i = 0; i < this->getToolSize(); i++) {
    float v = this->getTool(i)->getMemProduction();
    if(v > 0)
      produced += v;
    else
      used -= v;
  }
  return min(used / produced, 1000000.0f);
}

bool Unit::meetsConstraints(/* OUT */ arx::ArrayList<QString> errorMessages) const {
  arx::ArrayList<Constraint*> constraints = this->game->getConstraintsForUnitClass(getType()->getClass());
  bool result = true;
  FOREACH(Constraint* c, constraints) {
    if(!c->isFulfilled(this)) {
      errorMessages.push_back(c->getMessage());
      result = false;
    }
  }
  return result;
}

bool Unit::canBeAdded(const ToolType* newToolType) const {
  arx::ArrayList<Constraint*> constraints = this->game->getConstraintsForUnitClass(getType()->getClass());
  FOREACH(Constraint* c, constraints)
    if(c->getToolClass()->getId() == newToolType->getClass()->getId() && !c->canBeAdded(this, newToolType))
      return false;
  return true;
}
