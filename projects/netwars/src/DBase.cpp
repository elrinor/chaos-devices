#include "config.h"
#include "DBase.h"
#include "Util.h"
#include "Tool.h"
#include "Unit.h"
#include "Defence.h"
#include "Attack.h"
#include "Constraint.h"
#include "Actor.h"

using namespace std;

UnitClass* DBase::getUnitClass(int id) const {
  return checkedGet(id, this->unitClasses);
}

UnitType* DBase::getUnitType(int id) const {
  return checkedGet(id, this->unitTypes);
}

UnitConfiguration* DBase::getUnitConfiguration(int id) const {
  return checkedGet(id, this->unitConfigurations);
}

ToolType* DBase::getToolType(int id) const {
  return checkedGet(id, this->toolTypes);
}

ToolClass* DBase::getToolClass(int id) const {
  return checkedGet(id, this->toolClasses);
}

DefenceClass* DBase::getDefenceClass(int id) const {
  return checkedGet(id, this->defenceClasses);
}

DefenceType* DBase::getDefenceType(int id) const {
  return checkedGet(id, this->defenceTypes);
}

AttackComponentType* DBase::getAttackComponentType(int id) const {
	return checkedGet(id, this->attackComponentTypes);
}

AttackType* DBase::getAttackType(int id) const {
	return checkedGet(id, this->attackTypes);
}

Constraint* DBase::getConstraint(int id) const {
  return checkedGet(id, this->constraints);
}

const arx::ArrayList<Constraint*> DBase::getConstraintsForUnitClass(UnitClass* unitClass) const {
  QHash<int, arx::ArrayList<Constraint*> >::const_iterator pos = this->constraintsForUnitClass.find(unitClass->getId());
  if(pos == this->constraintsForUnitClass.end())
    return arrayList<Constraint*>();
  else
    return *pos;
}

void DBase::addUnitClass(UnitClass* unitClass) {
  checkedAdd(this, unitClass, this->unitClasses);
	notify();
}

void DBase::addUnitType(UnitType* unitType) {
  checkedAdd(this, unitType, this->unitTypes);
  notify();
}

void DBase::addUnitConfiguration(UnitConfiguration* unitConfiguration) {
  checkedAdd(this, unitConfiguration, this->unitConfigurations);
  this->unitConfigurationsList.add(unitConfiguration);
  notify();
}

void DBase::addToolType(ToolType* toolType) {
  checkedAdd(this, toolType, this->toolTypes);
  this->toolTypesList.add(toolType);
  notify();
}

void DBase::addToolClass(ToolClass* toolClass) {
  checkedAdd(this, toolClass, this->toolClasses);
  notify();
}

void DBase::addDefenceType(DefenceType* defenceType) {
  checkedAdd(this, defenceType, this->defenceTypes);
  notify();
}

void DBase::addDefenceClass(DefenceClass* defenceClass) {
  checkedAdd(this, defenceClass, this->defenceClasses);
  notify();
}

void DBase::addAttackComponentType(AttackComponentType* attackComponentType) {
	checkedAdd(this, attackComponentType, this->attackComponentTypes);
	notify();
}

void DBase::addAttackType(AttackType* attackType) {
	checkedAdd(this, attackType, this->attackTypes);
  this->attackTypesList.add(attackType);
	notify();
}

void DBase::addConstraint(Constraint* constraint) {
  checkedAdd(this, constraint, this->constraints);
  this->constraintsForUnitClass[constraint->getUnitClass()->getId()].push_back(constraint);
  notify();
}

void DBase::addAllowedDefenderTypeName(QString typeName) {
  if(typeName == "*") {
    FOREACH(QString s, Actor::getDefenderTypeNames())
      if(!this->allowedDefenderTypeNames.contains(s))
        this->allowedDefenderTypeNames.add(s);
  } else {
    assert(!this->allowedDefenderTypeNames.contains(typeName));
    this->allowedDefenderTypeNames.add(typeName);
  }
}

void DBase::addAllowedAttackerTypeName(QString typeName) {
  if(typeName == "*") {
    FOREACH(QString s, Actor::getAttackerTypeNames())
      if(!this->allowedAttackerTypeNames.contains(s))
        this->allowedAttackerTypeNames.add(s);
  } else {
    assert(!this->allowedAttackerTypeNames.contains(typeName));
    this->allowedAttackerTypeNames.add(typeName);
  }
}

void DBase::subscribe(ChangeNotificationConsumer* consumer) {
  assert(this->notificationConsumers.find(consumer) == this->notificationConsumers.end());
  this->notificationConsumers.insert(consumer);
}

void DBase::unsubscribe(ChangeNotificationConsumer* consumer) {
  QSet<ChangeNotificationConsumer*>::iterator pos = this->notificationConsumers.find(consumer);
  assert(pos != this->notificationConsumers.end());
  this->notificationConsumers.erase(pos);
}

void DBase::notify() const {
  FOREACH(ChangeNotificationConsumer* consumer, notificationConsumers)
    consumer->notify();
}

void DBase::deserialize(std::istream& stream, DBase* target) {
  target->deserializeQHashAndAdd(stream, target, &DBase::addToolClass);
  target->deserializeQHashAndAdd(stream, target, &DBase::addToolType);
  target->deserializeQHashAndAdd(stream, target, &DBase::addUnitClass);
  target->deserializeQHashAndAdd(stream, target, &DBase::addUnitType);
  target->deserializeQHashAndAdd(stream, target, &DBase::addUnitConfiguration);
  target->deserializeQHashAndAdd(stream, target, &DBase::addDefenceType);
  target->deserializeQHashAndAdd(stream, target, &DBase::addDefenceClass);
  target->deserializeQHashAndAdd(stream, target, &DBase::addConstraint);
	target->deserializeQHashAndAdd(stream, target, &DBase::addAttackComponentType);
	target->deserializeQHashAndAdd(stream, target, &DBase::addAttackType);
  ::deserialize(stream, target->allowedAttackerTypeNames);
  ::deserialize(stream, target->allowedDefenderTypeNames);
}

DBase* DBase::deserialize(std::istream& stream) {
  DBase* dBase = new DBase();
  deserialize(stream, dBase);
  return dBase;
}

void DBase::serialize(std::ostream& stream) const {
  ::serialize(stream, this->toolClasses, this->toolTypes, this->unitClasses, this->unitTypes, this->unitConfigurations);
  ::serialize(stream, this->defenceTypes, this->defenceClasses, this->constraints, this->attackComponentTypes, this->attackTypes);
  ::serialize(stream, this->allowedAttackerTypeNames, this->allowedDefenderTypeNames);
}

DBase::~DBase() {
  freeQHash(this->unitClasses);
  freeQHash(this->unitTypes);
  freeQHash(this->unitConfigurations);

  freeQHash(this->toolClasses);
  freeQHash(this->toolTypes);

  freeQHash(this->defenceClasses);
  freeQHash(this->defenceTypes);

  freeQHash(this->constraints);
  freeQHash(this->attackComponentTypes);
  freeQHash(this->attackTypes);
}