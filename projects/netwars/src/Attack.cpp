#include "config.h"
#include "Attack.h"
#include <exception>
#include "Util.h"
#include "Unit.h"

using namespace std;

AttackComponentType* AttackComponentType::deserialize(std::istream& stream) {
	int id;
	int defenceClassId;
	float baseAttack;
	float attackPerStrength;
	float baseDamage;
	float damagePerStrength;
  ::deserialize(stream, id, defenceClassId, baseAttack, attackPerStrength, baseDamage, damagePerStrength);
  if(stream.fail())
    throw runtime_error("Failed to deserialize AttackComponentType from stream");
  return new AttackComponentType(id, defenceClassId, baseAttack, attackPerStrength, baseDamage, damagePerStrength);
}

void AttackComponentType::serialize(std::ostream& stream) const {
  ::serialize(stream, getId(), getDefenceClass()->getId(), getBaseAttack(), getAttackPerStrength(), getBaseDamage(), getDamagePerStrength());
}

AttackComponent* AttackComponentType::spawnAttackComponent(Game* target, int attackId) const {
	AttackComponent* result = new AttackComponent(INVALID_ID, getId(), attackId);
	target->addAttackComponent(result);
	return result;
}

AttackType* AttackType::deserialize(std::istream& stream) {
	int id;
	QString name;
	bool fromInternetOnly;
	bool selfPropagating;
	int baseCost;
	float costPerStrength, costPerStrengthSqr;
	arx::ArrayList<int> componentTypeIds;
	::deserialize(stream, id, name, fromInternetOnly, selfPropagating, baseCost, costPerStrength, costPerStrengthSqr, componentTypeIds);
	if(stream.fail())
		throw runtime_error("Failed to deserialize AttackType from stream");
	return new AttackType(id, name, fromInternetOnly, selfPropagating, baseCost, costPerStrength, costPerStrengthSqr, componentTypeIds);
}

void AttackType::serialize(std::ostream& stream) const {
	::serialize(stream, getId(), getName(), isFromInternetOnly(), isSelfPropagating(), getBaseCost(), getCostPerStrength(), getCostPerStrengthSqr(), this->componentTypeIds);
}

Attack* AttackType::spawnAttack(Game* target, int targetUnitId, int strength) const {
	Attack* result = new Attack(INVALID_ID, getId(), targetUnitId, strength);
	target->addAttack(result);

	for(int i = 0; i < getComponentTypeSize(); i++)
		getComponentType(i)->spawnAttackComponent(target, result->getId());

	return result;
}

AttackComponent* AttackComponent::deserialize(std::istream& stream) {
	int id;
	int typeId;
	int attackId;
	::deserialize(stream, id, typeId, attackId);
	if(stream.fail())
		throw runtime_error("Failed to deserialize AttackComponent from stream");
	return new AttackComponent(id, typeId, attackId);
}

void AttackComponent::serialize(std::ostream& stream) const {
	::serialize(stream, getId(), getType()->getId(), getAttack()->getId());
}

int AttackComponent::attackRoll() const {
  int d = max(10, this->getAttackValue() / 2);
  return ((random(20) == 0) ? 3 : 1) * (this->getAttackValue() - d + random(2 * d));
}

int AttackComponent::damageRoll() const {
  int d = this->getDamageValue() / 2;
  return ((random(20) == 0) ? 3 : 1) * (this->getDamageValue() - d + random(2 * d));
}

Attack* Attack::deserialize(std::istream& stream) {
	int id;
	int typeId;
	int targetUnitId;
	int strength;
	::deserialize(stream, id, typeId, targetUnitId, strength);
	if(stream.fail())
		throw runtime_error("Failed to deserialize Attack from stream");
	return new Attack(id, typeId, targetUnitId, strength);
}

void Attack::serialize(std::ostream& stream) const {
	::serialize(stream, getId(), getType()->getId(), getTargetUnit()->getId(), getStrength());
}

