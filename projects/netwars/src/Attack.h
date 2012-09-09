#ifndef __OFFENCE_H__
#define __OFFENCE_H__

#include "config.h"
#include "arx/Collections.h"
#include "arx/Utility.h"
#include "Game.h"
#include "Defence.h"

class AttackComponentType {
private:
	int id;
	int defenceClassId;
	float baseAttack;
	float attackPerStrength;
	float baseDamage;
	float damagePerStrength;

	DBase* dBase;

	friend class DBase;

public:
	AttackComponentType(int id, int defenceClassId, float baseAttack, float attackPerStrength, float baseDamage, float damagePerStrength): 
		id(id), defenceClassId(defenceClassId), baseAttack(baseAttack), attackPerStrength(attackPerStrength), baseDamage(baseDamage), damagePerStrength(damagePerStrength) {}

	int getId() const {return this->id;}
  DefenceClass* getDefenceClass() const { return this->dBase->getDefenceClass(this->defenceClassId); }
  float getBaseAttack() const { return this->baseAttack; }
  float getAttackPerStrength() const { return this->attackPerStrength; }
	float getBaseDamage() const { return this->baseDamage; }
	float getDamagePerStrength() const { return this->damagePerStrength; }

  static AttackComponentType* deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;

	AttackComponent* spawnAttackComponent(Game* target, int attackId) const;
};

class AttackType {
private:
	int id;
	QString name;
	bool fromInternetOnly;
	bool selfPropagating;
	int baseCost;
	float costPerStrength;
  float costPerStrengthSqr;
	arx::ArrayList<int> componentTypeIds;

	DBase* dBase;

	friend class DBase;

public:
	AttackType(int id, QString name, bool fromInternetOnly, bool selfPropagating,	int baseCost, float costPerStrength, float costPerStrengthSqr, arx::ArrayList<int> componentTypeIds): 
		id(id), name(name), fromInternetOnly(fromInternetOnly), selfPropagating(selfPropagating), baseCost(baseCost), costPerStrength(costPerStrength), costPerStrengthSqr(costPerStrengthSqr), componentTypeIds(componentTypeIds) {}

	int getId() const { return this->id; }
	QString getName() const { return this->name; }
	bool isSelfPropagating() const { return this->selfPropagating; }
	bool isFromInternetOnly() const { return this->fromInternetOnly; }
	int getBaseCost() const { return this->baseCost; }
	float getCostPerStrength() const { return this->costPerStrength; }
  float getCostPerStrengthSqr() const { return this->costPerStrengthSqr; }
  int getCost(int strength) const { return (int) (getBaseCost() + strength * getCostPerStrength() + arx::sqr(strength) * getCostPerStrengthSqr()); }

	int getComponentTypeSize() const { return this->componentTypeIds.size(); }
	AttackComponentType* getComponentType(int index) const { return this->dBase->getAttackComponentType(this->componentTypeIds[index]); }

	static AttackType* deserialize(std::istream& stream);
	void serialize(std::ostream& stream) const;

	Attack* spawnAttack(Game* target, int targetUnitId, int strength) const;
};

class Attack {
private:
	int id;
	int typeId;
	int targetUnitId;
	int strength;
	arx::ArrayList<int> componentIds;

	bool activated;
	bool successful;
	int damageInflicted;
	int unitsAttacked;
	arx::ArrayList<int> attackedUnitIds;
  arx::ArrayList<int> successfullyAttackedUnitIds;

	Game* game;

	friend class DBase;
	friend class Game;

public:
	Attack(int id, int typeId, int targetUnitId, int strength):
		id(id), typeId(typeId), targetUnitId(targetUnitId), strength(strength), activated(false), successful(false), damageInflicted(0), unitsAttacked(0) {};

	int getId() const { return this->id; }
	AttackType* getType() const { return this->game->getAttackType(this->typeId); }
	Unit* getTargetUnit() const { return this->game->getUnit(this->targetUnitId); }
	int getStrength() const { return this->strength; }

	int getComponentSize() const { return this->componentIds.size(); }
	AttackComponent* getComponent(int index) const { return this->game->getAttackComponent(this->componentIds[index]); }

  bool isActivated() const { return this->activated; }
  bool isSuccessful() const { return this->successful; }
  int getDamageInflicted() const { return this->damageInflicted; }
  int getUnitsAttacked() const { return this->unitsAttacked; }

	static Attack* deserialize(std::istream& stream);
	void serialize(std::ostream& stream) const;

  int getCost() const { return getType()->getCost(getStrength()); }
};

class AttackComponent {
private:
	int id;
	int typeId;
	int attackId;

	Game* game;

	friend class DBase;
	friend class Game;

public:
	AttackComponent(int id, int typeId, int attackId):
		id(id), typeId(typeId), attackId(attackId) {}

	int getId() const { return this->id; }
	AttackComponentType* getType() const { return this->game->getAttackComponentType(this->typeId); }
	Attack* getAttack() const { return this->game->getAttack(this->attackId); }

	int getAttackValue() const { return (int) (getType()->getBaseAttack() + getAttack()->getStrength() * getType()->getAttackPerStrength()); }
	int getDamageValue() const { return (int) (getType()->getBaseDamage() + getAttack()->getStrength() * getType()->getDamagePerStrength()); }
  int attackRoll() const;
  int damageRoll() const;

	static AttackComponent* deserialize(std::istream& stream);
	void serialize(std::ostream& stream) const;
};

#endif