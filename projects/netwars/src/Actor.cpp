#include "Actor.h"
#include <exception>
#include <cassert>
#include "Util.h"
#include "Unit.h"

using namespace std;
using namespace arx;

// -------------------------------------------------------------------------- //
// AttackerUtils
// -------------------------------------------------------------------------- //
class AttackerUtils {
public:
  static ArrayList<Unit*> getUnitsApplicableForAttackType(const Game* game, const AttackType* attackType) {
    ArrayList<Unit*> units = game->getUnits().clone();
    if(attackType->isFromInternetOnly()) {
      for(int i = units.size() - 1; i >= 0; i--) {
        if(!units[i]->isInternetConnection())
          units.erase(i);
      }
    }
    if(!attackType->isSelfPropagating()) {
      for(int i = units.size() - 1; i >= 0; i--) {
        if(units[i]->isInternetConnection())
          units.erase(i);
      }
    }
    return units;
  }
};


// -------------------------------------------------------------------------- //
// Action
// -------------------------------------------------------------------------- //
QHash<int, Action::DeserializeFunction> Action::deserializers;

void Action::registerAction(int type, DeserializeFunction deserializeFunc) {
  assert(!deserializers.contains(type));
  deserializers.insert(type, deserializeFunc);
}

void Action::serialize(std::ostream& stream) const {
  ::serialize(stream, getType());
}

Action* Action::deserialize(std::istream& stream) {
  int type;
  ::deserialize(stream, type);
  if(deserializers.contains(type) && !stream.fail())
    return deserializers.value(type)(stream);
  else
    throw runtime_error("Failed to deserialize Action from stream");
}

// -------------------------------------------------------------------------- //
// ActionAttack
// -------------------------------------------------------------------------- //
void ActionAttack::serialize(std::ostream& stream) const {
  Action::serialize(stream);
  ::serialize(stream, this->getId(), this->getAttack()->getId());
}

Action* ActionAttack::deserialize(std::istream& stream) {
  int attackId, id;
  ::deserialize(stream, id, attackId);
  if(stream.fail())
    throw runtime_error("Failed to deserialize ActionAttack from stream");
  return new ActionAttack(id, attackId);
}


// -------------------------------------------------------------------------- //
// EmptyActor
// -------------------------------------------------------------------------- //
class EmptyActor: public Actor {
public:
  virtual ArrayList<int> act() {
    return arrayList<int>();
  }

  virtual QString getName() const {
    return "EmptyActor";
  }

  virtual void serialize(std::ostream& stream) const {
    Actor::serialize(stream);
  }

  static Actor* deserialize(std::istream& stream) {
    return new EmptyActor();
  }

  static Actor* create() {
    return new EmptyActor();
  }
};


// -------------------------------------------------------------------------- //
// SmartyAttacker
// -------------------------------------------------------------------------- //
class SmartyAttacker: public Actor {
private:
  struct AttackEstimate {
    int attackId;
    int targetUnitId;
    int damageInflicted;
    int cost;
    float efficiency;

    AttackEstimate(const Attack* attack) {
      this->attackId = attack->getId();
      this->targetUnitId = attack->getTargetUnit()->getId();
      this->damageInflicted = attack->getDamageInflicted();
      this->cost = attack->getCost();
      this->efficiency = ((float) this->damageInflicted) / this->cost;
    }
  };

  struct EstimateCmp {
  public:
    bool operator() (const AttackEstimate& a, const AttackEstimate& b) {
      return a.efficiency > b.efficiency;
    }
  };

public:
  virtual ArrayList<int> act() {
    const ArrayList<AttackType*> attackTypes = this->game->getAttackTypes();
    ArrayList<AttackEstimate> attackEstimates;

    for(int u = 0; u < 100; u++) {
      AttackType* attackType = attackTypes[random(attackTypes.size())];
      ArrayList<Unit*> applicableUnits = AttackerUtils::getUnitsApplicableForAttackType(game, attackType);
      Unit* targetUnit = applicableUnits[random(applicableUnits.size())];
      Attack* attack = attackType->spawnAttack(this->game, targetUnit->getId(), random(20));

      if(attack->getCost() > this->game->getAttackerResources())
        continue;

      this->game->performAttack(attack->getId(), true);
      if(attack->getDamageInflicted() > 0)
        attackEstimates.add(AttackEstimate(attack));
    }

    sort(attackEstimates.begin(), attackEstimates.end(), EstimateCmp());

    ArrayList<int> result;
    int moneyLeft = game->getAttackerResources();
    unsigned int attacksCount = 1 + random(5);
    for(unsigned int i = 0; i < attacksCount && i < attackEstimates.size(); i++) {
      AttackEstimate e = attackEstimates[i];
      if(e.cost > moneyLeft)
        break;
      moneyLeft -= e.cost;

      Attack* oldAttack = game->getAttack(e.attackId);
      ActionAttack* attackAction = 
        new ActionAttack(INVALID_ID, oldAttack->getType()->spawnAttack(this->game, oldAttack->getTargetUnit()->getId(), oldAttack->getStrength())->getId());
      this->game->addAction(attackAction);
      result.add(attackAction->getId());
    }

    return result;
  }

  virtual QString getName() const {
    return "Smarty";
  }

  virtual void serialize(std::ostream& stream) const {
    Actor::serialize(stream);
  }

  static Actor* deserialize(std::istream& stream) {
    return new SmartyAttacker();
  }

  static Actor* create() {
    return new SmartyAttacker();
  }
};


// -------------------------------------------------------------------------- //
// KillerAttacker
// -------------------------------------------------------------------------- //
class KillerAttacker: public Actor {
private:
  class AttackEstimate {
  private:
    int targetUnitId;
    int attackTypeId;
    float meanDamage;
    int cost;
    int strength;
    float efficiency;

    static float estimateMeanDamage(Game* testGame, int targetUnitId, int attackTypeId, int strength) {
      int sumDamage = 0, maxIterations = 5;
      for(int i = 0; i < maxIterations; i++) {
        Attack* attack = testGame->getAttackType(attackTypeId)->spawnAttack(testGame, targetUnitId, strength);
        testGame->performAttack(attack->getId(), true);
        sumDamage += attack->getDamageInflicted();
      }
      return (float) sumDamage / maxIterations;
    }

    static int getCost(Game* testGame, int attackTypeId, int strength) {
      return testGame->getAttackType(attackTypeId)->getCost(strength);
    }

    static float estimateEfficiency(Game* testGame, int targetUnitId, int attackTypeId, int strength) {
      return estimateMeanDamage(testGame, targetUnitId, attackTypeId, strength) / getCost(testGame, attackTypeId, strength);
    }

    static int estimateBestStrength(Game* testGame, int targetUnitId, int attackTypeId) {
      int iter = 0;
      int str = 0;
      int strStep = 10;
      float eff = estimateEfficiency(testGame, targetUnitId, attackTypeId, str);
      int cost = getCost(testGame, attackTypeId, str);

      AttackType* attackType = testGame->getAttackType(attackTypeId);

      while(true) {
        iter++;
        if(iter > 6)
          break;
        int newStr = str + strStep;
        int newCost = getCost(testGame, attackTypeId, newStr);
        float newEff = estimateEfficiency(testGame, targetUnitId, attackTypeId, newStr);
        if(newEff >= eff && newCost <= testGame->getAttackerResources()) {
          str = newStr;
          eff = newEff;
        } else {
          if((strStep /= 2) == 0)
            break;
          int backStr = str - strStep;
          if(backStr >= 0) {
            float backEff = estimateEfficiency(testGame, targetUnitId, attackTypeId, backStr);
            if(backEff >= newEff) {
              str = backStr;
              eff = backEff;
              if((strStep /= 2) == 0)
                break;
            }
          }
        }
      }

      return str;
    }

  public:
    int getTargetUnitId() const { return this->targetUnitId; }
    int getAttackTypeId() const { return this->attackTypeId; }
    int getMeanDamage() const { return this->meanDamage; }
    int getCost() const { return this->cost; }
    int getStrength() const { return this->strength; }
    float getEfficiency() const { return this->efficiency; }

    AttackEstimate(Game* testGame, int targetUnitId, int attackTypeId) {
      this->targetUnitId = targetUnitId;
      this->attackTypeId = attackTypeId;
      this->strength = estimateBestStrength(testGame, targetUnitId, attackTypeId);
      this->cost = testGame->getAttackType(attackTypeId)->getCost(this->strength);
      this->meanDamage = estimateMeanDamage(testGame, targetUnitId, attackTypeId, this->strength);
      this->efficiency = this->meanDamage / this->cost;
    }
  };

  struct EstimateCmp {
  public:
    bool operator() (const AttackEstimate& a, const AttackEstimate& b) {
      return a.getEfficiency() > b.getEfficiency();
    }
  };

public:
  virtual ArrayList<int> act() {
    Game* testGame = this->game->clone();
    const ArrayList<AttackType*> attackTypes = testGame->getAttackTypes();
    ArrayList<AttackEstimate> attackEstimates;

    FOREACH(AttackType* attackType, attackTypes) {
      ArrayList<Unit*> applicableUnits = AttackerUtils::getUnitsApplicableForAttackType(testGame, attackType);
      Unit* targetUnit = applicableUnits[random(applicableUnits.size())];

      AttackEstimate e = AttackEstimate(testGame, targetUnit->getId(), attackType->getId());

      if(e.getCost() > this->game->getAttackerResources())
        continue;

      attackEstimates.add(e);
    }

    sort(attackEstimates.begin(), attackEstimates.end(), EstimateCmp());

    ArrayList<int> result;
    int moneyLeft = game->getAttackerResources();
    for(unsigned int i = 0; i < attackEstimates.size(); i++) {
      AttackEstimate e = attackEstimates[i];
      if(e.getCost() > moneyLeft)
        continue;
      moneyLeft -= e.getCost();

      Attack* attack = this->game->getAttackType(e.getAttackTypeId())->spawnAttack(this->game, e.getTargetUnitId(), e.getStrength());
      ActionAttack* attackAction = 
        new ActionAttack(INVALID_ID, attack->getId());
      this->game->addAction(attackAction);
      result.add(attackAction->getId());

      break;
    }
    
    delete testGame;
    return result;
  }

  virtual QString getName() const {
    return "Killer";
  }

  virtual void serialize(std::ostream& stream) const {
    Actor::serialize(stream);
  }

  static Actor* deserialize(std::istream& stream) {
    return new KillerAttacker();
  }

  static Actor* create() {
    return new KillerAttacker();
  }
};


// -------------------------------------------------------------------------- //
// Actor 
// -------------------------------------------------------------------------- //
QHash<QString, Actor::DeserializeFunction> Actor::deserializers;
QHash<QString, Actor::FactoryFunction> Actor::factories;
arx::ArrayList<QString> Actor::attackerTypeNames;
arx::ArrayList<QString> Actor::defenderTypeNames;

Actor* Actor::deserialize(std::istream& stream) {
  QString typeName;
  ::deserialize(stream, typeName);
  if(deserializers.contains(typeName) && !stream.fail())
    return deserializers.value(typeName)(stream);
  else
    throw runtime_error("Failed to deserialize Actor from stream");
}

void Actor::serialize(std::ostream& stream) const {
  ::serialize(stream, this->getName());
}

void Actor::registerActor(QString typeName, DeserializeFunction deserializeFunc, FactoryFunction factoryFunc, int typeFlags) {
  assert(!deserializers.contains(typeName));
  deserializers.insert(typeName, deserializeFunc);

  assert(!factories.contains(typeName));
  factories.insert(typeName, factoryFunc);

  if(typeFlags & TYPE_ATTACKER) {
    assert(attackerTypeNames.indexOf(typeName) == -1);
    attackerTypeNames.push_back(typeName);
  }

  if(typeFlags & TYPE_DEFENDER) {
    assert(defenderTypeNames.indexOf(typeName) == -1);
    defenderTypeNames.push_back(typeName);
  }
}

Actor* Actor::create(QString typeName) {
  if(deserializers.contains(typeName))
    return factories.value(typeName)();
  else
    throw runtime_error("Failed to create Actor");
}

// -------------------------------------------------------------------------- //
// Static Initializer
// -------------------------------------------------------------------------- //
class ActorStaticInitializer {
public:
  ActorStaticInitializer() {
    Actor::registerActor(EmptyActor().getName(),     EmptyActor::deserialize,     EmptyActor::create,     Actor::TYPE_ATTACKER | Actor::TYPE_DEFENDER);
    Actor::registerActor(KillerAttacker().getName(), KillerAttacker::deserialize, KillerAttacker::create, Actor::TYPE_ATTACKER);
    Actor::registerActor(SmartyAttacker().getName(), SmartyAttacker::deserialize, SmartyAttacker::create, Actor::TYPE_ATTACKER);

    Action::registerAction(Action::TYPE_ATTACK, ActionAttack::deserialize);
  }
};
ActorStaticInitializer init;
