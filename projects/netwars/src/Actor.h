#ifndef __ATTACKER_H__
#define __ATTACKER_H__

#include <QHash>
#include "Attack.h"
#include "Game.h"
#include "arx/Collections.h"
#include "Util.h"

// -------------------------------------------------------------------------- //
// Action
// -------------------------------------------------------------------------- //
class Action {
public:
  enum {
    TYPE_ATTACK
  };

private:
  typedef Action* (*DeserializeFunction)(std::istream&);

  static QHash<int, DeserializeFunction> deserializers;

  static void registerAction(int type, DeserializeFunction deserializeFunc);

  int id;
  bool activated;
  int turn;

  friend class Game;
  friend class DBase;
  friend class ActorStaticInitializer;

protected:
  Game* game;

public:
  Action(int id): activated(false), turn(-1), id(id) {}

  int getId() const { return this->id; }
  int getTurn() const { return this->turn; }
  bool isActivated() const { return this->activated; }

  virtual int getType() const = 0;
  
  virtual void performAt(Game* game) const = 0;
  
  virtual void serialize(std::ostream& stream) const;

  static Action* deserialize(std::istream& stream);
};


// -------------------------------------------------------------------------- //
// ActionAttack
// -------------------------------------------------------------------------- //
class ActionAttack: public Action {
private:
  int attackId;

public:
  ActionAttack(int id, int attackId): Action(id), attackId(attackId) {}

  Attack* getAttack() const { return this->game->getAttack(this->attackId); }

  virtual int getType() const { return TYPE_ATTACK; }
  
  virtual void performAt(Game* game) const { 
    game->performAttack(this->attackId);
  }

  virtual void serialize(std::ostream& stream) const;
  static Action* deserialize(std::istream& stream);
};


// -------------------------------------------------------------------------- //
// Actor
// -------------------------------------------------------------------------- //
class Actor {
public:
  enum {
    TYPE_ATTACKER = 1,
    TYPE_DEFENDER = 2
  };

private:
  typedef Actor* (*DeserializeFunction)(std::istream&);
  typedef Actor* (*FactoryFunction)();
  static QHash<QString, DeserializeFunction> deserializers;
  static QHash<QString, FactoryFunction> factories;
  static arx::ArrayList<QString> attackerTypeNames;
  static arx::ArrayList<QString> defenderTypeNames;

  static void registerActor(QString typeName, DeserializeFunction deserializeFunc, FactoryFunction factoryFunc, int typeFlags);

  friend class Game;
  friend class DBase;
  friend class ActorStaticInitializer;

  static const arx::ArrayList<QString> getAttackerTypeNames() { return attackerTypeNames; }
  static const arx::ArrayList<QString> getDefenderTypeNames() { return defenderTypeNames; }

protected:
  Game* game;

public:
  virtual arx::ArrayList<int> act() = 0;
  virtual QString getName() const = 0;
  virtual void serialize(std::ostream& stream) const;

  static Actor* deserialize(std::istream& stream);
  
  static Actor* create(QString typeName);
};

#endif