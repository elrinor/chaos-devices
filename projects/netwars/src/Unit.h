#ifndef __UNIT_H__
#define __UNIT_H__
#include "config.h"
#include <QString>
#include <iostream>
#include "Game.h"
#include "arx/Collections.h"
#include "Tool.h"
#include "Link.h"
#include "FuzzySet.h"

enum {
  UNIT_CLASS_INTERNET = 0
};

// -------------------------------------------------------------------------- //
// UnitClass
// -------------------------------------------------------------------------- //
class UnitClass {
private:
  DBase* dBase;
  int id;
  QString name;
  
  friend class DBase;

public:
  UnitClass(int id, QString name): id(id), name(name) {}

  int getId() const {return this->id;}
  QString getName() const {return this->name;}

  static UnitClass* deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;
};


// -------------------------------------------------------------------------- //
// UnitType
// -------------------------------------------------------------------------- //
class UnitType {
private:
  DBase* dBase;
  int id;
  QString name;
  int classId;
  QString imageName;
  int cost;

  friend class DBase;

public:
  UnitType(int id, QString name, int cost, int classId, QString imageName): 
    id(id), name(name), cost(cost), classId(classId), imageName(imageName) {}

  int getId() const {return this->id;}
  QString getName() const {return this->name;}
  UnitClass* getClass() const {return this->dBase->getUnitClass(this->classId);}
  QString getImageName() const {return this->imageName;}
  int getCost() const {return this->cost;}

  static UnitType* deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;
};

// -------------------------------------------------------------------------- //
// UnitConfiguration
// -------------------------------------------------------------------------- //
class UnitConfiguration {
private:
  DBase* dBase;
  int id;
  QString name;
  int typeId;
  arx::ArrayList<int> toolTypeIds;

  friend class DBase;

public:
  UnitConfiguration(int id, QString name, int typeId, arx::ArrayList<int> toolTypeIds): 
    id(id), name(name), typeId(typeId), toolTypeIds(toolTypeIds) {}

  int getId() const {return this->id;}
  QString getName() const {return this->name;}
  UnitType* getType() const {return this->dBase->getUnitType(this->typeId);}

  int getToolTypeSize() const {return this->toolTypeIds.size();}
  ToolType* getToolType(int index) const {return this->dBase->getToolType(this->toolTypeIds[index]);}

  int getCost() const;

  Unit* spawnUnit(Game* target, QString name, int x, int y, bool movable, bool removable) const;

  static UnitConfiguration* deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;
};

// -------------------------------------------------------------------------- //
// Unit
// -------------------------------------------------------------------------- //
class Unit {
private:
  Game* game;
  int id;
  QString name;
  int typeId;
  arx::ArrayList<int> toolIds;
  arx::ArrayList<int> linkIds;
  FuzzySet needsToStrengthen;
  FuzzySet willSurvive;
  int x, y;
  bool movable;
  bool removable;

  friend class DBase;
  friend class Game;

public:
  Unit(int id, QString name, int typeId, FuzzySet surviveSet, FuzzySet strengthenSet, int x, int y, bool movable, bool removable): 
    id(id), name(name), typeId(typeId), needsToStrengthen(strengthenSet), willSurvive(surviveSet), x(x), y(y), movable(movable), removable(removable) {};

  int getId() const {return this->id;}
  QString getName() const {return this->name;}
  UnitType* getType() const {return this->game->getUnitType(this->typeId);}
  FuzzySet getSurviveSet() const {return this->willSurvive;}
  FuzzySet getStrengthenSet() const {return this->needsToStrengthen;}
  int getX() const {return this->x;}
  int getY() const {return this->y;}

  int getHP() const;
  int getMaxHP() const;
  float getLife() const {int maxHP = getMaxHP(); return (maxHP == 0) ? 1 : (float) getHP() / maxHP;};
  bool isAlive() const {return getHP() > 0;}

  int getToolSize() const {return this->toolIds.size();}
  int getLinksSize() const {return this->linkIds.size();}
  Tool* getTool(int index) const {return this->game->getTool(this->toolIds[index]);}
  Link* getLink(int index) const {return this->game->getLink(this->linkIds[index]);}

  float getCPUUsage() const;
  float getPowerUsage() const;
  float getMemoryUsage() const;

  bool isMovable() const { return this->movable; }
  bool isRemovable() const { return this->removable; }

  bool isInternetConnection() const { return this->getType()->getClass()->getId() == UNIT_CLASS_INTERNET; }

  void setName(const QString name) {this->name = name; this->game->notify();}

  int getEmptyCost() const {return this->getType()->getCost() * this->getLife();}
  int getCost() const;

  bool hasLinkTo(int unitId) const;
  Link* linkTo(int unitId, bool removable);

  bool meetsConstraints(/* OUT */ arx::ArrayList<QString> errorMessages) const;
  bool canBeAdded(const ToolType* newToolType) const;

  static Unit* deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;
};

#endif