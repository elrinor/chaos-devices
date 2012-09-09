#ifndef __TOOL_H__
#define __TOOL_H__
#include "config.h"
#include <QString>
#include "arx/Collections.h"
#include "Game.h"

// -------------------------------------------------------------------------- //
// ToolClass
// -------------------------------------------------------------------------- //
class ToolClass {
private:
  DBase* dBase;
  int id;
  QString name;

  friend class DBase;

public:
  ToolClass(int id, QString name): id(id), name(name) {}

  int getId() const {return this->id;}
  QString getName() const {return this->name;}

  static ToolClass* deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;
};


// -------------------------------------------------------------------------- //
// ToolType
// -------------------------------------------------------------------------- //
class ToolType {
private:
  DBase* dBase;
  int id;
  QString name;
  QString imageName;
  int cpuProduction;
  int memProduction;
  int powerProduction;
  int maxHP;
  int cost;
  int classId;
  arx::ArrayList<int> defenceTypeIds;

  friend class DBase;

public:
  ToolType(int id, QString name, QString imageName, int cost, int maxHP, int cpuProduction, int memProduction, int powerProduction, int classId, arx::ArrayList<int> defenceTypeIds): 
    id(id), name(name), imageName(imageName), cost(cost), cpuProduction(cpuProduction), memProduction(memProduction), powerProduction(powerProduction), maxHP(maxHP), classId(classId), defenceTypeIds(defenceTypeIds) {};

  int getId() const {return this->id;}
  QString getName() const {return this->name;}
  QString getImageName() const {return this->imageName;}
  ToolClass* getClass() const {return this->dBase->getToolClass(this->classId);}
  int getMaxHP() const {return this->maxHP;}
  int getMemProduction() const {return this->memProduction;}
  int getCpuProduction() const {return this->cpuProduction;}
  int getPowerProduction() const {return this->powerProduction;}
  int getCost() const {return this->cost;}

  int getDefenceTypeSize() const {return this->defenceTypeIds.size();}
  DefenceType* getDefenceType(int index) const {return this->dBase->getDefenceType(this->defenceTypeIds[index]);}

  Tool* spawnTool(Game* target, int unitId, bool removable) const;

  static ToolType* deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;
};

// -------------------------------------------------------------------------- //
// Tool
// -------------------------------------------------------------------------- //
class Tool {
private:
  Game* game;
  int id;
  int hp;
  int typeId;
  int unitId;
	arx::ArrayList<int> defenceIds;
  bool removable;

  friend class DBase;
  friend class Game;

public:
  Tool(int id, int hp, int typeId, int unitId, bool removable): id(id), hp(hp), typeId(typeId), unitId(unitId), removable(removable) {};

  int getId() const {return this->id;}
  ToolType* getType() const {return this->game->getToolType(this->typeId);}
  Unit* getUnit() const {return this->game->getUnit(this->unitId);}

  int getHP() const {return this->hp;}
  int getMaxHP() const {return getType()->getMaxHP();}
  float getLife() const {float maxHP = getMaxHP(); return (maxHP == 0) ? 1 : (float) getHP() / maxHP;};

  int getMemProduction() const {float v = this->getType()->getMemProduction(); return (v > 0) ? (int) (v * this->getLife()) : v;}
  int getCpuProduction() const {float v = this->getType()->getCpuProduction(); return (v > 0) ? (int) (v * this->getLife()) : v;}
  int getPowerProduction() const {float v = this->getType()->getPowerProduction(); return (v > 0) ? (int) (v * this->getLife()) : v;}

  int getCost() const {return static_cast<int>(this->getType()->getCost() * getLife());}

  int getDefenceSize() const { return this->defenceIds.size(); }
  Defence* getDefence(int index) const { return this->game->getDefence(this->defenceIds[index]); }

  bool isRemovable() const { return this->removable; }

  static Tool* deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;
};

#endif