#ifndef __DEFENCE_H__
#define __DEFENCE_H__

#include "config.h"
#include "Game.h"
#include "Tool.h"

enum DefenceTypeIds {
  ACL,
  AVP,
  DSC,
  ENC,
  FRW,
  IND,
  BUP,
  UTR,
  PPR,
	/*REDUNDANT_SYSTEMS,*/
  NO_DEFENCE = -1
};

class DefenceClass {
private:
  DBase* dBase;
  int id;
  QString name;
  QString imageName;

  friend class DBase;

public:
  DefenceClass(int id, QString name, QString imageName): id(id), name(name), imageName(imageName) {}

  int getId() const { return this->id; }
  QString getName() const { return this->name; }
  QString getImageName() const { return this->imageName; }

  static DefenceClass* deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;
};

class DefenceType {
private:
  DBase* dBase;
	int id;
	int classId;
  int defence;
  int armor;

  friend class DBase;
	friend class Game;

public:
  DefenceType(int id, int classId, int defence, int armor): id(id), classId(classId), defence(defence), armor(armor) {}

  int getId() const { return this->id; }
  DefenceClass* getClass() const { return this->dBase->getDefenceClass(this->classId); }
  int getDefence() const { return this->defence; }
  int getArmor() const { return this->armor; }

  static DefenceType* deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;

	Defence* spawnDefence(Game* target, int toolId) const;
};

class Defence {
private:
	Game* game;
	int id;
	int typeId;
	int toolId;

	friend class DBase;
	friend class Game;

public:
	Defence(int id, int typeId, int toolId): id(id), typeId(typeId), toolId(toolId) {}

	int getId() const { return this->id; }
	DefenceType* getType() const { return this->game->getDefenceType(this->typeId); }
	Tool* getTool() const { return this->game->getTool(this->toolId); }
	int getDefence() const { return (int) (this->getType()->getDefence() * getTool()->getLife()); }
	int getArmor() const { return (int) (this->getType()->getArmor() * getTool()->getLife()); }

	static Defence* deserialize(std::istream& stream);
	void serialize(std::ostream& stream) const;
};

#endif