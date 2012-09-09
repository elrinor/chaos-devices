#ifndef __GAME_H__
#define __GAME_H__
#include "config.h"
#include "DBase.h"
#include "forward.h"

class Game: public DBase {
public:
  enum ActorType {
    NOBODY,
    ATTACKER,
    DEFENDER
  };

private:
  QHash<int, Unit*> units;
  QHash<int, Tool*> tools;
  QHash<int, Link*> links;
  arx::ArrayList<Unit*> unitsList;
  arx::ArrayList<Tool*> toolsList;
  arx::ArrayList<Link*> linksList;
  GameMap* map;

	QHash<int, AttackComponent*> attackComponents;
	QHash<int, Attack*> attacks;
	
	QHash<int, Defence*> defences;

  QHash<int, Action*> actions;
  QHash<int, arx::ArrayList<Action*> > actionsForTurn;

  int defenderResources, defenderStartingResources;
  int attackerResources, attackerStartingResources;

	bool started;
  bool over;
  ActorType winner;
  int turn;

	Actor* defender;
	Actor* attacker;

  Logger* logger;

  QString objectives;

  Game();

  void logMessage(QString message);
  int act(Actor* actor);
  int damageUnitForOverusage(int id);
  int damageUnitForConstraints(int id);

protected:
  template<class KeyT, class MappedT> void checkedDelete(KeyT key, QHash<KeyT, MappedT>& hashMap);

public:
  Unit* getUnit(int id) const;
  Link* getLink(int id) const;
  Tool* getTool(int id) const;
  GameMap* getMap() const { return this->map; }

	AttackComponent* getAttackComponent(int id) const;
	Attack* getAttack(int id) const;

	Defence* getDefence(int id) const;

  Action* getAction(int id) const;
  const arx::ArrayList<Action*> getActionsForTurn(int turn) const;

  const arx::ArrayList<Unit*> getUnits() const {return this->unitsList;}
  const arx::ArrayList<Tool*> getTools() const {return this->toolsList;}
  const arx::ArrayList<Link*> getLinks() const {return this->linksList;}

  int getDefenderResources() const { return this->defenderResources; }
  int getAttackerResources() const { return this->attackerResources; }
  int getDefenderStartingResources() const { return this->defenderStartingResources; }
  int getAttackerStartingResources() const { return this->attackerStartingResources; }
  int getDefenderSpentResources() const { return getDefenderStartingResources() - getDefenderResources(); }
  int getAttackerSpentResources() const { return getAttackerStartingResources() - getAttackerResources(); }

  Actor* getAttacker() const { return this->attacker; }
  Actor* getDefender() const { return this->defender; }

  Logger* getLogger() const { return this->logger; }

  QString getObjectives() const { return this->objectives; }

  int getTurn() const { return this->turn; }

  bool isStarted() const { return this->started; }
  bool isOver() const { return this->over; }
  
  ActorType getWinner() const { return this->winner; }

  void addUnit(Unit* unit);
  void addLink(Link* link);
  void addTool(Tool* software);
	
	void addAttack(Attack* attack);
	void addAttackComponent(AttackComponent* attackComponent);

	void addDefence(Defence* defence);

  void addAction(Action* action);

  void deleteUnit(int id);
  void deleteLink(int id);
  void deleteTool(int id);
	void deleteDefence(int id);

  void moveUnit(int unitId, int newX, int newY);

  void setLogger(Logger* logger) { this->logger = logger; }

  static Game* deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;

  static Game* createEmptyGame(int mapWidth, int mapHeight, int defenderResources, int attackerResources, QString objectives);

  void start(QString defenderTypeName, QString attackerTypeName);
	void makeTurn();

  void performAttack(int id, bool estimateOnly = false);

  virtual ~Game();

  Game* clone();
};


#endif