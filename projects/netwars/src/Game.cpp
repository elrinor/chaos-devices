#include "config.h"
#include "Game.h"
#include <cassert>
#include <algorithm>
#include <limits>
#include <sstream>
#include "Util.h"
#include "Unit.h"
#include "Tool.h"
#include "GameMap.h"
#include "Defence.h"
#include "Attack.h"
#include "Constraint.h"
#include "Actor.h"
#include "Logger.h"

using namespace arx;
using namespace std;

#define TT(x) QString::fromWCharArray(L ## x )

class Colorer {
public:
  static QString attacker(QString s) {
    return "<font color=#FF0000>" + s + "</font>";
  }

  static QString defender(QString s) {
    return "<font color=#10A0FF>" + s + "</font>";
  }
};

class DefenceListCreator {
private:
	ArrayList<int> defenceIds;

public:
	DefenceListCreator() {}
	DefenceListCreator operator() (int classId, float defence, float armor) {
		DefenceType* d = new DefenceType(INVALID_ID, classId, defence, armor);
		this->dBase->addDefenceType(d);
		this->defenceIds.push_back(d->getId());
		return *this;
	}
	operator ArrayList<int>() {
		return this->defenceIds;
	}

	static DBase* dBase;
};

DBase* DefenceListCreator::dBase = NULL;

DefenceListCreator defenceList(int classId, float defence, float armor) {
	return DefenceListCreator()(classId, defence, armor);
}

DefenceListCreator defenceList() {
	return DefenceListCreator();
}

Game::Game() {
  this->logger = NULL;
  this->started = false;
  this->over = false;
  this->turn = 0;
  this->attacker = Actor::create("EmptyActor");
  this->defender = Actor::create("EmptyActor");
}

Game* Game::createEmptyGame(int mapWidth, int mapHeight, int defenderResources, int attackerResources, QString objectives) {
  Game* game = new Game();
	DefenceListCreator::dBase = game;

  game->defenderResources = std::numeric_limits<int>::max();
  game->attackerResources = std::numeric_limits<int>::max();

  game->map = new GameMap(game, mapWidth, mapHeight);
  game->objectives = objectives;

  game->addDefenceClass(new DefenceClass(ACL, TT("Контроль доступа"),       ""));
  game->addDefenceClass(new DefenceClass(AVP, TT("Антивирусная защита"),    ""));
  game->addDefenceClass(new DefenceClass(DSC, TT("Отключение"),             ""));
  game->addDefenceClass(new DefenceClass(ENC, TT("Шифрование"),             ""));
  game->addDefenceClass(new DefenceClass(FRW, TT("Брандмауэр"),             ""));
  game->addDefenceClass(new DefenceClass(IND, TT("Обнаружение вторжений"),  ""));
  game->addDefenceClass(new DefenceClass(BUP, TT("Резервное копирование"),  ""));
  game->addDefenceClass(new DefenceClass(UTR, TT("Обучение пользователей"), ""));
  game->addDefenceClass(new DefenceClass(PPR, TT("Физическая защита"),      ""));

  game->addToolClass(new ToolClass(0,  TT("Центральный процессор")));
  game->addToolClass(new ToolClass(1,  TT("Блок питания")));
  game->addToolClass(new ToolClass(2,  TT("Модуль оперативной памяти")));
  game->addToolClass(new ToolClass(3,  TT("Сетевой адаптер")));
  game->addToolClass(new ToolClass(4,  TT("Корпус")));

  game->addToolClass(new ToolClass(10, TT("ОС")));
  game->addToolClass(new ToolClass(11, TT("Антивирус")));
  game->addToolClass(new ToolClass(12, TT("Брандмауэр")));

  game->addToolType(new ToolType(0,   "Intel Celeron",                   ":/pic_tool_cpu.png",      10,  20,   1000,  1,    -70,  0,  defenceList(PPR, 0, 0)));
  game->addToolType(new ToolType(1,   "Intel Pentium 4",                 ":/pic_tool_cpu.png",      20,  20,   2000,  1,    -130, 0,  defenceList(PPR, 0, 0)));
  game->addToolType(new ToolType(2,   "AMD Athlon XP",                   ":/pic_tool_cpu.png",      30,  20,   2500,  1,    -100, 0,  defenceList(PPR, 0, 0)));
  game->addToolType(new ToolType(3,   "AMD Athlon 64",                   ":/pic_tool_cpu.png",      70,  20,   5000,  1,    -120, 0,  defenceList(PPR, 0, 0)));
  game->addToolType(new ToolType(4,   "Intel Core2Duo",                  ":/pic_tool_cpu.png",      100, 30,   7000,  4,    -140, 0,  defenceList(PPR, 0, 0)));
  game->addToolType(new ToolType(5,   "Intel Core2Quad",                 ":/pic_tool_cpu.png",      300, 30,   17000, 12,   -250, 0,  defenceList(PPR, 0, 0)));

  game->addToolType(new ToolType(10,  TT("Блок питания 250W"),           ":/pic_tool_psu_1.png",    10,  100,  0,     0,    250,  1,  defenceList(PPR, 15, 2)));
  game->addToolType(new ToolType(11,  TT("Блок питания 350W"),           ":/pic_tool_psu_1.png",    20,  100,  0,     0,    350,  1,  defenceList(PPR, 15, 2)));
  game->addToolType(new ToolType(12,  TT("Блок питания 450W"),           ":/pic_tool_psu_1.png",    30,  100,  0,     0,    450,  1,  defenceList(PPR, 10, 2)));
  game->addToolType(new ToolType(13,  TT("Блок питания 600W"),           ":/pic_tool_psu_2.png",    50,  100,  0,     0,    600,  1,  defenceList(PPR, 10, 2)));
  game->addToolType(new ToolType(14,  TT("Блок питания 1000W"),          ":/pic_tool_psu_3.png",    100, 100,  0,     0,   1000,  1,  defenceList(PPR, 10, 2)));

  game->addToolType(new ToolType(20,  TT("Модуль памяти 256MB"),         ":/pic_tool_ram.png",      15,  10,   0,     256,  -8,   2,  defenceList(PPR, 0, 0)));
  game->addToolType(new ToolType(21,  TT("Модуль памяти 512MB"),         ":/pic_tool_ram.png",      30,  10,   0,     512,  -8,   2,  defenceList(PPR, 0, 0)));
  game->addToolType(new ToolType(22,  TT("Модуль памяти 1GB"),           ":/pic_tool_ram.png",      50,  10,   0,     1024, -10,  2,  defenceList(PPR, 0, 0)));
  game->addToolType(new ToolType(23,  TT("Модуль памяти 2GB"),           ":/pic_tool_ram.png",      90,  10,   0,     2048, -10,  2,  defenceList(PPR, 0, 0)));

  game->addToolType(new ToolType(30,  TT("Программный сетевой адаптер"), ":/pic_tool_nic.png",      5,   10,   -100,  -20,  -10,  3,  defenceList(PPR, 5, 0)));
  game->addToolType(new ToolType(31,  TT("Стандартный сетевой адаптер"), ":/pic_tool_nic.png",      10,  10,   0,     0,    -10,  3,  defenceList(PPR, 5, 0)));
  game->addToolType(new ToolType(32,  TT("Защищенный сетевой адаптер"),  ":/pic_tool_nic_2.png",    40,  50,   0,     0,    -20,  3,  defenceList(PPR, 5, 0)(DSC, 25, 10)));

  game->addToolType(new ToolType(40,  TT("Стандратный корпус"),          ":/pic_tool_case_1.png",   30,  100,  0,     0,    0,    4,  defenceList(PPR, 15, 0)));
  game->addToolType(new ToolType(41,  TT("Защищенный корпус"),           ":/pic_tool_case_2.png",   70,  200,  0,     0,    -30,  4,  defenceList(PPR, 25, 0)));
  game->addToolType(new ToolType(42,  TT("Бронированный корпус"),        ":/pic_tool_case_3.png",   200, 600,  0,     0,    -100, 4,  defenceList(PPR, 50, 0)));

  game->addToolType(new ToolType(100, "MS DOS",                          ":/pic_tool_os_dos.png",   1,   10,   -30,   -4,   0,    10, defenceList(ACL, 0,  0)(AVP,  0, 0)(ENC, 0,  0)(FRW, 0,  0)));
  game->addToolType(new ToolType(101, "Windows 98",                      ":/pic_tool_os_98.png",    20,  20,   -100,  -32,  0,    10, defenceList(ACL, 5,  0)(AVP,  0, 0)(ENC, 0,  0)(FRW, 0,  0)));
  game->addToolType(new ToolType(102, "Windows XP",                      ":/pic_tool_os_xp.png",    50,  50,   -400,  -128, 0,    10, defenceList(ACL, 15, 3)(AVP,  0, 1)(ENC, 5,  1)(FRW, 15, 5)));
  game->addToolType(new ToolType(103, "Windows 2003 Server",             ":/pic_tool_os_xp.png",    100, 100,  -600,  -192, 0,    10, defenceList(ACL, 20, 5)(AVP,  0, 1)(ENC, 5,  1)(FRW, 20, 7)));

  game->addToolType(new ToolType(110, TT("Антивирус Касперского"),       ":/pic_tool_av_avp.png",   100, 100,  -1000, -256, 0,    11, defenceList(ACL, 25, 10)(AVP, 30, 10)));
  game->addToolType(new ToolType(111, TT("Антивирус NOD32"),             ":/pic_tool_av_nod32.png", 200, 150,  -700,  -128, 0,    11, defenceList(ACL, 35, 10)(IND, 10, 5)(AVP, 50, 20)));
  game->addToolType(new ToolType(112, TT("Антивирус McAfee VirusScan"),  ":/pic_tool_av_mcaff.png", 200, 150,  -1200, -256, 0,    11, defenceList(ACL, 25, 10)(IND, 15, 7)(AVP, 35, 20)(FRW, 15, 7)));

  game->addToolType(new ToolType(120, TT("Брандмауэр OutPost Firewall"), ":/pic_tool_frw_outp.png", 200, 200,  -2000, -256, 0,    12, defenceList(ACL, 35, 10)(FRW, 50, 20)(IND, 25, 10)));
  

  game->addUnitClass(new UnitClass(UNIT_CLASS_INTERNET, TT("Интернет")));
  game->addUnitClass(new UnitClass(1, TT("Компьютер")));

  game->addUnitType(new UnitType(0, TT("Выход в Интернет"),       0,   UNIT_CLASS_INTERNET, ":/pic_unit_internet.png"));
  game->addUnitType(new UnitType(1, TT("Персональный компьютер"), 100, 1,                   ":/pic_unit_pc.png"));
  game->addUnitType(new UnitType(2, TT("Сервер"),                 100, 1,                   ":/pic_unit_server.png"));

  game->addUnitConfiguration(new UnitConfiguration(0, TT("Выход в Интернет"),       0, arrayList<int>()));
  game->addUnitConfiguration(new UnitConfiguration(1, TT("Персональный компьютер"), 1, arrayList<int>(1)(10)(21)(30)(102)));
  game->addUnitConfiguration(new UnitConfiguration(2, TT("Сервер"),                 2, arrayList<int>(4)(11)(22)(32)(103)));

  game->addConstraint(new Constraint(0,    1,  0,  MUST_HAVE,   TT("У ПК должен быть ЦП!")));
  game->addConstraint(new Constraint(1,    1,  0,  CAN_HAVE_1,  TT("У ПК не может быть более одного ЦП!")));
  game->addConstraint(new Constraint(2,    1,  1,  MUST_HAVE,   TT("У ПК должен быть БП!")));
  game->addConstraint(new Constraint(3,    1,  2,  MUST_HAVE,   TT("У ПК должен быть модуль памяти!")));
  game->addConstraint(new Constraint(4,    1,  3,  MUST_HAVE,   TT("У ПК должен быть сетевой адаптер!")));
  game->addConstraint(new Constraint(5,    1,  10, MUST_HAVE,   TT("На ПК должна быть установлена ОС!")));
  game->addConstraint(new Constraint(6,    1,  10, CAN_HAVE_1,  TT("На ПК не может быть установлено более одной ОС!")));
  game->addConstraint(new Constraint(7,    1,  4,  CAN_HAVE_1,  TT("У ПК не может быть более одного корпуса!")));

  game->addConstraint(new Constraint(100,  0,  0,  CANNOT_HAVE, ""));
  game->addConstraint(new Constraint(101,  0,  1,  CANNOT_HAVE, ""));
  game->addConstraint(new Constraint(102,  0,  2,  CANNOT_HAVE, ""));
  game->addConstraint(new Constraint(103,  0,  3,  CANNOT_HAVE, ""));
  game->addConstraint(new Constraint(104,  0,  10, CANNOT_HAVE, ""));
  game->addConstraint(new Constraint(105,  0,  11, CANNOT_HAVE, ""));
  game->addConstraint(new Constraint(106,  0,  12, CANNOT_HAVE, ""));

  game->addAttackComponentType(new AttackComponentType(0,  AVP, 5,  1,   0, 2));
  game->addAttackComponentType(new AttackComponentType(1,  ACL, 10, 0.3, 0, 0));
  game->addAttackType(new AttackType(0, "Virus", true, true, 50, 5, 2, arrayList(0)(1)));

  game->addAttackComponentType(new AttackComponentType(10, FRW, 5,  1, 0, 2));
  game->addAttackComponentType(new AttackComponentType(11, DSC, 10, 0.5, 0, 0));
  game->addAttackType(new AttackType(1, "Flooding", true, true, 50, 5, 2, arrayList(10)(11)));

  game->addAttackComponentType(new AttackComponentType(20, DSC, 5,  1, 0, 2));
  game->addAttackType(new AttackType(2, "Jamming", false, false, 20, 2, 1, arrayList(20)));

  game->addAttackComponentType(new AttackComponentType(30, ACL, 5,  1, 0, 2));
  game->addAttackComponentType(new AttackComponentType(31, ENC, 10, 0.5, 0, 0));
  game->addAttackType(new AttackType(3, "Packet Sniffer", false, false, 20, 2, 1, arrayList(30)(31)));

  game->addAttackComponentType(new AttackComponentType(40, ACL, 5,  1, 0, 2));
  game->addAttackComponentType(new AttackComponentType(41, IND, 10, 0.5, 0, 0));
  game->addAttackType(new AttackType(4, "Data Modification", false, false, 20, 2, 1, arrayList(30)(31)));

  game->addAttackComponentType(new AttackComponentType(50, PPR, 5,  1, 0, 2));
  game->addAttackType(new AttackType(5, "Physical Attack", false, false, 50, 5, 2, arrayList(50)));

  game->addAllowedDefenderTypeName("*");
  game->addAllowedAttackerTypeName("*");

/*  game->addAllowedAttackerTypeName("Killer");*/

/*  game->getUnitConfiguration(0)->spawnUnit(game, TT("Интернет"), 2, 2, false, false)->linkTo(
    game->getUnitConfiguration(1)->spawnUnit(game, TT("Персональный Компьютер"), 0, 2, false, false)->getId(), false);

  game->objectives = TT("Защитить сеть от нападений атакующего");*/

  //game->getUnitConfiguration(0)->spawnUnit(game, "Katobr", mapWidth / 2, mapHeight / 2, false, false);

  game->defenderStartingResources = game->defenderResources = defenderResources;
  game->attackerStartingResources = game->attackerResources = attackerResources;

  /*game->defenderStartingResources = game->defenderResources + 215;*/

  return game;
}

void Game::start(QString defenderTypeName, QString attackerTypeName) {
  assert(getAllowedDefenderTypeNames().contains(defenderTypeName));
  assert(getAllowedAttackerTypeNames().contains(attackerTypeName));

  delete this->defender;
  this->defender = Actor::create(defenderTypeName);
  this->defender->game = this;

  delete this->attacker;
  this->attacker = Actor::create(attackerTypeName);
	this->attacker->game = this;

	this->started = true;

  FOREACH(Unit* unit, units)
    damageUnitForConstraints(unit->getId());

  logMessage(QString::fromWCharArray(L"<font color=#000000>&nbsp;&nbsp;ИГРА НАЧАЛАСЬ</font>"));
  logMessage("");
}

int Game::act(Actor* actor) {
  arx::ArrayList<int> actionIds = actor->act();
  for(unsigned int i = 0; i < actionIds.size(); i++) {
    Action* action = getAction(actionIds[i]);
    action->activated = true;
    action->turn = this->turn;
    action->performAt(this);

    assert(!this->actionsForTurn[action->getTurn()].contains(action));
    this->actionsForTurn[action->getTurn()].add(action);
  }
  return actionIds.size();
}

void Game::makeTurn() {
  this->turn++;
  
  logMessage(QString::fromWCharArray(L"<font color=#000000>&nbsp;&nbsp;ХОД ") + QString().setNum(this->turn) + "</font>");
  logMessage(QString::fromWCharArray(L"Ход ") + Colorer::attacker(QString::fromWCharArray(L"атакующего")));
  int attackerActionCount = act(this->attacker);

  logMessage(QString::fromWCharArray(L"Ход ") + Colorer::defender(QString::fromWCharArray(L"Защищающего")));
  int defenderActionCount = act(this->defender);
  logMessage("");

  static int attackerInactiveTurnCount = 0;
  if(attackerActionCount > 0)
    attackerInactiveTurnCount = 0;
  else
    attackerInactiveTurnCount++;

  if(!isOver()) {
    bool aliveUnitExists = false;
    FOREACH(Unit* unit, getUnits()) {
      if(unit->getHP() > 0) {
        aliveUnitExists = true;
        break;
      }
    }
    if(!aliveUnitExists) {
      this->over = true;
      this->winner = ATTACKER;
    }

    if(attackerInactiveTurnCount == 3) {
      this->over = true;
      this->winner = DEFENDER;
    }

    if(isOver()) {
      logMessage(QString::fromWCharArray(L"<font color=#000000>&nbsp;&nbsp;ИГРА ОКОНЧЕНА</font>")); 
      if(getWinner() == DEFENDER)
        logMessage(Colorer::defender(QString::fromWCharArray(L"ПОБЕДИЛ ЗАЩИЩАЮЩИЙ")));
      else
        logMessage(Colorer::attacker(QString::fromWCharArray(L"ПОБЕДИЛ АТАКУЮЩИЙ")));
      logMessage(TT("Рейтинг ") + Colorer::defender(TT("Защищающего")) + " " + QString().setNum((float) getAttackerSpentResources() / getDefenderSpentResources()));
    }
  }
  
  notify();
}

int Game::damageUnitForOverusage(int id) {
  Unit* unit = getUnit(id);
  int damageInflicted = 0;

  for(int iter = 0; iter < 2; iter++) { // this is needed!
    float powerUsage = unit->getPowerUsage();
    float cpuUsage = unit->getCPUUsage();
    float memUsage = unit->getMemoryUsage();

    if(powerUsage < 1 && cpuUsage < 1 && memUsage < 1) 
      break;

    for(int i = 0; i < unit->getToolSize(); i++) {
      Tool* tool = unit->getTool(i);

      if(tool->getHP() == 0)
        continue;

      float toolKill = 1.0f;

      if(tool->getPowerProduction() < 0 && powerUsage > 1)
        toolKill /= powerUsage;

      if(tool->getMemProduction() < 0 && memUsage > 1)
        toolKill /= memUsage;

      if(tool->getCpuProduction() < 0 && cpuUsage > 1)
        toolKill /= cpuUsage;

      int newHp = min((int) (tool->getMaxHP() * toolKill), tool->hp);
      damageInflicted += tool->hp - newHp;
      tool->hp = newHp;
    }
  }

  return damageInflicted;
}

int Game::damageUnitForConstraints(int id) {
  Unit* unit = getUnit(id);
  int damageInflicted = 0;
  if(!unit->meetsConstraints(ArrayList<QString>())) {
    for(int i = 0; i < unit->getToolSize(); i++) {
      Tool* tool = unit->getTool(i);
      damageInflicted += tool->hp;
      tool->hp = 0;
    }
  }
  return damageInflicted;
}

struct AttackComponentCmp {
  bool operator()(const AttackComponent* a, const AttackComponent* b) {
    return a->getAttack() < b->getAttack();
  }
};

struct DefenceCmp {
  bool operator()(const Defence* a, const Defence* b) {
    return a->getDefence() > b->getDefence();
  }
};

void Game::performAttack(int id, bool estimateOnly) {
  Attack* attack = getAttack(id);

  assert(estimateOnly || this->attackerResources >= attack->getCost());
  assert(!attack->isActivated());
  assert(!attack->getType()->isFromInternetOnly() || attack->getTargetUnit()->isInternetConnection());

  // we cheat a little for estimation
  Logger* oldLogger = this->logger;
  if(estimateOnly)
    this->setLogger(NULL);

  QString message = QString::fromWCharArray(L"Атака ") + Colorer::attacker(attack->getType()->getName() + "[" + QString().setNum(attack->getStrength()) + "]");

  if(!estimateOnly)
    this->attackerResources -= attack->getCost();

  attack->activated = true;
  attack->successful = false;
  attack->unitsAttacked = 0;
  attack->damageInflicted = 0;
  attack->attackedUnitIds.clear();
  attack->successfullyAttackedUnitIds.clear();

  // Create attack components list for current attack
  ArrayList<AttackComponent*> components;
  for(int j = 0; j < attack->getComponentSize(); j++)
    components.push_back(attack->getComponent(j));
  sort(components.begin(), components.end(), AttackComponentCmp());

  // Cycle through target units for current attack
  ArrayList<Unit*> targetUnits = arrayList(attack->getTargetUnit());
  for(unsigned int j = 0; j < targetUnits.size(); j++) {
    Unit* targetUnit = targetUnits[j];

    attack->unitsAttacked++;
    attack->attackedUnitIds.push_back(targetUnit->getId());

    bool success = true;
    bool damaged = false;

    if(targetUnit->isAlive()) {
      // Cycle through all attack components
      for(unsigned int k = 0; k < components.size() && success; k++) {
        AttackComponent* attackComponent = components[k];

        // Build defence list
        ArrayList<Defence*> defences;
        for(int l = 0; l < targetUnit->getToolSize(); l++) {
          Tool* targetTool = targetUnit->getTool(l);
          for(int m = 0; m < targetTool->getDefenceSize(); m++) {
            Defence* defence = targetTool->getDefence(m);
            if(defence->getType()->getClass()->getId() == attackComponent->getType()->getDefenceClass()->getId())
              defences.push_back(defence);
          }
        }
        sort(defences.begin(), defences.end(), DefenceCmp());

        // Cycle through all defences
        for(unsigned int l = 0; l < defences.size() && success; l++) {
          Defence* defence = defences[l];

          int ar = attackComponent->attackRoll();

          if(ar > defence->getDefence()) {
            int dr = attackComponent->damageRoll();

            if(dr > defence->getArmor() && defence->getTool()->getHP() > 0) {
              int damage = min(defence->getTool()->getHP(), dr - defence->getArmor());
              if(!estimateOnly)
                defence->getTool()->hp -= damage; 
              attack->damageInflicted += damage;
              damaged = true;
            }
          } else {
            logMessage(message + QString::fromWCharArray(L" была отражена ") + 
              Colorer::defender(targetUnit->getName()) + " -> " + Colorer::defender(defence->getTool()->getType()->getName()) + " -> " + Colorer::defender(defence->getType()->getClass()->getName()));
            success = false;
          }
        }

        // Damage main unit if attack component was successful (with x10 modifier)
        if(success) {
          int damageLeft = 0;
          for(int i = 0; i < 10; i++)
            damageLeft += attackComponent->damageRoll();

          for(int l = 0; l < targetUnit->getToolSize(); l++) {
            Tool* targetTool = targetUnit->getTool(l);

            if(targetTool->getHP() > 0) {
              int damage = min(targetTool->getHP(), damageLeft);
              if(!estimateOnly)
                targetTool->hp -= damage;
              damageLeft -= damage;
              attack->damageInflicted += damage;
              damaged = true;
            }

            if(damageLeft == 0)
              break;
          }
        }
      }
    }

    // Handle CPU/Mem/Power overusage
    if(damaged && !estimateOnly) /* TODO: think it over */
      attack->damageInflicted += damageUnitForOverusage(targetUnit->getId());

    if(success) {
      if(!targetUnit->isInternetConnection()) {
        logMessage(message + QString::fromWCharArray(L" была успешна на ") + Colorer::defender(targetUnit->getName()));
        attack->successfullyAttackedUnitIds.push_back(targetUnit->getId());

        if(attack->successful == false)
          attack->successful = true;
      }

      // propagate
      if(attack->getType()->isSelfPropagating()) {
        bool propagated = false;
        for(int k = 0; k < targetUnit->getLinksSize(); k++) {
          Unit* newUnit = targetUnit->getLink(k)->getOtherEnd(targetUnit->getId());

          bool found = false;
          for(unsigned int l = 0; l < targetUnits.size(); l++) {
            if(targetUnits[l]->getId() == newUnit->getId()) {
              found = true;
              break;
            }
          }
          if(!found) {
            targetUnits.push_back(newUnit);
            propagated = true;
          }
        }
        if(propagated)
          logMessage(message + QString::fromWCharArray(L" распространилась по сети"));
      }
    }
  }

  logMessage(message + QString::fromWCharArray(L" была ") + (attack->successful ? "" : QString::fromWCharArray(L"не")) + 
    QString::fromWCharArray(L"успешна с нанесенным уроном ") + Colorer::attacker(QString().setNum(attack->damageInflicted)));

  notify();

  // leave it as it was
  if(estimateOnly)
    this->setLogger(oldLogger);

}

template<class KeyT, class MappedT>
void Game::checkedDelete(KeyT key, QHash<KeyT, MappedT>& hashMap) {
  QHash<KeyT, MappedT>::iterator pos = hashMap.find(key);
  assert(pos != hashMap.end());
  hashMap.erase(pos);
}

void Game::moveUnit(int unitId, int newX, int newY) {
  assert(!this->isStarted());
  Unit* unit = getUnit(unitId);
  assert(unit->isMovable());
  this->map->ensureBounds(unit->getX(), unit->getY());
  assert(!this->map->hasUnit(newX, newY));
  this->map->unitIds[unit->getX()][unit->getY()] = INVALID_ID;
  this->map->unitIds[newX][newY] = unit->id;
  unit->x = newX;
  unit->y = newY;
}

void Game::logMessage(QString message) {
  if(this->logger != NULL)
    this->logger->logMessage(message);
}

Unit* Game::getUnit(int id) const {
  return checkedGet(id, this->units);
}

Link* Game::getLink(int id) const {
  return checkedGet(id, this->links);
}

Tool* Game::getTool(int id) const {
  return checkedGet(id, this->tools);
}

AttackComponent* Game::getAttackComponent(int id) const {
	return checkedGet(id, this->attackComponents);
}

Attack* Game::getAttack(int id) const {
	return checkedGet(id, this->attacks);
}

Defence* Game::getDefence(int id) const {
	return checkedGet(id, this->defences);
}

Action* Game::getAction(int id) const {
  return checkedGet(id, this->actions);
}

const arx::ArrayList<Action*> Game::getActionsForTurn(int turn) const {
  assert(turn <= this->turn);
  return this->actionsForTurn[turn];
}

void Game::addUnit(Unit* unit) {
  assert(!this->isStarted());

  this->map->ensureBounds(unit->getX(), unit->getY());
  assert(!this->map->hasUnit(unit->getX(), unit->getY()));
  checkedAdd(this, unit, this->units);
  this->map->unitIds[unit->getX()][unit->getY()] = unit->getId();
  
  assert(this->getDefenderResources() >= unit->getEmptyCost());
  this->defenderResources -= unit->getEmptyCost();

  this->unitsList.add(unit);

  notify();
}

void Game::addLink(Link* link) {
  assert(!this->isStarted());

  checkedAdd(this, link, this->links);
  assert(!link->getSource()->hasLinkTo(link->getTarget()->getId()) && !link->getTarget()->hasLinkTo(link->getSource()->getId()));
  assert(link->getSource()->getId() != link->getTarget()->getId());
  link->getSource()->linkIds.push_back(link->getId());
  link->getTarget()->linkIds.push_back(link->getId());

  this->linksList.add(link);

  notify();
}

void Game::addTool(Tool* tool) {
  assert(!this->isStarted());

  checkedAdd(this, tool, this->tools);
  assert(tool->getUnit()->canBeAdded(tool->getType()));
  assert(tool->getUnit()->toolIds.indexOf(tool->getId()) == -1);
  tool->getUnit()->toolIds.push_back(tool->getId());

  assert(this->getDefenderResources() >= tool->getCost());
  this->defenderResources -= tool->getCost();

  this->toolsList.add(tool);

  notify();
}

void Game::addAttack(Attack* attack) {
	checkedAdd(this, attack, this->attacks);
	notify();
}

void Game::addAttackComponent(AttackComponent* attackComponent) {
	checkedAdd(this, attackComponent, this->attackComponents);
	assert(attackComponent->getAttack()->componentIds.indexOf(attackComponent->getId()) == -1);
	attackComponent->getAttack()->componentIds.push_back(attackComponent->getId());
	notify();
}

void Game::addDefence(Defence* defence) {
  assert(!this->isStarted());

  checkedAdd(this, defence, this->defences);
	assert(defence->getTool()->defenceIds.indexOf(defence->getId()) == -1);
	defence->getTool()->defenceIds.push_back(defence->getId());
	notify();
}

void Game::addAction(Action* action) {
  checkedAdd(this, action, this->actions);
  if(action->isActivated()) {
    assert(!this->actionsForTurn[action->getTurn()].contains(action));
    this->actionsForTurn[action->getTurn()].add(action);
  }
  notify();
}

void Game::deleteTool(int id) {
  assert(!this->isStarted());
  Tool* tool = getTool(id);
  assert(tool->isRemovable());

	while(tool->getDefenceSize() > 0)
		deleteDefence(tool->getDefence(0)->getId());
	checkedDelete(id, this->tools);
  tool->getUnit()->toolIds.remove(id);

  this->defenderResources += tool->getCost();

  this->toolsList.remove(tool);

  delete tool;
  notify();
}

void Game::deleteUnit(int id) {
  assert(!this->isStarted());
  Unit* unit = getUnit(id);
  assert(unit->isRemovable());

  while(unit->getLinksSize() > 0)
    deleteLink(unit->getLink(0)->getId());
  while(unit->getToolSize() > 0)
    deleteTool(unit->getTool(0)->getId());
  this->map->ensureBounds(unit->x, unit->y);
  this->map->unitIds[unit->getX()][unit->getY()] = INVALID_ID;
  checkedDelete(id, this->units);

  this->defenderResources += unit->getEmptyCost();

  this->unitsList.remove(unit);

  delete unit;
  notify();
}

void Game::deleteLink(int id) {
  assert(!this->isStarted());
  Link* link = getLink(id);
  assert(link->isRemovable());

  checkedDelete(id, this->links);
  link->getTarget()->linkIds.remove(link->getId());
  link->getSource()->linkIds.remove(link->getId());

  this->linksList.remove(link);

  delete link;
  notify();
}

void Game::deleteDefence(int id) {
  assert(!this->isStarted());

  Defence* defence = getDefence(id);
	checkedDelete(id, this->defences);
	defence->getTool()->defenceIds.remove(id);
	delete defence;
	notify();
}

Game* Game::deserialize(std::istream& stream) {
  Game* game = new Game();
  DBase::deserialize(stream, game);
  int w, h;
  ::deserialize(stream, w, h);
  game->map = new GameMap(game, w, h);

  // cheat )
  game->attackerResources = std::numeric_limits<int>::max();
  game->defenderResources = std::numeric_limits<int>::max();

  DBase::deserializeQHashAndAdd(stream, game, &Game::addUnit);
  DBase::deserializeQHashAndAdd(stream, game, &Game::addTool);
  DBase::deserializeQHashAndAdd(stream, game, &Game::addLink);
  DBase::deserializeQHashAndAdd(stream, game, &Game::addAttack);
  DBase::deserializeQHashAndAdd(stream, game, &Game::addAttackComponent);
  DBase::deserializeQHashAndAdd(stream, game, &Game::addDefence);
  DBase::deserializeQHashAndAdd(stream, game, &Game::addAction);
  ::deserialize(stream, game->defenderResources, game->attackerResources, game->defenderStartingResources, game->attackerStartingResources, game->started, game->over, game->turn);

  int winner;
  ::deserialize(stream, winner);
  game->winner = (ActorType) winner;

  ::deserialize(stream, game->attacker, game->defender);
  game->attacker->game = game;
  game->defender->game = game;

  ::deserialize(stream, game->objectives);

  return game;
}

void Game::serialize(std::ostream& stream) const {
  ::serialize(stream, (DBase*)this, getMap()->getWidth(), getMap()->getHeight());
  ::serialize(stream, this->units, this->tools, this->links, this->attacks, this->attackComponents, this->defences, this->actions);
	::serialize(stream, getDefenderResources(), getAttackerResources(), this->defenderStartingResources, this->attackerStartingResources, isStarted(), isOver(), getTurn());
  ::serialize(stream, (int) getWinner());
  ::serialize(stream, getAttacker(), getDefender());
  ::serialize(stream, getObjectives());
}

Game::~Game() {
  freeQHash(this->units);
  freeQHash(this->tools);
  freeQHash(this->links);

  freeQHash(this->attackComponents);
  freeQHash(this->attacks);

  freeQHash(this->defences);

  freeQHash(this->actions);

  delete this->map;
}


Game* Game::clone() {
  stringstream s;
  serialize(s);
  return Game::deserialize(s);
}
