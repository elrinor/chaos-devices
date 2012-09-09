#ifndef __DBASE_H__
#define __DBASE_H__
#include <QHash>
#include <QSet>
#include "arx/Collections.h"
#include "config.h"
#include "forward.h"

class ChangeNotificationConsumer {
public:
  virtual void notify() = 0;
};

class DBase {
private:
  QSet<ChangeNotificationConsumer*> notificationConsumers;

  QHash<int, UnitClass*> unitClasses;
  QHash<int, UnitType*> unitTypes;
  QHash<int, UnitConfiguration*> unitConfigurations;
  arx::ArrayList<UnitConfiguration*> unitConfigurationsList;

  QHash<int, ToolClass*> toolClasses;
  QHash<int, ToolType*> toolTypes;
  arx::ArrayList<ToolType*> toolTypesList;

  QHash<int, DefenceClass*> defenceClasses;
  QHash<int, DefenceType*> defenceTypes;

  QHash<int, Constraint*> constraints;
  QHash<int, arx::ArrayList<Constraint*> > constraintsForUnitClass;

	QHash<int, AttackComponentType*> attackComponentTypes;
	QHash<int, AttackType*> attackTypes;
  arx::ArrayList<AttackType*> attackTypesList;

  arx::ArrayList<QString> allowedAttackerTypeNames;
  arx::ArrayList<QString> allowedDefenderTypeNames;

protected:
  template<class MappedT> static int getFreeKey(const QHash<int, MappedT> hashMap) {
    int key;
    do {
      key = rand() + RAND_MAX * rand();
    } while(hashMap.find(key) != hashMap.end());
    return key;
  }

  template<class MappedT> static void setDataSource(DBase* dataSource, MappedT value)  {
    value->dBase = dataSource;
  }

  template<class MappedT> static void setDataSource(Game* dataSource, MappedT value)  {
    value->game = dataSource;
  }

  template<class DataSource, class KeyT, class MappedT> static void checkedAdd(DataSource* dataSource, MappedT value, QHash<KeyT, MappedT>& hashMap)  {
    assert(hashMap.find(value->id) == hashMap.end());
    if(value->id == INVALID_ID)
      value->id = getFreeKey(hashMap);
    setDataSource(dataSource, value);
    hashMap.insert(value->id, value);
  }

  template<class KeyT, class MappedT> static MappedT checkedGet(KeyT key, const QHash<KeyT, MappedT>& hashMap) {
    QHash<KeyT, MappedT>::const_iterator pos = hashMap.find(key);
    assert(pos != hashMap.end());
    return *pos;
  }

  template<class S, class DataTarget, class T> static void deserializeQHashAndAdd(S& stream, DataTarget* target, void (DataTarget::* adder) (T*)) {
    QHash<int, T*> hashMap;
    ::deserialize(stream, hashMap);
    for(QHash<int, T*>::iterator i = hashMap.begin(); i != hashMap.end(); i++)
      (target->*adder)(*i);
  }

  template<class KeyT, class MappedT> static void freeQHash(const QHash<KeyT, MappedT*>& hashMap) {
    QHashIterator<KeyT, MappedT*> i(hashMap);
    while(i.hasNext()) {
      i.next();
      delete i.value();
    } 
  }

public:
  void subscribe(ChangeNotificationConsumer* consumer);
  void unsubscribe(ChangeNotificationConsumer* consumer);

  void notify() const;

  UnitClass* getUnitClass(int id) const;
  UnitType* getUnitType(int id) const;
  UnitConfiguration* getUnitConfiguration(int id) const;
  const arx::ArrayList<UnitConfiguration*> getUnitConfigurations() const { return this->unitConfigurationsList; }

  ToolClass* getToolClass(int id) const;
  ToolType* getToolType(int id) const;
  const arx::ArrayList<ToolType*> getToolTypes() const { return this->toolTypesList; }

  DefenceClass* getDefenceClass(int id) const;
	DefenceType* getDefenceType(int id) const;

	AttackComponentType* getAttackComponentType(int id) const;
	AttackType* getAttackType(int id) const;
  const arx::ArrayList<AttackType*> getAttackTypes() const { return this->attackTypesList; };

  const arx::ArrayList<QString> getAllowedAttackerTypeNames() const { return this->allowedAttackerTypeNames; }
  const arx::ArrayList<QString> getAllowedDefenderTypeNames() const { return this->allowedDefenderTypeNames; }

  Constraint* getConstraint(int id) const;
  const arx::ArrayList<Constraint*> getConstraintsForUnitClass(UnitClass* unitClass) const;

  void addUnitClass(UnitClass* unitClass);
  void addUnitType(UnitType* unitType);
  void addUnitConfiguration(UnitConfiguration* unitConfiguration);

  void addToolType(ToolType* toolType);
  void addToolClass(ToolClass* toolClass);

  void addDefenceType(DefenceType* defenceType);
  void addDefenceClass(DefenceClass* defenceClass);

	void addAttackComponentType(AttackComponentType* attackComponentType);
	void addAttackType(AttackType* attackType);

  void addConstraint(Constraint* constraint);

  void addAllowedDefenderTypeName(QString typeName);
  void addAllowedAttackerTypeName(QString typeName);

  static DBase* deserialize(std::istream& stream);
  static void deserialize(std::istream& stream, DBase* target);
  void serialize(std::ostream& stream) const;

  virtual ~DBase();
};

#endif