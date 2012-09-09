#ifndef __UNITICON_H__
#define __UNITICON_H__

#include "uiConfig.h"
#include <QLabel>
#include "../NetWars.h"

class MainForm;

class UnitIcon: public QLabel {
  //Q_OBJECT
public:
  UnitIcon(int unitConfigId, MainForm* mainForm, QWidget *parent = 0);

  Game* getGame() const;
  UnitConfiguration* getUnitConfig() {return this->getGame()->getUnitConfiguration(this->unitConfigId);}

protected:
  void mousePressEvent(QMouseEvent* event);

private:

  MainForm* mainForm;
  //UnitConfiguration* config;
  int unitConfigId;
};


#endif