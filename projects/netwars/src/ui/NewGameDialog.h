#ifndef __NEWGAMEDIALOG_H__
#define __NEWGAMEDIALOG_H__

#include "uiConfig.h"
#include <QDialog>
#include "../NetWars.h"

class QSpinBox;

class NewGameDialog: public QDialog {
  Q_OBJECT
public:
  NewGameDialog(QWidget *parent = 0);

  Game* execute();

protected:

private:
  QSpinBox *spinBoxX, *spinBoxY;

  QSpinBox *attackerResBox, *defenderResBox;
};



#endif