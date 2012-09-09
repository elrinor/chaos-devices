#ifndef __STARTGAMEDIALOG_H__
#define __STARTGAMEDIALOG_H__

#include "uiConfig.h"
#include <QComboBox>
#include <QDialog>
#include "../NetWars.h"

class StartGameDialog: public QDialog {
  Q_OBJECT
public:
  StartGameDialog(Game* game, QWidget *parent = 0);

  bool execute(QString& defenderName, QString& attackerName);

protected:

private:
  Game* game;

  QComboBox *defendersComboBox;
  QComboBox *attackersComboBox;
};


#endif