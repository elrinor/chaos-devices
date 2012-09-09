#ifndef __UNITCONFIGURATIONSBOX_H__
#define __UNITCONFIGURATIONSBOX_H__

#include "uiConfig.h"
#include <QDockWidget>
#include <QLabel>
#include <QList>
#include "UnitIcon.h"
#include "../NetWars.h"
#include "GameStateNotifications.h"

class MainForm;

class UnitConfigurationsBox: public QDockWidget, GameStateNotificationConsumer {
  Q_OBJECT
public:
  explicit UnitConfigurationsBox(MainForm* mainForm, QWidget* parent = 0);

  virtual void newGame(Game* game);
  virtual void startGame();
  virtual void gameOver();
  virtual void closeGame();

protected:
  
private:
  void clearCurrentGame();

  MainForm* mainForm;

  Game* game;

  QWidget* centralWidget;
};

#endif