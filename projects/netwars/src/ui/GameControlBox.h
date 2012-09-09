#ifndef __GAMECONTROLBOX_H__
#define __GAMECONTROLBOX_H__

#include "uiConfig.h"
#include <QDockWidget>
#include <QLabel>
#include "../NetWars.h"
#include "GameStateNotifications.h"

class MainForm;

class GameControlBox: public QDockWidget, GameStateNotificationConsumer {
  Q_OBJECT
public:
  explicit GameControlBox(MainForm* mainForm, QWidget* parent = 0);

  virtual void newGame(Game* game);
  virtual void startGame();
  virtual void makeTurn();
  virtual void gameOver();
  virtual void closeGame();

protected:

private:
  MainForm* mainForm;
  
  Game* game;

  QLabel* turnLabel;
};

#endif