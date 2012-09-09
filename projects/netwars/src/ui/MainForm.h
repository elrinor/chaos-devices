#ifndef __MAINFORM_H__
#define __MAINFORM_H__

#include "uiConfig.h"
#include <QMainWindow>
#include <QAction>
#include "../NetWars.h"
#include "UnitConfigurationsBox.h"
#include "MapWidget.h"
#include "LogBox.h"
#include "ResourceBox.h"
#include "GameStateNotifications.h"
#include "GameControlBox.h"

class MainForm: public QMainWindow, GameStateNotificationConsumer, Logger {
  Q_OBJECT
public:
  explicit MainForm();

  Game* getGame() const {return this->game;};

  static const QPalette& getDefaultPalette();

  virtual void logMessage(QString line)     { this->logBox->logMessage(line); }

  GameStateNotifier* getGameStateNotifier() const { return this->gameStateNotifier; }

protected:
  virtual void newGame(Game* game);
  virtual void startGame();
  virtual void makeTurn();
  virtual void gameOver();
  virtual void closeGame();

  void changeGame(Game* game);

private slots:
  void newGameDlg();
  void endGameDlg();
  void saveGameDlg();
  void loadGameDlg();
  void startGameDlg();
  
  void nextTurnPressed();

  void mission(QAction* action);

  void about();
  void aboutQt();

private:
  Game* game;

  UnitConfigurationsBox* unitConfigsBox;
  LogBox* logBox;
  MapWidget* mapWidget;
  ResourceBox* resourceBox;
  GameControlBox* gameControlBox;

  GameStateNotifier* gameStateNotifier;

  QAction *closeAction, *newFreeGameAction, *saveGameAction, *openGameAction, *closeGameAction, *startGameAction, *aboutAction, *aboutQtAction;
};

#endif