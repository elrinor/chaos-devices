#ifndef __INFOBOX_H__
#define __INFOBOX_H__

#include "uiConfig.h"
#include <QDockWidget>
#include <QLabel>
#include "GameStateNotifications.h"
#include "../NetWars.h"

class MainForm;

class ResourceBox: public QDockWidget, ChangeNotificationConsumer, GameStateNotificationConsumer {
  Q_OBJECT
public:
  explicit ResourceBox(MainForm* mainForm, QWidget* parent = 0);

  virtual void notify();
  
  virtual void newGame(Game* game);
  virtual void closeGame();

protected:

private:
  MainForm* mainForm;

  Game* game;

  QLabel *defRes, *attRes;
  QLabel *coinLabel0, *coinLabel1;
};

#endif