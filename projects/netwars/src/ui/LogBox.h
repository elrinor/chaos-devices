#ifndef __LOGBOX_H__
#define __LOGBOX_H__

#include "uiConfig.h"
#include <QDockWidget>
#include <QTextEdit>
#include <QSize>
#include "GameStateNotifications.h"
#include "../NetWars.h"

class MainForm;

class LogBox: public QDockWidget, GameStateNotificationConsumer, Logger {
  Q_OBJECT
public:
  explicit LogBox(MainForm* mainForm, QWidget* parent = 0);
  
  virtual void logMessage(QString line);

  virtual void closeGame();

protected:

private:
  class LogTextEdit: public QTextEdit {
  public:
    LogTextEdit(QWidget* parent = 0);
    QSize sizeHint() const;
  };

  MainForm* mainForm;
  LogTextEdit* editBox;
};

#endif