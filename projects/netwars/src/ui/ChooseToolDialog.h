#ifndef __CHOOSETOOLDIALOG_H__
#define __CHOOSETOOLDIALOG_H__

#include "uiConfig.h"
#include <QDialog>
#include <QTreeWidgetItem>
#include <QSplitter>
#include <QTreeWidget>
#include "ToolInfoWidget.h"
#include "../NetWars.h"

class ChooseToolDialog: public QDialog {
  Q_OBJECT
public:
  ChooseToolDialog(Game* game, int unitId, QWidget* parent = 0);
  ~ChooseToolDialog();

  int execute();

protected slots:
  void itemChanged(QTreeWidgetItem* current, QTreeWidgetItem* previous);

protected:

private:
  Game* game;
  int unitId;

  QSplitter *splitter;
  ToolInfoWidget* infoWidget;
  QTreeWidget *list;
};


#endif