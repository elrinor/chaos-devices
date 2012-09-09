#ifndef __UNITDIALOG_H__
#define __UNITDIALOG_H__

#include "uiConfig.h"
#include <QDialog>
#include <QLabel>
#include <QModelIndex>
#include <QLineEdit>
#include <QTreeView>
#include <QStandardItemModel>
#include "../NetWars.h"

class MainForm;

class UnitDialog: public QDialog, ChangeNotificationConsumer {
  Q_OBJECT
public:
  UnitDialog(MainForm* mainForm, Game* game, int unitId, QWidget *parent = 0);
  ~UnitDialog();

  void execute();

  virtual void notify();

protected slots:
  void toolDoubleClick(const QModelIndex& index);
  void deleteUnitClick();
  void addUnitClick();

protected:

private:
  int unitId;
  Game* game;

  MainForm* mainForm;

  QStandardItemModel *toolsModel;
  QTreeView* toolsView;

  QLineEdit *nameEdit;
  QLabel *cost;
  QLabel *hp;
  QLabel *cpuUsage;
  QLabel *memUsage;
  QLabel *powerUsage;
};


#endif
