#ifndef __TOOLINFODIALOG_H__
#define __TOOLINFODIALOG_H__

#include "uiConfig.h"
#include <QDialog>
#include "../NetWars.h"

class ToolInfoDialog: public QDialog {
  Q_OBJECT
public:
  ToolInfoDialog(Game* game, int toolId, QWidget *parent = 0);
  ~ToolInfoDialog();

  void execute();

protected:

private:
  Game* game;
  int toolId;
};


#endif