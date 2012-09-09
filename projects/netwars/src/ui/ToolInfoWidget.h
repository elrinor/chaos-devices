#ifndef __TOOLINFOWIDGET_H__
#define __TOOLINFOWIDGET_H__

#include "uiConfig.h"
#include <QWidget>
#include <QStandardItemModel>
#include "../NetWars.h"

class ToolInfoWidget: public QWidget {
  Q_OBJECT
public:
  enum InfoType {
    ToolInfo,
    ToolTypeInfo
  };

  ToolInfoWidget(Game* game, InfoType type, int id, QWidget *parent = 0);
  ~ToolInfoWidget();

private:
  Game* game;
  int id;
  
  QStandardItemModel *dModel;
};


#endif