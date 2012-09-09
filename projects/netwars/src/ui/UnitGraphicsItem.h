#ifndef __UNITGRAPHICSITEM_H__
#define __UNITGRAPHICSITEM_H__

#include "uiConfig.h"
#include <QGraphicsItem>
#include "MapWidget.h"
#include "LinkGraphicsItem.h"
#include "../NetWars.h"

class UnitGraphicsItem: public QGraphicsItem {
public:
  UnitGraphicsItem(MapWidget* mapWidget, Game* game, int unitId);
  QRectF boundingRect() const;
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);

  int type() const {return UNIT_GRAPHICS_TYPE;}

  Unit* getUnit() const {return this->game->getUnit(this->unitId);}

protected:
  void mouseMoveEvent(QGraphicsSceneMouseEvent *event);
  void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);
  void keyPressEvent(QKeyEvent *event);
  void mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event);
  QVariant itemChange(GraphicsItemChange change, const QVariant &value);

private:
  Game* game;
  MapWidget* mapWidget;
  int unitId;
  QPixmap pixmap;
};


#endif