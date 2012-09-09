#ifndef __LINKGRAPHICSITEM_H__
#define __LINKGRAPHICSITEM_H__

#include "uiConfig.h"
#include <QGraphicsItem>
#include "MapWidget.h"
#include "../NetWars.h"

class LinkGraphicsItem: public QGraphicsItem {
public:
  LinkGraphicsItem(MapWidget* mapWidget, Game* game, int linkId);
  QRectF boundingRect() const;
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);

  int type() const {return LINK_GRAPHICS_TYPE;}

//  Game* getGame() const {return this->mapWidget->getGame();}
  Link* getLink() const {return this->game->getLink(this->linkId);}

protected:
  //void mousePressEvent(QGraphicsSceneMouseEvent *event);
  void keyPressEvent(QKeyEvent *event);

  QPainterPath shape() const;

private:
  QRectF calculateBoundingRect() const;

  Game* game;
  int linkId;

  MapWidget* mapWidget;
  QRectF boundingRectF;
};


#endif