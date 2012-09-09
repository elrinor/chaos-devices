#ifndef __SELECTIONGRAPHICSITEM_H__
#define __SELECTIONGRAPHICSITEM_H__

#include "uiConfig.h"
#include <QGraphicsItem>

class HighlightGraphicsItem: public QGraphicsItem {
public:
  HighlightGraphicsItem();
  QRectF boundingRect() const;
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);

  int type() const {return HIGHLIGHT_GRAPHICS_TYPE;}

  void setGood();
  void setBad();
  void setBlue();

protected:

private:
  QColor color;

};

#endif