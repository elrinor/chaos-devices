#include <QtGui>
#include <algorithm>
#include "LinkGraphicsItem.h"
#include "MainForm.h"

using namespace std;

LinkGraphicsItem::LinkGraphicsItem(MapWidget* mapWidget, Game* game, int linkId) {
  this->game = game;
  this->linkId = linkId;
  this->mapWidget = mapWidget;
  this->setZValue(1);
  this->setFlag(QGraphicsItem::ItemIsSelectable, true);
  this->setFlag(QGraphicsItem::ItemIsFocusable, true);
  this->boundingRectF = calculateBoundingRect();
}

QRectF LinkGraphicsItem::calculateBoundingRect() const {
  int x0 = getLink()->getSource()->getX();
  int x1 = getLink()->getTarget()->getX();
  int y0 = getLink()->getSource()->getY();
  int y1 = getLink()->getTarget()->getY();
  return QRectF(0.4 + min(x0, x1), 0.4 + min(y0, y1), 0.2 + abs(x0 - x1), 0.2 + abs(y0 - y1));
}

QPainterPath LinkGraphicsItem::shape() const {
  QPointF p0(this->getLink()->getSource()->getX() + 0.5, this->getLink()->getSource()->getY() + 0.5);
  QPointF p1(this->getLink()->getTarget()->getX() + 0.5, this->getLink()->getTarget()->getY() + 0.5);
  QPointF p00(p0), p01(p0), p10(p1), p11(p1);
  if(p0.x() < p1.x()) {
    p00.setX(p0.x() - 0.1);
    p10.setX(p1.x() + 0.1);
  } else {
    p00.setX(p0.x() + 0.1);
    p10.setX(p1.x() - 0.1);
  }
  if(p0.y() < p1.y()) {
    p01.setY(p0.y() - 0.1);
    p11.setY(p1.y() + 0.1);
  } else {
    p01.setY(p0.y() + 0.1);
    p11.setY(p1.y() - 0.1);
  }
  QPainterPath path;
  path.moveTo(p00);
  path.lineTo(p01);
  path.lineTo(p10);
  path.lineTo(p11);
  path.closeSubpath();
  return path;
} 

/*void LinkGraphicsItem::mousePressEvent(QGraphicsSceneMouseEvent *event) {
  return;
}*/

void LinkGraphicsItem::keyPressEvent(QKeyEvent *event) {
  if(!isSelected())
    return;
  if(event->key() == Qt::Key_Delete) {
    if(this->game->isStarted())
      return;

    if(this->getLink()->isRemovable()) {
      this->mapWidget->deleteLinkGraphicsItem(this->getLink()->getId());
      this->game->deleteLink(this->getLink()->getId());
      delete this;
    } else
      this->mapWidget->getMainForm()->logMessage(QString::fromWCharArray(L"<font color=#FF0000>ВНИМАНИЕ:</font> Эту связь нельзя удалить"));
    return;
  }
}

QRectF LinkGraphicsItem::boundingRect() const {
  return this->boundingRectF.unite(calculateBoundingRect());
}

void LinkGraphicsItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  if(isSelected())
    painter->setPen(QColor(0, 255, 0));
  else
    painter->setPen(QColor(128, 128, 255));
  painter->drawLine(QPointF(0.5 + this->getLink()->getSource()->getX(), 0.5 + this->getLink()->getSource()->getY()), QPointF(0.5 + this->getLink()->getTarget()->getX(), 0.5 + this->getLink()->getTarget()->getY()));
  this->boundingRectF = calculateBoundingRect();
}

