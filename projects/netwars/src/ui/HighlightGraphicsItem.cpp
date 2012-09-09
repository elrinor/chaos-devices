#include <QtGui>
#include "HighlightGraphicsItem.h"


// -------------------------------------------------------------------------- //
// HighlightGraphicsItem
// -------------------------------------------------------------------------- //
HighlightGraphicsItem::HighlightGraphicsItem() {
  this->setBad();
  this->setZValue(10);
}

QRectF HighlightGraphicsItem::boundingRect() const {
  return QRectF(0, 0, 1, 1);
}

void HighlightGraphicsItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  float d = 1 / 32.0f;
  painter->setBrush(this->color);
  painter->setPen(QColor(0, 0, 0, 0));
  painter->drawRect(QRectF(d, d, 1 - d, 1 - d));
}

void HighlightGraphicsItem::setGood() {
  this->color = QColor(0, 255, 0, 64);
}

void HighlightGraphicsItem::setBad() {
  this->color = QColor(255, 0, 0, 64);
}

void HighlightGraphicsItem::setBlue() {
  this->color = QColor(16, 160, 255, 64);
}
