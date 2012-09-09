#include <QtGui>
#include "UnitGraphicsItem.h"
#include "HighlightGraphicsItem.h"
#include "LinkGraphicsItem.h"
#include "UnitDialog.h"
#include "MainForm.h"

using namespace std;

// -------------------------------------------------------------------------- //
// UnitGraphicsItem
// -------------------------------------------------------------------------- //
UnitGraphicsItem::UnitGraphicsItem(MapWidget* mapWidget, Game* game, int unitId) {
  this->unitId = unitId;
  this->mapWidget = mapWidget;
  this->game = game;

  //this->setFlag(QGraphicsItem::ItemIsMovable, true);
  this->setFlag(QGraphicsItem::ItemIsSelectable, true);
  this->setFlag(QGraphicsItem::ItemIsFocusable, true);

  this->setZValue(2);
  this->pixmap = QPixmap(getUnit()->getType()->getImageName());
  this->setPos(getUnit()->getX(), getUnit()->getY());
}

QRectF UnitGraphicsItem::boundingRect() const {
  return QRectF(0, 0, 1, 1);
}

void UnitGraphicsItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  painter->setRenderHint(QPainter::Antialiasing, true);
  painter->drawPixmap(0, 0, 1, 1, this->pixmap);
  painter->setRenderHint(QPainter::Antialiasing, false);

  float life = getUnit()->getLife();;
  painter->setPen(QColor(255 * (1 - life), 255 * life, 0));
  painter->drawRect(QRectF(0, 0, life, 1.0 / 32));

  if(isSelected()) {
    painter->setPen(QColor(0, 255, 0));
    painter->drawRect(QRectF(0, 0, 1, 1));
  }
}

void UnitGraphicsItem::mouseMoveEvent(QGraphicsSceneMouseEvent *event) {
  if(this->game->isStarted())
    return;

  if(event->buttons() == Qt::LeftButton) {
    QPoint scenePos = this->mapWidget->toScenePos(event->scenePos());

    if(!this->mapWidget->insideScene(scenePos))
      return;

    if(!this->getUnit()->isMovable() || game->getMap()->hasUnit(scenePos.x(), scenePos.y())) {
      this->mapWidget->getHightlight()->setBad();
      return;
    }

    game->moveUnit(this->getUnit()->getId(), scenePos.x(), scenePos.y());

    this->setPos(scenePos);
  } else if(event->buttons() == Qt::RightButton) {
    QPoint scenePos = this->mapWidget->toScenePos(event->scenePos());

    if(!this->mapWidget->insideScene(scenePos))
      return;

    if(!game->getMap()->hasUnit(scenePos.x(), scenePos.y())) {
      this->mapWidget->getHightlight()->setBad();
      return;
    }

    int targetId = game->getMap()->getUnit(scenePos.x(), scenePos.y())->getId();

    if(this->getUnit()->hasLinkTo(targetId) || this->getUnit()->getId() == targetId) {
      this->mapWidget->getHightlight()->setBad();
      return;
    }

    this->mapWidget->getHightlight()->setGood();
  }

  /*if(QLineF(event->screenPos(), event->buttonDownScreenPos(Qt::LeftButton)).length() < QApplication::startDragDistance())
    return;*/

  //this->update();
}

void UnitGraphicsItem::mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event) {
  UnitDialog *dlg = new UnitDialog(this->mapWidget->getMainForm(), this->game, this->unitId, this->mapWidget);
  dlg->execute();
  delete dlg;

  if(!this->game->isStarted()) {
    arx::ArrayList<QString> messages;
    if(!this->game->getUnit(this->unitId)->meetsConstraints(messages))
      FOREACH(QString s, messages)
        this->mapWidget->getMainForm()->logMessage(QString::fromWCharArray(L"<font color=#FF0000>¬Õ»Ã¿Õ»≈:</font> ") + s);
  }
}


QVariant UnitGraphicsItem::itemChange(GraphicsItemChange change, const QVariant &value) {
  if(change == ItemPositionChange) {
    // QPointF newPos = value.toPointF();
    for(int i = 0; i < this->getUnit()->getLinksSize(); i++)
      this->mapWidget->getLinkGraphicsItem(this->getUnit()->getLink(i)->getId())->update();
  }
  return QGraphicsItem::itemChange(change, value);
} 

void UnitGraphicsItem::keyPressEvent(QKeyEvent *event) {
  if(!isSelected())
    return;
  if(event->key() == Qt::Key_Delete) {
    if(this->game->isStarted())
      return;

    if(this->getUnit()->isRemovable()) {
      for(int i = 0; i < this->getUnit()->getLinksSize(); i++)
        this->mapWidget->deleteLinkGraphicsItem(this->getUnit()->getLink(i)->getId());
      this->game->deleteUnit(this->getUnit()->getId());
      this->scene()->removeItem(this);
      delete this;
    } else
      this->mapWidget->getMainForm()->logMessage(QString::fromWCharArray(L"<font color=#FF0000>¬Õ»Ã¿Õ»≈:</font> ÕÂ ÏÓ„Û Û‰‡ÎËÚ¸ \"") + this->getUnit()->getName() + "\"");
    return;
  }
}

void UnitGraphicsItem::mouseReleaseEvent(QGraphicsSceneMouseEvent *event) {
  if(this->game->isStarted())
    return;

  if(event->button() == Qt::RightButton) {
    QPoint scenePos = this->mapWidget->toScenePos(event->scenePos());

    if(!this->mapWidget->insideScene(scenePos))
      return;

    if(!this->game->getMap()->hasUnit(scenePos.x(), scenePos.y()))
      return;

    int targetId = this->game->getMap()->getUnit(scenePos.x(), scenePos.y())->getId();

    if(this->getUnit()->hasLinkTo(targetId) || targetId == this->getUnit()->getId())
      return;

    Link* link = new Link(INVALID_ID, this->getUnit()->getId(), targetId, true);
    this->game->addLink(link);

    this->mapWidget->addLinkGraphicsItem(link->getId(), new LinkGraphicsItem(this->mapWidget, this->game, link->getId()));
  }
}
