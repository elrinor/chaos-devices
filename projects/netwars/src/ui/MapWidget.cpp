#include <QtGui>
#include <cmath>
#include <algorithm>
#include "MapWidget.h"
#include "UnitGraphicsItem.h"
#include "MainForm.h"

using namespace std;
using namespace arx;

MapWidget::MapWidget(MainForm* mainForm, QWidget* parent): QGraphicsView(parent) {
  this->mainForm = mainForm;

  mainForm->getGameStateNotifier()->subscribe(this);

  this->setCacheMode(CacheBackground);
  
  this->setScene(new QGraphicsScene(this));

  scale(32, 32);
}

void MapWidget::closeGame() {
  this->setScene(new QGraphicsScene(this));
  this->setAcceptDrops(false);
  // TODO: free the damn resources here!
}


void MapWidget::newGame(Game* game) {
  this->game = game;

  QGraphicsScene *scene = new QGraphicsScene(this);
  scene->setItemIndexMethod(QGraphicsScene::NoIndex);
  scene->setSceneRect(0, 0, game->getMap()->getWidth(), game->getMap()->getHeight());
  this->setScene(scene);
  this->setAcceptDrops(true);

  // add selection graphic item to scene
  setHighlightPos(QPoint(-1, -1));
  scene->addItem(&this->hightlight);

  // create links (we must create them first since units use them)
  const ArrayList<Link*> links = game->getLinks();
  FOREACH(Link* link, links) 
    addLinkGraphicsItem(link->getId(), new LinkGraphicsItem(this, game, link->getId()));

  // create units
  const ArrayList<Unit*> units = game->getUnits();
  FOREACH(Unit* unit, units)
    this->scene()->addItem(new UnitGraphicsItem(this, game, unit->getId()));
}

MainForm* MapWidget::getMainForm() const {
  return this->mainForm;
}

bool MapWidget::insideScene(const QPoint& scenePos) {
  return (scenePos.x() >= 0 && scenePos.x() < this->scene()->width() && scenePos.y() >= 0 && scenePos.y() < this->scene()->height());
}

QPoint MapWidget::toScenePos(const QPointF& scenePosF) {
  QPointF scenePos = scenePosF;
  scenePos.setX(floor(scenePos.x()));
  scenePos.setY(floor(scenePos.y()));
  return scenePos.toPoint();
}

QPoint MapWidget::toScenePos(const QPoint& windowPos) {
  return toScenePos(mapToScene(windowPos));
}

void MapWidget::setHighlightPos(const QPoint& scenePos) {
  if(insideScene(scenePos)) {
    this->hightlight.setVisible(true);
    this->hightlight.setPos(scenePos);
  } else
    this->hightlight.setVisible(false);
  //this->hightlight.setPos(QPoint(0,0));
}

void MapWidget::addLinkGraphicsItem(int id, LinkGraphicsItem* link) {
  this->links.insert(id, link);
  this->scene()->addItem(link);
}

void MapWidget::deleteLinkGraphicsItem(int id) {
  LinkGraphicsItem* linkGraphicsItem = getLinkGraphicsItem(id);
  this->scene()->removeItem(linkGraphicsItem);
  this->links.remove(id);
}

void MapWidget::dragEnterEvent(QDragEnterEvent *event) {
  if(event->mimeData()->hasFormat("application/x-unit-config")) {
    event->accept();
  } else {
    event->ignore();
  }
}

void MapWidget::dragMoveEvent(QDragMoveEvent *event) {
  if(event->mimeData()->hasFormat("application/x-unit-config")) {
    QPoint scenePos = toScenePos(event->pos());
    setHighlightPos(scenePos);
    if(insideScene(scenePos)) {
      if(this->game->getMap()->hasUnit(scenePos.x(), scenePos.y()) || this->game->isStarted()) {
        event->ignore();
        this->hightlight.setBad();
      } else {
        event->accept();
        this->hightlight.setGood();
      }
    } else {
      event->ignore();
      this->hightlight.setGood();
    }
  } else {
    event->ignore();
  }
}

void MapWidget::dropEvent(QDropEvent *event) {
  if(event->mimeData()->hasFormat("application/x-unit-config")) {
    QPoint scenePos = toScenePos(event->pos());
    if(!insideScene(scenePos) || game->isStarted()) {
      event->ignore();
      return;
    }

    QByteArray pieceData = event->mimeData()->data("application/x-unit-config");
    QDataStream dataStream(&pieceData, QIODevice::ReadOnly);
    int id;
    dataStream >> id;

    UnitConfiguration* config = this->game->getUnitConfiguration(id);
    if(config->getCost() > this->game->getDefenderResources()) {
      event->ignore();
      this->mainForm->logMessage(QString::fromWCharArray(L"<font color=#FF0000>ВНИМАНИЕ:</font> Недостаточно ресурсов для \"") + config->getName() + "\"");
    } else {
      UnitGraphicsItem* unit = new UnitGraphicsItem(this, this->game, 
        this->game->getUnitConfiguration(id)->spawnUnit(this->game, this->game->getUnitConfiguration(id)->getType()->getName(), scenePos.x(), scenePos.y(), true, true)->getId());
      this->scene()->addItem(unit);
      event->accept();
    }
  } else {
    event->ignore();
  }
}

void MapWidget::mouseMoveEvent(QMouseEvent *event) {
  setHighlightPos(toScenePos(event->pos()));
  this->hightlight.setGood();
  QGraphicsView::mouseMoveEvent(event);
}

void MapWidget::drawBackground(QPainter *painter, const QRectF &rect) {
  painter->fillRect(rect, Qt::black);

  qreal w = this->scene()->width();
  qreal h = this->scene()->height();

  qreal xD = max(qreal(0), floor(rect.left()));
  qreal xU = min(w, rect.right());

  qreal yD = max(qreal(0), floor(rect.top()));
  qreal yU = min(h, rect.bottom());

  painter->setPen(QColor(32, 32, 32));
  for(int x = xD; x <= xU; x++)
    painter->drawLine(QPointF(x, yD), QPointF(x, yU));
  for(int y = yD; y <= yU; y++)
    painter->drawLine(QPointF(xD, y), QPointF(xU, y));
}