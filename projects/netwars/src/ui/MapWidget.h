#ifndef __MAPWIDGET_H__
#define __MAPWIDGET_H__

#include "uiConfig.h"
#include <QGraphicsView>
#include <QHash>
#include "HighlightGraphicsItem.h"
#include "GameStateNotifications.h"
#include "../NetWars.h"

class LinkGraphicsItem;
class MainForm;

class MapWidget: public QGraphicsView, GameStateNotificationConsumer {
  Q_OBJECT
public:
  explicit MapWidget(MainForm* mainForm, QWidget* parent = 0);
  
  virtual void newGame(Game* game);
  virtual void closeGame();

  QPoint toScenePos(const QPoint& windowPos);
  QPoint toScenePos(const QPointF& scenePosF);
  bool insideScene(const QPoint& scenePos);
  void setHighlightPos(const QPoint& scenePos);

  MainForm* getMainForm() const;
  HighlightGraphicsItem* getHightlight() {return &this->hightlight;}

  LinkGraphicsItem* getLinkGraphicsItem(int id) const {return *this->links.find(id);}
  void addLinkGraphicsItem(int id, LinkGraphicsItem* link);
  void deleteLinkGraphicsItem(int id);

protected:
  void dragEnterEvent(QDragEnterEvent *event);
  void dragMoveEvent(QDragMoveEvent *event);
  void dropEvent(QDropEvent *event);

  void mouseMoveEvent(QMouseEvent *event);

  void drawBackground(QPainter *painter, const QRectF &rect);


private:
  MainForm* mainForm;

  Game* game;

  HighlightGraphicsItem hightlight;

  QHash<int, LinkGraphicsItem*> links;
};


#endif