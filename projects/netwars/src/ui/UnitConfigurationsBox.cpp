#include <QtGui>
#include <cassert>
#include "UnitConfigurationsBox.h"
#include "MainForm.h"
#include "FlowLayout.h"

using namespace arx;

UnitConfigurationsBox::UnitConfigurationsBox(MainForm* mainForm, QWidget* parent): QDockWidget(QString::fromWCharArray(L"Доступные Устройства"), parent) {
  this->mainForm = mainForm;
  this->game = NULL;

  mainForm->getGameStateNotifier()->subscribe(this);
  
  setPalette(MainForm::getDefaultPalette());

  centralWidget = 0;

  this->setMinimumHeight(80);

  setEnabled(false);
}

void UnitConfigurationsBox::newGame(Game* game) {
  this->game = game;
  centralWidget = new QWidget(this);
  setWidget(centralWidget);
  FlowLayout* layout = new FlowLayout();
  FOREACH(UnitConfiguration* config, game->getUnitConfigurations()) {
    UnitIcon *icon = new UnitIcon(config->getId(), this->mainForm);
    layout->addWidget(icon);
  }
  centralWidget->setLayout(layout);

  setEnabled(true);
}

void UnitConfigurationsBox::startGame() {
  setEnabled(false);
}

void UnitConfigurationsBox::gameOver() {
  //setEnabled(true);
}

void UnitConfigurationsBox::closeGame() {
  //setEnabled(false);
  delete centralWidget;
  centralWidget = NULL;
  this->game = NULL;
}


/*
void MenuBox::mousePressEvent(QMouseEvent *event) {
  QLabel *child = static_cast<QLabel*>(this->childAt(event->pos()));
  if(!child)
    return;

  QPixmap pixmap = *child->pixmap();

  QByteArray itemData;
  QDataStream dataStream(&itemData, QIODevice::WriteOnly);
  dataStream << ;

  QMimeData *mimeData = new QMimeData;
  mimeData->setData("application/x-unit", itemData);

  QDrag *drag = new QDrag(this);
  drag->setMimeData(mimeData);
  drag->setPixmap(pixmap);
  drag->setHotSpot(event->pos() - child->pos());

  
  QPixmap tempPixmap = pixmap;
  QPainter painter;
  painter.begin(&tempPixmap);
  painter.fillRect(pixmap.rect(), QColor(127, 127, 127, 127));
  painter.end();

  child->setPixmap(tempPixmap);
  

  if(drag->exec(Qt::CopyAction | Qt::MoveAction, Qt::CopyAction) == Qt::MoveAction)
    child->close();
  else {
    child->show();
    child->setPixmap(pixmap);
  }
} */

