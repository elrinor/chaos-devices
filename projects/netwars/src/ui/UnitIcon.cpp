#include <QtGui>
#include "UnitIcon.h"
#include "MainForm.h"

using namespace boost;
using namespace std;

Game* UnitIcon::getGame() const {
  return this->mainForm->getGame();
}

UnitIcon::UnitIcon(int unitConfigId, MainForm* mainForm, QWidget *parent): QLabel(parent) {
  this->unitConfigId = unitConfigId;
  this->mainForm = mainForm;
  QPixmap icon = QPixmap(getUnitConfig()->getType()->getImageName()).scaled(64, 64, Qt::KeepAspectRatio, Qt::SmoothTransformation);
  QPainter painter(&icon);

  painter.setFont(QFont("Arial", 8));

  QString str = QString().setNum(getUnitConfig()->getCost());

  QRect rect = painter.boundingRect(0, 0, 64, 64, Qt::AlignLeft | Qt::AlignTop, str);

  painter.setPen(QColor(255, 255, 255, 128));
  painter.drawText(1, 1, rect.width(), rect.height(), Qt::AlignLeft | Qt::AlignTop, str);
  painter.setPen(QCOLOR_GOLD);
  painter.drawText(0, 0, rect.width(), rect.height(), Qt::AlignLeft | Qt::AlignTop, str);
  painter.drawPixmap(rect.topRight() + QPointF(1, 0), QPixmap(":/pic_gold.png").scaled(rect.height(), rect.height(), Qt::KeepAspectRatio, Qt::SmoothTransformation));

  this->setPixmap(icon);
  this->setToolTip(getUnitConfig()->getName());
}

void UnitIcon::mousePressEvent(QMouseEvent* event) {
  QByteArray itemData;
  QDataStream dataStream(&itemData, QIODevice::WriteOnly);
  dataStream << this->getUnitConfig()->getId();

  QMimeData *mimeData = new QMimeData;
  mimeData->setData("application/x-unit-config", itemData);

  QDrag *drag = new QDrag(this);
  drag->setMimeData(mimeData);
  drag->setPixmap(this->pixmap()->scaled(32, 32, Qt::KeepAspectRatio, Qt::SmoothTransformation));
  drag->setHotSpot(event->pos() / 2);

  if(drag->exec(Qt::CopyAction | Qt::MoveAction, Qt::CopyAction) == Qt::MoveAction)
    this->close();
}
