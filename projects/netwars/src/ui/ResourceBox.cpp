#include <QtGui>
#include "ResourceBox.h"
#include "FlowLayout.h"
#include "MainForm.h"

using namespace boost;
using namespace std;

ResourceBox::ResourceBox(MainForm* mainForm, QWidget* parent): QDockWidget(QString::fromWCharArray(L"Ресурсы"), parent) {
  this->game = NULL;

  this->mainForm = mainForm;

  mainForm->getGameStateNotifier()->subscribe(this);

  this->setPalette(MainForm::getDefaultPalette());
  QPalette pal = MainForm::getDefaultPalette();
  pal.setColor(QPalette::WindowText, QCOLOR_GOLD);

  this->defRes = new QLabel("");
  this->attRes = new QLabel("");
  this->defRes->setPalette(pal);
  this->attRes->setPalette(pal);

  coinLabel0 = new QLabel();
  coinLabel1 = new QLabel();
  QPixmap coinPic = QPixmap(":/pic_gold.png").scaled(16, 16, Qt::KeepAspectRatio, Qt::SmoothTransformation);
  coinLabel0->setPixmap(coinPic);
  coinLabel1->setPixmap(coinPic);
  coinLabel0->setVisible(false);
  coinLabel1->setVisible(false);

  QHBoxLayout *defResLayout = new QHBoxLayout();
  defResLayout->setContentsMargins(0, 0, 0, 0);
  defResLayout->addWidget(this->defRes);
  defResLayout->addWidget(coinLabel0);
  defResLayout->addStretch(1);

  QHBoxLayout *attResLayout = new QHBoxLayout();
  attResLayout->setContentsMargins(0, 0, 0, 0);
  attResLayout->addWidget(this->attRes);
  attResLayout->addWidget(coinLabel1);
  attResLayout->addStretch(1);

  QGridLayout* layout = new QGridLayout();
  layout->addWidget(new QLabel(QString::fromWCharArray(L"Защищающий:")), 0, 0);
  layout->addWidget(new QLabel(QString::fromWCharArray(L"Атакующий:")), 1, 0);
  layout->addLayout(defResLayout,                0, 1);
  layout->addLayout(attResLayout,                1, 1);
  layout->addItem(new QSpacerItem(0, 0),         2, 0);
  layout->setRowStretch(2, 1);
  layout->setColumnStretch(1, 1);

  QWidget *centralWidget = new QWidget();
  centralWidget->setLayout(layout);
  setWidget(centralWidget);
}

void ResourceBox::notify() {
  this->defRes->setNum(this->game->getDefenderResources());
  this->attRes->setNum(this->game->getAttackerResources());
}

void ResourceBox::newGame(Game* game) {
  this->game = game;
  this->game->subscribe(this);
  this->coinLabel0->setVisible(true);
  this->coinLabel1->setVisible(true);
  notify();
}

void ResourceBox::closeGame() {
  this->game->unsubscribe(this);
  this->game = NULL;
  this->defRes->setText("");
  this->attRes->setText("");
  this->coinLabel0->setVisible(false);
  this->coinLabel1->setVisible(false);
}
