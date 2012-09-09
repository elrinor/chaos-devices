#include <QtGui>
#include "GameControlBox.h"
#include "MainForm.h"

GameControlBox::GameControlBox(MainForm* mainForm, QWidget* parent): QDockWidget(QString::fromWCharArray(L"Управление Игрой"), parent) {
  this->game = NULL;

  this->mainForm = mainForm;

  mainForm->getGameStateNotifier()->subscribe(this);

  setPalette(MainForm::getDefaultPalette());

  QVBoxLayout *mainLayout = new QVBoxLayout();
  
  QPushButton *stepButton = new QPushButton(QString::fromWCharArray(L"Следующий &Ход"));
  
  QHBoxLayout *turnLayout = new QHBoxLayout();
  QLabel* turn = new QLabel(QString::fromWCharArray(L"Ход:"));
  this->turnLabel = new QLabel("");
  turnLayout->addWidget(turn);
  turnLayout->addWidget(this->turnLabel, 1);
  
  mainLayout->addWidget(stepButton);
  mainLayout->addLayout(turnLayout);
  mainLayout->addStretch(1);

  connect(stepButton, SIGNAL(pressed()), mainForm, SLOT(nextTurnPressed()));

  QWidget *centralWidget = new QWidget();
  centralWidget->setLayout(mainLayout);
  setWidget(centralWidget);

  setEnabled(false);
}

void GameControlBox::newGame(Game* game) {
  this->game = game;
  this->turnLabel->setNum(this->game->getTurn());
}

void GameControlBox::closeGame() {
  this->game = NULL;
  this->turnLabel->setText("");
}

void GameControlBox::startGame() {
  setEnabled(true);
}

void GameControlBox::gameOver() {
  setEnabled(false);
}

void GameControlBox::makeTurn() {
  this->turnLabel->setNum(this->game->getTurn());
}
