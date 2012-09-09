#include "uiConfig.h"
#include <QtGui>
#include "StartGameDialog.h"
#include "NewGameDialog.h"
#include "MainForm.h"

StartGameDialog::StartGameDialog(Game* game, QWidget *parent): QDialog(parent) {
  this->game = game;

  setPalette(MainForm::getDefaultPalette());

  defendersComboBox = new QComboBox();
  defendersComboBox->setEditable(false);
  FOREACH(QString s, game->getAllowedDefenderTypeNames())
    defendersComboBox->addItem(s);

  attackersComboBox = new QComboBox();
  attackersComboBox->setEditable(false);
  FOREACH(QString s, game->getAllowedAttackerTypeNames())
    attackersComboBox->addItem(s);

  QLabel* labelDefender = new QLabel(QString::fromWCharArray(L"Тип &Защищающего:"));
  QLabel* labelAttacker = new QLabel(QString::fromWCharArray(L"Тип &Атакующего:"));
  labelDefender->setBuddy(defendersComboBox);
  labelAttacker->setBuddy(attackersComboBox);

  QGridLayout *gridLayout = new QGridLayout();
  gridLayout->addWidget(labelDefender, 0, 0);
  gridLayout->addWidget(defendersComboBox, 0, 1);
  gridLayout->addWidget(labelAttacker, 1, 0);
  gridLayout->addWidget(attackersComboBox, 1, 1);
  gridLayout->setContentsMargins(5, 5, 5, 5);

  QGroupBox *gameParamBox = new QGroupBox(QString::fromWCharArray(L"&Параметры Игры"));
  gameParamBox->setLayout(gridLayout);

  QDialogButtonBox *buttonBox = new QDialogButtonBox(QDialogButtonBox::Cancel | QDialogButtonBox::Ok, Qt::Horizontal);
  connect(buttonBox, SIGNAL(accepted()), this, SLOT(accept()));
  connect(buttonBox, SIGNAL(rejected()), this, SLOT(reject())); 

  QVBoxLayout *mainLayout = new QVBoxLayout();
  mainLayout->addWidget(gameParamBox);
  mainLayout->addWidget(buttonBox);
  mainLayout->setContentsMargins(0, 0, 0, 0);
  setLayout(mainLayout);

  setWindowTitle(QString::fromWCharArray(L"Начать Игру"));
}

bool StartGameDialog::execute(QString& defenderName, QString& attackerName) {
  exec();
  if(result() == Accepted) {
    defenderName = defendersComboBox->itemText(defendersComboBox->currentIndex());
    attackerName = attackersComboBox->itemText(attackersComboBox->currentIndex());
    return true;
  } else
    return false;
}