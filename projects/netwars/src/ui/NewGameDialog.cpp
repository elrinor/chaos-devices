#include <QtGui>
#include "NewGameDialog.h"
#include "MainForm.h"

NewGameDialog::NewGameDialog(QWidget *parent): QDialog(parent) {
  setPalette(MainForm::getDefaultPalette());

  spinBoxX = new QSpinBox();
  spinBoxX->setMinimum(1);
  spinBoxX->setMaximum(256);
  spinBoxX->setSingleStep(1);
  spinBoxX->setValue(16);
  spinBoxY = new QSpinBox();
  spinBoxY->setMinimum(1);
  spinBoxY->setMaximum(256);
  spinBoxY->setSingleStep(1);
  spinBoxY->setValue(16);

  QLabel *labelX = new QLabel(QString::fromWCharArray(L"&Ширина:"));
  QLabel *labelY = new QLabel(QString::fromWCharArray(L"&Высота:"));
  labelX->setBuddy(spinBoxX);
  labelY->setBuddy(spinBoxY);

  QGridLayout *xyLayout = new QGridLayout();
  xyLayout->addWidget(labelX, 0, 0);
  xyLayout->addWidget(spinBoxX, 0, 1);
  xyLayout->addWidget(labelY, 1, 0);
  xyLayout->addWidget(spinBoxY, 1, 1);
  xyLayout->setContentsMargins(5, 5, 5, 5);

  QGroupBox* mapSizeBox = new QGroupBox(QString::fromWCharArray(L"Размер &Карты"));
  mapSizeBox->setLayout(xyLayout);

  defenderResBox = new QSpinBox();
  defenderResBox->setMinimum(0);
  defenderResBox->setMaximum(1000000);
  defenderResBox->setSingleStep(100);
  defenderResBox->setValue(1000);
  attackerResBox = new QSpinBox();
  attackerResBox->setMinimum(0);
  attackerResBox->setMaximum(1000000);
  attackerResBox->setSingleStep(100);
  attackerResBox->setValue(2000);

  QLabel *labelD = new QLabel(QString::fromWCharArray(L"&Защищающий:"));
  QLabel *labelA = new QLabel(QString::fromWCharArray(L"&Атакующий:"));
  labelD->setBuddy(defenderResBox);
  labelA->setBuddy(attackerResBox);

  QGridLayout *adLayout = new QGridLayout();
  adLayout->addWidget(labelD, 0, 0);
  adLayout->addWidget(defenderResBox, 0, 1);
  adLayout->addWidget(labelA, 1, 0);
  adLayout->addWidget(attackerResBox, 1, 1);
  adLayout->setContentsMargins(5, 5, 5, 5);

  QGroupBox* resBox = new QGroupBox(QString::fromWCharArray(L"Начальные &Ресурсы"));
  resBox->setLayout(adLayout);

  QDialogButtonBox* buttonBox = new QDialogButtonBox(QDialogButtonBox::Cancel | QDialogButtonBox::Ok, Qt::Horizontal);
  connect(buttonBox, SIGNAL(accepted()), this, SLOT(accept()));
  connect(buttonBox, SIGNAL(rejected()), this, SLOT(reject())); 

  QVBoxLayout *mainLayout = new QVBoxLayout();
  mainLayout->addWidget(mapSizeBox);
  mainLayout->addWidget(resBox);
  mainLayout->addWidget(buttonBox);
  mainLayout->setContentsMargins(0, 0, 0, 0);
  setLayout(mainLayout);

  setWindowTitle(QString::fromWCharArray(L"Новая Игра"));
}

Game* NewGameDialog::execute() {
  exec();
  if(result() == Accepted) {
    return Game::createEmptyGame(spinBoxX->value(), spinBoxY->value(), defenderResBox->value(), attackerResBox->value(), QString::fromWCharArray(L"Свободное строительство"));
  } else
    return NULL;
}