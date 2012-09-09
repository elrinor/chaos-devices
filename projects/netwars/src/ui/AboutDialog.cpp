#include <QtGui>
#include "AboutDialog.h"
#include "MainForm.h"
#include "../config.h"

AboutDialog::AboutDialog(QWidget *parent) {
  setPalette(MainForm::getDefaultPalette());

  QHBoxLayout* mainLayout = new QHBoxLayout();

  QLabel *picLabel = new QLabel();
  picLabel->setPixmap(QPixmap(":/pic_logo.png"));
  mainLayout->addWidget(picLabel);

  QVBoxLayout* leftLayout = new QVBoxLayout();
  leftLayout->addWidget(new QLabel(QString::fromWCharArray(L"Netwars v") + VERSION + QString::fromWCharArray(L" - симулятор сетевых атак")));
  leftLayout->addWidget(new QLabel(QString::fromWCharArray(L"Copyright (C) 2007-2008 Александр 'Elric' Фокин")));
  leftLayout->addWidget(new QLabel(QString::fromWCharArray(L"Выражаем благодарность:\nМихаилу Борисову\nАндрею 'Xroft' Татаринову\nВладимиру Борисову\n")));

  QDialogButtonBox* buttonBox = new QDialogButtonBox(QDialogButtonBox::Close, Qt::Horizontal);
  connect(buttonBox, SIGNAL(accepted()), this, SLOT(accept()));
  connect(buttonBox, SIGNAL(rejected()), this, SLOT(reject())); 
  leftLayout->addWidget(buttonBox);

  mainLayout->addLayout(leftLayout);

  setLayout(mainLayout);
}
