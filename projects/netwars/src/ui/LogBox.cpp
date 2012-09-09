#include <QtGui>
#include "LogBox.h"
#include "MainForm.h"

LogBox::LogTextEdit::LogTextEdit(QWidget* parent): QTextEdit(parent) {
  setReadOnly(true);
  setFontFamily(tr("Courier New"));
  return;
}

QSize LogBox::LogTextEdit::sizeHint() const {
  return QSize(256, 64);
} 

LogBox::LogBox(MainForm* mainForm, QWidget* parent): QDockWidget(QString::fromWCharArray(L"Ëîã"), parent) {
  this->mainForm = mainForm;

  mainForm->getGameStateNotifier()->subscribe(this);

  setPalette(MainForm::getDefaultPalette());

  this->editBox = new LogTextEdit();

  setWidget(this->editBox);
}

void LogBox::closeGame() {
  editBox->clear();
}

void LogBox::logMessage(QString line) {
  editBox->append("<font face=\"Courier New\">" + line + "</font>");
}

