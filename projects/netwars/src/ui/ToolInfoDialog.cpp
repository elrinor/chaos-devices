#include <QtGui>
#include "ToolInfoDialog.h"
#include "ToolInfoWidget.h"
#include "MainForm.h"

static QSize gsize;
static QPoint gpos;

ToolInfoDialog::ToolInfoDialog(Game* game, int toolId, QWidget *parent): QDialog(parent) {
  this->game = game;
  this->toolId = toolId;

  setPalette(MainForm::getDefaultPalette());

  QDialogButtonBox *buttonBox = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);
  connect(buttonBox, SIGNAL(accepted()), this, SLOT(accept()));
  connect(buttonBox, SIGNAL(rejected()), this, SLOT(reject())); 

  QVBoxLayout *layout = new QVBoxLayout();
  layout->addWidget(new ToolInfoWidget(game, ToolInfoWidget::ToolInfo, toolId));
  layout->addWidget(buttonBox);
  layout->setContentsMargins(0, 0, 0, 0);
  
  setLayout(layout);
  setWindowTitle(QString::fromWCharArray(L"Свойства оборудования"));
  
  static QSize a0 = (gsize = QSize(640, 480));
  static QPoint b0 = (gpos = QApplication::desktop()->screenGeometry().center() - QPoint(gsize.width(), gsize.height()) / 2);

  resize(gsize);
  move(gpos);
}

ToolInfoDialog::~ToolInfoDialog() {
  gsize = size();
  gpos = pos();
}

void ToolInfoDialog::execute() {
  exec();
}
