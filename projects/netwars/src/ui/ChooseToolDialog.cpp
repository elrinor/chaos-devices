#include <QtGui>
#include "ChooseToolDialog.h"
#include "MainForm.h"
#include "ToolInfoWidget.h"

using namespace boost;
using namespace std;
using namespace arx;

static QSize gsize;
static QPoint gpos;

ChooseToolDialog::ChooseToolDialog(Game* game, int unitId, QWidget* parent): QDialog(parent) {
  this->game = game;
  this->unitId = unitId;
  this->infoWidget = NULL;
  setPalette(MainForm::getDefaultPalette());

  Unit* unit = game->getUnit(unitId);

  splitter = new QSplitter(Qt::Horizontal);
  
  list = new QTreeWidget();
  list->setColumnCount(2);
  list->setRootIsDecorated(false);
  list->setAlternatingRowColors(true);
  QTreeWidgetItem* item = NULL;
  FOREACH(ToolType* toolType, game->getToolTypes()) 
    if(toolType->getCost() <= this->game->getDefenderResources() && unit->canBeAdded(toolType))
      list->insertTopLevelItem(0, item = new QTreeWidgetItem(QStringList() << QString().setNum(toolType->getId()) << toolType->getName() << toolType->getClass()->getName()));
  list->setHeaderLabels(QStringList() << tr("Id") << QString::fromWCharArray(L"Тип") << QString::fromWCharArray(L"Класс"));
  for(int i = 0; i < 2; i++)
    list->resizeColumnToContents(i);
  list->sortItems(2, Qt::DescendingOrder);

  splitter->addWidget(list);
   
  connect(list, SIGNAL(currentItemChanged(QTreeWidgetItem*, QTreeWidgetItem*)), this, SLOT(itemChanged(QTreeWidgetItem*, QTreeWidgetItem*)));
  connect(list, SIGNAL(itemDoubleClicked(QTreeWidgetItem*, int)), this, SLOT(accept()));
  if(item != NULL)
    list->setCurrentItem(item);
    
  QDialogButtonBox *buttonBox = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);
  connect(buttonBox, SIGNAL(accepted()), this, SLOT(accept()));
  connect(buttonBox, SIGNAL(rejected()), this, SLOT(reject())); 
 
  QVBoxLayout *mainLayout = new QVBoxLayout();
  mainLayout->addWidget(splitter);
  mainLayout->addWidget(buttonBox);
  mainLayout->setContentsMargins(0, 0, 0, 0);
  setLayout(mainLayout);
 
  setWindowTitle(QString::fromWCharArray(L"Выберите оборудование"));

  static QSize a0 = (gsize = QSize(640, 480));
  static QPoint b0 = (gpos = QApplication::desktop()->screenGeometry().center() - QPoint(gsize.width(), gsize.height()) / 2);

  resize(gsize);
  move(gpos);
}

void ChooseToolDialog::itemChanged(QTreeWidgetItem* current, QTreeWidgetItem* previous) {
  bool needsResize = false;
  QList<int> s;
  if(this->infoWidget != NULL) {
    s = this->splitter->sizes();
    delete this->infoWidget;
    needsResize = true;
  }
  this->infoWidget = new ToolInfoWidget(this->game, ToolInfoWidget::ToolTypeInfo, current->data(0, Qt::DisplayRole).toInt());
  this->splitter->addWidget(this->infoWidget);
  if(needsResize)
    this->splitter->setSizes(s);
}


ChooseToolDialog::~ChooseToolDialog() {
  gsize = size();
  gpos = pos();
}

int ChooseToolDialog::execute() {
  if(list->topLevelItemCount() == 0)
    return INVALID_ID;
  exec();
  if(result() == Rejected)
    return INVALID_ID;
  else
    return list->currentItem()->data(0, Qt::DisplayRole).toInt();
}
