#include <QtGui>
#include "UnitDialog.h"
#include "ToolInfoDialog.h"
#include "MainForm.h"
#include "ChooseToolDialog.h"

using namespace boost;
using namespace std;

static QSize gsize;
static QPoint gpos;

UnitDialog::UnitDialog(MainForm* mainForm, Game* game, int unitId, QWidget *parent): QDialog(parent) {
  this->unitId = unitId;
  this->game = game;
  this->mainForm = mainForm;

  Unit* unit = game->getUnit(unitId);

  setPalette(MainForm::getDefaultPalette());

  // Pic
  QLabel *picLabel = new QLabel();
  picLabel->setPixmap(QPixmap(unit->getType()->getImageName()).scaled(128, 128, Qt::KeepAspectRatio, Qt::SmoothTransformation));

  // ParamBox
  QLabel* nameLabel = new QLabel(QString::fromWCharArray(L"&Имя:"));
  QLabel* idLabel = new QLabel(tr("Id:"));
  QLabel* typeLabel = new QLabel(QString::fromWCharArray(L"Тип:"));
  QLabel* costLabel = new QLabel(QString::fromWCharArray(L"Цена:"));
  QLabel* hpLabel = new QLabel(QString::fromWCharArray(L"Жизнь:"));
  QLabel* cpuUsageLabel = new QLabel(QString::fromWCharArray(L"Использование выч. ресурсов:"));
  QLabel* memUsageLabel = new QLabel(QString::fromWCharArray(L"Использование памяти:"));
  QLabel* powerUsageLabel = new QLabel(QString::fromWCharArray(L"Использование питания:"));

  nameEdit = new QLineEdit();
  QLabel *id = new QLabel(QString().setNum(unit->getId()));
  QLabel *typeName = new QLabel(unit->getType()->getName() + " (" + QString().setNum(unit->getType()->getId()) + ")");
  cost = new QLabel();
  hp = new QLabel();
  cpuUsage = new QLabel();
  memUsage = new QLabel();
  powerUsage = new QLabel();

  QPalette pal = cost->palette();
  pal.setColor(QPalette::WindowText, QCOLOR_GOLD);
  cost->setPalette(pal);

  QLabel *coinLabel = new QLabel();
  QPixmap coinPic = QPixmap(":/pic_gold.png").scaled(16, 16, Qt::KeepAspectRatio, Qt::SmoothTransformation);
  coinLabel->setPixmap(coinPic);

  QHBoxLayout *costLayout = new QHBoxLayout();
  costLayout->setContentsMargins(0, 0, 0, 0);
  costLayout->addWidget(cost);
  costLayout->addWidget(coinLabel);
  costLayout->addStretch(1);

  nameLabel->setBuddy(nameEdit);
  idLabel->setBuddy(id);
  typeLabel->setBuddy(typeName);
  //costLabel->setBuddy(cost);
  hpLabel->setBuddy(hp);
  cpuUsageLabel->setBuddy(cpuUsage);
  memUsageLabel->setBuddy(memUsage);
  powerUsageLabel->setBuddy(powerUsage);

  QGridLayout *paramLayout = new QGridLayout();
  paramLayout->addWidget(nameLabel,       0, 0);
  paramLayout->addWidget(nameEdit,        0, 1);
  paramLayout->addWidget(idLabel,         1, 0);
  paramLayout->addWidget(id,              1, 1);
  paramLayout->addWidget(typeLabel,       2, 0);
  paramLayout->addWidget(typeName,        2, 1);
  paramLayout->addWidget(costLabel,       3, 0);
  paramLayout->addLayout(costLayout,      3, 1);
  paramLayout->addWidget(hpLabel,         4, 0);
  paramLayout->addWidget(hp,              4, 1);
  paramLayout->addWidget(cpuUsageLabel,   5, 0);
  paramLayout->addWidget(cpuUsage,        5, 1);
  paramLayout->addWidget(memUsageLabel,   6, 0);
  paramLayout->addWidget(memUsage,        6, 1);
  paramLayout->addWidget(powerUsageLabel, 7, 0);
  paramLayout->addWidget(powerUsage,      7, 1);
  paramLayout->setContentsMargins(0, 0, 0, 0);
  paramLayout->setColumnStretch(1, 1);

  // UpperBox
  QGroupBox *upperBox = new QGroupBox(QString::fromWCharArray(L"&Параметры Устройства"));
  QHBoxLayout *upperLayout = new QHBoxLayout();
  upperLayout->addWidget(picLabel);
  upperLayout->addLayout(paramLayout, 1);
  upperBox->setLayout(upperLayout);

  // Tools
  QGroupBox *toolsBox = new QGroupBox(QString::fromWCharArray(L"&Оборудование"));
  QBoxLayout *toolLayout = new QVBoxLayout();
  
  toolsView = new QTreeView();
  toolsView->setRootIsDecorated(false);
  toolsView->setAlternatingRowColors(true);

  toolLayout->addWidget(toolsView);

  QHBoxLayout *buttonLayout = new QHBoxLayout();
  QPushButton* addButton = new QPushButton(QString::fromWCharArray(L"&Добавить"));
  QPushButton* deleteButton = new QPushButton(QString::fromWCharArray(L"&Удалить"));
  buttonLayout->addWidget(addButton);
  buttonLayout->addWidget(deleteButton);
  buttonLayout->addStretch(1);

  connect(addButton, SIGNAL(clicked()), this, SLOT(addUnitClick()));
  connect(deleteButton, SIGNAL(clicked()), this, SLOT(deleteUnitClick()));

  toolLayout->addLayout(buttonLayout);

  toolsBox->setLayout(toolLayout);

  connect(toolsView, SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(toolDoubleClick(const QModelIndex&)));

  // BtnBox
  QDialogButtonBox *buttonBox = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);
  connect(buttonBox, SIGNAL(accepted()), this, SLOT(accept()));
  connect(buttonBox, SIGNAL(rejected()), this, SLOT(reject())); 

  // Overall
  QVBoxLayout *mainLayout = new QVBoxLayout();
  mainLayout->addWidget(upperBox, 0, Qt::AlignTop);
  mainLayout->addWidget(toolsBox);
  mainLayout->addWidget(buttonBox);
  mainLayout->setContentsMargins(0, 0, 0, 0);
  setLayout(mainLayout);

  setWindowTitle(QString::fromWCharArray(L"Свойства Устройства"));

  static QSize a0 = (gsize = QSize(640, 480));
  static QPoint b0 = (gpos = QApplication::desktop()->screenGeometry().center() - QPoint(gsize.width(), gsize.height()) / 2);

  resize(gsize);
  move(gpos);

  toolsModel = NULL;
  notify();
}

void UnitDialog::notify() {
  Unit* unit = game->getUnit(unitId);

  nameEdit->setText(unit->getName());
  cost->setText(QString().setNum(unit->getCost()));
  hp->setText(QString().setNum(unit->getHP()) + " / " + QString().setNum(unit->getMaxHP()));
  cpuUsage->setText(QString().setNum((int) (unit->getCPUUsage() * 100)) + "%");
  memUsage->setText(QString().setNum((int) (unit->getMemoryUsage() * 100)) + "%");
  powerUsage->setText(QString().setNum((int) (unit->getPowerUsage() * 100)) + "%");

  if(toolsModel != NULL)
    delete toolsModel;
  toolsModel = new QStandardItemModel(0, 5);
  toolsModel->setHeaderData(0, Qt::Horizontal, tr("Id"));
  toolsModel->setHeaderData(1, Qt::Horizontal, QString::fromWCharArray(L"Тип"));
  toolsModel->setHeaderData(2, Qt::Horizontal, QString::fromWCharArray(L"Класс"));
  toolsModel->setHeaderData(3, Qt::Horizontal, QString::fromWCharArray(L"Цена"));
  toolsModel->setHeaderData(4, Qt::Horizontal, QString::fromWCharArray(L"Жизнь"));
  for(int i = 0; i < unit->getToolSize(); i++) {
    Tool* tool = unit->getTool(i);
    QStandardItem *idItem = new QStandardItem(QString().setNum(tool->getId()));
    idItem->setEditable(false);
    QStandardItem *typeItem = new QStandardItem(tool->getType()->getName() + " (" + QString().setNum(tool->getType()->getId()) + ")");
    typeItem->setEditable(false);
    QStandardItem *classItem = new QStandardItem(tool->getType()->getClass()->getName() + " (" + QString().setNum(tool->getType()->getClass()->getId()) + ")");
    classItem->setEditable(false);
    QStandardItem *costItem = new QStandardItem(QString().setNum(tool->getCost()));
    costItem->setEditable(false);
    QStandardItem *hpItem = new QStandardItem(QString().setNum(tool->getHP()) + " / " + QString().setNum(tool->getMaxHP()));
    hpItem->setEditable(false);

    toolsModel->appendRow(QList<QStandardItem*>() << idItem << typeItem << classItem << costItem << hpItem);
  }
  toolsView->setModel(toolsModel);
  for(int i = 0; i < 5; i++)
    toolsView->resizeColumnToContents(i);
}

UnitDialog::~UnitDialog() {
  gsize = size();
  gpos = pos();
  delete this->toolsModel;
}

void UnitDialog::execute() {
  this->game->subscribe(this);
  exec();
  this->game->unsubscribe(this);
  if(result() == QDialog::Accepted)
    this->game->getUnit(this->unitId)->setName(this->nameEdit->text());
}

void UnitDialog::toolDoubleClick(const QModelIndex& index) {
  ToolInfoDialog* dlg = new ToolInfoDialog(this->game, toolsModel->data(toolsModel->index(index.row(), 0)).toInt(), this);
  dlg->execute();
  delete dlg;
}

void UnitDialog::deleteUnitClick() {
  if(this->game->isStarted())
    return;

  QModelIndex idx = toolsView->currentIndex();
  if(idx.column() == -1 || idx.row() == -1)
    return;
  int id = toolsModel->data(toolsModel->index(idx.row(), 0)).toInt();
  if(this->game->getTool(id)->isRemovable()) {
    this->game->deleteTool(id);
    if(this->toolsView->model()->rowCount() >= idx.row())
      idx = toolsModel->index(this->toolsView->model()->rowCount() - 1, idx.column());
    this->toolsView->setCurrentIndex(idx);
  } else
    this->mainForm->logMessage(QString::fromWCharArray(L"<font color=#FF0000>ВНИМАНИЕ:</font> Не могу удалить \"") + this->game->getTool(id)->getType()->getName() + "\"");
}

void UnitDialog::addUnitClick() {
  if(this->game->isStarted())
    return;

  ChooseToolDialog* dlg = new ChooseToolDialog(this->game, this->unitId, this);
  int id = dlg->execute();
  delete dlg;
  if(id != INVALID_ID) {
    ToolType* toolType = this->game->getToolType(id);
    if(toolType->getCost() > game->getDefenderResources())
      this->mainForm->logMessage(QString::fromWCharArray(L"<font color=#FF0000>ВНИМАНИЕ:</font> Недостаточно ресурсов для \"") + toolType->getName() + "\"");
    else
      toolType->spawnTool(this->game, this->unitId, true);
  }
}
