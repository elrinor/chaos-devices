#include <QtGui>
#include "ToolInfoWidget.h"
#include "MainForm.h"

using namespace boost;
using namespace std;

ToolInfoWidget::ToolInfoWidget(Game* game, InfoType type, int id, QWidget *parent): QWidget(parent) {
  this->game = game;
  this->id = id;

  Tool* tool;
  ToolType* toolType;
  
  if(type == ToolInfo) {
    tool = game->getTool(id);
    toolType = tool->getType();
  } else if(type == ToolTypeInfo) {
    tool = NULL;
    toolType = game->getToolType(id);
  } else
    assert(!"Unreachable");

  setPalette(MainForm::getDefaultPalette());

  // Pic
  QLabel *picLabel = new QLabel();
  picLabel->setPixmap(QPixmap(toolType->getImageName()).scaled(128, 128, Qt::KeepAspectRatio, Qt::SmoothTransformation));

  // Params
  QLabel* idLabel = new QLabel(tr("Id:"));
  QLabel* typeLabel = new QLabel(QString::fromWCharArray(L"Тип:"));
  QLabel* classLabel = new QLabel(QString::fromWCharArray(L"Класс:"));
  QLabel* costLabel = new QLabel(QString::fromWCharArray(L"Цена:"));
  QLabel* hpLabel = new QLabel(QString::fromWCharArray(L"Жизнь:"));
  QLabel* cpuLabel = new QLabel(QString::fromWCharArray(L"Производимая выч. мощность:"));
  QLabel* memLabel = new QLabel(QString::fromWCharArray(L"Производимая память:"));
  QLabel* powerLabel = new QLabel(QString::fromWCharArray(L"Производимое питание:"));

  QLabel *idd = new QLabel(QString().setNum(id));
  QLabel *typeName = new QLabel(toolType->getName() + " (" + QString().setNum(toolType->getId()) + ")");
  QLabel* className = new QLabel(toolType->getClass()->getName() + " (" + QString().setNum(toolType->getClass()->getId()) + ")");
  QLabel *cost = new QLabel(); 
  QLabel *hp = new QLabel();
  QLabel *cpu = new QLabel();
  QLabel *mem = new QLabel();
  QLabel *power = new QLabel();

  if(tool != NULL) {
    cost->setNum(tool->getCost());
    hp->setText(QString().setNum(tool->getHP()) + " / " + QString().setNum(tool->getMaxHP()));
    cpu->setNum(tool->getCpuProduction());
    mem->setNum(tool->getMemProduction());
    power->setNum(tool->getPowerProduction());
  } else {
    cost->setNum(toolType->getCost());
    hp->setNum(toolType->getMaxHP());
    cpu->setNum(toolType->getCpuProduction());
    mem->setNum(toolType->getMemProduction());
    power->setNum(toolType->getPowerProduction());
  }

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

  idLabel->setBuddy(idd);
  typeLabel->setBuddy(typeName);
  classLabel->setBuddy(className);
  hpLabel->setBuddy(hp);
  cpuLabel->setBuddy(cpu);
  memLabel->setBuddy(mem);
  powerLabel->setBuddy(power);

  QGridLayout *paramLayout = new QGridLayout();
  paramLayout->addWidget(idLabel,         0, 0);
  paramLayout->addWidget(idd,             0, 1);
  paramLayout->addWidget(typeLabel,       1, 0);
  paramLayout->addWidget(typeName,        1, 1);
  paramLayout->addWidget(classLabel,      2, 0);
  paramLayout->addWidget(className,       2, 1);
  paramLayout->addWidget(costLabel,       3, 0);
  paramLayout->addLayout(costLayout,      3, 1);
  paramLayout->addWidget(hpLabel,         4, 0);
  paramLayout->addWidget(hp,              4, 1);
  paramLayout->addWidget(cpuLabel,        5, 0);
  paramLayout->addWidget(cpu,             5, 1);
  paramLayout->addWidget(memLabel,        6, 0);
  paramLayout->addWidget(mem,             6, 1);
  paramLayout->addWidget(powerLabel,      7, 0);
  paramLayout->addWidget(power,           7, 1);
  paramLayout->setContentsMargins(5, 5, 5, 5);
  paramLayout->setColumnStretch(1, 1);

  // Upper Box
  QGroupBox *upperBox = new QGroupBox(QString::fromWCharArray(L"&Параметры Оборудования"));
  QHBoxLayout *upperLayout = new QHBoxLayout();
  upperLayout->addWidget(picLabel);
  upperLayout->addLayout(paramLayout);
  upperBox->setLayout(upperLayout);

  // Lower Box
  QGroupBox *defenceBox = new QGroupBox(QString::fromWCharArray(L"&Защита"));
  QBoxLayout *defenceLayout = new QVBoxLayout();

  QTreeView *dView = new QTreeView();
  dView->setRootIsDecorated(false);
  dView->setAlternatingRowColors(true);

  dModel = new QStandardItemModel(0, 4);
  dModel->setHeaderData(0, Qt::Horizontal, tr("Id"));
  dModel->setHeaderData(1, Qt::Horizontal, QString::fromWCharArray(L"Тип"));
  dModel->setHeaderData(2, Qt::Horizontal, QString::fromWCharArray(L"Защита"));
  dModel->setHeaderData(3, Qt::Horizontal, QString::fromWCharArray(L"Устойчивость к повреждениям"));
  for(int i = 0; i < toolType->getDefenceTypeSize(); i++) {
    DefenceType* d = toolType->getDefenceType(i);
		QStandardItem *idItem, *typeItem, *defenceItem, *armorItem;
		if(tool != NULL) {
			Defence* dd = tool->getDefence(i);
			idItem = new QStandardItem(QString().setNum(dd->getId()));
			defenceItem = new QStandardItem(QString().setNum(dd->getDefence()));
			armorItem = new QStandardItem(QString().setNum(dd->getArmor()));
			typeItem = new QStandardItem(d->getClass()->getName() + " (" + QString().setNum(d->getClass()->getId()) + ") (" + QString().setNum(d->getId()) + ")");
		} else {
			idItem = new QStandardItem(QString().setNum(d->getId()));
			defenceItem = new QStandardItem(QString().setNum(d->getDefence()));
			armorItem = new QStandardItem(QString().setNum(d->getArmor()));
			typeItem = new QStandardItem(d->getClass()->getName() + " (" + QString().setNum(d->getClass()->getId()) + ")");
		}

		typeItem->setEditable(false);
		defenceItem->setEditable(false);
    armorItem->setEditable(false);
		idItem->setEditable(false);

    dModel->appendRow(QList<QStandardItem*>() << idItem << typeItem << defenceItem << armorItem);
  }
  dView->setModel(dModel);
  for(int i = 0; i < 5; i++)
    dView->resizeColumnToContents(i);

  defenceLayout->addWidget(dView);

  defenceBox->setLayout(defenceLayout);

  // Main Layout
  QVBoxLayout *mainLayout = new QVBoxLayout();
  mainLayout->addWidget(upperBox, 0, Qt::AlignTop);
  mainLayout->addWidget(defenceBox);
  mainLayout->setContentsMargins(0, 0, 0, 0);
  setLayout(mainLayout);

  resize(640, 480);  
  setWindowTitle(QString::fromWCharArray(L"Свойства Устройства"));
}

ToolInfoWidget::~ToolInfoWidget() {
  delete dModel;
}