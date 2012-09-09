#include <QtGui>
#include <fstream>
#include "MainForm.h"
#include "MapWidget.h"
#include "UnitConfigurationsBox.h"
#include "NewGameDialog.h"
#include "StartGameDialog.h"
#include "ResourceBox.h"
#include "LogBox.h"
#include "AboutDialog.h"

const QPalette& MainForm::getDefaultPalette() {
  static bool initialized = false;
  static QPalette pal;
  if(!initialized) {
    pal.setCurrentColorGroup(QPalette::Active);
    pal.setColor(QPalette::Window,        QColor(224, 224, 224));
    pal.setColor(QPalette::WindowText,    QColor(0,   32,  0));
    pal.setColor(QPalette::Base,          QColor(224, 224, 224));
    pal.setColor(QPalette::AlternateBase, QColor(204, 204, 204));
    pal.setColor(QPalette::Text,          QColor(32,  128, 32));
    pal.setColor(QPalette::Button,        QColor(192, 192, 192));
    pal.setColor(QPalette::ButtonText,    QColor(0,   0,   0));
    pal.setColor(QPalette::BrightText,    QColor(0,   0,   0));

    pal.setColor(QPalette::Light,         QColor(255, 255, 255));
    pal.setColor(QPalette::Midlight,      QColor(224, 224, 224));
    pal.setColor(QPalette::Dark,          QColor(128, 128, 128));
    pal.setColor(QPalette::Mid,           QColor(160, 160, 160));
    pal.setColor(QPalette::Shadow,        QColor(64,  64,  64));
  }
  return pal;
}


MainForm::MainForm() {
  // Game
  this->game = NULL;

  // Notifier
  this->gameStateNotifier = new GameStateNotifier();

  // Palette
  setPalette(getDefaultPalette());

  // Actions
  QAction* closeAction = new QAction(QString::fromWCharArray(L"В&ыход"), this);
  connect(closeAction, SIGNAL(triggered()), this, SLOT(close()));

  newFreeGameAction = new QAction(QString::fromWCharArray(L"&Свободное Строительство"), this);
  newFreeGameAction->setShortcut(QKeySequence::New);
  connect(newFreeGameAction, SIGNAL(triggered()), this, SLOT(newGameDlg()));

  saveGameAction = new QAction(QString::fromWCharArray(L"&Сохранить игру"), this);
  saveGameAction->setShortcut(QKeySequence::Save);
  connect(saveGameAction, SIGNAL(triggered()), this, SLOT(saveGameDlg()));
  saveGameAction->setDisabled(true);

  openGameAction = new QAction(QString::fromWCharArray(L"&Загрузить игру"), this);
  openGameAction->setShortcut(QKeySequence::Open);
  connect(openGameAction, SIGNAL(triggered()), this, SLOT(loadGameDlg()));

  closeGameAction = new QAction(QString::fromWCharArray(L"&Окончить игру"), this);
  closeGameAction->setShortcut(QKeySequence::Close);
  connect(closeGameAction, SIGNAL(triggered()), this, SLOT(endGameDlg()));
  closeGameAction->setDisabled(true);

  startGameAction = new QAction(QString::fromWCharArray(L"&Начать игру"), this);
  startGameAction->setShortcut(QKeySequence("F5"));
  connect(startGameAction, SIGNAL(triggered()), this, SLOT(startGameDlg()));
  startGameAction->setDisabled(true);

  aboutAction = new QAction(QString::fromWCharArray(L"&О программе"), this);
  connect(aboutAction, SIGNAL(triggered()), this, SLOT(about()));

  aboutQtAction = new QAction(QString::fromWCharArray(L"О &Qt"), this);
  connect(aboutQtAction, SIGNAL(triggered()), this, SLOT(aboutQt()));


  // Menu
  QMenu *fileMenu = menuBar()->addMenu(QString::fromWCharArray(L"&Игра"));
  QMenu *newGameMenu = fileMenu->addMenu(QString::fromWCharArray(L"&Новая игра"));
  fileMenu->addAction(saveGameAction);
  fileMenu->addAction(openGameAction);
  fileMenu->addAction(closeGameAction);
  fileMenu->addSeparator();
  fileMenu->addAction(startGameAction);
  fileMenu->addSeparator();
  fileMenu->addAction(closeAction);

  // NewGame Menu
  newGameMenu->addAction(newFreeGameAction);
  newGameMenu->addSeparator();
  QMenu* missionsMenu = newGameMenu->addMenu(QString::fromWCharArray(L"&Миссии"));

  // Missions Menu
  QDir missionDir = QDir("./missions");
  if(missionDir.exists()) {
    QStringList missionList = missionDir.entryList(QStringList() << "*.game");
    FOREACH(QString mission, missionList) {
      QAction *missionAction = new QAction(mission, this);
      missionAction->setData(mission);
      missionsMenu->addAction(missionAction);
    }
  }
  connect(missionsMenu, SIGNAL(triggered(QAction*)), this, SLOT(mission(QAction*)));

  QMenu *viewMenu = menuBar()->addMenu(QString::fromWCharArray(L"&Вид"));

  QMenu *helpMenu = menuBar()->addMenu(QString::fromWCharArray(L"&Помощь"));
  helpMenu->addAction(aboutAction);
  helpMenu->addAction(aboutQtAction);

  this->unitConfigsBox = new UnitConfigurationsBox(this, this);
  addDockWidget(Qt::TopDockWidgetArea, unitConfigsBox); 
  viewMenu->addAction(unitConfigsBox->toggleViewAction());

  this->logBox = new LogBox(this, this);
  addDockWidget(Qt::BottomDockWidgetArea, logBox); 
  viewMenu->addAction(logBox->toggleViewAction());

  this->resourceBox = new ResourceBox(this, this);
  addDockWidget(Qt::LeftDockWidgetArea, resourceBox); 
  viewMenu->addAction(resourceBox->toggleViewAction());

  this->gameControlBox = new GameControlBox(this, this);
  addDockWidget(Qt::LeftDockWidgetArea, gameControlBox);
  viewMenu->addAction(gameControlBox->toggleViewAction());

  // Central Widget
  QWidget* centralWidget = new QWidget(this);
  
  this->mapWidget = new MapWidget(this, centralWidget);
  QVBoxLayout *vLayout = new QVBoxLayout(centralWidget);
  vLayout->addWidget(this->mapWidget);
  //vLayout->addWidget(this->menuBox);
  vLayout->setContentsMargins(0, 0, 0, 0);
  centralWidget->setLayout(vLayout);
  this->setCentralWidget(centralWidget);

  // Size & Position
  this->resize(640, 480);
  QRect screen = QApplication::desktop()->screenGeometry();
  move(screen.center() - QPoint(width() / 2, height() / 2 ));

  // Enables...
  this->gameControlBox->setEnabled(false);
  this->startGameAction->setEnabled(false);
}

void MainForm::newGame(Game* game) {
  assert(this->game == NULL);
  this->game = game;
  this->game->setLogger(this);

  this->gameStateNotifier->newGame(game);

  this->logMessage(QString::fromWCharArray(L"<font color=#000000>&nbsp;&nbsp;ЦЕЛЬ МИССИИ:</font>"));
  this->logMessage(game->getObjectives());
  this->logMessage("");

  saveGameAction->setEnabled(true);
  closeGameAction->setEnabled(true);
  startGameAction->setEnabled(true);

  if(game->isStarted())
    startGame();
}

void MainForm::startGame() {
  this->startGameAction->setEnabled(false);
  this->gameStateNotifier->startGame();
}

void MainForm::makeTurn() {
  this->gameStateNotifier->makeTurn();
}

void MainForm::gameOver() {
  //this->startGameAction->setEnabled(true);
  this->gameStateNotifier->gameOver();
}

void MainForm::closeGame() {
  assert(this->game != NULL);
  if(this->game->isStarted())
    this->gameStateNotifier->gameOver();

  this->gameStateNotifier->closeGame();

  delete this->game;
  this->game = NULL;

  saveGameAction->setDisabled(true);
  closeGameAction->setDisabled(true);
  startGameAction->setDisabled(true);
}

void MainForm::changeGame(Game* game) {
  if(this->game != NULL)
    closeGame();
  newGame(game);
}

void MainForm::newGameDlg() {
  NewGameDialog* dlg = new NewGameDialog(this);
  Game* game = dlg->execute();
  delete dlg;
  if(game != NULL)
    changeGame(game);
}

void MainForm::endGameDlg() {
  if(QMessageBox::question(this, tr("?"), QString::fromWCharArray(L"Вы действительно хотите закончить текущую игру?"), QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes) == QMessageBox::Yes)
    closeGame();
}

void MainForm::saveGameDlg() {
  QString fileName = QFileDialog::getSaveFileName(this, QString::fromWCharArray(L"Сохранить игру"), tr("."), QString::fromWCharArray(L"Игровые файлы (*.game)")); 
  if(!fileName.isEmpty()) {
    std::ofstream f(fileName.toAscii().constData());
    this->game->serialize(f);
    f.close();
  }
}

void MainForm::loadGameDlg() {
  QString fileName = QFileDialog::getOpenFileName(this, QString::fromWCharArray(L"Загрузить игру"), tr("."), QString::fromWCharArray(L"Игровые файлы (*.game)")); 
  if(!fileName.isEmpty()) {
		try {
			std::ifstream f(fileName.toAscii().constData());
			Game* game = Game::deserialize(f);
			f.close();

			changeGame(game);
		} catch (std::runtime_error e) {
			QMessageBox::critical(this, QString::fromWCharArray(L"Ошибка!"), e.what());
		}
  }
}

void MainForm::startGameDlg() {
  StartGameDialog* dlg = new StartGameDialog(this->game, this);
  QString defenderName, attackerName;
  if(dlg->execute(defenderName, attackerName)) {
    getGame()->start(defenderName, attackerName);
    startGame();
  }
  delete dlg;
}

void MainForm::nextTurnPressed() {
  this->game->makeTurn();
  
  this->mapWidget->invalidateScene();
  //this->mapWidget->update(); // TODO: do we need this?
  
  makeTurn();

  if(this->game->isOver())
    gameOver();
}

void MainForm::mission(QAction* action) {
  QString fileName = "./missions/" + action->data().toString();
  try {
    std::ifstream f(fileName.toAscii().constData());
    Game* game = Game::deserialize(f);
    f.close();

    changeGame(game);
  } catch (std::runtime_error e) {
    QMessageBox::critical(this, QString::fromWCharArray(L"Ошибка!"), e.what());
  }
}

void MainForm::about() {
  AboutDialog* dlg = new AboutDialog();
  dlg->exec();
  delete dlg;
}

void MainForm::aboutQt() {
  QApplication::aboutQt();
}
