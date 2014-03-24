#include <QtGui>
#include <QtXml>
#include "MainForm.h"
#include "QuestDialog.h"
#include "ChooseTopicDialog.h"
#include "ChooseTestDialog.h"
#include "Util.h"

MainForm::MainForm() {
    // Create templates
    if(!QFile::exists("template.xml")) {
        QMessageBox::critical(this, "Error", "File \"template.xml\" not found!");
        exit(1);
    }
    templates = TemplateList("template.xml");
    if(templates.getTemplateList().size() == 0) {
        QMessageBox::critical(this, "Error", "No templates found in file \"template.xml\"!");
        exit(1);
    }

    // Init
    this->inTestMode = false;

    // Actions
    closeAction = new QAction(tr("E&xit"), this);
    connect(closeAction, SIGNAL(triggered()), this, SLOT(close()));

    nextAction = new QAction(tr("&Next"), this);
    connect(nextAction, SIGNAL(triggered()), this, SLOT(next()));

    askAction = new QAction(tr("&Ask Question"), this);
    connect(askAction, SIGNAL(triggered()), this, SLOT(ask()));

    changeTopicAction = new QAction(tr("&Change Topic"), this);
    connect(changeTopicAction, SIGNAL(triggered()), this, SLOT(changeTopic()));

    startTestAction = new QAction(tr("&Start Test"), this);
    connect(startTestAction, SIGNAL(triggered()), this, SLOT(startTest()));

    // Menu
    QMenu *fileMenu = menuBar()->addMenu(tr("&Test"));
    fileMenu->addAction(nextAction);
    fileMenu->addSeparator();
    fileMenu->addAction(startTestAction);
    fileMenu->addSeparator();
    fileMenu->addAction(changeTopicAction);
    fileMenu->addAction(askAction);
    fileMenu->addSeparator();
    fileMenu->addAction(closeAction);

    // Next Box
    QDockWidget* buttonBox = new QDockWidget(tr("Control"));
    QVBoxLayout* buttonBoxLayout = new QVBoxLayout();
    QPushButton* nextButton = new QPushButton(tr("&Next"));
    connect(nextButton, SIGNAL(clicked()), nextAction, SIGNAL(triggered()));
    buttonBoxLayout->addWidget(nextButton);
    QWidget* buttonBoxWidget = new QWidget();
    buttonBoxWidget->setLayout(buttonBoxLayout);
    buttonBox->setWidget(buttonBoxWidget);
    addDockWidget(Qt::BottomDockWidgetArea, buttonBox);

    // Score Box
    scoreBox = new QDockWidget(tr("Score"));
    QGridLayout* scoreBoxLayout = new QGridLayout();
    this->okCountLabel = new QLabel("0");
    this->errorCountLabel = new QLabel("0");
    scoreBoxLayout->addWidget(new QLabel(tr("Passed:")),         0, 0);
    scoreBoxLayout->addWidget(new QLabel(tr("Failed:")),         1, 0);
    scoreBoxLayout->addWidget(okCountLabel,                      0, 1);
    scoreBoxLayout->addWidget(errorCountLabel,                   1, 1);
    scoreBoxLayout->setColumnStretch(1, 1);
    QWidget* scoreBoxWidget = new QWidget();
    scoreBoxWidget->setLayout(scoreBoxLayout);
    scoreBox->setWidget(scoreBoxWidget);
    addDockWidget(Qt::BottomDockWidgetArea, scoreBox);

    // View menu
    showScoreBoxAction = scoreBox->toggleViewAction();
    QMenu *viewMenu = menuBar()->addMenu(tr("&View"));
    viewMenu->addAction(buttonBox->toggleViewAction());
    viewMenu->addAction(showScoreBoxAction);

    // Central Widget
    QWidget* centralWidget = new QWidget();
    this->setCentralWidget(centralWidget);
    mainLayout = new QVBoxLayout();
    QHBoxLayout* upLayout = new QHBoxLayout();
    upLayout->addWidget(new QLabel(tr("Topic: ")));
    upLayout->addWidget(topicLabel = new QLabel(""), 1);
    mainLayout->addLayout(upLayout);
    centralWidget->setLayout(mainLayout);
    initNextTopic(0);

    // Size & Position
    this->resize(640, 480);
    QRect screen = QApplication::desktop()->screenGeometry();
    move(screen.center() - QPoint(width() / 2, height() / 2 ));
}

void MainForm::initNextTest() {
    this->topicLabel->setText(this->currentTemplate.name());
    this->currentContext = Context();
    try {
        this->testWidget = new QWidget();
        this->testWidget->setLayout(this->currentTemplate.execBlock("testui", this->currentContext));
        this->mainLayout->addWidget(this->testWidget);
    } catch (std::runtime_error e) {
        QMessageBox::critical(this, "Exception!", e.what());
    }
}

void MainForm::initNextTopic(int index) {
    this->topicIndex = index;
    this->currentTemplate = this->templates.getTemplateList()[this->topicIndex];
    this->okCount = 0;
    this->errorCount = 0;

    updateLabels();
    initNextTest();
}

void MainForm::updateLabels() {
    this->okCountLabel->setNum(this->okCount);
    this->errorCountLabel->setNum(this->errorCount);
}

void MainForm::next() {
    try {
        bool result = false;
        try {
            this->currentTemplate.execBlock("testcheck", this->currentContext, false);
            result = this->currentContext.var("result").toBool();
        } catch (std::runtime_error e) {
            e.what();
        }
        if(!result) {
            this->currentTemplate.execBlock("askgen", this->currentContext, false);
            if(this->inTestMode && this->currentTest.showMistakes() || !this->inTestMode)
                QMessageBox::critical(this, "Error!", "Right answer:\n" + this->currentContext.var("answer").toString());
            this->errorCount++;
        } else
            this->okCount++;
    } catch (std::runtime_error e) {
        QMessageBox::critical(this, "Exception!", e.what());
    }

    delete this->testWidget;
    if(this->inTestMode)
        testNextQuest();
    else
        initNextTest();

    updateLabels();
}

void MainForm::ask() {
    QuestDialog* questDialog = new QuestDialog(this->templates, this);
    questDialog->exec();
    delete questDialog;
}

void MainForm::changeTopic() {
    ChooseTopicDialog* dlg = new ChooseTopicDialog(this->templates, this);
    if(dlg->exec() == QDialog::Accepted && this->topicIndex != dlg->currentIndex()) {
        delete this->testWidget;
        initNextTopic(dlg->currentIndex());
    }
    delete dlg;
}

void MainForm::startTest() {
    ChooseTestDialog* dlg = new ChooseTestDialog(this->templates, this);
    if(dlg->exec() == QDialog::Accepted) {
        this->askAction->setDisabled(true);
        this->changeTopicAction->setDisabled(true);
        delete this->testWidget;

        this->errorCount = 0;
        this->okCount = 0;
        this->inTestMode = true;
        this->currentTest = this->templates.getTestList()[dlg->currentIndex()];
        this->testsLeft = this->currentTest.testTemplates();
        if(!this->currentTest.showMistakes()) {
            this->scoreBox->hide();
            this->showScoreBoxAction->setDisabled(true);
        }
        testNextQuest();
    }
    delete dlg;
}

void MainForm::endTest() {
    this->askAction->setEnabled(true);
    
}

void MainForm::testNextQuest() {
    if(this->testsLeft.size() == 0) {
        this->inTestMode = false;
        this->scoreBox->show();
        this->showScoreBoxAction->setEnabled(true);
        this->askAction->setEnabled(true);
        this->changeTopicAction->setEnabled(true);

        int totalCount = this->okCount + this->errorCount;
        int okCount = this->okCount;
        float result = (float) okCount / totalCount;
        int mark = 4;
        while(mark > 0) {
            if(result >= this->currentTest.markCriteria()[mark])
                break;
            mark--;
        }
        mark++;


        initNextTopic(this->topicIndex);

        QMessageBox::information(this, "End of Test", 
            QString().setNum(okCount) + " of " + QString().setNum(totalCount) + " answers correct\n" +
            "This makes " + QString().setNum((int) (result * 100)) + "%\n" + 
            "Your mark is " + QString().setNum(mark));

    } else {
        int index = random(0, this->testsLeft.size() - 1);
        this->currentTemplate = this->testsLeft[index];
        this->testsLeft.removeAt(index);
        initNextTest();
    }
}