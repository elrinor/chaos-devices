#include <QtGui>
#include "QuestDialog.h"

QuestDialog::QuestDialog(TemplateList templates, QWidget* parent): QDialog(parent), templates(templates) {
    QPushButton* showAnsButton = new QPushButton("Show Answer");
    connect(showAnsButton, SIGNAL(clicked()), this, SLOT(showAnswer()));

    QVBoxLayout* mainLayout = new QVBoxLayout();
    
    QComboBox* comboBox = new QComboBox();
    for(int i = 0; i < templates.getTemplateList().size(); i++)
        comboBox->addItem(templates.getTemplateList()[i].name());
    QHBoxLayout* upperLayout = new QHBoxLayout();
    upperLayout->addWidget(new QLabel("Topic: "));
    upperLayout->addWidget(comboBox);
    upperLayout->addStretch(1);
    mainLayout->addLayout(upperLayout);

    this->questLayout = new QVBoxLayout();
    mainLayout->addLayout(this->questLayout, 1);

    mainLayout->addStretch(1);

    QHBoxLayout* hLayout = new QHBoxLayout();
    hLayout->addWidget(new QLabel(QString::fromLocal8Bit("Answer: ")));
    hLayout->addWidget(this->answerLabel = new QLabel());
    hLayout->addStretch(1);

    mainLayout->addLayout(hLayout);
    mainLayout->addWidget(showAnsButton);
    this->setLayout(mainLayout);

    initNextQuest(0);
    connect(comboBox, SIGNAL(currentIndexChanged(int)), this, SLOT(indexChanged(int)));

    // Size & Position
    this->resize(640, 480);
    QRect screen = QApplication::desktop()->screenGeometry();
    move(screen.center() - QPoint(width() / 2, height() / 2 ));
}

void QuestDialog::showAnswer() {
    try {
        this->currentTemplate.execBlock("askgen", this->currentContext, false);
        this->answerLabel->setText(this->currentContext.var("answer").toString());
    } catch (std::runtime_error e) {
        QMessageBox::critical(this, "Exception!", e.what());
    }
}

void QuestDialog::indexChanged(int index) {
    delete this->questWidget;
    initNextQuest(index);
}

void QuestDialog::initNextQuest(int index) {
    this->currentTemplate = this->templates.getTemplateList()[index];
    this->currentContext = Context();
    try {
        this->questWidget = new QWidget();
        this->questWidget->setLayout(this->currentTemplate.execBlock("askui", this->currentContext));
        this->questLayout->addWidget(this->questWidget);
    } catch (std::runtime_error e) {
        QMessageBox::critical(this, "Exception!", e.what());
    }
}
