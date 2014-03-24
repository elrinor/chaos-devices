#ifndef __MAINFORM_H__
#define __MAINFORM_H__

#include <QMainWindow>
#include "Template.h"

class QAction;
class QVBoxLayout;
class QLabel;
class QDockWidget;

class MainForm: public QMainWindow {
    Q_OBJECT
public:
    MainForm();

protected slots:
    void next();
    void ask();
    void changeTopic();
    void startTest();
    void endTest();

private:
    void initNextTest();
    void initNextTopic(int index);
    void updateLabels();
    void testNextQuest();

    TemplateList templates;

    QDockWidget* scoreBox;

    QAction *closeAction, *nextAction, *askAction, *changeTopicAction, *startTestAction, *showScoreBoxAction;
    QVBoxLayout* mainLayout;
    QLabel *okCountLabel, *errorCountLabel, *topicLabel;

    QWidget* testWidget;

    int okCount, errorCount;

    int topicIndex;

    Test currentTest;
    QList<Template> testsLeft;
    bool inTestMode;

    Template currentTemplate;
    Context currentContext;
};

#endif