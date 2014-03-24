#ifndef __QUESTDIALOG_H__
#define __QUESTDIALOG_H__

#include <QDialog>
#include "Template.h"

class QVBoxLayout;
class QLabel;

class QuestDialog: public QDialog {
    Q_OBJECT
public:
    QuestDialog(TemplateList templates, QWidget* parent = 0);

protected slots:
    void showAnswer();
    void indexChanged(int index);

private:
    void initNextQuest(int index);

    TemplateList templates;

    Template currentTemplate;
    Context currentContext;

    QWidget* questWidget;
    QVBoxLayout* questLayout;
    QLabel* answerLabel;
};


#endif