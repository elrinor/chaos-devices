#ifndef __CHOOSETOPICDIALOG_H__
#define __CHOOSETOPICDIALOG_H__

#include <QDialog>
#include "Template.h"

class QComboBox;

class ChooseTopicDialog: public QDialog {
    Q_OBJECT
public:
    ChooseTopicDialog(TemplateList templates, QWidget* parent = 0);

    int currentIndex() const;

protected:

private:
    QComboBox* comboBox;
};

#endif