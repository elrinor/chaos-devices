#ifndef __CHOOSETESTDIALOG_H__
#define __CHOOSETESTDIALOG_H__

#include <QDialog>
#include "Template.h"

class QComboBox;

class ChooseTestDialog: public QDialog {
    Q_OBJECT
public:
    ChooseTestDialog(TemplateList templates, QWidget* parent = 0);

    int currentIndex() const;

protected:

private:
    QComboBox* comboBox;
};

#endif