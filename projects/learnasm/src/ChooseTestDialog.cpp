#include <QtGui>
#include "ChooseTestDialog.h"

ChooseTestDialog::ChooseTestDialog(TemplateList templates, QWidget* parent) {
    QVBoxLayout* mainLayout = new QVBoxLayout();

    comboBox = new QComboBox();
    for(int i = 0; i < templates.getTestList().size(); i++)     
        comboBox->addItem(templates.getTestList()[i].name());
    mainLayout->addWidget(comboBox);

    QDialogButtonBox* buttonBox = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel, Qt::Horizontal);
    connect(buttonBox, SIGNAL(accepted()), this, SLOT(accept()));
    connect(buttonBox, SIGNAL(rejected()), this, SLOT(reject())); 
    mainLayout->addWidget(buttonBox);

    setLayout(mainLayout);
}

int ChooseTestDialog::currentIndex() const {
    return this->comboBox->currentIndex();
}