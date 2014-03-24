#include <QtGui>
#include "ChooseTopicDialog.h"

ChooseTopicDialog::ChooseTopicDialog(TemplateList templates, QWidget* parent) {
    QVBoxLayout* mainLayout = new QVBoxLayout();

    comboBox = new QComboBox();
    for(int i = 0; i < templates.getTemplateList().size(); i++)     
        comboBox->addItem(templates.getTemplateList()[i].name());
    mainLayout->addWidget(comboBox);

    QDialogButtonBox* buttonBox = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel, Qt::Horizontal);
    connect(buttonBox, SIGNAL(accepted()), this, SLOT(accept()));
    connect(buttonBox, SIGNAL(rejected()), this, SLOT(reject())); 
    mainLayout->addWidget(buttonBox);

    setLayout(mainLayout);
}

int ChooseTopicDialog::currentIndex() const {
    return this->comboBox->currentIndex();
}
