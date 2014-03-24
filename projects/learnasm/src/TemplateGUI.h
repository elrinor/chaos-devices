#ifndef __TEMPLATEGUI_H__
#define __TEMPLATEGUI_H__

#include <QtGui>
#include "Context.h"
#include "Util.h"

class TemplateSpinBox: public QSpinBox, public ValueAccessor {
    Q_OBJECT;
public:
    virtual QVariant getValue() const {
        return this->value();
    }
};

class TemplateLineEdit: public QLineEdit, public ValueAccessor {
    Q_OBJECT;
public:
    virtual QVariant getValue() const {
        return this->text();
    }
};

class TemplateComboBox: public QComboBox, public ValueAccessor {
    Q_OBJECT;
public:
    virtual QVariant getValue() const {
        return this->currentText();
    }
};

class TemplateLabel: public QLabel, public ValueAccessor {
    Q_OBJECT;
public:
    virtual QVariant getValue() const {
        return this->text();
    }
};

class TemplateGUIFactory {
public:
    class GUIElement: public QPair<QWidget*, ValueAccessor*> {
    public:
        template<class T> GUIElement(T arg): QPair(arg, arg) {}
        QWidget* widget() const { return this->first; }
        ValueAccessor* valueAccessor() const { return this->second; }
    };

    static GUIElement createGUIElement(QString type, QList<QVariant> args) {
        if(type == "SpinBox") {
            throwIfNotSize("SpinBox", args, 2);
            TemplateSpinBox* result = new TemplateSpinBox();
            result->setRange(args[0].toInt(), args[1].toInt());
            return GUIElement(result);
        } else if(type == "ComboBox") {
            TemplateComboBox* result = new TemplateComboBox();
            result->setEditable(false);
            for(int i = 0; i < args.size(); i++)
                result->addItem(args[i].toString());
            return GUIElement(result);
        } else if(type == "LineEdit") {
            throwIfNotSize("LineEdit", args, 1);
            TemplateLineEdit* result = new TemplateLineEdit();
            result->setText(args[0].toString());
            return GUIElement(result);
        } else if(type == "Label") {
            throwIfNotSize("Label", args, 1);
            TemplateLabel* result = new TemplateLabel();
            result->setText(args[0].toString());
            //result->setWordWrap(true);
            return GUIElement(result);
        } else 
            throw std::runtime_error("Unknown GUI element: \"" + type.toStdString() + "\"");
    }

};



#endif