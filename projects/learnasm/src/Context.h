#ifndef __CONTEXT_H__
#define __CONTEXT_H__

#include <QBoxLayout>
#include <QString>
#include <QHash>
#include <QVariant>

typedef QVariant (*ScriptFunction)(QList<QVariant>);

class ValueAccessor {
public:
    virtual QVariant getValue() const = 0;
};

class Context {
private:
    QHash<QString, ValueAccessor*> varAccessors;
    QHash<QString, QVariant> vars;
    QHash<QString, ScriptFunction> funcs;
    QVBoxLayout* myvl;
    QHBoxLayout* myhl;

    void init();

public:
    Context();

    Context(QVBoxLayout* vl, QHBoxLayout* hl);

    QVariant var(QString name) const {
        if(name.startsWith('$'))
            name.remove(0, 1);
        if(this->varAccessors.contains(name))
            return this->varAccessors.value(name)->getValue();
        else if(this->vars.contains(name))
            return this->vars.value(name);
        else 
            throw std::runtime_error("Variable \"" + name.toStdString() + "\" not found.");
    }

    QVariant call(QString name, QList<QVariant> args) const {
        if(this->funcs.contains(name))
            return this->funcs.value(name)(args);
        else
            throw std::runtime_error("Function \"" + name.toStdString() + "\" not found.");
    }

    QVariant setVar(QString name, QVariant value) {
        QString s = name + "=" + value.toString();
        return this->vars[name] = value;
    }

    void setAccessor(QString name, ValueAccessor* accessor) {
        this->varAccessors[name] = accessor;
    }

    QVBoxLayout* vl() const {
        return this->myvl;
    }

    QHBoxLayout* hl() const {
        return this->myhl;
    }

    void setVl(QVBoxLayout* vl) {
        this->myvl = vl;
    }

    void setHl(QHBoxLayout* hl) {
        this->myhl = hl;
    }
};


#endif
