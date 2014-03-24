#ifndef __TEMPLATE_H__
#define __TEMPLATE_H__

#include <QDomDocument>
#include <QHash>
#include <QBoxLayout>
#include <boost/shared_ptr.hpp>
#include <boost/array.hpp>
#include "Context.h"

class Template;
class TemplateList;

class TemplateElement {
public:
    enum Type {
        ELEM,
        EVAL,
        EXEC,
        COND,
        SPLIT
    };

private:
    struct TemplateElementData;
    boost::shared_ptr<TemplateElementData> data;

public:
    TemplateElement(QDomElement root, const Template& parent);

    void exec(Context& c) const;
};

class TemplateBlock {
private:
    struct TemplateBlockData;
    boost::shared_ptr<TemplateBlockData> data;

public:
    TemplateBlock() {}

    TemplateBlock(QDomElement root, const Template& parent);

    void exec(Context& c) const;
};

class Template {
private:
    struct TemplateData;
    boost::shared_ptr<TemplateData> data;

public:
    Template() {};

    Template(QDomElement root);

    QLayout* execBlock(QString blockName, Context& c, bool createLayout = true) const;

    QString name() const;

    QString id() const;
};

class Test {
private:
    struct TestData;
    boost::shared_ptr<TestData> data;

public:
    Test() {};

    Test(QDomElement root, const TemplateList& parent);

    QString name() const;

    QList<Template> testTemplates() const;

    const boost::array<float, 5>& markCriteria() const;

    bool showMistakes() const;
};

class TemplateList {
    struct TemplateListData;
    boost::shared_ptr<TemplateListData> data;

public:
    TemplateList() {};

    const QList<Template>& getTemplateList();

    Template getTemplate(QString id);

    const QList<Test>& getTestList();

    TemplateList(QString fileName);
};

#endif