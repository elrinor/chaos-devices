#include "Template.h"
#include "Parser.h"
#include "TemplateGUI.h"
#include <QFile>
#include <exception>

using namespace std;

// -------------------------------------------------------------------------- //
// TemplateElement
// -------------------------------------------------------------------------- //
struct TemplateElement::TemplateElementData {
    Type myType;
    QHash<QString, QString> attrs;
    Template parent;
    TemplateBlock block;

    bool hasAttr(QString name) const {
        return this->attrs.contains(name);
    }

    QString attr(QString name) const {
        if(hasAttr(name))
            return this->attrs.value(name);
        else
            throw std::runtime_error("Attribute \"" + name.toStdString() + "\" does not exists");
    }
};

TemplateElement::TemplateElement(QDomElement root, const Template& parent): data(new TemplateElementData()) {
    this->data->parent = parent;

    if(root.nodeName() == "elem")
        this->data->myType = ELEM;
    else if(root.nodeName() == "eval")
        this->data->myType = EVAL;
    else if(root.nodeName() == "exec")
        this->data->myType = EXEC;
    else if(root.nodeName() == "split")
        this->data->myType = SPLIT;
    else if(root.nodeName() == "cond")
        this->data->myType = COND;
    else
        throw std::runtime_error("Unknown element name: \"" + root.nodeName().toStdString() + "\"");
    QDomNamedNodeMap map = root.attributes();
    for(int i = 0; i < map.size(); i++)
        this->data->attrs[map.item(i).toAttr().name()] = map.item(i).toAttr().value();
    if(this->data->myType == COND)
        this->data->block = TemplateBlock(root, parent);
}

void TemplateElement::exec(Context& c) const {
    switch(this->data->myType) {
        case EXEC:
            this->data->parent.execBlock(this->data->attr("value"), c, false);
            break;
        case SPLIT:
            c.hl()->addStretch(1);
            c.setHl(new QHBoxLayout());
            c.vl()->addLayout(c.hl());
            break;
        case EVAL:
            Parser::exec(c, this->data->attr("value"));
            break;
        case COND:
            if(Parser::exec(c, this->data->attr("value")).toBool())
                this->data->block.exec(c);
            break;
        case ELEM:
            TemplateGUIFactory::GUIElement e = TemplateGUIFactory::createGUIElement(this->data->attr("type"), Parser::exec(c, this->data->attr("value")).toList());
            if(this->data->hasAttr("name"))
                c.setAccessor(this->data->attr("name"), e.valueAccessor());
            c.hl()->addWidget(e.widget());
            break;
    }
}

// -------------------------------------------------------------------------- //
// TemplateBlock
// -------------------------------------------------------------------------- //
struct TemplateBlock::TemplateBlockData {
    QList<TemplateElement> elems;
    Template parent;
};

TemplateBlock::TemplateBlock(QDomElement root, const Template& parent): data(new TemplateBlockData()) {
    this->data->parent = parent;
    QDomNodeList list = root.childNodes();
    for(int i = 0; i < list.size(); i++) {
        if(list.at(i).isElement())
            this->data->elems += TemplateElement(list.at(i).toElement(), parent);
    }
}

void TemplateBlock::exec(Context& c) const {
    for(int i = 0; i < this->data->elems.size(); i++)
        this->data->elems[i].exec(c);
}


// -------------------------------------------------------------------------- //
// Template
// -------------------------------------------------------------------------- //
struct Template::TemplateData {
    QHash<QString, TemplateBlock> blocks;
    QString name;
    QString id;
};

Template::Template(QDomElement root): data(new TemplateData()) {
    if(root.nodeName() != "template")
        throw std::runtime_error("Expected \"template\" XML element, got \"" + root.nodeName().toStdString() + "\"");
    if(!root.hasAttribute("name"))
        throw std::runtime_error("Template must have a \"name\" attribute");
    if(!root.hasAttribute("id"))
        throw std::runtime_error("Template must have an \"id\" attribute");
    this->data->name = root.attribute("name");
    this->data->id = root.attribute("id");
    QDomNodeList list = root.childNodes();
    for(int i = 0; i < list.size(); i++)
        if(list.at(i).isElement())
            this->data->blocks[list.at(i).nodeName()] = TemplateBlock(list.at(i).toElement(), *this);
}

QLayout* Template::execBlock(QString blockName, Context& c, bool createLayout) const {
    if(!this->data->blocks.contains(blockName))
        throw std::runtime_error("Block \"" + blockName.toStdString() + "\" does not exist");

    if(createLayout) {
        c.setVl(new QVBoxLayout());
        c.setHl(new QHBoxLayout());
        c.vl()->addLayout(c.hl());
    }

    this->data->blocks.value(blockName).exec(c);

    if(createLayout) {
        c.hl()->addStretch(1);
        c.vl()->addStretch(1);
    }
    return c.vl();
}

QString Template::name() const {
    return this->data->name; 
}

QString Template::id() const {
    return this->data->id;
}


// -------------------------------------------------------------------------- //
// 
// -------------------------------------------------------------------------- //
struct Test::TestData {
    TemplateList parent;
    QString name;
    QList<QString> testIds;
    boost::array<float, 5> criteria;
    bool showMistakes;
};

Test::Test(QDomElement root, const TemplateList& parent): data(new TestData()) {
    this->data->parent = parent;

    if(root.nodeName() != "test")
        throw std::runtime_error("Expected \"test\" XML element, got \"" + root.nodeName().toStdString() + "\"");
    if(!root.hasAttribute("name"))
        throw std::runtime_error("Template must have a \"name\" attribute");
    if(!root.hasAttribute("showMistakes"))
        throw std::runtime_error("Template must have a \"showMistakes\" attribute");
    if(!root.hasAttribute("criteria"))
        throw std::runtime_error("Template must have a \"criteria\" attribute");
    this->data->name = root.attribute("name");
    this->data->showMistakes = (bool) root.attribute("showMistakes").toInt();

    QList<QVariant> criteria = Parser::exec(Context(), root.attribute("criteria")).toList();
    if(criteria.size() != 5) 
        throw std::runtime_error("Criteria list must be of size 5");
    for(int i = 0; i < 5; i++)
        this->data->criteria[i] = criteria[i].toInt() / 100.0f;

    QDomNodeList list = root.childNodes();
    for(int i = 0; i < list.size(); i++) {
        if(list.at(i).isElement() && list.at(i).nodeName() == "quest") {
            QDomElement e = list.at(i).toElement();
            if(!e.hasAttribute("id"))
                throw std::runtime_error("Quest must have an \"id\" attribute");
            if(!e.hasAttribute("count"))
                throw std::runtime_error("Quest must have a \"count\" attribute");
            QString id = e.attribute("id");
            int count = e.attribute("count").toInt();
            for(int j = 0; j < count; j++)
                this->data->testIds += id;
        }
    }
}

QString Test::name() const {
    return this->data->name;
}

QList<Template> Test::testTemplates() const {
    QList<Template> result;
    for(int i = 0; i < this->data->testIds.size(); i++)
        result += this->data->parent.getTemplate(this->data->testIds[i]);
    return result;
}

const boost::array<float, 5>& Test::markCriteria() const {
    return this->data->criteria;
}

bool Test::showMistakes() const {
    return this->data->showMistakes;
}


// -------------------------------------------------------------------------- //
// TemplateList
// -------------------------------------------------------------------------- //
struct TemplateList::TemplateListData {
    QList<Template> templates;
    QHash<QString, Template> templatesById;
    QList<Test> tests;
};

TemplateList::TemplateList(QString fileName): data(new TemplateListData()) {
    QDomDocument doc;
    QFile* xmlFile = new QFile(fileName);
    doc.setContent(xmlFile);
    QDomNodeList list = doc.documentElement().childNodes();
    for(int i = 0; i < list.size(); i++) {
        if(list.at(i).isElement() && list.at(i).nodeName() == "template") {
            Template t = Template(list.at(i).toElement());
            this->data->templates += t;
            this->data->templatesById[t.id()] = t;
        }
        if(list.at(i).isElement() && list.at(i).nodeName() == "test") {
            Test t = Test(list.at(i).toElement(), *this);
            this->data->tests += t;
        }
    }
    delete xmlFile;
}

const QList<Template>& TemplateList::getTemplateList() {
    return this->data->templates;
}

Template TemplateList::getTemplate(QString id) {
    if(this->data->templatesById.contains(id))
        return this->data->templatesById.value(id);
    else
        throw runtime_error("Test with id=" + id.toStdString() + " does not exist");
}

const QList<Test>& TemplateList::getTestList() {
    return this->data->tests;
}