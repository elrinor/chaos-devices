#include <QRegExp>
#include "Parser.h"

class ParserImpl {
private:
    Context& c;

    static const QRegExp identifier;
    static const QRegExp string;
    static const QRegExp number;

public:
    ParserImpl(Context& c): c(c) {
        return;
    }

    QVariant list(QString& expr) {
        if(expr[0] != '(')
            throw std::runtime_error("Expected \"(\" at: \"" + expr.toStdString() + "\"");
        expr = expr.mid(1).trimmed();
        
        QList<QVariant> result;

        while(true) {
            if(expr[0] == ')') {
                expr = expr.mid(1).trimmed();
                break;
            }
            result << root(expr);
            if(expr[0] == ',')
                expr = expr.mid(1).trimmed();
            else if(expr[0] == ')') {
                expr = expr.mid(1).trimmed();
                break;
            } else 
                throw std::runtime_error("Expected \")\" or \",\" at: \"" + expr.toStdString() + "\"");
        }

        return result;
    }

    QVariant root(QString& expr) {
        QRegExp id(identifier);
        if(expr.startsWith('$')) {
            expr = expr.mid(1);
            if(id.indexIn(expr) == 0) {
                QString varName = id.cap();
                expr = expr.mid(id.matchedLength()).trimmed();
                if(expr.startsWith('=')) {
                    expr = expr.mid(1).trimmed();
                    return this->c.setVar(varName, root(expr));
                } else
                    return this->c.var(varName);
            } else 
                throw std::runtime_error("Required identifier at: \"" + expr.toStdString() + "\"");
        } else if(expr.startsWith("'")) {
            QRegExp str(string);
            if(str.indexIn(expr) == 0) {
                QString strVal = str.cap();
                expr = expr.mid(str.matchedLength()).trimmed();
                strVal = strVal.mid(1, strVal.length() - 2);
                strVal.replace("\\'", "'").replace("\\n", "\n");
                return strVal;
            } else
                throw std::runtime_error("Required string at: \"" + expr.toStdString() + "\"");
        } else if(expr[0].isDigit() || expr[0] == '-') {
            QRegExp num(number);
            if(num.indexIn(expr) == 0) {
                QString numVal = num.cap();
                expr = expr.mid(num.matchedLength()).trimmed();
                return numVal.toInt();
            } else
                throw std::runtime_error("Required number at: \"" + expr.toStdString() + "\"");
        } else if(expr[0] == '(') {
            return list(expr);
        } else if(id.indexIn(expr) == 0) {
            QString funcName = id.cap();
            expr = expr.mid(id.matchedLength()).trimmed();
            return c.call(funcName, list(expr).toList());
        } else
            throw std::runtime_error("Parse error at: \"" + expr.toStdString() + "\"");
    }
};

const QRegExp ParserImpl::identifier = QRegExp("^[a-zA-Z_][a-zA-Z_0-9]*");
const QRegExp ParserImpl::string = QRegExp("^'([^']|\\\\')*'");
const QRegExp ParserImpl::number = QRegExp("^[-]?[0-9]+");

QVariant Parser::exec(Context& c, QString expr) {
    ParserImpl p(c);
    return p.root(expr.trimmed());
}
