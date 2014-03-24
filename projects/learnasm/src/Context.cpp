#include "Context.h"
#include "Util.h"

class ContextHelpers {
public:
    static QVariant if_(QList<QVariant> args) {
        throwIfNotSize("if", args, 3); /* if(cond, then, else) */
        return args[0].toBool() ? (args[1]) : (args[2]);
    }

    static QVariant not(QList<QVariant> args) {
        throwIfNotSize("not", args, 1); /* not(a) */
        return !args[0].toBool();
    }

    static QVariant and(QList<QVariant> args) {
        throwIfSizeLessThan("and", args, 1); /* and(a, ...) */
        bool result = true;
        for(int i = 0; i < args.size(); i++)
            result = result && args[i].toBool();
        return result;
    }

    static QVariant or(QList<QVariant> args) {
        throwIfSizeLessThan("or", args, 1); /* or(a, ...) */
        bool result = false;
        for(int i = 0; i < args.size(); i++)
            result = result || args[i].toBool();
        return result;
    }

    static QVariant toHex(QList<QVariant> args) {
        throwIfNotSize("toHex", args, 2); /* toHex(num, len) */
        int len = args[1].toInt();
        QString result = QString::number(args[0].toInt(), 16).right(len);
        while(result.size() < len)
            result = "0" + result;
        result += "h";
        if(!result[0].isDigit())
            result = "0" + result;
        return result;
    }

    static QVariant fromHex(QList<QVariant> args) {
        throwIfNotSize("fromHex", args, 2); /* fromHex(str, len) */
        int size = args[1].toInt();
        QString s = args[0].toString();
        if(!s.toLower().endsWith("h"))
            throw std::runtime_error("Bad hex number format: \"" + s.toStdString() + "\"");
        s = s.mid(0, s.size() - 1);
        long long val = s.toLongLong(0, 16);
        if(val & ((long long) 1 << (size * 4 - 1)))
            val -= ((long long) 1 << (size * 4));
        return (int) val; 
    }

    static QVariant toLower(QList<QVariant> args) {
        throwIfNotSize("toLower", args, 1); /* toLower(str) */
        return args[0].toString().toLower();
    }

    static QVariant toUpper(QList<QVariant> args) {
        throwIfNotSize("toUpper", args, 1); /* toUpper(str) */
        return args[0].toString().toUpper();
    }

    static QVariant randomNumber(QList<QVariant> args) {
        throwIfNotSize("random", args, 2); /* random(min, max) */
        return random(args[0].toInt(), args[1].toInt());
    }

    static QVariant randomSelect(QList<QVariant> args) {
        throwIfSizeLessThan("randomSelect", args, 1); /* randomSelect(a, ...) */
        return args[random(0, args.size() - 1)];
    }

    static QVariant cat(QList<QVariant> args) {
        throwIfSizeLessThan("cat", args, 1); /* cat(s, ...) */
        QString result = args[0].toString();
        for(int i = 1; i < args.size(); i++)
            result += args[i].toString();
        return result;
    }

    static QVariant substr(QList<QVariant> args) {
        throwIfNotSize("substr", args, 3); /* substr(s, pos, n) */
        return args[0].toString().mid(qMax(args[1].toInt(), 0), args[2].toInt());
    }

    static QVariant length(QList<QVariant> args) {
        throwIfNotSize("length", args, 1); /* length(s) */
        return args[0].toString().size();
    }

    static QVariant add(QList<QVariant> args) {
        throwIfSizeLessThan("add", args, 1); /* add(a, ...) */
        int sum = 0;
        for(int i = 0; i < args.size(); i++)
            sum += args[i].toInt();
        return sum;
    }

    static QVariant mul(QList<QVariant> args) {
        throwIfSizeLessThan("mul", args, 1); /* mul(a, ...) */
        int prod = 1;
        for(int i = 0; i < args.size(); i++)
            prod *= args[i].toInt();
        return prod;
    }

    static QVariant sub(QList<QVariant> args) {
        throwIfNotSize("sub", args, 2); /* sub(a, b) */
        return args[0].toInt() - args[1].toInt();
    }

    static QVariant div(QList<QVariant> args) {
        throwIfNotSize("div", args, 2); /* div(a, b) */
        return args[0].toInt() / args[1].toInt();
    }

    static QVariant mod(QList<QVariant> args) {
        throwIfNotSize("mod", args, 2); /* mod(a, b) */
        return args[0].toInt() % args[1].toInt();
    }

    static QVariant eq(QList<QVariant> args) {
        throwIfNotSize("eq", args, 2); /* eq(a, b) */
        QString s;
        s = args[0].toString();
        s = args[1].toString();
        return args[0] == args[1];
    }

    static QVariant ne(QList<QVariant> args) {
        throwIfNotSize("ne", args, 2); /* ne(a, b) */
        return args[0] != args[1];
    }

    static QVariant lt(QList<QVariant> args) {
        throwIfNotSize("lt", args, 2); /* lt(a, b) */
        return args[0].toInt() < args[1].toInt();
    }

    static QVariant le(QList<QVariant> args) {
        throwIfNotSize("le", args, 2); /* le(a, b) */
        return args[0].toInt() <= args[1].toInt();
    }

    static QVariant gt(QList<QVariant> args) {
        throwIfNotSize("gt", args, 2); /* gt(a, b) */
        return args[0].toInt() > args[1].toInt();
    }

    static QVariant ge(QList<QVariant> args) {
        throwIfNotSize("ge", args, 2); /* ge(a, b) */
        return args[0].toInt() >= args[1].toInt();
    }
};

void Context::init() {
    this->funcs["if"] = ContextHelpers::if_;
    this->funcs["or"] = ContextHelpers::or;
    this->funcs["and"] = ContextHelpers::and;
    this->funcs["not"] = ContextHelpers::not;
    this->funcs["toHex"] = ContextHelpers::toHex;
    this->funcs["fromHex"] = ContextHelpers::fromHex;
    this->funcs["toUpper"] = ContextHelpers::toUpper;
    this->funcs["toLower"] = ContextHelpers::toLower;
    this->funcs["random"] = ContextHelpers::randomNumber;
    this->funcs["randomSelect"] = ContextHelpers::randomSelect;
    this->funcs["cat"] = ContextHelpers::cat;
    this->funcs["substr"] = ContextHelpers::substr;
    this->funcs["length"] = ContextHelpers::length;

    this->funcs["add"] = ContextHelpers::add;
    this->funcs["sub"] = ContextHelpers::sub;
    this->funcs["mod"] = ContextHelpers::mod;
    this->funcs["div"] = ContextHelpers::div;
    this->funcs["mul"] = ContextHelpers::mul;

    this->funcs["eq"] = ContextHelpers::eq;
    this->funcs["lt"] = ContextHelpers::lt;
    this->funcs["gt"] = ContextHelpers::gt;
    this->funcs["le"] = ContextHelpers::le;
    this->funcs["ge"] = ContextHelpers::ge;
    this->funcs["ne"] = ContextHelpers::ne;
}


Context::Context(QVBoxLayout* vl, QHBoxLayout* hl): myhl(hl), myvl(vl) {
    init();
}

Context::Context(): myhl(NULL), myvl(NULL) {
    init();
}