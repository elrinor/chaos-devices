#ifndef __PARSER_H__
#define __PARSER_H__

#include <boost/shared_ptr.hpp>
#include "Context.h"

class Parser {
public:
    static QVariant exec(Context& c, QString expr);
};

#endif