#ifndef __LOGGER_H__
#define __LOGGER_H__

#include <QString>

class Logger {
public:
  virtual void logMessage(QString msg) = 0;
};

#endif