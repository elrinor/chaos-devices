#ifndef __UTIL_H__
#define __UTIL_H__

#include <QString>
#include <QList>
#include <QVariant>

QString intToHex(int value, int size);

void throwIfNotSize(QString func, QList<QVariant> args, int size);

void throwIfSizeLessThan(QString func, QList<QVariant> args, int size);

int random(int min, int max);

#endif