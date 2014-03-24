#include "Util.h"

QString charToHex(unsigned char c) {
    static const char* hex = "0123456789ABCDEF";
    return QString() + hex[c >> 4] + hex[c & 0xF];
}

QString intToHex(int value, int size) {
    QString result;
    for(int i = 0; i < size; i++)
        result = charToHex(((unsigned char*)&value)[i]) + result;
    if(!result[0].isDigit())
        result = "0" + result;
    return result + "h";
}

void throwIfNotSize(QString func, QList<QVariant> args, int size) {
    if(args.size() != size)
        throw std::runtime_error(func.toStdString() + ": expected " + QString::number(size).toStdString() + " argument(s), got " + QString::number(args.size()).toStdString());
}

void throwIfSizeLessThan(QString func, QList<QVariant> args, int size) {
    if(args.size() < size)
        throw std::runtime_error(func.toStdString() + ": expected " + QString::number(size).toStdString() + " or more argument(s), got " + QString::number(args.size()).toStdString());
}

int random(int min, int max) {
    return (int)(min + ((long long) max - min + 1) * ((long long) rand() * RAND_MAX + rand()) / ((long long) RAND_MAX * RAND_MAX + 1));
}

