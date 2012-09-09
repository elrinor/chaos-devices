#ifndef __UTIL_H__
#define __UTIL_H__
#include "config.h"
#include <QString>
#include <exception>
#include <cstdlib>
#include <QHash>
#include "arx/Collections.h"

/** @returns random number in [0..max) */
inline int random(int max) {
	return (int)((((long long) max) * rand()) / (RAND_MAX + 1));
}

namespace detail {
	std::string encode(const std::string s);
	std::string decode(const std::string s);
}

template<class T> 
class ArrayListCreator {
private:
  arx::ArrayList<T> arrayList;
public:
  ArrayListCreator() {}
  ArrayListCreator operator() (T value) {
    arrayList.push_back(value);
    return *this;
  }
  operator arx::ArrayList<T>() {
    return this->arrayList;
  }
};

template<class T> ArrayListCreator<T> arrayList(T value) {
  return ArrayListCreator<T>()(value);
}

template<class T> ArrayListCreator<T> arrayList() {
  return ArrayListCreator<T>();
}

template<class S, class T>
void deserialize(S& stream, T*& that) {
  that = T::deserialize(stream);
}

template<class S, class T>
void deserialize(S& stream, T& that) {
  that = T::deserialize(stream);
}

template<class S, class T>
void serialize(S& stream, const T that) {
  that.serialize(stream);
  stream << " ";
}

template<class S, class T>
void serialize(S& stream, T* const that) {
  that->serialize(stream);
  stream << " ";
}

template<class S, class T> 
void deserialize(S& stream, arx::ArrayList<T>& that) {
  int size;
  stream >> size;
  for(int i = 0; i < size; i++) {
    T elem;
    deserialize(stream, elem);
    that.push_back(elem);
  }
}

template<class S, class T> 
void serialize(S& stream, const arx::ArrayList<T>& that) {
  serialize(stream, that.size());
  for(unsigned int i = 0; i < that.size(); i++)
    serialize(stream, that[i]);
}

template<class S, class KeyT, class MappedT> 
void deserialize(S& stream, QHash<KeyT, MappedT>& that) {
  int size;
  deserialize(stream, size);
  for(int i = 0; i < size; i++) {
    KeyT key;
    MappedT elem;
    deserialize(stream, key, elem);
    that.insert(key, elem);
  }
}

template<class S, class KeyT, class MappedT> 
void serialize(S& stream, const QHash<KeyT, MappedT*>& that) {
  serialize(stream, that.size());
  for(QHash<KeyT, MappedT*>::const_iterator i = that.begin(); i != that.end(); i++)
    serialize(stream, i.key(), *i);
}


template<class S, class T1, class T2>
void serialize(S& stream, const T1 v1, const T2 v2) {
  serialize(stream, v1);
  serialize(stream, v2);
}

template<class S, class T1, class T2, class T3>
void serialize(S& stream, const T1 v1, const T2 v2, const T3 v3) {
  serialize(stream, v1, v2);
  serialize(stream, v3);
}

template<class S, class T1, class T2, class T3, class T4>
void serialize(S& stream, const T1 v1, const T2 v2, const T3 v3, const T4 v4) {
  serialize(stream, v1, v2, v3);
  serialize(stream, v4);
}

template<class S, class T1, class T2, class T3, class T4, class T5>
void serialize(S& stream, const T1 v1, const T2 v2, const T3 v3, const T4 v4, const T5 v5) {
  serialize(stream, v1, v2, v3, v4);
  serialize(stream, v5);
}

template<class S, class T1, class T2, class T3, class T4, class T5, class T6>
void serialize(S& stream, const T1 v1, const T2 v2, const T3 v3, const T4 v4, const T5 v5, const T6 v6) {
  serialize(stream, v1, v2, v3, v4, v5);
  serialize(stream, v6);
}

template<class S, class T1, class T2, class T3, class T4, class T5, class T6, class T7>
void serialize(S& stream, const T1 v1, const T2 v2, const T3 v3, const T4 v4, const T5 v5, const T6 v6, const T7 v7) {
  serialize(stream, v1, v2, v3, v4, v5, v6);
  serialize(stream, v7);
}

template<class S, class T1, class T2, class T3, class T4, class T5, class T6, class T7, class T8>
void serialize(S& stream, const T1 v1, const T2 v2, const T3 v3, const T4 v4, const T5 v5, const T6 v6, const T7 v7, const T8 v8) {
  serialize(stream, v1, v2, v3, v4, v5, v6, v7);
  serialize(stream, v8);
}

template<class S, class T1, class T2, class T3, class T4, class T5, class T6, class T7, class T8, class T9>
void serialize(S& stream, const T1 v1, const T2 v2, const T3 v3, const T4 v4, const T5 v5, const T6 v6, const T7 v7, const T8 v8, const T9 v9) {
  serialize(stream, v1, v2, v3, v4, v5, v6, v7, v8);
  serialize(stream, v9);
}

template<class S, class T1, class T2, class T3, class T4, class T5, class T6, class T7, class T8, class T9, class T10>
void serialize(S& stream, const T1 v1, const T2 v2, const T3 v3, const T4 v4, const T5 v5, const T6 v6, const T7 v7, const T8 v8, const T9 v9, const T10 v10) {
  serialize(stream, v1, v2, v3, v4, v5, v6, v7, v8, v9);
  serialize(stream, v10);
}


template<class S, class T1, class T2>
void deserialize(S& stream, T1& v1, T2& v2) {
  deserialize(stream, v1);
  deserialize(stream, v2);
}

template<class S, class T1, class T2, class T3>
void deserialize(S& stream, T1& v1, T2& v2, T3& v3) {
  deserialize(stream, v1, v2);
  deserialize(stream, v3);
}

template<class S, class T1, class T2, class T3, class T4>
void deserialize(S& stream, T1& v1, T2& v2, T3& v3, T4& v4) {
  deserialize(stream, v1, v2, v3);
  deserialize(stream, v4);
}

template<class S, class T1, class T2, class T3, class T4, class T5>
void deserialize(S& stream, T1& v1, T2& v2, T3& v3, T4& v4, T5& v5) {
  deserialize(stream, v1, v2, v3, v4);
  deserialize(stream, v5);
}

template<class S, class T1, class T2, class T3, class T4, class T5, class T6>
void deserialize(S& stream, T1& v1, T2& v2, T3& v3, T4& v4, T5& v5, T6& v6) {
  deserialize(stream, v1, v2, v3, v4, v5);
  deserialize(stream, v6);
}

template<class S, class T1, class T2, class T3, class T4, class T5, class T6, class T7>
void deserialize(S& stream, T1& v1, T2& v2, T3& v3, T4& v4, T5& v5, T6& v6, T7& v7) {
  deserialize(stream, v1, v2, v3, v4, v5, v6);
  deserialize(stream, v7);
}

template<class S, class T1, class T2, class T3, class T4, class T5, class T6, class T7, class T8>
void deserialize(S& stream, T1& v1, T2& v2, T3& v3, T4& v4, T5& v5, T6& v6, T7& v7, T8& v8) {
  deserialize(stream, v1, v2, v3, v4, v5, v6, v7);
  deserialize(stream, v8);
}

template<class S, class T1, class T2, class T3, class T4, class T5, class T6, class T7, class T8, class T9>
void deserialize(S& stream, T1& v1, T2& v2, T3& v3, T4& v4, T5& v5, T6& v6, T7& v7, T8& v8, T9& v9) {
  deserialize(stream, v1, v2, v3, v4, v5, v6, v7, v8);
  deserialize(stream, v9);
}

template<class S, class T1, class T2, class T3, class T4, class T5, class T6, class T7, class T8, class T9, class T10>
void deserialize(S& stream, T1& v1, T2& v2, T3& v3, T4& v4, T5& v5, T6& v6, T7& v7, T8& v8, T9& v9, T10& v10) {
  deserialize(stream, v1, v2, v3, v4, v5, v6, v7, v8, v9);
  deserialize(stream, v10);
}


#define STREAM_DESERIALIZATION(T) template<class S> void deserialize(S& stream, T& that) {stream >> that;}
#define STREAM_SERIALIZATION(T) template<class S> void serialize(S& stream, const T that) {stream << that << " ";}
#define FOREACH_SIMPLE_TYPE(MACRO) \
	MACRO(bool) \
  MACRO(char) \
  MACRO(short) \
  MACRO(int) \
  MACRO(long) \
  MACRO(long long) \
  MACRO(unsigned char) \
  MACRO(unsigned short) \
  MACRO(unsigned int) \
  MACRO(unsigned long) \
  MACRO(unsigned long long) \
  MACRO(float) \
  MACRO(double) \
  MACRO(long double)
FOREACH_SIMPLE_TYPE(STREAM_DESERIALIZATION)
FOREACH_SIMPLE_TYPE(STREAM_SERIALIZATION)


template<class S> void deserialize(S& stream, std::string& that) {
	stream >> that;
	that = detail::decode(that);
}

template<class S> void serialize(S& stream, const std::string that) {
	stream << detail::encode(that) << " ";
}

template<class S> void serialize(S& stream, const char* that) {
	stream << detail::encode(that) << " ";
}

template<class S> void serialize(S& stream, const QString that) {
  serialize(stream, that.size());
  const QChar* p = that.data();
  for(int i = 0; i < that.size(); i++)
    serialize(stream, p[i].unicode());
}

template<class S> void deserialize(S& stream, QString& that) {
  int size;
  deserialize(stream, size);
  that = QString(size, QChar(' '));
  QChar* p = that.data();
  for(int i = 0; i < size; i++) {
    ushort unicode; 
    deserialize(stream, unicode);
    p[i] = QChar(unicode);
  }
}

template<class S> void deserialize(S& stream, std::wstring& that) {
  QString s;
  deserialize(stream, s);
  that = s.toStdWString();
}

template<class S> void serialize(S& stream, const std::wstring that) {
  serialize(stream, QString::fromStdString(that));
}

template<class S> void serialize(S& stream, const wchar_t* that) {
  serialize(stream, QString::fromWCharArray(that));
}



#endif