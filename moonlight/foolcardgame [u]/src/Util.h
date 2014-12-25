#ifndef __UTIL_H__
#define __UTIL_H__

#include <vector>
#include <string>
#include <sstream>

std::vector<std::string> strExplode(std::string s, char c);
unsigned int getTime();

template<class T> std::string toString(T i)
{
  std::stringstream s;
  s << i;
  return s.str();
}


#endif
