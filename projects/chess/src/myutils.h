#ifndef _MYUTILS
#define _MYUTILS

#include <string>

unsigned int GetTime();

#ifndef max
#define max(a,b) (((a)>(b))?(a):(b))
#endif

std::string IntToStr(int i);

#endif
