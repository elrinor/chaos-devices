#include <stdlib.h>
#include <string>
#include <sstream>
using namespace std;

#ifdef _WIN32
  #include <windows.h>
#else
  #include <time.h>
#endif

/// Returns time passed from some (unknown) moment in milliseconds (1/1000 sec)
unsigned int GetTime()
{
#ifdef _WIN32
  return GetTickCount();
#else
  timespec tim;
	clock_gettime(CLOCK_REALTIME,&tim);
  return tim.tv_sec*1000+tim.tv_nsec/1000000;
#endif
}

string IntToStr(int i)
{
#ifdef _WIN32
  char s[20];
  return itoa(i,s,10);
#else
  stringstream ss;
  ss<<i;
  return ss.str();
#endif
}

