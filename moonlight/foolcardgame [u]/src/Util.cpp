#include "Util.h"

#ifdef _WIN32
#  include <windows.h>
#else
#  include <time.h>
#endif

std::vector<std::string> strExplode(std::string s, char c) {
  std::vector<std::string> v;
  while(true) {
    int p = -1;
    for(int i = 0; i < s.size(); i++)
      if(s[i] == c) {
        p = i;
        break;
      }
    if(p != -1) {
      v.push_back(s.substr(0, p));
      s = s.substr(p + 1);
    }
    else {
      v.push_back(s);
      break;
    }
  }
  return v;
}

unsigned int getTime()
{
#ifdef _WIN32
  return GetTickCount();
#else
  timespec tim;
  clock_gettime(CLOCK_REALTIME,&tim);
  return tim.tv_sec*1000+tim.tv_nsec/1000000;
#endif
}


