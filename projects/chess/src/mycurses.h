#ifndef _MYCURSES
#define _MYCURSES

#include <string>

#ifdef _WIN32
  #define COLOR_BLACK	0
  #define COLOR_NAVY 1
  #define COLOR_GREEN 2
  #define COLOR_TEAL 3
  #define COLOR_MAROON 4
  #define COLOR_PURPLE 5
  #define COLOR_OLIVE 6
  #define COLOR_GRAY 7
  #define COLOR_SILVER 8
  #define COLOR_BLUE 9
  #define COLOR_LIME 10
  #define COLOR_AQUA 11
  #define COLOR_RED 12
  #define COLOR_FUCHSIA 13
  #define COLOR_YELLOW 14
  #define COLOR_WHITE	15
#else
  #define COLOR_BLACK	0
  #define COLOR_NAVY 1
  #define COLOR_GREEN 2
  #define COLOR_TEAL 3
  #define COLOR_MAROON 4
  #define COLOR_PURPLE 5
  #define COLOR_OLIVE 6
  #define COLOR_GRAY 0
  #define COLOR_SILVER 7
  #define COLOR_BLUE 1
  #define COLOR_LIME 2
  #define COLOR_AQUA 3
  #define COLOR_RED 4
  #define COLOR_FUCHSIA 5
  #define COLOR_YELLOW 6
  #define COLOR_WHITE	7    
#endif

#define KEY_DOWN	0x102
#define KEY_UP		0x103
#define KEY_LEFT	0x104	
#define KEY_RIGHT	0x105	
#define KEY_ESC 27
#define KEY_ENTER 10
#define BUFFER_SIZE 512

#define	ERR	(-1)
#define OK 0

void crRefresh();
int crReadKey();
void crClrScr();
unsigned int crColor(int n);
void crInitColor(int n,int fgColor, int bgColor);
void crInit();
void crClose();
void crPut(int x, int y, unsigned int color, std::string s);
void crShowCursor(bool show);
std::string crReadStr(int x, int y, unsigned int color);
int crReadInt(int x, int y, unsigned int color);
std::string crReadPsw(int x, int y);
void crGotoXY(int x, int y);
void crDelayTime(int millisec);

#endif
