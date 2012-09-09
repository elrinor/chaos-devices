#include "mycurses.h"

#ifdef _WIN32
  #include "curses/curses.h"
  #pragma comment(lib, "curses/pdcurses.lib")
#else
  #include <curses.h>
#endif

#include <string>

void crRefresh() 
{
  refresh();
}

int crReadKey() 
{
  return getch();
}

void crClrScr()
{
  clear();
}

unsigned int crColor(int n)
{ 
  return COLOR_PAIR(n);
}

void crInitColor(int n,int fgColor, int bgColor)
{
  init_pair(n,fgColor,bgColor);
}

void crInit()
{
  initscr();
  start_color();
  noecho();
  keypad(stdscr,TRUE);
}

void crClose()
{
  endwin();
}

void crPut(int x, int y, unsigned int color, std::string s)
{
  chtype c[BUFFER_SIZE];
  int i, l=s.size();
  for(i=0;i<l;i++)
    c[i]=s[i] | color;
  c[i]=0;
  mvaddchstr(y,x,c);
}

void crShowCursor(bool show)
{
  if(show)
    curs_set(1);
  else
    curs_set(0);
}

std::string crReadStr(int x, int y, unsigned int color)
{
  char c[BUFFER_SIZE];
  int prevcurs=curs_set(1);
  echo();
  attron(color);
  mvgetnstr(y,x,c,BUFFER_SIZE-1);
  attroff(color);
  noecho();
  curs_set(prevcurs);
  return (std::string)c;
}

int crReadInt(int x, int y, unsigned int color)
{
  int result;
  int prevcurs=curs_set(1);
  echo();
  attron(color);
  mvscanw(y,x,"%d",&result);
  attroff(color);
  noecho();
  curs_set(prevcurs);
  return result;
} 

std::string crReadPsw(int x, int y)
{
  char c[BUFFER_SIZE];
  int prevcurs=curs_set(1);
  mvgetnstr(y,x,c,BUFFER_SIZE-1);
  curs_set(prevcurs);
  return (std::string)c;
}

void crGotoXY(int x, int y)
{
  move(y,x);
}

void crDelayTime(int millisec)
{
  timeout(millisec);
}
