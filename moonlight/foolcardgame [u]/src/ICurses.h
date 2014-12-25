#ifndef __ICURSES_H__
#define __ICURSES_H__

#define NOMACROS
#ifdef _WIN32
#  include "curses/curses.h"
#  pragma comment(lib, "curses/pdcurses.lib")
#else
#  include <curses.h>
#endif
#undef NOMACROS

#undef getch
#undef clear

namespace Scr {
  void refresh() {
    wrefresh(stdscr);
  }

  int getch() {
    return wgetch(stdscr);
  }

  void clear() {
    wclear(stdscr);
  }
  
  int color(int n) {
    return COLOR_PAIR(n);
  }

  void initColor(int n, int fgColor, int bgColor) {
    init_pair(n, fgColor, bgColor);
  }

  void init() {
    initscr();
    start_color();
    noecho();
    keypad(stdscr, TRUE);
  }

  void close()
  {
    endwin();
  }

  void put(int x, int y, unsigned int colorN, std::string s)
  {
    chtype c[1024];
    int i, l = s.size();
    for(i = 0; i < l; i++)
      c[i] = s[i] | color(colorN);
    c[i] = 0;
    mvaddchstr(y, x, c);
  }

  void setDelayTime(int millisec)
  {
    timeout(millisec);
  }
};

#endif

