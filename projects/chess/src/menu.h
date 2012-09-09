#ifndef _MENU
#define _MENU

#include <string>

extern std::string Login;
extern std::string CurOpponent;

void menu_cycle();
void DrawTitle();
void SingleMenu(int x, int y, std::string s);

#endif
