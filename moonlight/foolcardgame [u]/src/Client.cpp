#include <iostream>
#include "Client.h"
#include "Socket.h"
#include "ICurses.h"
#include "Game.h"

using namespace std;

vector<string> vLog;
vector<string> sizes;
vector<Card> hand;
Table ctable;
int ctrumps;
int chosenCard = -1;
bool myTurn = false, imCovering = false;
string clientName;
string deckSize = "?";

void drawScreen() {
  Scr::clear();
  if(myTurn) {
    Scr::put(0, 0, 1, string("(a-") + (char)('a' + hand.size() - 1) + ") - make a turn");
  }
  if(imCovering) {
    Scr::put(0, 0, 1, string("(a-") + (char)('a' + hand.size() - 1) + "), (1-" + (char)('1' + ctable.cardCount - 1) + ") - cover a card,  (ESC) - take all cards");
  }
  Scr::put(0, 2, 1, "Hand:");
  for(int i = 0; i < hand.size(); i++) {
    Scr::put(0 + 10 * (i / 10), 3 + i % 10, 1, (char)((i > 25) ? ('A' + i - 25) : ('a' + i)) + string(") "));
    Scr::put(3 + 10 * (i / 10), 3 + i % 10, (i == chosenCard)? 3 : ((hand[i].suit == ctrumps) ? 2 : 1), hand[i].toString());
  }
  Scr::put(40, 2, 1, "Table:");
  for(int i = 0; i < 6; i++) {
    Scr::put(40, 3 + i, 1, (char)('1' + i) + string(") "));
    int color = 3;
    if(ctable.cardsDown[i].code != 0 && ctable.cardsUp[i].code != 0)
      color = 4;
    if(ctable.cardsDown[i].code != 0)
      Scr::put(43, 3 + i, color, ctable.cardsDown[i].toString());
    if(ctable.cardsUp[i].code != 0)
      Scr::put(47, 3 + i, color, ctable.cardsUp[i].toString());
  }
  Scr::put(52, 2, 1, "Trumps:");
  if(Card::suitDefaultMode)
    Scr::put(60, 2, 2, string("") + ((ctrumps != -1)?((char)(ctrumps + 3)):('?')));
  else {
    Card c;
    c.code = 0;
    c.suit = ctrumps;
    char cc = c.toString()[1];
    Scr::put(60, 2, 2, string("") + ((ctrumps != -1)?(cc):('?')));
  }
  Scr::put(52, 3, 1, "Name: " + clientName);
  Scr::put(52, 4, 1, "Deck: ");
  Scr::put(58, 4, (deckSize == "0") ? 3 : 2, deckSize);
  Scr::put(52, 6, 1, "Cards in hand:");
  for(int i = 0; i < sizes.size(); i++)
    Scr::put(52, 7 + i, 1, sizes[i]);
  Scr::put(0, 14, 1, "Log:");
  int pos = 15;
  for(int i = vLog.size() - 1; i >= 0 && pos <= 35; i--)
    Scr::put(0, pos++, (i == vLog.size() - 1) ? 2 : 1, vLog[i]);
  Scr::refresh();
}

void dbgMsg(string s) {/*
  Scr::put(10, 14, 1, s);
  Scr::refresh();
  Scr::getch();*/
}

int leaveGame() {
  //dbgMsg("Leave!");
  vLog.push_back("Server quit, press any key to exit");
  drawScreen();
  Scr::getch();
  return -1;
}


int clientLoop(Socket s) {
  drawScreen();
  Scr::init();
  Scr::initColor(1, COLOR_WHITE,  COLOR_BLACK);
  Scr::initColor(2, COLOR_YELLOW, COLOR_BLACK);
  Scr::initColor(3, COLOR_RED,    COLOR_BLACK);
  Scr::initColor(4, COLOR_GREEN,  COLOR_BLACK);
  Scr::clear();
  ctrumps = -1;

  do {
    string msg;
    //dbgMsg("tryRecv  ");
    while(s.tryRecvPacket(msg)) {
      //dbgMsg("received " + msg + "     ");
      char code = msg[0];
      msg = msg.substr(1);
      if(code == 'E') {
        vLog.push_back(msg);
      } else if(code == 'Q') {
        vLog.push_back(msg);
        return leaveGame();
      } else if(code == 'S') {
        sizes = strExplode(msg, ':');
      } else if(code == 'T') {
        ctable.fromString(msg);
        //dbgMsg("ctable successful    ");
      } else if(code == 'H') {
        hand = handFromString(msg);
        //dbgMsg("handFromString successful    ");
      } else if(code == 'K') {
        ctrumps = msg[0] - '0';
      } else if(code == 'D') {
        deckSize = msg;
      } else if(code == '!') {
        myTurn = true;
        imCovering = false;
        chosenCard = -1;
      } else if(code == '?') {
        imCovering = true;
        myTurn = false;
        chosenCard = -1;
      } else if(code == '#') {
        imCovering = false;
        myTurn = false;
        chosenCard = -1;
      }
    }

    //dbgMsg("no more packets    ");

    if(s.broken())
      return leaveGame();
    drawScreen();
    
    Scr::setDelayTime(100);
    int c = Scr::getch();
    Scr::setDelayTime(-1);

    if((myTurn || !imCovering) && ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))) {
      int index = c - 'a';
      if(index >= 0 && index < hand.size()) {
        s.sendPacket("P" + hand[index].toString());
        myTurn = false;
      }
    }
    if(imCovering) {
      if(c == 27) { // ESC
        s.sendPacket("T");
        chosenCard = -1;
      }
      if((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
        int index = c - 'a';
        if(index >= 0 && index < hand.size())
          chosenCard = index;
      }
      else if(c >= '1' && c <= '6' && chosenCard != -1) {
        int index = c - '1';
        if(index < ctable.cardCount) {
          s.sendPacket("O" + hand[chosenCard].toString() + ":" + ctable.cardsDown[index].toString());
          chosenCard = -1;
        }
      }
    }

  } while (true);

  return 0;
}

int client(string addr, int port, string name) {
  Socket s;
  clientName = name;
  if(!s.connect(addr, port)) {
    cout << "Could not connect to " << addr << ":" << port << endl;
    return -1;
  }
  s.sendPacket("R" + name);
  return clientLoop(s);
  return 0;
}


