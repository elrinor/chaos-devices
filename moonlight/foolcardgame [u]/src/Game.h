#ifndef __GAME_H__
#define __GAME_H__

#include "Util.h"

class Card {
public:
  int code;
  int suit;
  std::string toString();
  void fromString(std::string s);
  bool operator== (const Card& c) {
    return (code == c.code && suit == c.suit);
  }
  static bool suitDefaultMode;
};

class Table {
public:
  Card cardsDown[6];
  Card cardsUp[6];
  int cardCount;
  
  void clear();
  Table();
  bool covered();
  int uncoveredCount();
  bool addCard(Card c);
  std::string toString();
  void fromString(std::string s);
};

std::vector<Card> handFromString(std::string s);

std::string handToString(std::vector<Card> h);

bool removeFromHand(std::vector<Card>& h, Card c);

#endif

