#include "Game.h"

using namespace std;

bool Card::suitDefaultMode = true;

std::string Card::toString() {
  std::string s;
  if(code < 10)
    s = (char)('0' + code);
  else if(code == 10)
    s = "10";
  else if(code == 11)
    s = "J";
  else if(code == 12)
    s = "Q";
  else if(code == 13)
    s = "K";
  else if(code == 14)
    s = "A";
  if(suitDefaultMode)
    s += (char)(3 + suit);
  else {
    if(suit == 0)
      s += '<';
    else if(suit == 1)
      s += '>';
    else if(suit == 2)
      s += 'v';
    else if(suit == 3)
      s += '^';
  }
  return s;
}

void Card::fromString(std::string s) {
  if(suitDefaultMode)
    suit = s[s.size() - 1] - 3;
  else {
    if(s[s.size() - 1] == '<')
      suit = 0;
    else if(s[s.size() - 1] == '>')
      suit = 1;
    else if(s[s.size() - 1] == 'v')
      suit = 2;
    else if(s[s.size() - 1] == '^')
      suit = 3;
  }
  s = s.substr(0, s.size() - 1);
  if(s == "J")
    code = 11;
  else if(s == "Q")
    code = 12;
  else if(s == "K")
    code = 13;
  else if(s == "A")
    code = 14;
  else
    code = atol(s.c_str());
}


void Table::clear() {
  for(int i = 0; i < 6; i++) {
    cardsDown[i].code = 0;
    cardsUp[i].code = 0;
  }
  cardCount = 0;
}

Table::Table() {
  clear();
}

bool Table::covered() {
  for(int i = 0; i < cardCount; i++)
    if(cardsUp[i].code == 0)
      return false;
  return true;
}

int Table::uncoveredCount() {
  int k = 0;
  for(int i = 0; i < cardCount; i++)
    if(cardsUp[i].code == 0 && cardsDown[i].code != 0)
      k++;
  return k;
}

bool Table::addCard(Card c) {
  if(cardCount >= 6)
    return false;
  cardsDown[cardCount] = c;
  cardCount++;
  return true;
}

std::string Table::toString() {
  std::string s;
  for(int i = 0; i < 6; i++)
    s+= cardsDown[i].toString() + ":" + cardsUp[i].toString() + ":";
  s = s.substr(0, s.size() - 1);
  return s;
}

void Table::fromString(std::string s) {
  std::vector<std::string> v = strExplode(s, ':');
  for(int i = 0; i < 6; i++) {
    cardsDown[i].fromString(v[i * 2]);
    cardsUp[i].fromString(v[i * 2 + 1]);
  }
  cardCount = 0;
  for(int i = 0; i < 6; i++) if(cardsDown[i].code != 0)
    cardCount++;
}


std::vector<Card> handFromString(std::string s) {
  vector<string> ss = strExplode(s, ':');
  vector<Card> h;
  if(s == "")
    return h;
  for(int i = 0; i < ss.size(); i++) {
    Card c;
    c.fromString(ss[i]);
    h.push_back(c);
  }
  return h;
}

std::string handToString(std::vector<Card> h) {
  std::string s;
  for(int i = 0; i < h.size(); i++) {
    s += h[i].toString();
    if(i != h.size() - 1)
      s += ":";
  }
  return s;
}

bool removeFromHand(std::vector<Card>& h, Card c) {
  for(int i = 0; i < h.size(); i++)
    if(h[i] == c) {
      h.erase(h.begin() + i);
      return true;
    }
  return false;
}

