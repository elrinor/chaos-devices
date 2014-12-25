#include <stdlib.h>
#include <iostream>
#include <algorithm>
#include "Server.h"
#include "Socket.h"
#include "Util.h"
#include "Game.h"

using namespace std;

class Player {
public:
  Socket sock;
  string name;
  vector<Card> hand;
  bool alive;
  Socket lastSock;

  Player(Socket s) {
    sock = s;
    lastSock = s;
    alive = true;
  }
  Player(const Player& p) {
    name = p.name;
    sock = p.sock;
    lastSock = p.lastSock;
    alive = p.alive;
    hand = p.hand;
  }
};

vector<Player> players;
int activePlayer;
bool covering;
vector<Card> deck;
int trumps;
bool playing;
bool gameEnded = false;
Table table;
SocketSet ss;

bool checkFool() {
  if(deck.size() == 0) {
    int k = 0;
    for(int i = 0; i < players.size(); i++)
      if(players[i].hand.size() > 0)
        k++;
    if(k == 0) {
      // No fools
      for(int i = 0; i < players.size(); i++)
        players[i].sock.putInSendBuffer("QNo fools today...");
      gameEnded = true;
      return true;
    }
    else if (k == 1) {
      // Fool!!!
      for(int i = 0; i < players.size(); i++) 
        if(players[i].hand.size() != 0) {
          activePlayer = i;
          break;
        }
      for(int i = 0; i < players.size(); i++) 
        if(players[i].hand.size() == 0)
          players[i].sock.putInSendBuffer("QToday " + players[activePlayer].name + " is a fool");
        else
          players[i].sock.putInSendBuffer("QYou're a fool today!");
      gameEnded = true;
      return true;
    }
    else {
      while(players[activePlayer].hand.size() == 0)
        activePlayer = (activePlayer + 1) % players.size();
    }
  }
  return false;
}

void beginTurn() {
  if(checkFool())
    return;
  players[activePlayer].sock.putInSendBuffer("EIt's your turn");
  for(int i = 0; i < players.size(); i++) if(i != activePlayer)
    players[i].sock.putInSendBuffer("EIt's " + players[activePlayer].name + "'s turn");
  players[activePlayer].sock.putInSendBuffer("!");
}

void updatePlayerData() {
  string sizes = "";
  for(int i = 0; i < players.size(); i++) {
    sizes += players[i].name + " - " + toString(players[i].hand.size()) + " card(s)";
    if(i != players.size() - 1)
      sizes += ":";
  }
  for(int i = 0; i < players.size(); i++) {
    players[i].sock.putInSendBuffer("H" + handToString(players[i].hand));
    players[i].sock.putInSendBuffer("T" + table.toString());
    players[i].sock.putInSendBuffer("D" + toString(deck.size()));
    players[i].sock.putInSendBuffer("S" + sizes);
    if(playing) {
      if(i == activePlayer && !covering)
        players[i].sock.putInSendBuffer("!");
      else if(i == activePlayer && covering)
        players[i].sock.putInSendBuffer("?");
      else
        players[i].sock.putInSendBuffer("#");
    }
    ss.addWSocket(players[i].sock);
  }
}

void coverPhase() {
  if(checkFool())
    return;
  players[activePlayer].sock.putInSendBuffer("EYou're covering");
  for(int i = 0; i < players.size(); i++) if(i != activePlayer)
    players[i].sock.putInSendBuffer("E" + players[activePlayer].name + " is covering");
  players[activePlayer].sock.putInSendBuffer("?");
}

void endTurn() {
  table.clear();
  int i = activePlayer;
  for(int k = 0; k < players.size(); k++) {
    i = (i - 1 + players.size()) % players.size();
    while(players[i].hand.size() < 6 && deck.size() > 0) {
      players[i].hand.push_back(deck[deck.size() - 1]);
      deck.pop_back();
    }
  }
  covering = false;
  updatePlayerData();
  beginTurn();
}

int server(int port, int delay) {
  Socket s;
  if(!s.bind(port)) {
    cout << "bind to port " << port << " failed" << endl;
    return -1;
  }
  if(!s.listen()) {
    cout << "listen failed" << endl;
    return -1;
  }
  cout << "server started at port " << port << endl;
  
  srand(getTime());
  for(int i = 6; i < 15; i++) {
    Card c;
    c.code = i;
    for(int j = 0; j < 4; j++) {
      c.suit = j;
      deck.push_back(c);
    }
  }
  random_shuffle(deck.begin(), deck.end());
  
  trumps = min((4 * rand() / RAND_MAX), 3);

  playing = false;
  ss.addRSocket(s);

  while(true) {
    // cout << "+ New Loop" << endl;
    if(gameEnded && ss.wSocks.empty())
      return 0;

    // cout << "+ Check alive" << endl;
    for(int i = 0; i < players.size(); i++) if(players[i].alive) {
      if(players[i].sock.broken() && players[i].alive) {
        ss.delRSocket(players[i].lastSock);
        ss.delWSocket(players[i].lastSock);
        players[i].alive = false;
        if(!playing) {
          for(int j = 0; j < players[i].hand.size(); j++)
            deck.push_back(players[i].hand[j]);
          players.erase(players.begin() + i);
          break;
        }
        else if(players[i].hand.size() > 0 || deck.size() > 0) {
          for(int j = 0; j < players.size(); j++)
            if(!players[j].sock.broken()) {
              players[j].sock.putInSendBuffer("Q" + players[i].name + " left and server was shut down");
              players[j].sock.trySendPacket();
            }
            return -1;
        }
      }
    }

    // cout << "+ Select" << endl;
    while(!ss.select(delay)) {
      if(playing && covering && table.covered()) {
        endTurn();
      }
      if((players.size() >= 2 || players.size() >= 6) && !playing) {
        playing = true;
        delay = 5000;
        ss.delRSocket(s);
        for(int i = 0; i < players.size(); i++) {
          players[i].sock.putInSendBuffer("EGame started");
          ss.addWSocket(players[i].sock);
        }
        activePlayer = 0;
        covering = false;
        beginTurn();
      }
    }

    // cout << "+ Checking server socket" << endl;
    if(ss.isRSet(s) && !playing) {
      // new client!
      Socket clientSock;
      if(s.accept(clientSock)) {
        cout << "+ New client connected" << endl;
        players.push_back(Player(clientSock));
        ss.addRSocket(clientSock);
        // cout << "+ Connection handled";
        if(players.size() >= 2) {
          for(int i = 0; i < players.size(); i++) {
            if(i != players.size() - 1)
              players[i].sock.putInSendBuffer("ENew client connected");
            players[i].sock.putInSendBuffer("EStarting game in " + toString(delay) + " millisecs");
            ss.addWSocket(players[i].sock);
          }
        }
      }
    }
    else for(int i = 0; i < players.size(); i++) if(players[i].alive) {
      // cout << "+ Player " << i << " isRset?" << endl;
      if(ss.isRSet(players[i].sock)) {
        // cout << "+ Player " << i << " isRset!" << endl;
        string msg;
        if(players[i].sock.tryRecvPacket(msg)) {
          cout << msg << endl;
          char code = msg[0];
          msg = msg.substr(1);
          if(code == 'R') {
            // Register
            players[i].name = msg;
            players[i].sock.putInSendBuffer("EYou were successfully registered as " + msg);
            for(int j = 0; j < 6; j++) {
              players[i].hand.push_back(deck[deck.size() - 1]);
              deck.pop_back();
            }
            players[i].sock.putInSendBuffer(string("K") + (char)('0' + trumps));
            updatePlayerData();
            for(int k = 0; k < players.size(); k++) if(i != k){
              players[k].sock.putInSendBuffer("ENew client is now knows as " + msg);
              ss.addWSocket(players[k].sock);
            }
          } else if(code == 'P') {
            // Play a card
            if(i == activePlayer && !covering) {
              Card c;
              c.fromString(msg);
              bool found = false;
              for(int j = 0; j < players[i].hand.size(); j++) 
                if(players[i].hand[j].code == c.code && players[i].hand[j].suit == c.suit) {
                  found = true;
                  table.addCard(players[i].hand[j]);
                  players[i].hand.erase(players[i].hand.begin() + j);
                  updatePlayerData();
                  activePlayer = (activePlayer + 1) % players.size();
                  covering = true;
                  coverPhase();
                }
              if(!found)
                beginTurn();
            }
            // Add a card to table
            else if(i != activePlayer && covering) {
              Card c;
              c.fromString(msg);
              if(table.cardCount == 6 || players[activePlayer].hand.size() <= table.uncoveredCount()) {
                players[i].sock.putInSendBuffer("ETable full");
                ss.addWSocket(players[i].sock);
              } else {
                bool found = false;
                for(int k = 0; k < table.cardCount; k++) 
                  if(c.code == table.cardsUp[k].code || c.code == table.cardsDown[k].code) {
                    found = removeFromHand(players[i].hand, c);
                    if(found) {
                      table.addCard(c);
                      for(int j = 0; j < players.size(); j++) if(i != j) {
                        players[j].sock.putInSendBuffer("E" + players[i].name + " plays " + c.toString());
                        ss.addWSocket(players[j].sock);
                      }
                    }
                    break;
                  }
                if(found)
                  updatePlayerData();
              }
            }
            else {
              players[i].sock.putInSendBuffer("EFailed");
              ss.addWSocket(players[i].sock);
            }
          } else if(code == 'O') {
            // Cover a card
            if(i == activePlayer && covering) {
              vector<string> v = strExplode(msg, ':');
              Card c1, c2;
              // we're putting c1 over c2
              c1.fromString(v[0]);
              c2.fromString(v[1]);
              if((c1.code > c2.code && c1.suit == c2.suit) || (c1.suit == trumps && c2.suit != trumps)) {
                bool found = false;
                for(int k = 0; k < table.cardCount; k++) 
                  if(c2 == table.cardsDown[k] && table.cardsUp[k].code == 0) {
                    found = removeFromHand(players[i].hand, c1);
                    if(found) {
                      table.cardsUp[k] = c1;
                      for(int j = 0; j < players.size(); j++) if(i != j) {
                        players[j].sock.putInSendBuffer("E" + players[i].name + " puts " + c1.toString() + " over " + c2.toString());
                        ss.addWSocket(players[j].sock);
                      }
                    }
                    break;
                  }
                if(found) {
                  updatePlayerData();
                  if(table.covered() && table.cardCount < 6) {
                    for(int j = 0; j < players.size(); j++) if(i != j) {
                      players[j].sock.putInSendBuffer("EAll cards covered, waiting " + toString(delay) + " millisecs...");
                      ss.addWSocket(players[j].sock);
                    }
                  }
                  if(table.covered() && table.cardCount == 6) {
                    for(int j = 0; j < players.size(); j++) if(i != j) {
                      players[j].sock.putInSendBuffer("EAll cards covered");
                      ss.addWSocket(players[j].sock);
                    }
                    endTurn();
                  }
                }
                else {
                  players[i].sock.putInSendBuffer("EFailed");
                  ss.addWSocket(players[i].sock);
                }
              }
              else {
                players[i].sock.putInSendBuffer("EFailed");
                ss.addWSocket(players[i].sock);
              }
            }
            else {
              players[i].sock.putInSendBuffer("EFailed");
              ss.addWSocket(players[i].sock);
            }
          }
          else if(code == 'T') {
            // Take all
            if(i == activePlayer && covering) {
              for(int j = 0; j < 6; j++) {
                if(table.cardsUp[j].code != 0)
                  players[i].hand.push_back(table.cardsUp[j]);
                if(table.cardsDown[j].code != 0)
                  players[i].hand.push_back(table.cardsDown[j]);
              }
              players[i].sock.putInSendBuffer("EYou took all cards");
              ss.addWSocket(players[i].sock);
              for(int j = 0; j < players.size(); j++) if(i != j) {
                players[j].sock.putInSendBuffer("E" + players[i].name + " took all cards");
                ss.addWSocket(players[j].sock);
              }
              activePlayer = (activePlayer + 1) % players.size();
              endTurn();
            }
          }
        }
      }
      // cout << "+ Player " << i << " isWset?" << endl;
      if(ss.isWSet(players[i].sock)) {
        // cout << "+ Writing to Socket...";
        players[i].sock.trySendPacket();
        if(players[i].sock.isSendBufEmpty())
          ss.delWSocket(players[i].sock);
        // cout << "+ Successful!";
      }
    }
  }

  return 0;
}


