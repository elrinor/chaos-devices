#include <iostream>
#include <string>
#include "Server.h"
#include "Client.h"
#include "Socket.h"
#include "Game.h"

using namespace std;

void synopsis() {
  cout << "Fool - Fool Card Game" << endl;
  cout << "USAGE:" << endl;
  cout << "Fool server PORT DELAY" << endl;
  cout << "  Start server at port PORT, wait DELAY secs for players 3-6" << endl;
  cout << "Fool join ADDRESS:PORT NAME" << endl;
  cout << "  Join server at ADDRESS:PORT as NAME" << endl;
}

int main(int argc, char** argv) {
  if(argc < 4) {
    synopsis();
    return -1;
  }
  if(argc == 5 && string(argv[4]) == "-poor") {
    Card::suitDefaultMode = false;
  }
  if(initSockets() != 0) {
    cout << "WSAStartup failed" << endl;
    return -1;
  }
  if(string(argv[1]) == "server") {
    int port = atol(argv[2]);
    int delay = atol(argv[3]);
    return server(port, delay);
  }
  if(string(argv[1]) == "join") {
    string addr = argv[2];
    string name = argv[3];
    int port = 0;
    for(int i = addr.size() - 1; i >= 0; i--)
      if(addr[i] == ':') {
        port = atol(addr.substr(i + 1).c_str());
        addr = addr.substr(0, i);
      }
      return client(addr, port, name);
  }
  return 0;
}

