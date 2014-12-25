#ifndef __SOCKET_H__
#define __SOCKET_H__

#ifdef _WIN32
#  include <WINSOCK2.H>
#  include <WS2TCPIP.H>
#  pragma comment(lib, "Ws2_32.lib")
#else
#  include <sys/types.h>
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <netdb.h>
#endif

#include <string>
#include <vector>

int initSockets();
int closeSockets();

class Socket {
private:
  int hSocket;
  int* refCount;
  bool* closed;
  std::string sendBuf;
  std::string recvBuf;
  bool send(const char* buf, int count);
  bool recv(char* buf, int count);
  bool recvToBuffer();
public:
  Socket& operator= (const Socket& sock);
  Socket();
  Socket(Socket& sock);
  Socket::Socket(int hSock);
  ~Socket();
  bool bind(const int port);
  bool listen();
  bool accept(Socket& sock);
  bool close();
  bool connect(const std::string host, const int port);
  bool sendPacket(std::string s);
  bool recvPacket(std::string& s);
  bool tryRecvPacket(std::string& s);
  bool putInSendBuffer(std::string s);
  bool trySendPacket();
  bool broken() const {return hSocket == -1 || *closed;}
  int handle() const {return hSocket;}
  bool isSendBufEmpty() {return sendBuf.size() == 0;}
  bool isRecvBufEmpty() {return recvBuf.size() == 0;}
  bool canRecv();
  bool canSend();
};

class SocketSet {
private:
  fd_set wSet;
  fd_set rSet;
public:
  std::vector<int> rSocks;
  std::vector<int> wSocks;
  void addRSocket(const Socket sock);
  void addWSocket(const Socket sock);
  void delRSocket(const Socket sock);
  void delWSocket(const Socket sock);
  bool isWSet(const Socket sock);
  bool isRSet(const Socket sock);
  bool select(int msecTimeout);
  bool select();
};

#endif
