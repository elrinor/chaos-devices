#include "Socket.h"

#ifndef INVALID_SOCKET
#  define INVALID_SOCKET (-1)
#endif
#ifndef SOCKET_ERROR
#  define SOCKET_ERROR (-1)
#endif

#include <iostream>

using namespace std;

int initSockets()
{
#ifdef  _WIN32
  WSADATA wsaData;
  return WSAStartup(MAKEWORD(1,1),&wsaData);
#else
  return 0;
#endif
}

int closeSockets()
{
#ifdef  _WIN32
  WSACleanup();
#endif
  return 0;
}


Socket::Socket() : recvBuf(), sendBuf() {
  hSocket = -1;
  refCount = new int;
  *refCount = 1;
  closed = new bool;
  *closed = true;
  
  hSocket = socket(AF_INET, SOCK_STREAM, 0);
  int sopt = 1;
  setsockopt(hSocket, SOL_SOCKET, SO_REUSEADDR, (const char*)&sopt, sizeof(sopt));
  if(hSocket != -1)
    *closed = false;
}

Socket::Socket(int hSock) : recvBuf(), sendBuf() {
  hSocket = hSock;
  refCount = new int;
  *refCount = 1;
  closed = new bool;
  if(hSock != -1)
    *closed = false;
  else
    *closed = true;
}

Socket::Socket(Socket& sock) : recvBuf(), sendBuf(){
  hSocket = sock.hSocket;
  refCount = sock.refCount;
  (*refCount)++;
  closed = sock.closed;
  recvBuf = sock.recvBuf;
  sendBuf = sock.sendBuf;
}

Socket& Socket::operator= (const Socket& sock) {
  if(this != &sock) {
    //this->~Socket();
    
    (*refCount)--;
    if(*refCount == 0) {
      if(!(*closed)) {
        close();
      }
    }

    hSocket = sock.hSocket;
    closed = sock.closed;
    refCount = sock.refCount;
    (*refCount)++;
    recvBuf = sock.recvBuf;
    sendBuf = sock.sendBuf;
  }
  return (*this);
}

Socket::~Socket() {
  (*refCount)--;
  if(*refCount == 0) {
    //delete refCount;
    if(!(*closed)) {
      close();
    }
    //delete closed;
  }
}

bool Socket::close() {
  int result = 0;
  if(!broken())
  {
    #ifdef _WIN32
    result = closesocket(hSocket);
    #else
    result = ::close(hSocket);
    #endif
  }
  hSocket = INVALID_SOCKET;
  if(result == 0)
    return true;
  else 
    return false;
}

bool Socket::bind(const int port) {
  if(broken()) 
    return false;
  sockaddr_in address;
  address.sin_family = AF_INET;
  address.sin_addr.s_addr = INADDR_ANY;
  address.sin_port = htons(port);
  if(::bind(hSocket, (sockaddr*)&address, sizeof(address)) != 0)
    return false;
  return true;
}

bool Socket::listen() {
  if(broken()) 
    return false;
  if(::listen(hSocket, 5) != 0)
    return false;
  return true;
}

bool Socket::accept(Socket& sock) {
  int addrLen = sizeof(sockaddr_in);
  sock = Socket(::accept(hSocket, 0, (socklen_t*)&addrLen));
  return !sock.broken();
}

bool Socket::connect(const std::string host, const int port) {
  if(broken()) 
    return false;
  sockaddr_in address;
  address.sin_family = AF_INET;
  address.sin_port = htons(port);
  hostent* remoteHost = gethostbyname(host.c_str());
  if(remoteHost != NULL)
    memcpy(&address.sin_addr, remoteHost->h_addr_list[0], sizeof(address.sin_addr));
  else
    return false;
  if(::connect(hSocket, (sockaddr*)&address, sizeof(address)) == 0)
    return true;
  else
    return false;
}

bool Socket::send(const char* buf, int count) {
  if(broken())
    return false;
  int totalSent = 0;
  do {
    int sent = ::send(hSocket, buf + totalSent, count - totalSent, 0);
    if(sent == -1) {
      close();
      return false;
    }
    totalSent += sent;
  } while (totalSent < count);
  return true;
}

bool Socket::recv(char* buf, int count) {
  if(broken())
    return false;
  int totalReceived = 0;
  do {
    int received = ::recv(hSocket, buf + totalReceived, count - totalReceived, 0);
    if(received <= 0) {
      close();
      return false;
    }
    totalReceived += received;
  } while (totalReceived < count);
  return true;
}

bool Socket::sendPacket(std::string s) {
  char buf[4];
  *((int*)buf) = htonl(s.size());
  string ssize;
  ssize += buf[0];
  ssize += buf[1];
  ssize += buf[2];
  ssize += buf[3];
  s = ssize + s;
  return send(s.c_str(), s.size());
}

bool Socket::recvPacket(std::string& s) {
  int size;
  char padding; // hm?
  if(!recv((char*)&size, 4))
    return false;
  size = ntohl(size);
  char buf[1025];
  s = "";
  do {
    if(!recv(buf, min(1024, size)))
      return false;
    size -= 1024;
    buf[min(1024, size)] = '\0';
    s += buf;
  } while(size <= 0);
  return true;
}

bool Socket::recvToBuffer() {
  char buf[1025];
  // cout << "+ recv()" << endl;
  int received = ::recv(hSocket, buf, 1024, 0);
  if(received <= 0) {
    close();
    return false;
  }
  // cout << "+ fillBuf(), received == " << received << endl;
  // cin >> received;
  // cout << "+ recvBuf == " << recvBuf << endl;
  for(int i = 0; i < received; i++)
    recvBuf += buf[i];
  return true;
}

bool Socket::tryRecvPacket(std::string& s) {
  if(!broken() && canRecv()) {
    // cout << "+ recvToBuffer()" << endl;
    recvToBuffer();
  }
  // cout << "+ Received something" << endl;
  if(recvBuf.size() >= 4) {
    int size = ntohl(*((u_long*)recvBuf.c_str()));
    if(recvBuf.size() >= 4 + size) {
      s = recvBuf.substr(4, size);
      recvBuf = recvBuf.substr(4 + size);
      return true;
    }
    else
      return false;
  }
  else
    return false;
}

bool Socket::putInSendBuffer(std::string s) {
  char buf[4];
  *((int*)buf) = htonl(s.size());
  string ssize;
  ssize += buf[0];
  ssize += buf[1];
  ssize += buf[2];
  ssize += buf[3];
  sendBuf += ssize + s;
  return true;
}

bool Socket::trySendPacket() {
  if(isSendBufEmpty())
    return true;
  int sent = ::send(hSocket, sendBuf.c_str(), sendBuf.size(), 0);
  if(sent == -1) {
    close();
    return false;
  }
  sendBuf = sendBuf.substr(sent);
  return true;
}

bool Socket::canRecv() {
  fd_set rSet;
  timeval t;
  t.tv_sec = 0;
  t.tv_usec = 0;
  FD_ZERO(&rSet);
  FD_SET(handle(), &rSet);
  if(select(handle() + 1, &rSet, 0, 0, &t) == 1)
    return true;
  else
    return false;
}

bool Socket::canSend() {
  fd_set wSet;
  timeval t;
  t.tv_sec = 0;
  t.tv_usec = 0;
  FD_ZERO(&wSet);
  FD_SET(handle(), &wSet);
  if(select(handle() + 1, 0, &wSet, 0, &t) == 1)
    return true;
  else
    return false;
}





bool SocketSet::select(int msecTimeout) {
  int n = 0;
  
  FD_ZERO(&rSet);
  for(std::vector<int>::iterator i = rSocks.begin(); i != rSocks.end(); i++)
  {
    n = max(n, *i);
    FD_SET(*i, &rSet);
  }

  FD_ZERO(&wSet);
  for(std::vector<int>::iterator i = wSocks.begin(); i != wSocks.end(); i++)
  {
    n = max(n,*i);
    FD_SET(*i, &wSet);
  }

  if(msecTimeout == 0)
    n = ::select(n + 1, &rSet, &wSet, 0, 0);
  else {
    timeval t;
    t.tv_usec = (msecTimeout % 1000) * 1000;
    t.tv_sec = msecTimeout / 1000;
    n = ::select(n + 1, &rSet, &wSet, 0, &t);
  }
  if(n == SOCKET_ERROR || n == 0)
    return false;
  else
    return true;
}

bool SocketSet::select() {
  return select(0);
}

void SocketSet::addRSocket(const Socket sock) {
  for(vector<int>::iterator i = rSocks.begin(); i != rSocks.end(); i++) 
    if(*i == sock.handle())
      return;
  rSocks.push_back(sock.handle());
}

void SocketSet::addWSocket(const Socket sock) {
  for(vector<int>::iterator i = wSocks.begin(); i != wSocks.end(); i++) 
    if(*i == sock.handle())
      return;
  wSocks.push_back(sock.handle());
}

void SocketSet::delRSocket(const Socket sock) {
  for(vector<int>::iterator i = rSocks.begin(); i != rSocks.end(); i++) 
    if(*i == sock.handle()) {
      rSocks.erase(i);
      break;
    }
}

void SocketSet::delWSocket(const Socket sock) {
  for(vector<int>::iterator i = wSocks.begin(); i != wSocks.end(); i++) 
    if(*i == sock.handle()) {
      wSocks.erase(i);
      break;
    }
}

bool SocketSet::isWSet(const Socket sock) {
  return (bool)FD_ISSET(sock.handle(), &wSet);
}

bool SocketSet::isRSet(const Socket sock) {
  return (bool)FD_ISSET(sock.handle(), &rSet);
}

