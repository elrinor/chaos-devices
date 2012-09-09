#ifndef _MYSOCKET
#define _MYSOCKET

#ifdef _WIN32
  #include <WINSOCK2.H>
  #include <WS2TCPIP.H>
  #pragma comment(lib, "Ws2_32.lib")
#else
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #include <netdb.h>
#endif

#include <string>
#include <vector>

#ifndef SOCKET_ERROR
#define SOCKET_ERROR -1
#endif

#ifndef INVALID_SOCKET
#define INVALID_SOCKET -1
#endif

const int MAXHOSTNAME=200;
const int MAXCONNECTIONS=5;
const int MAXRECV=500;

int InitSockets();
int CloseSockets();

union TNetInt
{
  char c[4];
  u_long n;
};

class TSocket
{
private:
  int FSocket;
  sockaddr_in FAddress;
public:
  TSocket();
  virtual ~TSocket();
  bool Create();
  bool Destroy();
  bool Bind(const int port);
  bool Listen() const;
  bool Accept(TSocket&) const;
  bool Connect(const std::string host,const int port);
  int Send(const std::string s) const;
  int Recv(std::string& s, int n) const;
  bool Broken() const {return FSocket==INVALID_SOCKET;}
  int Handle() {return FSocket;}
};

class TBufSocket:public TSocket
{
private:
  int FMaxBufPos;
  std::string FSendBuf, FRecvBuf;
  int FSendBufPos, FRecvBufPos;
protected:
  int Send(const std::string s) const;
  int Recv(std::string& s, int n) const;
public:
  virtual ~TBufSocket(){};
  TBufSocket():FSendBuf(),FRecvBuf(),FSendBufPos(0),FRecvBufPos(0),FMaxBufPos(1024){};
  TBufSocket(int ChunkSize):FSendBuf(),FRecvBuf(),FSendBufPos(0),FRecvBufPos(0),FMaxBufPos(ChunkSize){};
  int Send();
  int Recv();
  bool CanSend();
  bool CanRecv();
  int Read(std::string& s, int n);
  void Write(std::string s);
  bool ReadPacket(std::string& s);
  void WritePacket(std::string s);
  void ClearSendBuf(){FSendBuf.clear();FSendBufPos=0;}
  void ClearRecvBuf(){FRecvBuf.clear();FRecvBufPos=0;}
  int SendBufSize(){return FSendBuf.size();}
  int RecvBufSize(){return FRecvBuf.size();}
  bool RecvBufIsEmpty(){return FRecvBuf.size()==FRecvBufPos;}
  bool SendBufIsEmpty(){return FSendBuf.size()==FSendBufPos;}
};

class TSocketSet
{
private:
  static const int FTimeOut=1000;
  fd_set FWS;
  fd_set FRS;
  std::vector<int> FRSet;
  std::vector<int> FWSet;
public:
  void AddRSocket(int HSocket);
  void AddWSocket(int HSocket);
  void DelRSocket(int HSocket);
  void DelWSocket(int HSocket);
  bool IsWSet(int HSocket);
  bool IsRSet(int HSocket);
  TSocketSet():FRSet(),FWSet(){};
  bool Select();
};

#endif
