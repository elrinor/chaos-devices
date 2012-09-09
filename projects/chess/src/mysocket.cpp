#include <string>
#include <iostream>
#include <algorithm>
#include <ctype.h>
#include "mysocket.h"
#include "myutils.h"


//========================================================//
// GLOBAL FUNCTIONS
//========================================================//
int InitSockets()
{
#ifdef  _WIN32
  WSADATA wsaData;
  return WSAStartup(MAKEWORD(1,1),&wsaData);
#else
  return 0;
#endif
}

int CloseSockets()
{
#ifdef  _WIN32
  WSACleanup();
#endif
  return 0;
}

//========================================================//
// TSOCKET CLASS
//========================================================//
TSocket::TSocket()
{
  FSocket=-1;
  memset(&FAddress,0,sizeof(FAddress));
}

TSocket::~TSocket()
{
  Destroy();
}

bool TSocket::Create()
{
  FSocket=socket(AF_INET,SOCK_STREAM,0);
  if(Broken()) return false;
  int sopt=1;
  if(setsockopt(FSocket,SOL_SOCKET,SO_REUSEADDR,(const char*)&sopt,sizeof(sopt))!=0)
    return false;
  return true;
}

bool TSocket::Destroy()
{
  int Result=0;
  if(!Broken())
  {
#ifdef _WIN32
    Result=closesocket(FSocket);
#else
    Result=close(FSocket);
#endif
  }
  FSocket=INVALID_SOCKET;
  if(Result==0)
    return true;
  else 
    return false;
}

bool TSocket::Bind(const int Port)
{
  if(Broken()) return false;
  FAddress.sin_family=AF_INET;
  FAddress.sin_addr.s_addr=INADDR_ANY;
  FAddress.sin_port=htons(Port);
  if(bind(FSocket,(sockaddr*)&FAddress,sizeof(FAddress))!=0)
    return false;
  return true;
}

bool TSocket::Listen() const
{
  if(Broken()) return false;
  if(listen(FSocket,MAXCONNECTIONS)!=0)
    return false;
  return true;
}

bool TSocket::Accept(TSocket& NewSocket) const
{
  int AddrLen=sizeof(FAddress);
  NewSocket.FSocket=accept(FSocket,0,(socklen_t*)&AddrLen);
  if(NewSocket.Broken())
    return false;
  else
    return true;
}

int TSocket::Send(const std::string s) const
{
  int status=send(FSocket,s.c_str(),s.size(),0);
  return status;
}

int TSocket::Recv(std::string& s, int n) const
{
  char* buf=new char[n+1];
  s="";
  memset(buf,0,n+1);
  int status=recv(FSocket,buf,n,0);
  if(status==SOCKET_ERROR || status==0)
  {
    delete[] buf;
    return status;
  }
  else
  {
    for(int i=0; i<status; i++)
      s+=buf[i];
    delete[] buf;
    return status;
  }
}

bool TSocket::Connect(const std::string HostName, const int Port)
{
  if(Broken()) return false;
  FAddress.sin_family=AF_INET;
  FAddress.sin_port=htons(Port);
  hostent* RemoteHost=gethostbyname(HostName.c_str());
  if(RemoteHost!=0)
    memcpy(&FAddress.sin_addr,RemoteHost->h_addr_list[0],sizeof(FAddress.sin_addr));
  else
    return false;
  int Result=connect(FSocket,(sockaddr*)&FAddress,sizeof(FAddress));
  if(Result==0)
    return true;
  else
    return false;
}

//========================================================//
// TBUFSOCKET CLASS
//========================================================//
int TBufSocket::Send()
{
  if((int)FSendBuf.size()-FSendBufPos>0)
  {
    int n=TSocket::Send(FSendBuf.substr(FSendBufPos,(int)FSendBuf.size()-FSendBufPos));
    if(n==SOCKET_ERROR)
      return n;
    FSendBufPos+=n;
    if(FSendBufPos>FMaxBufPos)
    {
      FSendBuf.erase(0,FSendBufPos);
      FSendBufPos=0;
    }
    return n;
  }
  return 0;
}

int TBufSocket::Recv()
{
  std::string Received("");
  int n=TSocket::Recv(Received, FMaxBufPos);
  FRecvBuf+=Received;
  return n;
}

int TBufSocket::Read(std::string& s, int n)
{
  if(n==-1)
  {
    if(FRecvBuf.size()>0)
    {
      s=FRecvBuf;
      FRecvBuf.clear();
      return s.size();
    }
    else
      return 0;
  }
  if((int)FRecvBuf.size()-FRecvBufPos>=n)
  {
    s=FRecvBuf.substr(FRecvBufPos,n);
    FRecvBufPos+=n;
    if(FRecvBufPos>FMaxBufPos)
    {
      FRecvBuf.erase(0,FRecvBufPos);
      FRecvBufPos=0;
    }
    return n;
  }
  else
    return 0;
}

void TBufSocket::Write(std::string s)
{
  FSendBuf+=s;
}

bool TBufSocket::ReadPacket(std::string& s)
{
  if((int)FRecvBuf.size()-FRecvBufPos>=sizeof(TNetInt))
  {
    TNetInt nsize;
    const char* c=FRecvBuf.substr(FRecvBufPos,sizeof(TNetInt)).c_str();
    nsize.c[0]=c[0];
    nsize.c[1]=c[1];
    nsize.c[2]=c[2];
    nsize.c[3]=c[3];
    int size=ntohl(nsize.n);
    if((int)FRecvBuf.size()-FRecvBufPos>=size+sizeof(TNetInt))
    {
      Read(s,sizeof(TNetInt));
      Read(s,size);
      return true;
    }
    else
      return false;
  }
  else
    return false;
}

void TBufSocket::WritePacket(std::string s)
{
  TNetInt size;
  size.n=htonl(s.size());
  std::string ssize;
  ssize+=size.c[0];
  ssize+=size.c[1];
  ssize+=size.c[2];
  ssize+=size.c[3];
  Write(ssize+s);
}

bool TBufSocket::CanSend()
{
  fd_set FWS;
  timeval t;
  t.tv_sec=0;
  t.tv_usec=0;
  FD_ZERO(&FWS);
  FD_SET(Handle(),&FWS);
  if(select(Handle()+1,0,&FWS,0,&t)==1 && (int)FRecvBuf.size()-FRecvBufPos>0)
    return true;
  else
    return false;
}

bool TBufSocket::CanRecv()
{
  fd_set FRS;
  timeval t;
  t.tv_sec=0;
  t.tv_usec=0;
  FD_ZERO(&FRS);
  FD_SET(Handle(),&FRS);
  if(select(Handle()+1,&FRS,0,0,&t)==1)
    return true;
  else
    return false;
}


//========================================================//
// TSOCKETSET CLASS
//========================================================//
bool TSocketSet::Select()
{
  int n=0;
  FD_ZERO(&FRS);
  for(std::vector<int>::iterator i=FRSet.begin(); i!=FRSet.end(); i++)
  {
    n=max(n,*i);
    FD_SET(*i,&FRS);
  }
  FD_ZERO(&FWS);
  for(std::vector<int>::iterator i=FWSet.begin(); i!=FWSet.end(); i++)
  {
    n=max(n,*i);
    FD_SET(*i,&FWS);
  }
  n=select(n+1,&FRS,&FWS,0,0);
  if(n==SOCKET_ERROR || n==0)
    return false;
  else
    return true;
}

void TSocketSet::AddRSocket(int HSocket){FRSet.push_back(HSocket);}
void TSocketSet::AddWSocket(int HSocket){FWSet.push_back(HSocket);}
void TSocketSet::DelRSocket(int HSocket){for(std::vector<int>::iterator i=FRSet.begin(); i!=FRSet.end();i++) if(*i==HSocket){FRSet.erase(i);break;};}
void TSocketSet::DelWSocket(int HSocket){for(std::vector<int>::iterator i=FWSet.begin(); i!=FWSet.end();i++) if(*i==HSocket){FWSet.erase(i);break;};}
bool TSocketSet::IsWSet(int HSocket){return (bool)FD_ISSET(HSocket,&FWS);}
bool TSocketSet::IsRSet(int HSocket){return (bool)FD_ISSET(HSocket,&FRS);}
