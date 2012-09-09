#include <string>
#include "mysocket.h"
#include "client.h"
using namespace std;

TBufSocket Sock;

bool ConnectToServer(std::string host, int port)
{
  Sock.Destroy();
  Sock.Create();
  return Sock.Connect(host,port);
}

void ClearBuffers()
{
  while(Sock.CanRecv())
    Sock.Recv();
  Sock.ClearRecvBuf();
  while(Sock.CanSend())
    Sock.Send();
  Sock.ClearSendBuf();
}
