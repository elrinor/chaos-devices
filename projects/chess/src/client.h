#ifndef _CLIENT
#define _CLIENT

#include <string>
#include "mysocket.h"

extern TBufSocket Sock;

bool ConnectToServer(std::string host, int port);
void ClearBuffers();

#endif
