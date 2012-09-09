#include <string>
//#include <vector>
#include <iostream>
#include <sstream>
#include "menu.h"
#include "server.h"
#include "mysocket.h"
using namespace std;

vector<string> param;

int main(int argc, char** argv)
{
  for(int i=0; i<argc; i++)
    param.push_back(argv[i]);
  InitSockets();
  if(param.size()==1)
    menu_cycle();
  else
  {
    if(param[1]=="server")
    {
      if(param.size()<4)
        cout<<"Possible arguments: \nserver PORT MAXCLNT\n";
      else
      {
        int port,maxclnt;
        stringstream s(param[2]+' '+param[3]);
        s>>port>>maxclnt;
        StartServer(port,maxclnt);
      }
    }
    else if(param[1]=="client" && param.size()>=6)
    {
    
    }
    else if(param[1]=="client" && param.size()>=4)
    {
       
    }
    else if(param[1]=="client")
    {
        cout<<"Possible arguments: \nclient HOST PORT\nclient HOST PORT register USER\nclient HOST PORT login USER\n";
    }
    else
      cout<<"Unknown argument: "<<param[1]<<"\n";
  }
  CloseSockets();
  return 0;
}
