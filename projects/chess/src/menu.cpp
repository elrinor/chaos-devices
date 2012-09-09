#include <vector>
#include <string>
#include <sstream> 
#include <list>
#include "client.h"
#include "mycurses.h"
#include "myutils.h"
#include "server.h"
#include "battle.h"
using namespace std;

string ServerName;
string ServerPort;
string Login;
string CurOpponent;
int MatchTime=0;

struct TOpponent
{
  string name;
  bool playing;
  int myrequest;
  int hisrequest;
};

vector<TOpponent> Opponents;

void DrawTitle()
{
  crPut(0,0,crColor(3),"Chess");
}

void SingleMenu(int x, int y, string s)
{
  crPut(x,y,crColor(3),"->");
  crPut(x+2,y,crColor(2),s);
  while(true)
  {
    int c=crReadKey();
    if(c==KEY_ESC || c==KEY_ENTER)
      break;
  }
}

bool GetClientsList()
{
  Opponents.clear();
  crClrScr();
  DrawTitle();
  crPut(0,1,crColor(2),"Receiving List of Players on the Server...");

  Sock.WritePacket("D");
  Sock.Send();

  while(true)
  {
    string result;
    while(!Sock.ReadPacket(result))
    {
      int received=Sock.Recv();
      if(received==SOCKET_ERROR || received==0)
      {
        crPut(0,3,crColor(2),"Socket Error!");
        SingleMenu(0,4,"Back");
        return false;
      }
    }
    if(result[0]=='Z')
      break;
    else if(result[0]=='R')
    {
      TOpponent c;
      stringstream ss(result.substr(1,result.size()-1));
      ss>>c.name>>c.playing>>c.myrequest>>c.hisrequest;
      if(c.name!=Login)
        Opponents.push_back(c);
    }
    else if(result[0]=='G')
    {
      battle();
      return false;
    }
    else
    {
      crPut(0,3,crColor(2),"Packet Error!");
      ClearBuffers();
      SingleMenu(0,4,"Back");
      return false;
    }
  }
  return true;
}

void battlerequestmenu()
{
  crClrScr();
  DrawTitle();
  crPut(0,1,crColor(1),"Battle Request");
  int p=0;
  int pc=4;
  int selected=-1;
  while(true)
  {
    crPut(0,2,crColor(1),"  ");
    crPut(0,3,crColor(1),"  ");
    crPut(0,4,crColor(1),"  ");
    crPut(0,5,crColor(1),"  ");
    crPut(0,2+p,crColor(3),"->");
    crPut(2,2,(p==0)?crColor(2):crColor(1),"Request  1 min match");
    crPut(2,3,(p==1)?crColor(2):crColor(1),"Request  5 min match");
    crPut(2,4,(p==2)?crColor(2):crColor(1),"Request 10 min match");
    crPut(2,5,(p==3)?crColor(2):crColor(1),"Back");

    int c=crReadKey();
    if(c==KEY_DOWN)
      p++;
    if(c==KEY_UP)
      p--;
    if(c==KEY_ESC)
      selected=pc-1;
    if(c==KEY_ENTER)
      selected=p;
    p=(p+pc)%pc;

    if(selected==0)
    {
      MatchTime=1;
      Sock.WritePacket("G"+CurOpponent+" "+IntToStr(MatchTime));
      Sock.Send();
      break;
    }
    if(selected==1)
    {
      MatchTime=5;
      Sock.WritePacket("G"+CurOpponent+" "+IntToStr(MatchTime));
      Sock.Send();
      break;
    }
    if(selected==2)
    {
      MatchTime=10;
      Sock.WritePacket("G"+CurOpponent+" "+IntToStr(MatchTime));
      Sock.Send();
      break;
    }
    if(selected==pc-1)
      break;
    selected=-1;
  }
}

bool chooseopponentmenu()
{
  if(!GetClientsList())
    return true;
  crClrScr();
  DrawTitle();
  if(Opponents.size()==0)
    crPut(0,1,crColor(1),"Players on the Server: none");
  else
  {
    crPut(0,1,crColor(1),"Players on the Server:");
    crPut(30,1,crColor(1),"State:");
    crPut(40,1,crColor(1),"Request (Your / Opponent's):");
  }
  static int p=0;
  int pc=Opponents.size()+2;
  if(p>pc-1) p=pc-1;
  int selected=-1;
  int requested=-1;
  int request=0;
  unsigned int starttime=GetTime();
  for(int i=0; i<pc-2; i++)
  {
    if(Opponents[i].playing)
      crPut(30,2+i,crColor(1),"Playing");
    else
      crPut(30,2+i,crColor(2),"Free");
  }
  for(int i=0; i<pc-2; i++)
    crPut(40,2+i,(Opponents[i].myrequest>0 || Opponents[i].hisrequest>0)?crColor(2):crColor(1),IntToStr(Opponents[i].myrequest)+"/"+IntToStr(Opponents[i].hisrequest));
  while(true)
  {
    for(int i=0; i<pc; i++)
      crPut(0,2+i,crColor(1),"  ");
    crPut(0,2+p,crColor(3),"->");
    for(int i=0; i<pc-2; i++)
      crPut(2,2+i,(p==i)?crColor(2):crColor(1),Opponents[i].name);
    crPut(2,2+pc-2,(p==pc-2)?crColor(2):crColor(1),"Refresh");
    crPut(2,2+pc-1,(p==pc-1)?crColor(2):crColor(1),"Back");

    int c;
    crDelayTime(100);
    while((c=crReadKey())==ERR && GetTime()-1000<starttime);
    crDelayTime(-1);
    
    if(c==ERR)
      selected=pc-2;
    if(c==KEY_DOWN)
      p++;
    if(c==KEY_UP)
      p--;
    if(c==KEY_ESC)
      selected=pc-1;
    if(c==KEY_ENTER)
      selected=p;
    p=(p+pc)%pc;

    if(selected<pc-2 && selected>=0)
    {
      if(Opponents[p].myrequest==0)
      {
        CurOpponent=Opponents[p].name;
        battlerequestmenu();
        return false;
      }
      else
      {
        Sock.WritePacket("G"+Opponents[p].name+" 0");
        Sock.Send();
        return false;
      }
    }
    if(selected==pc-2)
    {
      return false;
    }
    if(selected==pc-1)
    {
      break;
    }
    selected=-1;

    string s;
    if(Sock.ReadPacket(s))
    {
      if(s[0]=='G')
      {
        battle();
        break;
      }
    }
    else if(Sock.CanRecv())
    {
      int received=Sock.Recv();
      if(received==SOCKET_ERROR || received==0)
      {
        crPut(0,2,crColor(2),"Socket Error!");
        SingleMenu(0,3,"Back");
        break;
      }
    }
  }
  Sock.ClearRecvBuf();
  return true;
}

void changepasswordmenu()
{
  crClrScr();
  DrawTitle();
  crPut(0,1,crColor(2),"Enter  Old  Password:");
  crPut(0,2,crColor(2),"Enter  New  Password:");
  crPut(0,3,crColor(2),"Confirm New Password:");
  string oldpsw=crReadPsw(22,1);
  string newpsw1=crReadPsw(22,2);
  string newpsw2=crReadPsw(22,3);
  Sock.WritePacket("C"+oldpsw);
  Sock.WritePacket("1"+newpsw1);
  Sock.WritePacket("2"+newpsw2);
  Sock.Send();
  crPut(0,5,crColor(2),"Waiting for Response...");
  
  string result;
  while(!Sock.ReadPacket(result))
  {
    int received=Sock.Recv();
    if(received==SOCKET_ERROR || received==0)
    {
      crPut(0,7,crColor(2),"Socket Error!");
      SingleMenu(0,8,"Back");
      return;
    }
  }
  if(result[0]=='A')
  {
    crPut(0,7,crColor(2),"Password was Succesfully Changed.");
    SingleMenu(0,8,"Ok");
    return;
  }
  else if(result[0]=='F')
  {
    crPut(0,7,crColor(2),"Wrong Password!");
    SingleMenu(0,8,"Back");
    return;
  }
  else if(result[0]=='M')
  {
    crPut(0,7,crColor(2),"New Password was Confirmed Incorrectly!");
    SingleMenu(0,8,"Back");
    return;
  }
  else
  {
    crPut(0,7,crColor(2),"Packet Error!");
    ClearBuffers();
    SingleMenu(0,8,"Back");
    return;
  }
}

void halloffamemenu()
{
  crClrScr();
  DrawTitle();
  crPut(0,1,crColor(2),"Retreiving Hall of Fame...");
  list<string> HallOfFame;
  Sock.WritePacket("H");
  Sock.Send();

  while(true)
  {
    string result;
    while(!Sock.ReadPacket(result))
    {
      int received=Sock.Recv();
      if(received==SOCKET_ERROR || received==0)
      {
        crPut(0,3,crColor(2),"Socket Error!");
        SingleMenu(0,4,"Back");
        return;
      }
    }
    if(result[0]=='E')
      break;
    else if(result[0]=='H')
      HallOfFame.push_back(result.substr(1,result.size()-1));
    else
    {
      crPut(0,3,crColor(2),"Packet Error!");
      ClearBuffers();
      SingleMenu(0,4,"Back");
      return;
    }
  }

  crClrScr();
  DrawTitle();
  crPut( 0,1,crColor(2),"Hall of Fame");
  crPut( 0,2,crColor(2)," #");
  crPut( 3,2,crColor(2),"Name");
  crPut(30,2,crColor(2),"Rating");
  crPut(40,2,crColor(2),"Games");
  crPut(50,2,crColor(2),"Victories");
  crPut(60,2,crColor(2),"Defeats");
  int p=1;
  for(list<string>::iterator i=HallOfFame.begin(); i!=HallOfFame.end(); i++)
  {
    stringstream ss(*i);
    string s;
    if(p<10)
      crPut(0,p+2,crColor(1),' '+IntToStr(p)+'.');
    else
      crPut(0,p+2,crColor(1),IntToStr(p)+'.');
    ss>>s; crPut( 3,p+2,crColor(1),s);
    ss>>s; 
    if(s[0]=='-') 
      crPut(29,p+2,crColor(1),s.substr(0,5));
    else
      crPut(30,p+2,crColor(1),s.substr(0,4));
    ss>>s; crPut(40,p+2,crColor(1),s);
    ss>>s; crPut(50,p+2,crColor(1),s);
    ss>>s; crPut(60,p+2,crColor(1),s);
    p++;
  }
  SingleMenu(0,p+2,"Ok");
}

void servermenu()
{
  crClrScr();
  DrawTitle();
  crPut(0,1,crColor(1),"Chess Server at "+ServerName+":"+ServerPort);
  int p=0;
  int pc=4;
  int selected=-1;
  while(true)
  {
    crPut(0,2,crColor(1),"  ");
    crPut(0,3,crColor(1),"  ");
    crPut(0,4,crColor(1),"  ");
    crPut(0,5,crColor(1),"  ");
    crPut(0,2+p,crColor(3),"->");
    crPut(2,2,(p==0)?crColor(2):crColor(1),"Change Password");
    crPut(2,3,(p==1)?crColor(2):crColor(1),"Find Opponent");
    crPut(2,4,(p==2)?crColor(2):crColor(1),"Hall of Fame");
    crPut(2,5,(p==3)?crColor(2):crColor(1),"Leave Server");

    int c=crReadKey();
    if(c==KEY_DOWN)
      p++;
    if(c==KEY_UP)
      p--;
    if(c==KEY_ESC)
      p=pc-1;
    if(c==KEY_ENTER)
      selected=p;
    p=(p+pc)%pc;

    if(selected==0)
    {
      changepasswordmenu();
      crClrScr();
      DrawTitle();
      crPut(0,1,crColor(1),"Chess Server at "+ServerName+":"+ServerPort);
    }
    if(selected==1)
    {
      while(!chooseopponentmenu());
      crClrScr();
      DrawTitle();
      crPut(0,1,crColor(1),"Chess Server at "+ServerName+":"+ServerPort);
    }
    if(selected==2)
    {
      halloffamemenu();
      crClrScr();
      DrawTitle();
      crPut(0,1,crColor(1),"Chess Server at "+ServerName+":"+ServerPort);
    }
    if(selected==pc-1)
    {
      Sock.Destroy();
      break;
    }
    selected=-1;
  }
}

void connectmenu()
{
  crClrScr();
  DrawTitle();
  crPut(0,1,crColor(2),"Host:");
  crPut(0,2,crColor(2),"Port:");
  ServerName=crReadStr(6,1,crColor(1));
  int port=crReadInt(6,2,crColor(1));
  stringstream ss;
  ss<<port;
  ServerPort=ss.str();
  
  crPut(0,4,crColor(2),"Connecting...");
  crRefresh();
  
  if(!ConnectToServer(ServerName, port))
  {
    crPut(0,5,crColor(2),"Failed.");
    SingleMenu(0,7,"Back");
    return;
  }
  else
  {
    crPut(0,5,crColor(2),"Connected.");
    crPut(0,7,crColor(2),"Login:");
    crPut(0,8,crColor(2),"Password:");
    Login=crReadStr(7,7,crColor(1));
    string password=crReadPsw(10,8);
    Sock.WritePacket("L"+Login);
    Sock.WritePacket("P"+password);
    Sock.Send();
    crPut(0,10,crColor(2),"Waiting for Response...");

    string result;
    while(!Sock.ReadPacket(result))
    {
      int received=Sock.Recv();
      if(received==SOCKET_ERROR || received==0)
      {
        crPut(0,12,crColor(2),"Socket Error!");
        SingleMenu(0,13,"Back");
        return;
      }
    }
    if(result[0]=='%')
    {
      crPut(0,12,crColor(2),"Server is Full!");
      SingleMenu(0,13,"Back");
      return;
    }
    else if(result[0]=='A')
    {
      crPut(0,12,crColor(2),"Login Succesful.");
      SingleMenu(0,13,"Ok");
      servermenu();
    }
    else if(result[0]=='F')
    {
      crPut(0,12,crColor(2),"Wrong Password!");
      SingleMenu(0,13,"Back");
      return;
    }
    else if(result[0]=='T')
    {
      crPut(0,12,crColor(2),"User \""+Login+"\" is Already Connected to this Server!");
      SingleMenu(0,13,"Back");
      return;
    }
    else
    {
      crPut(0,12,crColor(2),"Packet Error!");
      ClearBuffers();
      SingleMenu(0,13,"Back");
      return;
    }
  }
}

void startservermenu()
{
  crClrScr();
  DrawTitle();
  crPut(0,1,crColor(2),"Start Server");
  crPut(0,3,crColor(2),"Port:");
  crPut(0,4,crColor(2),"MaxClients:");
  int port=crReadInt(6,3,crColor(1));
  int maxclnt=crReadInt(12,4,crColor(1));
  crClrScr();crGotoXY(0,0);crRefresh();
  if(!StartServer(port,maxclnt))
  {
    DrawTitle();
    crPut(0,6,crColor(2),"Failed to Start Server.");
    SingleMenu(0,7,"Back");
    return;
  }
}

void mainmenu()
{
  crClrScr();
  DrawTitle();
  int p=0;
  int pc=3;
  int selected=-1;
  while(true)
  {
    crPut(0,1,crColor(1),"  ");
    crPut(0,2,crColor(1),"  ");
    crPut(0,3,crColor(1),"  ");
    crPut(0,1+p,crColor(3),"->");
    crPut(2,1,(p==0)?crColor(2):crColor(1),"Connect to Server");
    crPut(2,2,(p==1)?crColor(2):crColor(1),"Start Server");
    crPut(2,3,(p==2)?crColor(2):crColor(1),"Exit");

    int c=crReadKey();
    if(c==KEY_DOWN)
      p++;
    if(c==KEY_UP)
      p--;
    if(c==KEY_ESC)
      selected=pc-1;
    if(c==KEY_ENTER)
      selected=p;
    p=(p+pc)%pc;

    if(selected==0)
    {
      connectmenu();
      crClrScr();
      DrawTitle();
    }
    if(selected==1)
    {
      startservermenu();
      crClrScr();
      DrawTitle();
    }
    if(selected==pc-1)
      break;
    selected=-1;
  }
}


void menu_cycle()
{
  crInit();
  crInitColor(1,COLOR_WHITE,COLOR_BLACK);
  crInitColor(2,COLOR_YELLOW,COLOR_BLACK);
  crInitColor(3,COLOR_MAROON,COLOR_BLACK);
  crInitColor(4,COLOR_OLIVE,COLOR_BLACK);
  crInitColor(5,COLOR_GRAY,COLOR_BLACK);
  crInitColor(6,COLOR_SILVER,COLOR_BLACK);
  crInitColor(7,COLOR_WHITE,COLOR_YELLOW);
  crInitColor(8,COLOR_MAROON,COLOR_YELLOW);
  crShowCursor(false);

  mainmenu();

  crClose();
}
