#include <string>
#include <sstream>
#include <list>
#include "mycurses.h"
#include "game.h"
#include "client.h"
#include "menu.h"
#include "myutils.h"

using namespace std;

TGame* game;
bool OurTurn;
int MyNumber;
int state=0;
list<string> MessageList;

const char outchars[16][2] = {" ","p","R","N","B","Q","K"," "," ","p","R","N","B","Q","K"," "};
const char empty[81]= "                                                                                ";

void AddMessage(string s)
{
  MessageList.push_front(s);
  if(MessageList.size()>10)
    MessageList.pop_back();
}

void SendChatMessage()
{
  crShowCursor(true);
  crPut(0,14,crColor(1),"> ");
  string s=crReadStr(2,14,crColor(1));
  if(s!="")
  {
    AddMessage(Login+": "+s);
    Sock.WritePacket("O "+s);
    Sock.Send();
  }
  crPut(0,14,crColor(1),empty);
  crShowCursor(false);
}

void DrawMessages()
{
  int i=14;
  for(list<string>::iterator j=MessageList.begin(); j!=MessageList.end(); j++)
  {
    i++;
    crPut(0,i,crColor(1),empty);
    crPut(0,i,crColor(1),*j);
  }
}

void drawfield()
{
  for(int i=-1; i<=8; i++) for(int j=-1; j<=8; j++)
  {
    if(i==-1 || i==8 || j==-1 || j==8)
      crPut(1+i,3+j,crColor(1),"#");
    else
    {
      int color=1;
      if(game->BelongsToPlayer(0,game->FTable[i][j]))
        color=1;
      if(game->BelongsToPlayer(1,game->FTable[i][j]))
        color=3;
      if(game->FStepMask[i][j])
      {
        if(color==1) color=7;
        if(color==3) color=8;
      }
      if(MyNumber==1)
        crPut(1+i,3+j,crColor(color),outchars[game->FTable[i][j]]);
      else
        crPut(1+7-i,3+7-j,crColor(color),outchars[game->FTable[i][j]]);
    }
  }
}

void DrawMenu(bool WithCursor, int CursorPos)
{
  if(WithCursor)
  {
    crPut(11,2,crColor(1),"  ");
    crPut(11,3,crColor(1),"  ");
    crPut(11,4,crColor(1),"  ");
    crPut(11,5,crColor(1),"  ");
    crPut(11,2+CursorPos,crColor(3),"->");
    crPut(13,2,(CursorPos==0)?crColor(2):crColor(1),"Make a Turn");
    crPut(13,3,(CursorPos==1)?crColor(2):crColor(1),"Chat");
    crPut(13,4,(CursorPos==2)?crColor(2):crColor(1),"Offer a Draw");
    crPut(13,5,(CursorPos==3)?crColor(2):crColor(1),"Surrender (Leave)");
  }
  else
  {
    crPut(11,2,crColor(1),"  ");
    crPut(11,3,crColor(1),"  ");
    crPut(11,4,crColor(1),"  ");
    crPut(11,5,crColor(1),"  ");
    crPut(13,2,crColor(1),"Make a Turn");
    crPut(13,3,crColor(1),"Chat");
    crPut(13,4,crColor(1),"Offer a Draw");
    crPut(13,5,crColor(1),"Surrender (Leave)");
  }
}

void battlemenu()
{
  crClrScr();
  DrawTitle();
  int p=0;
  int pc=4;
  int selected=-1;
  int x=0,y=0,ox,oy;
  while(true)
  {
    drawfield();
    DrawMessages();
    if(state==0)
      crShowCursor(false);
    if(state==1 || state==2)
      crShowCursor(true);

    if(MyNumber==0)
    {
      crPut( 0,1,crColor(1),"Your Color:      .");
      crPut(12,1,crColor(1),"White");
    }
    else if(MyNumber==1)
    {
      crPut( 0,1,crColor(1),"Your Color:    .");
      crPut(12,1,crColor(3),"Red");
    }
    if(OurTurn)
      crPut(20,1,crColor(1),"It's your Turn.                     ");
    else
      crPut(20,1,crColor(1),"Waiting for Opponent...             ");
    if(!(game->IsTimeLimitSet()))
    {
      crPut(0,12,crColor(1),"Your Time: ?        ");
      crPut(20,12,crColor(1),"Opponent's Time: ?        ");
    }
    else
    {
      crPut(0,12,crColor(1),"Your Time: "+game->TimeLeftString(MyNumber)+"     ");
      crPut(20,12,crColor(1),"Opponent's Time: "+game->TimeLeftString(1-MyNumber)+"     ");
    }

    DrawMenu(true,p);

    int c;
    crDelayTime(100);
    if(state==1 || state==2)
    {
      if(MyNumber==1)
        crGotoXY(x+1,y+3);
      else
        crGotoXY(7-x+1,7-y+3);
    }
    c=crReadKey();
    crDelayTime(-1);

    if(state==0)
    {
      if(c==KEY_DOWN)
        p++;
      if(c==KEY_UP)
        p--;
      if(c==KEY_ENTER)
      {
        selected=p;
        if(p==0 && !OurTurn)
          selected=-1;
      }
      if(c==KEY_ESC)
        p=pc-1;
      p=(p+pc)%pc;
    }
    if(state==1 || state==2)
    {
      if(MyNumber==0)
      {
        if(c==KEY_DOWN)
          y--;
        if(c==KEY_UP)
          y++;
        if(c==KEY_LEFT)
          x++;
        if(c==KEY_RIGHT)
          x--;
      }
      else
      {
        if(c==KEY_DOWN)
          y++;
        if(c==KEY_UP)
          y--;
        if(c==KEY_LEFT)
          x--;
        if(c==KEY_RIGHT)
          x++;
      }
      if(x<0) x=0;
      if(y<0) y=0;
      if(x>7) x=7;
      if(y>7) y=7;
      if(state==1)
      {
        game->ClearStepMask();
        game->AddToStepMask(x,y);
      }
      if(c==KEY_ESC && state==1)
      {
        state=0;
      }
      if(c==KEY_ESC && state==2)
      {
        x=ox;
        y=oy;
        state=1;
      }
      if(c==KEY_ENTER && state==2)
      {
        if(game->MakeTurn(MyNumber,ox,oy,x,y))
        {
          Sock.WritePacket("S "+IntToStr(ox)+" "+IntToStr(oy)+" "+IntToStr(x)+" "+IntToStr(y));
          Sock.Send();
          state=0;
          OurTurn=false;
          game->ClearStepMask();
        }
        else
        {
          AddMessage("Wrong Move!");
        }
      }
      if(c==KEY_ENTER && state==1)
      {
        if(game->BelongsToPlayer(MyNumber,game->FTable[x][y]))
        {
          ox=x;
          oy=y;
          state=2;
        }
      }
    }

    string s;
    if(Sock.ReadPacket(s))
    {
      if(s[0]=='I')
      {
        stringstream ss(s.substr(1,s.size()-1));
        ss>>MyNumber>>*game;
      }
      else if(s[0]=='S')
      {
        stringstream ss(s.substr(1,s.size()-1));
        OurTurn=true;
        ss>>*game;
      }
      else if(s[0]=='O')
      {
        AddMessage(CurOpponent+":"+s.substr(1,s.size()-1));
      }
      else if(s[0]=='N')
      {
        DrawMenu(false,p);
        crPut(11,7,crColor(2),"Draw Game.         ");
        SingleMenu(11,8,"Leave");
        return;   
      }
      else if(s[0]=='W')
      {
        DrawMenu(false,p);
        crPut(11,7,crColor(2),"You Win! "+s.substr(1,s.size()-1));
        SingleMenu(11,8,"Leave");
        return;        
      }
      else if(s[0]=='X')
      {
        DrawMenu(false,p);
        crPut(11,7,crColor(2),"You Lose! "+s.substr(1,s.size()-1));
        SingleMenu(11,8,"Leave");
        return;        
      }
    }
    else if(Sock.CanRecv())
    {
      int received=Sock.Recv();
      if(received==SOCKET_ERROR || received==0)
      {
        DrawMenu(false,p);
        crPut(11,7,crColor(2),"Socket Error!     ");
        SingleMenu(11,8,"Leave");
        return;
      }
    }
    else if(game->TimeLeft(0)<=0 || game->TimeLeft(1)<=0)
    {
      Sock.WritePacket("Y");
      Sock.Send();
    }

    if(state==0)
    {
      if(selected==0)
      {
        state=1;
        selected=-1;
      }
      if(selected==1)
      {
        //chat
        SendChatMessage();
        selected=-1;
      }
      if(selected==2)
      {
        //draw
        Sock.WritePacket("N");
        Sock.Send();
        selected=-1;
      }
      if(selected==pc-1)
      {
        //surrender
        Sock.WritePacket("X");
        Sock.Send();
        return;         
      }
    }
  }
}

void battle()
{
  MyNumber=-1;
  MessageList.clear();
  OurTurn=false;
  game = new TGame();
  battlemenu();
  delete game;
}


  








