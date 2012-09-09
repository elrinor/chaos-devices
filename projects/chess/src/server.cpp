#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <ios>
#include <map>
#include <list>
#include "mysocket.h"
#include "server.h"
#include "settings.h"
#include "game.h"
using namespace std;

TBufSocket ServerSocket;
int MaxClients;
int ClientCount;

struct TPlayerInfo
{
  string Password;
  string name;
  int victories;
  int defeats;
  int games;
  float rating;
};

int PlayerInfoCmp(const void* a, const void* b)
{
  if( ((TPlayerInfo*)a)->rating < ((TPlayerInfo*)b)->rating)
    return 1;
  else
    return -1;
}

ostream& operator<< (ostream& s, const TPlayerInfo& p) 
{ 
  return s<<p.name<<' '<<p.Password<<' '<<p.games<<' '<<p.victories<<' '<<p.defeats<<' '<<p.rating;
}

istream& operator>> (istream& s, TPlayerInfo& p)       
{ 
  return s>>p.name>>p.Password>>p.games>>p.victories>>p.defeats>>p.rating;
}

map<string,TPlayerInfo> Players;

struct TClient{
  TBufSocket sock;
  int state;   
  //0-login   
  //1-choose opponent  
  //2-game 
  //3-pchange step1 (old succ)
  //4-pchange step2 (new)
  string name;
  string NewPassword;
  map<string,int> opponent;
  string enemy;
  TClient* COpponent;
  //int request;
  bool destroy;
  TGame* game;
  int gamer;
  bool WantDraw;
};

vector<TClient*> Clients;

void LoadServerData()
{
  std::fstream f;
  f.open(DATAFILE,ios_base::in);
  string name, psw;
  TPlayerInfo pi;
  while(f>>pi)
    Players[pi.name]=pi;
  f.close();
}

void SaveServerData()
{
  std::fstream f;
  f.open(DATAFILE,ios_base::out);
  for(map<string,TPlayerInfo>::iterator i=Players.begin(); i!=Players.end(); i++)
    if(i->second.defeats>=0 && i->second.victories>=0 && i->second.games>=0 && i->second.name!="")
      f<<i->second<<'\n';
  f.close();
}

void ModifyRating(string name, int event, TGame* game)
{
  if(Players.find(name)==Players.end())
    return;
  TPlayerInfo& pi=Players[name];
  if(event==-1)
  {
    pi.games++;
    pi.defeats++;
    pi.rating-=1.0f/pi.defeats;
  }
  else if(event==0)
  {
    pi.games++;
    pi.rating-=0.2f/(pi.games-pi.victories);
  }
  else if(event==1)
  {
    pi.games++;
    pi.victories++;
    pi.rating+=1.0f/pi.victories;
  }
  SaveServerData();
}

int ServerMainLoop()
{
  TSocketSet SS;
  SS.AddRSocket(ServerSocket.Handle());

  TClient TempClient;

  while(true)
  {
    SS.Select();
    if(SS.IsRSet(ServerSocket.Handle()))
    {
      TClient* TempClient=new TClient;
      if(ServerSocket.Accept(TempClient->sock))
      {
        cout<<"New Client Connected!\n";
        TempClient->name="";
        TempClient->state=0;
        TempClient->opponent.clear();
        //TempClient->request=0;
        TempClient->game=0;
        TempClient->destroy=false;
        TempClient->WantDraw=false;
        SS.AddRSocket(TempClient->sock.Handle());
        ClientCount++;
        Clients.push_back(TempClient);
        if(ClientCount>MaxClients)
        {
          TempClient->sock.WritePacket("%");
          SS.AddWSocket(TempClient->sock.Handle());
          TempClient->destroy=true;
        }
      }
    }
    for(vector<TClient*>::reverse_iterator i=Clients.rbegin(); i!=Clients.rend(); i++) 
    {
      vector<TClient*>::iterator ri=i.base();
      ri--;
      if(SS.IsRSet((**i).sock.Handle()))
      {
        int received=(**i).sock.Recv();
        if(received==SOCKET_ERROR || received==0)
        {
          SS.DelWSocket((**i).sock.Handle());
          SS.DelRSocket((**i).sock.Handle());
          if((**i).game!=0) (**i).game->Kill();
          Clients.erase(ri);
          ClientCount--;
          continue;
        }
        else
        {
          string s;
          while((**i).sock.ReadPacket(s))
          {
            cout<<s<<"\n";
            char Type=s[0];
            string Msg=s.substr(1,s.size()-1);
            if(s[0]=='L')
              (**i).name=Msg;
            else if(s[0]=='P')
            {
              bool ProcessNext=true;
              for(vector<TClient*>::iterator j=Clients.begin(); j!=Clients.end(); j++)
                if((**j).name==(**i).name && (**j).state>0)
                {
                  (**i).sock.WritePacket("T");
                  (**i).destroy=true;
                  SS.AddWSocket((**i).sock.Handle());
                  ProcessNext=false;
                }
              if(ProcessNext)
              {
                if(Players.find((**i).name)==Players.end() && (**i).name!="")
                {
                  Players[(**i).name].Password=Msg;
                  Players[(**i).name].name=(**i).name;
                  Players[(**i).name].victories=0;
                  Players[(**i).name].defeats=0;
                  Players[(**i).name].games=0;
                  Players[(**i).name].rating=0;
                  SaveServerData();
                  (**i).sock.WritePacket("A");
                  SS.AddWSocket((**i).sock.Handle());
                  (**i).state=1;
                }
                else if(Players.find((**i).name)!=Players.end() && Players[(**i).name].Password==Msg && (**i).name!="")
                {
                  Players[(**i).name].Password=Msg;
                  SaveServerData();
                  (**i).sock.WritePacket("A");
                  SS.AddWSocket((**i).sock.Handle());
                  (**i).state=1;
                }
                else
                {
                  (**i).sock.WritePacket("F");
                  SS.AddWSocket((**i).sock.Handle());
                }
              }
            }
            else if(s[0]=='C')
            {
              if(Players[(**i).name].Password==Msg)
                (**i).state=3;
              else
              {
                (**i).sock.WritePacket("F");
                SS.AddWSocket((**i).sock.Handle());
              }
            }
            else if(s[0]=='1')
            {
              (**i).NewPassword=Msg;
            }
            else if(s[0]=='2')
            {
              if((**i).state==3)
              {
                if((**i).NewPassword!="" && (**i).NewPassword==Msg)
                {
                  (**i).sock.WritePacket("A");
                  SS.AddWSocket((**i).sock.Handle());
                  Players[(**i).name].Password=Msg;
                  SaveServerData();
                  (**i).state=1;
                }
                else
                {
                  (**i).sock.WritePacket("M");
                  SS.AddWSocket((**i).sock.Handle());
                }
              }
            }
            else if(s[0]=='H')
            {
              TPlayerInfo* SortedPlayers = new TPlayerInfo[Players.size()];
              int p=0;
              for(map<string,TPlayerInfo>::iterator j=Players.begin(); j!=Players.end(); j++)
              {
                SortedPlayers[p]=j->second;
                p++;
              }
              qsort(SortedPlayers,p,sizeof(TPlayerInfo),PlayerInfoCmp);
              for(int j=0; j<p; j++)
              {
                stringstream ss;
                ss<<SortedPlayers[j].name<<' ';
                if(SortedPlayers[j].rating>0.01 || SortedPlayers[j].rating<-0.01)
                  ss<<SortedPlayers[j].rating<<' ';
                else
                  ss<<0<<' ';
                ss<<SortedPlayers[j].games<<' '<<SortedPlayers[j].victories<<' '<<SortedPlayers[j].defeats;
                (**i).sock.WritePacket("H"+ss.str());
              }
              (**i).sock.WritePacket("E");
              SS.AddWSocket((**i).sock.Handle());
            }
            else if(s[0]=='D')
            {      
              for(vector<TClient*>::iterator j=Clients.begin(); j!=Clients.end(); j++)
              {
                if((**j).name=="")
                  continue;
                stringstream ss;
                ss<<(**j).name<<' '<<((**j).game!=0)<<' '<<(**i).opponent[(**j).name]<<' '<<(**j).opponent[(**i).name];
                (**i).sock.WritePacket("R"+ss.str());
              }
              (**i).sock.WritePacket("Z");
              SS.AddWSocket((**i).sock.Handle());
            }
            else if(s[0]=='G')
            {
              stringstream ss(s.substr(1,s.size()-1));
              string name;
              int request;
              ss>>name>>request;
              (**i).opponent[name]=request;
              //(**i).request=request;
              if(request>0)
              {
                for(vector<TClient*>::iterator j=Clients.begin(); j!=Clients.end(); j++) 
                  if((**j).opponent[(**i).name]==(**i).opponent[(**j).name] && (**i).name!=(**j).name && (**j).name==name)
                  {
                    TGame* NewGame = new TGame;
                    NewGame->SetGameTimeLimit(request*60);
                    (**i).WantDraw=false;
                    (**j).WantDraw=false;
                    (**i).game=NewGame;
                    (**i).gamer=0;
                    (**i).COpponent=*j;
                    (**j).game=NewGame;
                    (**j).gamer=1;
                    (**j).COpponent=*i;
                    ss.clear();
                    ss<<*NewGame;
                    (**i).sock.WritePacket("G");
                    (**i).sock.WritePacket("I 0 "+ss.str());
                    (**i).sock.WritePacket("S "+ss.str());
                    (**j).sock.WritePacket("G");
                    (**j).sock.WritePacket("I 1 "+ss.str());
                    (**i).enemy=(**j).name;
                    (**j).enemy=(**i).name;
                    (**i).opponent.clear();
                    (**j).opponent.clear();
                    SS.AddWSocket((**i).sock.Handle());
                    SS.AddWSocket((**j).sock.Handle());
                    break;
                  }
              }
            }
            else if((s[0]=='Y' || s[0]=='S' || s[0]=='X' || s[0]=='N' || s[0]=='O') && ((**i).game==0 || ((**i).game->IsDead())))
            {
              // game is dead - end game
              TClient* CurOpponent=(**i).COpponent;
              ModifyRating((**i).enemy,-1,(**i).game);
              ModifyRating((**i).name,1,(**i).game);
              if((**i).game!=0) {delete (**i).game;(**i).game=0;}
              (**i).sock.WritePacket("W Enemy Disconnected.");
              SS.AddWSocket((**i).sock.Handle());
            }
            else if((s[0]=='Y' || s[0]=='S' || s[0]=='X' || s[0]=='N' || s[0]=='O') && ((**i).game->TimeLeft(0)<=0 || (**i).game->TimeLeft(0)<=0))
            {
              TClient* CurOpponent=(**i).COpponent;
              if((**i).game->TimeLeft(CurOpponent->gamer)<=0)
              {
                // opponent TL
                ModifyRating((**i).enemy,-1,(**i).game);
                ModifyRating((**i).name,1,(**i).game);
                if((**i).game!=0) {delete (**i).game;(**i).game=0;CurOpponent->game=0;}
                (**i).sock.WritePacket("W");
                CurOpponent->sock.WritePacket("X Timelimit Hit!");
                SS.AddWSocket(CurOpponent->sock.Handle());
                SS.AddWSocket((**i).sock.Handle());
              }
              else
              {
                // i TL
                ModifyRating((**i).enemy,1,(**i).game);
                ModifyRating((**i).name,-1,(**i).game);
                if((**i).game!=0) {delete (**i).game;(**i).game=0;CurOpponent->game=0;}
                (**i).sock.WritePacket("X Timelimit Hit!");
                CurOpponent->sock.WritePacket("W");
                SS.AddWSocket(CurOpponent->sock.Handle());
                SS.AddWSocket((**i).sock.Handle());
              }
            }
            else if(s[0]=='S')
            {
              TClient* CurOpponent=(**i).COpponent;
              stringstream ss(s.substr(1,s.size()-1));
              int x1,y1,x2,y2;
              ss>>x1>>y1>>x2>>y2;
              (**i).game->TimeLeft(0);
              if((**i).game->MakeTurn((**i).gamer,x1,y1,x2,y2))
              {
                if((**i).game->PlayerAlive[0] && (**i).game->PlayerAlive[1])
                {
                  //everybody alive
                  ss.clear();
                  ss<<*((**i).game);
                  CurOpponent->sock.WritePacket("S "+ss.str());
                  SS.AddWSocket(CurOpponent->sock.Handle());
                }
                else
                {
                  if(!((**i).game->PlayerAlive[(**i).gamer]))
                  {
                    // i's dead - end game
                    ModifyRating((**i).enemy,1,(**i).game);
                    ModifyRating((**i).name,-1,(**i).game);
                    if((**i).game!=0) {delete (**i).game;(**i).game=0;CurOpponent->game=0;}
                    (**i).sock.WritePacket("X Your King was Killed!");
                    CurOpponent->sock.WritePacket("W");
                    SS.AddWSocket(CurOpponent->sock.Handle());
                    SS.AddWSocket((**i).sock.Handle());
                  }
                  else
                  {
                    //opponent's dead  - end game
                    ModifyRating((**i).enemy,-1,(**i).game);
                    ModifyRating((**i).name,1,(**i).game);
                    if((**i).game!=0) {delete (**i).game;(**i).game=0;CurOpponent->game=0;}
                    (**i).sock.WritePacket("W");
                    CurOpponent->sock.WritePacket("X Your King was Killed!");
                    SS.AddWSocket(CurOpponent->sock.Handle());
                    SS.AddWSocket((**i).sock.Handle());
                  }
                }
              }
              else
              {
                //invalid turn - send back
                ss.clear();
                ss<<*((**i).game);
                (**i).sock.WritePacket("S "+ss.str());
                SS.AddWSocket((**i).sock.Handle());
              }
            }
            else if(s[0]=='X')
            {
              // surrender - end game
              TClient* CurOpponent=(**i).COpponent;
              ModifyRating((**i).enemy,1,(**i).game);
              ModifyRating((**i).name,-1,(**i).game);
              if((**i).game!=0) {delete (**i).game;(**i).game=0;CurOpponent->game=0;}
              //(**i).sock.WritePacket("X");
              CurOpponent->sock.WritePacket("W Enemy Surrendered!");
              SS.AddWSocket(CurOpponent->sock.Handle());
              //SS.AddWSocket((**i).sock.Handle());
            }
            else if(s[0]=='N')
            {
              TClient* CurOpponent=(**i).COpponent;
              (**i).WantDraw=true;
              if((**i).WantDraw && CurOpponent->WantDraw)
              {
                // draw - end game
                ModifyRating((**i).enemy,0,(**i).game);
                ModifyRating((**i).name,0,(**i).game);
                if((**i).game!=0) {delete (**i).game;(**i).game=0;CurOpponent->game=0;}
                (**i).sock.WritePacket("N");
                CurOpponent->sock.WritePacket("N");
                SS.AddWSocket(CurOpponent->sock.Handle());
                SS.AddWSocket((**i).sock.Handle());
              }
              else
              {
                CurOpponent->sock.WritePacket("O I offer you a Draw.");
                SS.AddWSocket(CurOpponent->sock.Handle());
              }
            }
            else if(s[0]=='O')
            {
              TClient* CurOpponent=(**i).COpponent;
              CurOpponent->sock.WritePacket(s);
              SS.AddWSocket(CurOpponent->sock.Handle());
            }
          }
        } 
      }
      if(SS.IsWSet((**i).sock.Handle()))
      {
        if((**i).sock.Send()==SOCKET_ERROR)
        {
          SS.DelWSocket((**i).sock.Handle());
          SS.DelRSocket((**i).sock.Handle());
          if((**i).game!=0) (**i).game->Kill();
          Clients.erase(ri);
          ClientCount--;
        }
        else if((**i).sock.SendBufIsEmpty())
        {
          SS.DelWSocket((**i).sock.Handle());
          if((**i).destroy)
          {
            SS.DelRSocket((**i).sock.Handle());
            if((**i).game!=0) (**i).game->Kill();
            Clients.erase(ri);
            ClientCount--;
          }
        }
      }
    }
  }
}

bool StartServer(int port, int maxclients)
{
  LoadServerData();
  ClientCount=0;
  MaxClients=maxclients;
  if(!ServerSocket.Create())   return false;
  if(!ServerSocket.Bind(port)) return false;
  if(!ServerSocket.Listen())   return false;
  cout<<"Chess server started at localhost:"<<port<<" (MaxClients="<<maxclients<<") \n";
  ServerMainLoop();
  return true;
}






