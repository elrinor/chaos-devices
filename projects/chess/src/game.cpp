#include <iostream>
#include <string>
#include "game.h"
#include "myutils.h"
using namespace std;

TGame::TGame():FTable(-1),FStepMask(false)
{
  ClearStepMask();
  FTurnCount=0;
  for(int i=0; i<8; i++) for(int j=0; j<8; j++)
    FTable[i][j]=0;
  
  FTable[0][0]=FTable[7][0]=2;
  FTable[1][0]=FTable[6][0]=3;
  FTable[2][0]=FTable[5][0]=4;
  FTable[3][0]=5;
  FTable[4][0]=6;
  FTable[0][7]=FTable[7][7]=2+8;
  FTable[1][7]=FTable[6][7]=3+8;
  FTable[2][7]=FTable[5][7]=4+8;
  FTable[3][7]=5+8;
  FTable[4][7]=6+8;
  for(int i=0; i<8; i++)
  {
    FTable[i][1]=1;
    FTable[i][6]=1+8;
  }
  
  FLastTime=GetTime();
  FTimeLimitSec=0;
  FTimeLeft[0]=0;
  FTimeLeft[1]=0;
  FDead=false;
  FCurrentPlayer=0;
  FFigureCount[0]=16;
  FFigureCount[1]=16;  
  PlayerAlive[0]=true;
  PlayerAlive[1]=true;
}

bool TGame::BelongsToPlayer(int player, int figure)
{
  if(player==0)
  {
    if(figure>0 && figure<7)
      return true;
    else
      return false;
  }
  else if(player==1)
  {
    if(figure>8 && figure<15)
      return true;
    else
      return false;
  }
  else
    return false;
}

int TGame::GetPlayer(int figure)
{
  if(BelongsToPlayer(0,figure))
    return 0;
  else if(BelongsToPlayer(1,figure))
    return 1;
  else
    return -1;
}

void TGame::ClearStepMask()
{
  for(int i=0; i<8; i++) for(int j=0; j<8; j++)
    FStepMask[i][j]=false;
}

void TGame::AddToStepMask(int x, int y)
{
  int player=GetPlayer(FTable[x][y]);
  int opponent=1-player;
  if(player==-1)
    return;
  if(FTable[x][y]==1)
  {
    if(FTable[x][y+1]==0)
      FStepMask[x][y+1]=true;
    if(FTurnCount<2 && FTable[x][y+2]==0)
      FStepMask[x][y+2]=true;
    if(GetPlayer(FTable[x+1][y+1])==opponent)
      FStepMask[x+1][y+1]=true;
    if(GetPlayer(FTable[x-1][y+1])==opponent)
      FStepMask[x-1][y+1]=true;
  }
  if(FTable[x][y]==9)
  {
    if(FTable[x][y-1]==0)
      FStepMask[x][y-1]=true;
    if(FTurnCount<2 && FTable[x][y-2]==0)
      FStepMask[x][y-2]=true;
    if(GetPlayer(FTable[x+1][y-1])==opponent)
      FStepMask[x+1][y-1]=true;
    if(GetPlayer(FTable[x-1][y-1])==opponent)
      FStepMask[x-1][y-1]=true;
  }
  if(FTable[x][y]==2 || FTable[x][y]==10 || FTable[x][y]==5 || FTable[x][y]==13)
  {
    int x1,y1;
    x1=x;y1=y;
    while(true) {
      x1++;if(FTable[x1][y1]==0) FStepMask[x1][y1]=true;  
      else if(GetPlayer(FTable[x1][y1])==opponent) {FStepMask[x1][y1]=true;break;}
      else break;}
    x1=x;y1=y;
    while(true) {
      x1--;if(FTable[x1][y1]==0) FStepMask[x1][y1]=true;  
      else if(GetPlayer(FTable[x1][y1])==opponent) {FStepMask[x1][y1]=true;break;}
      else break;}
    x1=x;y1=y;
    while(true) {
      y1++;if(FTable[x1][y1]==0) FStepMask[x1][y1]=true;  
      else if(GetPlayer(FTable[x1][y1])==opponent) {FStepMask[x1][y1]=true;break;}
      else break;}
    x1=x;y1=y;
    while(true) {
      y1--;if(FTable[x1][y1]==0) FStepMask[x1][y1]=true;  
      else if(GetPlayer(FTable[x1][y1])==opponent) {FStepMask[x1][y1]=true;break;}
      else break;}
  }
  if(FTable[x][y]==4 || FTable[x][y]==12 || FTable[x][y]==5 || FTable[x][y]==13)
  {
    int x1,y1;
    x1=x;y1=y;
    while(true) {
      x1++;y1++;if(FTable[x1][y1]==0) FStepMask[x1][y1]=true;  
      else if(GetPlayer(FTable[x1][y1])==opponent) {FStepMask[x1][y1]=true;break;}
      else break;}
    x1=x;y1=y;
    while(true) {
      x1--;y1++;if(FTable[x1][y1]==0) FStepMask[x1][y1]=true;  
      else if(GetPlayer(FTable[x1][y1])==opponent) {FStepMask[x1][y1]=true;break;}
      else break;}
    x1=x;y1=y;
    while(true) {
      x1++;y1--;if(FTable[x1][y1]==0) FStepMask[x1][y1]=true;  
      else if(GetPlayer(FTable[x1][y1])==opponent) {FStepMask[x1][y1]=true;break;}
      else break;}
    x1=x;y1=y;
    while(true) {
      x1--;y1--;if(FTable[x1][y1]==0) FStepMask[x1][y1]=true;  
      else if(GetPlayer(FTable[x1][y1])==opponent) {FStepMask[x1][y1]=true;break;}
      else break;}
  }
  if(FTable[x][y]==6 || FTable[x][y]==14)
  {
    int x1,y1;
    x1=x+1;y1=y+1;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
    x1=x+1;y1=y  ;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
    x1=x+1;y1=y-1;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
    x1=x  ;y1=y+1;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
    x1=x  ;y1=y-1;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
    x1=x-1;y1=y+1;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
    x1=x-1;y1=y  ;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
    x1=x-1;y1=y-1;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
  }
  if(FTable[x][y]==3 || FTable[x][y]==11)
  {
    int x1,y1;
    x1=x+2;y1=y+1;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
    x1=x+2;y1=y-1;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
    x1=x+1;y1=y+2;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
    x1=x+1;y1=y-2;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
    x1=x-1;y1=y+2;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
    x1=x-1;y1=y-2;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
    x1=x-2;y1=y+1;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
    x1=x-2;y1=y-1;if(FTable[x1][y1]==0 || GetPlayer(FTable[x1][y1])==opponent) FStepMask[x1][y1]=true;
  }
}

bool TGame::IsTurnValid(int Player, int x1, int y1, int x2, int y2)
{
  if(GetPlayer(FTable[x1][y1])!=Player)
    return false;
  if(GetPlayer(FTable[x2][y2])==Player)
    return false;
  ClearStepMask();
  AddToStepMask(x1,y1);
  return FStepMask[x2][y2];
}

void TGame::Turn(int x1, int y1, int x2, int y2)
{
  if(FTable[x1][y1]==2 || FTable[x1][y1]==3 || FTable[x1][y1]==4 || FTable[x1][y1]==5 || FTable[x1][y1]==6 ||
    FTable[x1][y1]==10 || FTable[x1][y1]==11 || FTable[x1][y1]==12 || FTable[x1][y1]==13 || FTable[x1][y1]==14 ||
    FTable[x1][y1]==1 || FTable[x1][y1]==9)
  {
    if(BelongsToPlayer(0,FTable[x2][y2]))
    {
      FFigureCount[0]--;
      if(FTable[x2][y2]==6)
        PlayerAlive[0]=false;
    }
    if(BelongsToPlayer(1,FTable[x2][y2]))
    {
      FFigureCount[1]--;
      if(FTable[x2][y2]==14)
        PlayerAlive[1]=false;
    }
    FTable[x2][y2]=FTable[x1][y1];
    FTable[x1][y1]=0;
  }
  FCurrentPlayer=1-FCurrentPlayer;
  FTurnCount++;
}

bool TGame::MakeTurn(int Player, int x1, int y1, int x2, int y2)
{
  if(IsTurnValid(Player,x1,y1,x2,y2))
  {
    Turn(x1,y1,x2,y2);
    return true;
  }
  else
    return false;
}

int TGame::TimeLeft(int Player)
{
  FTimeLeft[FCurrentPlayer]-=GetTime()-FLastTime;
  FLastTime=GetTime();
  return max(FTimeLeft[Player],0);
}

string TGame::TimeLeftString(int Player)
{
  string sec=IntToStr((TimeLeft(Player)/1000)%60);
  if(sec.size()==1)
    sec='0'+sec;
  return IntToStr(TimeLeft(Player)/60000)+':'+sec;
}

istream& operator>> (istream& s, TGame& game)
{
  game.FLastTime=GetTime();
  for(int i=0; i<8; i++) for(int j=0; j<8; j++)
    s>>game.FTable[i][j];
  return s>>game.FTimeLimitSec>>game.FTurnCount>>game.FTimeLeft[0]>>game.FTimeLeft[1]>>game.FCurrentPlayer;
}

ostream& operator<< (ostream& s, const TGame& game)
{
  for(int i=0; i<8; i++) for(int j=0; j<8; j++)
    s<<game.FTable[i][j]<<' ';
  return s<<game.FTimeLimitSec<<' '<<game.FTurnCount<<' '<<game.FTimeLeft[0]<<' '<<game.FTimeLeft[1]<<' '<<game.FCurrentPlayer;
}






