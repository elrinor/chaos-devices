#ifndef _GAME
#define _GAME

#include <iostream>
#include <string>

template<class T> class Vector8
{
private:
  T FVector[8];
  T FBadValue;
  T FDefaultValue;
  bool FBad;
public:
  T& operator[] (int n) {
    if(n<0 || n>7 || FBad) {FBadValue=FDefaultValue;return FBadValue;}
    else                    return FVector[n];
  }
  const T& operator[] (int n) const {
    if(n<0 || n>7 || FBad)  return FDefaultValue;
    else                    return FVector[n];
  }
  Vector8(){FBad=false;}
  Vector8(bool isBad){FBad=isBad;}
  void SetDefaultValue(T DefaultValue) {FDefaultValue=DefaultValue;}
};

template<class T> class Array8x8
{
private:
  Vector8<T> FArray[8];
  Vector8<T> FBadVector;
  void SetDefaultValues(T DefaultValue)
  {
    FBadVector.SetDefaultValue(DefaultValue);
    for(int i=0; i<8; i++)
      FArray[i].SetDefaultValue(DefaultValue);
  }
public:
  Vector8<T>& operator[] (int n) {
    if(n<0 || n>7) return FBadVector;
    else           return FArray[n];
  }
  const Vector8<T>& operator[] (int n) const {
    if(n<0 || n>7) return FBadVector;
    else           return FArray[n];
  }
  Array8x8():FBadVector(true){SetDefaultValues(T());}
  Array8x8(T DefaultValue):FBadVector(true){SetDefaultValues(DefaultValue);}
};

class TGame
{
private:
  int FTimeLimitSec;
  int FTimeLeft[2];
  int FLastTime;
  int FTurnCount;
  bool FDead;
  void Turn(int x1, int y1, int x2, int y2);
public:
  int FFigureCount[2];
  bool PlayerAlive[2];
  int FCurrentPlayer;
  Array8x8<int> FTable;
  Array8x8<bool> FStepMask;
  bool BelongsToPlayer(int player, int figure);
  bool IsWhite(int figure){return BelongsToPlayer(1,figure);};
  bool IsBlack(int figure){return BelongsToPlayer(0,figure);};
  int GetPlayer(int figure);
  void ClearStepMask();
  void AddToStepMask(int x, int y);
  void SetGameTimeLimit(int TimeLimitSec) {FTimeLimitSec=TimeLimitSec;FTimeLeft[0]=FTimeLeft[1]=TimeLimitSec*1000;};
  int TimeLeft(int Player);
  std::string TimeLeftString(int Player);
  TGame();
  bool IsTurnValid(int Player, int x1, int y1, int x2, int y2);
  bool MakeTurn(int Player, int x1, int y1, int x2, int y2);
  friend std::istream& operator>> (std::istream& s, TGame& game);
  friend std::ostream& operator<< (std::ostream& s, const TGame& game);
  bool IsDead(){return FDead;}
  bool IsTimeLimitSet(){return FTimeLimitSec!=0;}
  void Kill(){FDead=true;}
};

#endif



