#ifndef __GAMEMAP_H__
#define __GAMEMAP_H__
#include "config.h"
#include "Game.h"
#include "Matrix.h"

class GameMap {
private:
  Game* game;
  Matrix<int> unitIds;

  friend class Game;

public:
  GameMap(Game* game, int w, int h);
  Unit* getUnit(int x, int y) const;
  bool hasUnit(int x, int y) const;

  int getWidth() const;
  int getHeight() const;

  void ensureBounds(int x, int y) const;

  /*static GameMap* deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;*/
};


#endif