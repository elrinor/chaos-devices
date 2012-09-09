#include "config.h"
#include "GameMap.h"
#include <exception>
#include <iostream>
#include <cassert>
#include "Util.h"

using namespace std;

GameMap::GameMap(Game* game, int w, int h): unitIds(w, h, INVALID_ID) {
  this->game = game;
}

void GameMap::ensureBounds(int x, int y) const {
  assert(x >= 0 && x < this->unitIds.sizeX() && y >= 0 && y < this->unitIds.sizeY());
}

Unit* GameMap::getUnit(int x, int y) const {
  ensureBounds(x, y);
  return this->game->getUnit(this->unitIds[x][y]);
}

bool GameMap::hasUnit(int x, int y) const {
  ensureBounds(x, y);
  return (this->unitIds[x][y] != INVALID_ID);
}

/*GameMap* GameMap::deserialize(std::istream& stream) {
  int w, h;
  ::deserialize(stream, w, h);
  GameMap* result = new GameMap(w, h);
  for(int x = 0; x < w; x++)
    for(int y = 0; y < h; y++)
      ::deserialize(stream, result->unitIds[x][y]);
  return result;
}

void GameMap::serialize(std::ostream& stream) const {
  ::serialize(stream, getWidth(), getHeight());
  for(int x = 0; x < this->unitIds.sizeX(); x++)
    for(int y = 0; y < this->unitIds.sizeY(); y++)
      ::serialize(stream, this->unitIds[x][y]);
}*/

int GameMap::getWidth() const {
  return this->unitIds.sizeX();
}

int GameMap::getHeight() const {
  return this->unitIds.sizeY();
}
