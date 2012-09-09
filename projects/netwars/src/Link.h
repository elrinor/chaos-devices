#ifndef __LINK_H__
#define __LINK_H__
#include "config.h"
#include <istream>
#include "Game.h"

// -------------------------------------------------------------------------- //
// Link
// -------------------------------------------------------------------------- //
class Link {
private:
  Game* game;
  int id;
  int targetId;
  int sourceId;
  bool removable;

  friend class DBase;
  friend class Game;

public:
  Link(int id, int sourceId, int targetId, bool removable): id(id), targetId(targetId), sourceId(sourceId), removable(removable) {}

  int getId() const { return this->id; }
  Unit* getTarget() const { return this->game->getUnit(this->targetId); }
  Unit* getSource() const { return this->game->getUnit(this->sourceId); }
  Unit* getOtherEnd(int unitId) const { return (unitId == targetId) ? (getSource()) : (getTarget()); }
  bool isRemovable() const { return this->removable; }


  static Link* deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;
};


#endif