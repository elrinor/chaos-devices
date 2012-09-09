#include "config.h"
#include "Link.h"
#include "Unit.h"
#include "Util.h"

using namespace std;

// -------------------------------------------------------------------------- //
// Link
// -------------------------------------------------------------------------- //
Link* Link::deserialize(std::istream& stream) {
  int id, sourceId, targetId;
  bool removable;
  ::deserialize(stream, id, sourceId, targetId, removable);
  if(stream.fail())
    throw runtime_error("Failed to deserialize Link from stream");
  return new Link(id, sourceId, targetId, removable);
}
 
void Link::serialize(std::ostream& stream) const {
  ::serialize(stream, getId(), getSource()->getId(), getTarget()->getId(), isRemovable());
} 

