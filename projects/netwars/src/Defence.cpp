#include "config.h"
#include "Defence.h"
#include <exception>
#include "Util.h"
#include "Tool.h"

using namespace std;

DefenceClass* DefenceClass::deserialize(std::istream& stream) {
  int id;
  QString name, imageName;
  ::deserialize(stream, id, name, imageName);
  if(stream.fail())
    throw runtime_error("Failed to deserialize DefenceClass from stream");
  return new DefenceClass(id, name, imageName);
}

void DefenceClass::serialize(std::ostream& stream) const {
  ::serialize(stream, getId(), getName(), getImageName());
}

DefenceType* DefenceType::deserialize(std::istream& stream) {
  int defence, armor;
  int id, classId;
  ::deserialize(stream, id, classId, defence, armor);
  if(stream.fail())
    throw runtime_error("Failed to deserialize DefenceType from stream");
  return new DefenceType(id, classId, defence, armor);
}

void DefenceType::serialize(std::ostream& stream) const {
  ::serialize(stream, getId(), getClass()->getId(), getDefence(), getArmor());
}

Defence* DefenceType::spawnDefence(Game* target, int toolId) const {
	Defence* result = new Defence(INVALID_ID, getId(), toolId);
	target->addDefence(result);
	return result;
}

Defence* Defence::deserialize(std::istream& stream) {
	int id;
	int typeId;
	int toolId;
	::deserialize(stream, id, typeId, toolId);
	if(stream.fail())
		throw runtime_error("Failed to deserialize DefenceType from stream");
	return new Defence(id, typeId, toolId);
}

void Defence::serialize(std::ostream& stream) const {
	::serialize(stream, getId(), getType()->getId(), getTool()->getId());
}