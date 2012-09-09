#include "config.h"
#include "Tool.h"
#include "Util.h"
#include "Unit.h"
#include "Defence.h"

using namespace std;

// -------------------------------------------------------------------------- //
// SoftwareClass
// -------------------------------------------------------------------------- //
ToolClass* ToolClass::deserialize(std::istream& stream) {
  int id;
  QString name;
  ::deserialize(stream, id, name);
  if(stream.fail())
    throw runtime_error("Failed to deserialize ToolClass from stream");
  return new ToolClass(id, name);
}

void ToolClass::serialize(std::ostream& stream) const {
  ::serialize(stream, getId(), getName());
}


// -------------------------------------------------------------------------- //
// ToolType
// -------------------------------------------------------------------------- //
ToolType* ToolType::deserialize(std::istream& stream) {
  int id;
  QString name, imageName;
  int maxHP, cpuProduction, memProduction, powerProduction;
  int classId, cost;
  arx::ArrayList<int> defenceTypeIds;
  ::deserialize(stream, id, name, imageName, cost, maxHP, cpuProduction, memProduction, powerProduction, classId, defenceTypeIds);
  if(stream.fail())
    throw runtime_error("Failed to deserialize ToolType from stream");
  return new ToolType(id, name, imageName, cost, maxHP, cpuProduction, memProduction, powerProduction, classId, defenceTypeIds);
}

void ToolType::serialize(std::ostream& stream) const {
  ::serialize(stream, getId(), getName(), getImageName(), getCost(), getMaxHP(), getCpuProduction(), getMemProduction(), getPowerProduction(), getClass()->getId(), this->defenceTypeIds);
}

Tool* ToolType::spawnTool(Game* target, int unitId, bool removable) const {
  Tool* result = new Tool(INVALID_ID, getMaxHP(), getId(), unitId, removable);
  target->addTool(result);

  for(int i = 0; i < getDefenceTypeSize(); i++)
    getDefenceType(i)->spawnDefence(target, result->getId());

  return result;
}

// -------------------------------------------------------------------------- //
// Tool
// -------------------------------------------------------------------------- //
Tool* Tool::deserialize(std::istream& stream) {
  int id, hp, typeId, unitId;
  bool removable;
  ::deserialize(stream, id, hp, typeId, unitId, removable);
  if(stream.fail())
    throw runtime_error("Failed to deserialize Tool from stream");
  return new Tool(id, hp, typeId, unitId, removable);
}

void Tool::serialize(std::ostream& stream) const {
  ::serialize(stream, getId(), getHP(), getType()->getId(), getUnit()->getId(), isRemovable());
}