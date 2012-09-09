#ifndef __EVENT_H__
#define __EVENT_H__

#include <sstream>
#include <string>
#include <exception>

// ------------------------------------------------------------------------- //
// EventType
// ------------------------------------------------------------------------- //
enum EventType {
  EVENT_POS,
  EVENT_SKIPPOS,
  EVENT_RELPOS,
  EVENT_SETPOS,
  EVENT_SETMOVEDIR,
  EVENT_SPEED,
  EVENT_SETSPEED,
  EVENT_SETACCEL,
  EVENT_WIDTH,
  EVENT_WIDTHSPEED,
  EVENT_SETWIDTH,
  EVENT_SETCOLOR,
  EVENT_TURNSPEED,
  EVENT_SETTURNSPEED,
  EVENT_SETTURNACCEL,
  EVENT_SEGMENTATION,
  EVENT_TOUCH,
  EVENT_DIRSPEED,
  EVENT_SETDIRSPEED,
  EVENT_SETDIRACCEL,
  EVENT_LOOKAT,
  EVENT_LOOKDIR,
  EVENT_SETLOOKDIR,
  EVENT_UP,
  EVENT_SETUP,
  EVENT_DONTMOVE,
  EVENT_TIMEBARRIER,
  EVENT_BARRIERBASE,
  EVENT_SETBBASE,
  EVENT_NEXTSTREAM,
  EVENT_END
};


// ------------------------------------------------------------------------- //
// eventParseTable
// ------------------------------------------------------------------------- //
struct {
  int type;
  const char* name;
  int argc;
} eventParseTable[] = {
  {EVENT_POS,          "POS",          3},
  {EVENT_SKIPPOS,      "SKIPPOS",      3},
  {EVENT_RELPOS,       "RELPOS",       3},
  {EVENT_SETPOS,       "SETPOS",       3},
  {EVENT_SETMOVEDIR,   "SETMOVEDIR",   3},
  {EVENT_SPEED,        "SPEED",        1},
  {EVENT_SETSPEED,     "SETSPEED",     1},
  {EVENT_SETACCEL,     "SETACCEL",     1},
  {EVENT_SETWIDTH,     "SETWIDTH",     1},
  {EVENT_WIDTHSPEED,   "WIDTHSPEED",   1},
  {EVENT_WIDTH,        "WIDTH",        1},
  {EVENT_SETCOLOR,     "SETCOLOR",     3},
  {EVENT_TURNSPEED,    "TURNSPEED",    1},
  {EVENT_SETTURNSPEED, "SETTURNSPEED", 1},
  {EVENT_SETTURNACCEL, "SETTURNACCEL", 1},
  {EVENT_SEGMENTATION, "SEGMENTATION", 1},
  {EVENT_TOUCH,        "TOUCH",        1},
  {EVENT_DIRSPEED,     "DIRSPEED",     1},
  {EVENT_SETDIRSPEED,  "SETDIRSPEED",  1},
  {EVENT_SETDIRACCEL,  "SETDIRACCEL",  1},
  {EVENT_LOOKAT,       "LOOKAT",       3},
  {EVENT_LOOKDIR,      "LOOKDIR",      3},
  {EVENT_SETLOOKDIR,   "SETLOOKDIR",   3},
  {EVENT_SETUP,        "SETUP",        3},
  {EVENT_UP,           "UP",           3},
  {EVENT_DONTMOVE,     "DONTMOVE",     1},
  {EVENT_TIMEBARRIER,  "TIMEBARRIER",  1},
  {EVENT_BARRIERBASE,  "BARRIERBASE",  0},
  {EVENT_SETBBASE,     "SETBBASE",     1},
  {EVENT_NEXTSTREAM,   "NEXTSTREAM",   0},
  {EVENT_END,          "END",          0},
  {-1,                 NULL,           0}
};


// ------------------------------------------------------------------------- //
// Event
// ------------------------------------------------------------------------- //
class Event {
public:
  Event(int type) {
    mType = type;
    mSize = 0;
  }

  Event(int type, double value) {
    mType = type;
    mSize = 1;
    mValues[0] = value;
  }

  Event(int type, const arx::Vector3d& value) {
    mType = type;
    mSize = 3;
    mValues[0] = value[0];
    mValues[1] = value[1];
    mValues[2] = value[2];
  }

  Event(std::string s) {
    assert(s == boost::to_upper_copy(s));
    assert(s == boost::trim_copy(s));

    std::stringstream stream(s);
    std::string key;
    stream >> key;

    mType = -1;

    for(int i = 0; eventParseTable[i].type != -1; i++) {
      if(key == eventParseTable[i].name) {
        mType = eventParseTable[i].type;
        mSize = eventParseTable[i].argc;
        for(int j = 0; j < mSize; j++)
          stream >> mValues[j];
        break;
      }
    }

    if(mType == -1)
      throw std::runtime_error("Invalid event string: \"" + s + "\"");
  }

  int getType() const {
    return mType;
  }

  double getValue(int index) const {
    assert(index < mSize);
    return mValues[index];
  }

private:
  int mType;
  int mSize;
  arx::array<double, 3> mValues;
};


#endif

