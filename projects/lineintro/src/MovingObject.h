#ifndef __MOVINGOBJECT_H__
#define __MOVINGOBJECT_H__

#include <istream>
#include <boost/algorithm/string.hpp>
#include "Event.h"
#include "Utility.h"

#ifdef max
#  undef max
#endif

#ifdef min
#  undef min
#endif

// ------------------------------------------------------------------------- //
// MovingObject
// ------------------------------------------------------------------------- //
template<class Derived>
class MovingObject: public arx::noncopyable {
private:
  class EventStream;

public:
  MovingObject() {
    addEventStream();
    initialize();
  }

  MovingObject(std::istream& stream) {
    addEventStream();
    while(true) {
      std::string s;
      getline(stream, s);
      boost::trim(s);
      boost::to_upper(s);

      if(stream.eof() || stream.fail())
        break;

      if(s.empty())
        continue;

      if(s[0] == '#')
        continue;

      Event e(s);
      if(e.getType() == EVENT_NEXTSTREAM) {
        addEventStream();
        continue;
      }
      if(e.getType() == EVENT_END)
        break;
      addEvent(e);
    }

    initialize();
    for(unsigned int i = 0; i < mEventStreams.size(); i++)
      crunchEvents(mEventStreams[i], 0);
  }

  void addEventStream() {
    mEventStreams.push_back(EventStream());
  }

  void addEvent(const Event& e) {
    mEventStreams.back().mEvents.push_back(e);
  }

  void addMoveEvent(const Event& e) {
    mEventStreams.front().mEvents.push_back(e);
  }

  void advance(const double t, const double pdt) {
    for(unsigned int i = 0; i < mEventStreams.size(); i++) {
      double dt = pdt;
      EventStream& s = mEventStreams[i];

      if(s.mNextEventIndex == 0)
        crunchEvents(s, t);

      bool inBarrier = false;
      double postBarrier;

      while(true) {
        if(s.mWait > dt) {
          s.mWait -= dt;
          dt = 0;
          break;
        } else {
          dt -= s.mWait;
          s.mWait = 0;
        }

        if(s.getCurrentEvent().getType() == EVENT_TIMEBARRIER) {
          if(!inBarrier) {
            if(t - s.mTimeBarrierBase > s.mTimeBarrier) {
              inBarrier = true;
              postBarrier = t - (s.mTimeBarrier + s.mTimeBarrierBase);
              dt = dt - postBarrier;
            }
          } else {
            inBarrier = false;
            dt = postBarrier;
            crunchEvents(s, t - dt);
            continue;
          }
        }

        if(dt < EPS && !inBarrier)
          break;

        if(i == 0) {
          if((mPos - mTarget).squaredNorm() < arx::sqr(mTouch)) {
            dt = 0;
            
            Event& e = s.getCurrentEvent();
            if(e.getType() == EVENT_POS && (mTarget - arx::Vector3d(e.getValue(0), e.getValue(1), e.getValue(2))).squaredNorm() < EPS)
              crunchEvents(s, t - dt);
          } else {
            while(true) {
              double dtSpent = advance(mPos, mPos + mDir * mSegmentation, mSpeed, dt);
              advance(mDir, (mTarget - mPos).normalized(), mTurnSpeed, dtSpent);
              mDir.normalize();
              dt -= dtSpent;

              derived().minorAdvance(dtSpent);

              if((mPos - mTarget).squaredNorm() < arx::sqr(mTouch)) {
                crunchEvents(s, t - dt);
                break;
              }

              if(dt < EPS)
                break;
            }
          }
        } else
          dt = 0;
      }
    }

    advance(mSpeed, mTargetSpeed, mAccel, pdt);
    advance(mTurnSpeed, mTargetTurnSpeed, mTurnAccel, pdt);
    derived().majorAdvance(pdt);
  }

  arx::Vector3d getPos() const {
    return mPos;
  }

  arx::Vector3d getDir() const {
    return mDir;
  }

  void crunchEvents(EventStream& s, double t) {
    while(s.getNextEvent().getType() != EVENT_POS && s.getNextEvent().getType() != EVENT_TIMEBARRIER)
      processNextEvent(s, t);
    processNextEvent(s, t);
  }


protected:
  void initialize() {
    mDir = arx::Vector3d(0, 0, 1);
    mTargetSpeed = mSpeed = 1;
    mAccel = 0;
    mTargetTurnSpeed = mTurnSpeed = 1;
    mTurnAccel = 0;
    mSegmentation = mTouch = 1;
  }

  double advance(double& current, double target, double change, double dt, double maxChange = std::numeric_limits<double>::max()) {
    if(std::abs(current - target) > EPS) {
      double direction = (target - current >= 0) ? 1 : -1;
      double realChange = std::min(std::min(std::abs(target - current), change * dt), maxChange);
      current += realChange * direction;
      return realChange / change;
    }
    return 0.0f;
  }

  double advance(arx::Vector3d& current, const arx::Vector3d& target, double change, double dt, double maxChange = std::numeric_limits<double>::max()) {
    if((current - target).squaredNorm() > EPS) {
      arx::Vector3d direction = (target - current).normalized();
      double realChange = std::min(std::min((target - current).norm(), change * dt), maxChange);
      current += realChange * direction;
      return realChange / change;
    }
    return 0.0f;
  }

  void processNextEvent(EventStream& s, double t) {
    Event& e = s.getNextEvent();
    s.mNextEventIndex++;
    switch(e.getType()) {
    case EVENT_POS:
      mTarget = arx::Vector3d(e.getValue(0), e.getValue(1), e.getValue(2));
      break;

    case EVENT_SKIPPOS:
      mTarget = arx::Vector3d(e.getValue(0), e.getValue(1), e.getValue(2));
      break;

    case EVENT_RELPOS:
      mTarget = mPos + arx::Vector3d(e.getValue(0), e.getValue(1), e.getValue(2));
      break;

    case EVENT_SETPOS:
      mPos = arx::Vector3d(e.getValue(0), e.getValue(1), e.getValue(2));
      break;

    case EVENT_SETMOVEDIR:
      mDir = arx::Vector3d(e.getValue(0), e.getValue(1), e.getValue(2)).normalized();
      break;

    case EVENT_SPEED:
      mTargetSpeed = e.getValue(0);
      break;

    case EVENT_SETSPEED:
      mTargetSpeed = mSpeed = e.getValue(0);
      break;

    case EVENT_SETACCEL:
      mAccel = e.getValue(0);
      break;

    case EVENT_TURNSPEED:
      mTargetTurnSpeed = e.getValue(0);
      break;

    case EVENT_SETTURNSPEED:
      mTargetTurnSpeed = mTurnSpeed = e.getValue(0);
      break;

    case EVENT_SETTURNACCEL:
      mTurnAccel = e.getValue(0);
      break;

    case EVENT_SEGMENTATION:
      mSegmentation = e.getValue(0);
      mTouch = std::max(mTouch, mSegmentation);
      break;

    case EVENT_TOUCH:
      mTouch = e.getValue(0);
      mSegmentation = std::min(mTouch, mSegmentation);
      break;

    case EVENT_DONTMOVE:
      s.mWait = e.getValue(0);
      break;

    case EVENT_TIMEBARRIER:
      s.mTimeBarrier = e.getValue(0);
      break;

    case EVENT_BARRIERBASE:
      s.mTimeBarrierBase = t;
      break;

    case EVENT_SETBBASE:
      s.mTimeBarrierBase = e.getValue(0);
      break;

    default:
      derived().processEvent(e);
    }
  }

private:
  const Derived& derived() const {
    return *static_cast<const Derived*>(this);
  }

  Derived& derived() {
    return *static_cast<Derived*>(this);
  }

  arx::Vector3d mTarget;
  arx::Vector3d mPos;
  arx::Vector3d mDir;
  double mSpeed;
  double mTargetSpeed;
  double mAccel;
  double mTurnSpeed;
  double mTargetTurnSpeed;
  double mTurnAccel;
  double mSegmentation;
  double mTouch;

  class EventStream {
  public:
    std::vector<Event> mEvents;
    int mNextEventIndex;
    double mTimeBarrier;
    double mTimeBarrierBase;
    double mWait;

    EventStream() {
      mNextEventIndex = 0;
      mTimeBarrier = 0;
      mTimeBarrierBase = 0;
      mWait = 0;
    }

    Event& getNextEvent() {
      if(mNextEventIndex >= (int) mEvents.size())
        return mEvents.back();
      else
        return mEvents[mNextEventIndex];
    }

    Event& getCurrentEvent() {
      if(mNextEventIndex > (int) mEvents.size())
        return mEvents.back();
      else
        return mEvents[mNextEventIndex - 1];
    }
  };

  std::vector<EventStream> mEventStreams;
};


#endif
