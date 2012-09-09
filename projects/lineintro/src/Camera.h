#ifndef __CAMERA_H__
#define __CAMERA_H__

#include "MovingObject.h"

// ------------------------------------------------------------------------- //
// Camera
// ------------------------------------------------------------------------- //
class Camera: public MovingObject<Camera> {
public:
  Camera(std::istream& stream): MovingObject(stream) {
  }

  arx::Vector3d getDir() const {
    return mLookDir;
  }

  arx::Vector3d getUp() const {
    return mUp;
  }

protected:
  friend class MovingObject<Camera>;

  void processEvent(const Event& e) {
    switch(e.getType()) {
      case EVENT_DIRSPEED:
        mTargetLookDirSpeed = e.getValue(0);
        break;

      case EVENT_SETDIRSPEED:
        mTargetLookDirSpeed = mLookDirSpeed = e.getValue(0);
        break;

      case EVENT_SETDIRACCEL:
        mLookDirAccel = e.getValue(0);
        break;

      case EVENT_LOOKAT:
        mLookAt = arx::Vector3d(e.getValue(0), e.getValue(1), e.getValue(2));
        mDoLookAt = true;
        break;

      case EVENT_LOOKDIR:
        mTargetLookDir = arx::Vector3d(e.getValue(0), e.getValue(1), e.getValue(2));
        mDoLookAt = false;
        break;

      case EVENT_SETLOOKDIR:
        mTargetLookDir = mLookDir = arx::Vector3d(e.getValue(0), e.getValue(1), e.getValue(2));
        mDoLookAt = false;
        break;

      case EVENT_UP:
        mTargetUp = arx::Vector3d(e.getValue(0), e.getValue(1), e.getValue(2)).normalized();
        break;

      case EVENT_SETUP:
        mTargetUp = mUp = arx::Vector3d(e.getValue(0), e.getValue(1), e.getValue(2)).normalized();
        break;

      default:
        throw std::runtime_error("Invalid event type");
    }
  }

  void minorAdvance(double dt) {
    return;
  }

  void majorAdvance(double dt) {
    if(mDoLookAt) {
      advance(mLookDir, (mLookAt - getPos()).normalized(), mLookDirSpeed, dt);
      mLookDir.normalize();
    } else {
      advance(mLookDir, mTargetLookDir, mLookDirSpeed, dt);
      mLookDir.normalize();
    }

    advance(mUp, mTargetUp, mLookDirSpeed, dt);
    mUp.normalize();

    advance(mLookDirSpeed, mTargetLookDirSpeed, mLookDirAccel, dt);
  }

private:
  arx::Vector3d mLookDir;
  arx::Vector3d mTargetLookDir;
  arx::Vector3d mLookAt;
  arx::Vector3d mUp;
  arx::Vector3d mTargetUp;
  bool mDoLookAt;
  double mLookDirSpeed;
  double mTargetLookDirSpeed;
  double mLookDirAccel;
};

#endif
