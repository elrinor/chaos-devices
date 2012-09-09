#ifndef __LINE_H__
#define __LINE_H__

#include "MovingObject.h"
#include "Utility.h"

// ------------------------------------------------------------------------- //
// Line
// ------------------------------------------------------------------------- //
class Line: public MovingObject<Line> {
public:
  Line() {
    mPoints.push_back(arx::Vector3d(0, 0, 0));
    mPoints.push_back(arx::Vector3d(0.1, 0.2, 0.3));
  }

  Line(std::istream& stream): MovingObject(stream) {
    mPoints.push_back(arx::Vector3d(0, 0, 0));
    mPoints.push_back(arx::Vector3d(0.1, 0.2, 0.3));
  }

  void draw(const arx::Vector3d& cam) {
    glBegin(GL_QUAD_STRIP);
    glColor(mColor);
    arx::Vector3d side = arx::Vector3d(0, 0, 0);

    for(unsigned int i = 2; i < mPoints.size() - 1; i++) {
      arx::Vector3d prevSide = side;
      side = (mPoints[i + 1] - mPoints[i]).cross(cam - mPoints[i]).normalized() * (mWidth * 0.5f);

      glVertex(mPoints[i] + side);
      glVertex(mPoints[i] - side);

      glVertex(mPoints[i + 1] + side);
      glVertex(mPoints[i + 1] - side);
    }
    glEnd();
  }

protected:
  friend class MovingObject<Line>;

  void processEvent(const Event& e) {
    switch(e.getType()) {
    case EVENT_SETWIDTH:
      mTargetWidth = mWidth = e.getValue(0);
      break;

    case EVENT_WIDTH:
      mTargetWidth = e.getValue(0);
      break;

    case EVENT_WIDTHSPEED:
      mWidthSpeed = e.getValue(0);
      break;

    case EVENT_SETCOLOR:
      mColor = arx::Vector3d(e.getValue(0), e.getValue(1), e.getValue(2));
      break;

    default:
      throw std::runtime_error("Invalid event type");
    }
  }

  void minorAdvance(double dt) {
    if(abs((mPoints[mPoints.size() - 1] - mPoints[mPoints.size() - 2]).normalized().dot((getPos() - mPoints[mPoints.size() - 1]).normalized()) - 1) < EPS) {
      mPoints[mPoints.size() - 1] = getPos();
    } else {
      mPoints.push_back(getPos());
    }
  }

  void majorAdvance(double dt) {
    advance(mWidth, mTargetWidth, mWidthSpeed, dt);
    return;
  }

private:
  arx::Vector3d mColor;
  double mWidth, mTargetWidth, mWidthSpeed;

  std::vector<arx::Vector3d> mPoints;
};


#endif