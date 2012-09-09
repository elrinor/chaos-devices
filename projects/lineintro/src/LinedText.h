#ifndef __LINEDTEXT_H__
#define __LINEDTEXT_H__

#include <arx/Image.h>
#include "Line.h"

class LinedText: private arx::noncopyable {
public:
  LinedText(arx::Image3f image, double startTime, const arx::Matrix4d& imageToWorld) {
    mImage = image;
    mStartTime = startTime;
    mImageToWorld = imageToWorld * scale(1.0f / mImage.getWidth(), 1.0f / mImage.getHeight(), 1);
    mWorldToImage = mImageToWorld.inverse();
    mCenter = transform3(arx::Vector3d(mImage.getWidth() / 2.0f, mImage.getHeight() / 2.0f, 0.5f), mImageToWorld);
  }

  void addLine(const arx::Vector3d& cam) {
    Line* line = new Line();

    arx::Vector3d startDir = (transform3(arx::Vector3d(random(0, mImage.getWidth()), random(0, mImage.getHeight()), random(0, 1)), mImageToWorld) - mCenter).normalized();
    line->addEvent(Event(EVENT_BARRIERBASE));
    line->addEvent(Event(EVENT_TIMEBARRIER, mStartTime));
    line->addEvent(Event(EVENT_SETPOS, mCenter - startDir * 50));
    line->addEvent(Event(EVENT_SETMOVEDIR, startDir));
    line->addEvent(Event(EVENT_SETSPEED, 500));
    line->addEvent(Event(EVENT_SETTURNSPEED, 3500));
    line->addEvent(Event(EVENT_SEGMENTATION, 0.1));
    line->addEvent(Event(EVENT_TOUCH, 0.1));
    line->addEvent(Event(EVENT_SETCOLOR, arx::Vector3d(0.8 + random(0, 0.2), 0, 0)));

    double x, y;
    do {
      static int index = 0;
      if(index < 2) {
        x = 770;
        y = 230;
      } else if(index < 4) {
        x = 690;
        y = 420;
      } else if(index < 6) {
        x = 450;
        y = 160;
      } else if(index < 8) {
        x = 460;
        y = 370;
      } else if(index < 10) {
        x = 930;
        y = 150;
      } else if(index < 12) {
        x = 230;
        y = 120;
      } else if(index < 14) {
        x = 300;
        y = 70;
      } else if(index < 16) {
        x = 630;
        y = 440;
      } else if(index < 18) {
        x = 990;
        y = 160;
      } else if(index < 22) {
        x = 550;
        y = 250;
      } else if(index < 23) {
        x = 90;
        y = 70;
      } else if(index < 24) {
        x = 90;
        y = 270;
      } else if(index < 25) {
        x = 680;
        y = 230;
      } else {
        x = random(0, mImage.getWidth());
        y = random(0, mImage.getHeight());
      }
      index++;
    } while (mImage.getPixel(x, y).r == 0.0f);

    arx::Vector3d pos = transform3(arx::Vector3d(x, y, random(0, 1)), mImageToWorld);
    for(int i = 0; i < 400; i++) {
      line->addEvent(Event(EVENT_POS, pos));

      arx::Vector3d imgPos;
      arx::Vector3d imgDir;
      arx::Vector3d imgTry;
      arx::Vector3d dir;
      do {
        dir = randomDirection();
        imgPos = transform3(pos, mWorldToImage);
        imgDir = (transform3(pos + dir, mWorldToImage) - imgPos).normalized();
        imgTry = imgPos + imgDir * 4;
      } while(mImage.getPixel(imgTry[0], imgTry[1]).r == 0.0f || imgTry[2] > 1.0f || imgTry[2] < 0.0f);

      do {
        imgPos += imgDir;
      } while(imgPos[2] >= 0 && imgPos[2] <= 1 && mImage.getPixel(imgPos[0], imgPos[1]).r != 0.0f);
      
      pos = transform3(imgPos, mImageToWorld);
    }
    line->addEvent(Event(EVENT_POS, mCenter - (startDir * 5 + randomDirection()) * 10));
    line->addEvent(Event(EVENT_TIMEBARRIER, 10000));

    line->addEventStream();
    line->addEvent(Event(EVENT_BARRIERBASE));
    line->addEvent(Event(EVENT_WIDTHSPEED, 1.0));
    for(int i = 0; i < 40; i++) {
      line->addEvent(Event(EVENT_TIMEBARRIER, mStartTime + i * 0.5));
      line->addEvent(Event(EVENT_SETWIDTH, 0.2));
      line->addEvent(Event(EVENT_WIDTH, 0.03));
    }
    line->addEvent(Event(EVENT_TIMEBARRIER, 10000));

    mLines.push_back(line);
  }

  void advance(double t, double dt) {
    for(unsigned int i = 0; i < mLines.size(); i++)
      mLines[i]->advance(t, dt);
  }

  void draw(const arx::Vector3d& camPos, const arx::Vector3d& camDir) {
    if((mCenter - camPos).dot(camDir) < 0 && (camPos - mCenter).norm() > 10)
      return;

    for(unsigned int i = 0; i < mLines.size(); i++)
      mLines[i]->draw(camPos);
  }

private:
  arx::Image3f mImage;
  arx::Matrix4d mWorldToImage, mImageToWorld;
  arx::Vector3d mCenter;
  double mStartTime;
  std::vector<Line*> mLines;
};



#endif
