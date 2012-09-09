#include <gl/glew.h>
#include <gl/glut.h>
#include <gl/GLAux.h>
#include <vector>
#include <sstream>
#include <fstream>
#include <string>
#include <iomanip>
#include <arx/LinearAlgebra.h>
#include <arx/Collections.h>

#include <boost/lexical_cast.hpp>

#include "Line.h"
#include "Camera.h"
#include "LinedText.h"

#include "Bass.h"
#pragma comment(lib, "lib/Bass.lib")
#pragma comment(lib, "glaux.lib")

//#define MUSIC
#define FILE_RENDER

// ------------------------------------------------------------------------- //
// Time management
// ------------------------------------------------------------------------- //
double getNextTime() {
#ifndef FILE_RENDER
  static double initialTime = glutGet(GLUT_ELAPSED_TIME) / 1000.0f;
  return glutGet(GLUT_ELAPSED_TIME) / 1000.0f - initialTime;
#else
  static double t = -1 / 25.0f;
  t += 1 / 25.0f;
  return t;
#endif
}


// ------------------------------------------------------------------------- //
// Useful types
// ------------------------------------------------------------------------- //
struct Sign {
  GLuint mTexture;
  arx::Vector3d mPos;
  arx::Vector3d mRight;
  arx::Vector3d mUp;

  void draw(const arx::Vector3d& camPos, const arx::Vector3d& camDir) {
    if((camPos - mPos).norm() > 40 + (mRight + mUp).norm() + 3)
      return;

    if((mPos - camPos).dot(camDir) < 0 && (camPos - mPos).norm() > (mRight + mUp).norm() + 3)
      return;

    glEnable(GL_BLEND);
    glEnable(GL_TEXTURE_2D);
    glDepthMask(GL_FALSE);
    glBindTexture(GL_TEXTURE_2D, mTexture);

    arx::Vector3d back = mRight.cross(mUp).normalized() * 0.05f;

    int maxi = 40 / (1 + 0.2 * (camPos - mPos).norm());

    glColor4f(1, 1, 1, 1 - pow(0.95, 40 / maxi));

    glBegin(GL_QUADS);
    for(int i = maxi; i > 0; i--) {
      glTexCoord2f(0, 0); glVertex(mPos - mRight - mUp + back * i);
      glTexCoord2f(1, 0); glVertex(mPos + mRight - mUp + back * i);
      glTexCoord2f(1, 1); glVertex(mPos + mRight + mUp + back * i);
      glTexCoord2f(0, 1); glVertex(mPos - mRight + mUp + back * i);
    }
    glEnd();

    glDepthMask(GL_TRUE);
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
  }
};



// ------------------------------------------------------------------------- //
// Globals
// ------------------------------------------------------------------------- //
double windowAspect;
double dt, t;

Line* mainLine;
std::vector<Line*> lines;

std::vector<Sign> signs;

Camera* camera;

LinedText* linedText;

HSTREAM music;



// ------------------------------------------------------------------------- //
// Implementation
// ------------------------------------------------------------------------- //
void display() {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT | GL_ACCUM_BUFFER_BIT);

  /* Time management */
  static bool firstFrame = true;
  if(firstFrame) {
    t = getNextTime();
    firstFrame = false;
  }
  double prevT = t;
  t = getNextTime();
  dt = t - prevT;

  if(t > 75)
    exit(0);

  /* FPS */
  static double lastFpsDisplayTime = t;
  static int framesFromLastFpsDisplay = 0;
  if(t - lastFpsDisplayTime > 0.3f && framesFromLastFpsDisplay > 0) {
    std::stringstream sStream;
    sStream << std::setprecision(5) << std::setw(6) << framesFromLastFpsDisplay / (t - lastFpsDisplayTime) << " fps, " << std::setw(6) << t << " secs from start.";
    glutSetWindowTitle(sStream.str().c_str());
    framesFromLastFpsDisplay = 0;
    lastFpsDisplayTime = t;
  } else
    framesFromLastFpsDisplay++;

#ifndef FILE_RENDER
#ifdef MUSIC
  /* Music */
  static bool playing = false;
  if(!playing) {
    playing = true;
    BASS_StreamPlay(music, 0, BASS_SAMPLE_LOOP);
  }
  BASS_Update();
#endif
#endif

  /* Camera */
  camera->advance(t, dt);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(90, windowAspect, 0.1, 100);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(camera->getPos(), camera->getPos() + camera->getDir(), camera->getUp());

  /* Peripheral lines. */
  for(unsigned int i = 0; i < lines.size(); i++) {
    lines[i]->advance(t, dt);
    lines[i]->draw(camera->getPos());
  }

  /* Main line. */
  mainLine->advance(t, dt);
  mainLine->draw(camera->getPos());

  /* Lined text. */
  linedText->advance(t, dt);
  linedText->draw(camera->getPos(), camera->getDir());

  /* Signs. */
  for(unsigned int i = signs.size(); i > 0; i--)
    signs[i - 1].draw(camera->getPos(), camera->getDir());

  /* Render to BMP. */
#ifdef FILE_RENDER
  static arx::Image3b img(1024, 576);
  for(int i = 0; i < img.getHeight(); i++)
    glReadPixels(0, i, img.getWidth(), 1, GL_BGR_EXT, GL_UNSIGNED_BYTE, img.getPixelDataAt(0, img.getHeight() - i - 1));
  static int frameNo = 10000;
  img.saveToFile("frames/frame" + boost::lexical_cast<std::string>(frameNo) + ".bmp");
  frameNo++;
#endif

  glutSwapBuffers();
  glutPostRedisplay();
}

void reshape(int w, int h) {
  windowAspect = (double) w / h;

  glViewport(0, 0, w, h);
}

GLuint loadTexture(const char* fileName, const char* alphaName, bool clamp) {
  GLuint result;
  glGenTextures(1, &result);

  GLuint components = 3;
  GLuint format = GL_RGB;

  AUX_RGBImageRec* img = auxDIBImageLoad(fileName);
  if (alphaName != NULL)
  {
    AUX_RGBImageRec* alpha = auxDIBImageLoad(alphaName);
    unsigned char *mix = new unsigned char [img->sizeX * img->sizeY * 4];
    for (int i = 0; i < img->sizeX * img->sizeY; i++)
    {
      mix[4 * i]     = img->data[3 * i];
      mix[4 * i + 1] = img->data[3 * i + 1];
      mix[4 * i + 2] = img->data[3 * i + 2];
      mix[4 * i + 3] = alpha->data[3 * i];
    }
    img->data = mix;
    components = 4;
    format = GL_RGBA;
  }

  glBindTexture(GL_TEXTURE_2D, result);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  if (clamp)
  {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  }
  else
  {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  }
  int code = gluBuild2DMipmaps(GL_TEXTURE_2D, components, img->sizeX, img->sizeY, format, GL_UNSIGNED_BYTE, img->data);

  return result;
}

int main(int argc, char** argv) {
  try {
    //srand(0xF00FD00F);
    srand(0xF00FD12F);

    if(argc < 2) {
      std::cout << "Usage: LineIntro <scenefile>";
      return 0;
    }


    /* Parse scene file. */
    std::ifstream f(argv[1]);
    if(f.fail())
      throw std::runtime_error("Cannot open file \"" + std::string(argv[1]) + "\"");

    while(true) {
      std::string s;
      getline(f, s);
      boost::trim(s);
      boost::to_upper(s);

      if(f.eof() || f.fail())
        break;

      if(s.empty())
        continue;

      std::stringstream stream(s);

      std::string key;
      stream >> key;

      if(key == "BEGIN") {
        stream >> key;
        if(key == "MAINLINE")
          mainLine = new Line(f);
        else if(key == "LINE")
          lines.push_back(new Line(f));
        else if(key == "CAMERA")
          camera = new Camera(f);
        else 
          throw std::runtime_error("Invalid object type \"" + key + "\"");
      }
    }
    f.close();


    /* Init GLUT */ 
    glutInitWindowSize(1024, 576);
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_DEPTH | GLUT_RGBA | GLUT_STENCIL | GLUT_ACCUM);
    glutCreateWindow("");

    glutReshapeFunc(reshape);
    glutDisplayFunc(display);


    /* Init OpenGL */
    glClearColor(0, 0, 0, 0);
    glEnable(GL_DEPTH_TEST);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    glShadeModel(GL_SMOOTH);
    glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_NORMALIZE);

    GLfloat fogcolor[] = {0, 0, 0, 0};
    glEnable(GL_FOG);
    glFogfv(GL_FOG_COLOR, fogcolor);
    glFogf(GL_FOG_START, 15.0f);
    glFogf(GL_FOG_END, 35.0f);
    glFogi(GL_FOG_MODE, GL_LINEAR);


    /* Load textures for signs. */
/*    signs.resize(5);

    signs[0].mTexture = loadTexture("red.bmp", "00.bmp", true);
    signs[1].mTexture = loadTexture("red.bmp", "01.bmp", true);
    signs[2].mTexture = loadTexture("red.bmp", "02.bmp", true);
    signs[3].mTexture = loadTexture("red.bmp", "03.bmp", true);
    signs[4].mTexture = loadTexture("red.bmp", "04.bmp", true);*/


    /* Generate. */
    arx::Vector3d dir = arx::Vector3d(0, 0, -1);
    arx::Vector3d prevDir = dir;
    arx::Vector3d pos = arx::Vector3d(20, 0, -15);
    arx::Vector3d up = arx::Vector3d(0, 1, 0);
    arx::Vector3d prevTarget = pos;
    double speed = 10;
    double totalT = 0;
    double maxTotalT = 3 * 6 + 13 + 5 + 2 * 4;
    for(int k = 0; k < 10; k++) {
      double t = 0;
      double dist = 0;
      bool signPlaced = false;
      bool logoPlaced = false;
      int imagePos = 0;
      while(true) {
        dist += random(1, 2);
        arx::Vector3d target = pos + dir * dist + randomDirection() * random(0.5, 1.0) - up * random(0, 0.5) - up;
        mainLine->addMoveEvent(Event(EVENT_POS, target));
        double dt = (target - prevTarget).norm() / speed;
        t += dt;
        totalT += dt;
        prevTarget = target;

        arx::Vector3d right = (dir.cross(up)).normalized();

        double realT = 15 + totalT / maxTotalT * 32;

        double cutR = 9;
        if(k == 6)
          cutR = 9 + random(0, 2);
        if(k == 7)
          cutR = 9 + t * 5;

        Line* line = new Line();
        double angle = random(0, 360);
        arx::Vector3d displacement = 30 * (up * sin(angle * M_PI / 180) + right * cos(angle * M_PI / 180));
        arx::Vector3d linePos = pos + dir * dist + random(2, 10) * dir + up * 8 + displacement + randomDirection() * 10;
        arx::Vector3d lineTarget = -displacement * 3 + randomDirection() * 20;
        line->addEvent(Event(EVENT_BARRIERBASE));
        line->addEvent(Event(EVENT_TIMEBARRIER, realT));
        line->addEvent(Event(EVENT_SETPOS, linePos));
        line->addEvent(Event(EVENT_SETMOVEDIR, dir));
        line->addEvent(Event(EVENT_SEGMENTATION, 0.3));
        line->addEvent(Event(EVENT_TOUCH, 0.6));
        line->addEvent(Event(EVENT_SETSPEED, 100));
        line->addEvent(Event(EVENT_SETTURNSPEED, 60));
        line->addEvent(Event(EVENT_SETWIDTH, 0.1));
        line->addEvent(Event(EVENT_SETCOLOR, arx::Vector3d(0.3, 0, 0)));
        arx::Vector3d prevLinePos = linePos;
        for(int i = 0; i < 30; i++) {
          arx::Vector3d nextLinePos = linePos + lineTarget * (i / 30.0) + 1.5 * randomDirection();
          arx::Vector3d camPos = pos + up * 8;
          arx::Vector3d relNextLinePos = nextLinePos - camPos;
          arx::Vector3d dirProj = relNextLinePos.dot(dir) * dir;
          arx::Vector3d dirPerp = relNextLinePos - dirProj;
          if(dirPerp.norm() < cutR)
            dirPerp = dirPerp.normalized() * random(cutR, cutR + 5);
          if((camPos + dirPerp + dirProj - prevLinePos).squaredNorm() > (camPos - dirPerp + dirProj - prevLinePos).squaredNorm()) {
            line->addEvent(Event(EVENT_POS, camPos - dirPerp + dirProj));
            prevLinePos = camPos - dirPerp + dirProj;
          } else {
            line->addEvent(Event(EVENT_POS, camPos + dirPerp + dirProj));
            prevLinePos = camPos + dirPerp + dirProj;
          }
        }
        line->addEvent(Event(EVENT_TIMEBARRIER, 10000));
        lines.push_back(line);


        if(k < 6) {
          if(t > 3.0)
            break;

          if(!signPlaced && t > 2) {
            signPlaced = true;

            signs.resize(signs.size() + 1);

            signs.back().mPos = pos + dir * dist + up * 8;
            signs.back().mUp = up * 3;
            signs.back().mRight = (dir.cross(up)).normalized() * 6;
            signs.back().mTexture = loadTexture((k != 5) ? "red.bmp" : "gray.bmp", ("0" + boost::lexical_cast<std::string>(k) + ".bmp").c_str(), true);
          }
        } else if(k < 7) {
          if(t > 13)
            break;

          if(t > imagePos * 1.8 + 1 && imagePos < 7) {
            signs.resize(signs.size() + 1);

            signs.back().mPos = pos + dir * dist + up * 8;
            signs.back().mUp = up * 2;
            signs.back().mRight = right * 4;
            signs.back().mTexture = loadTexture("red.bmp", ("1" + boost::lexical_cast<std::string>(imagePos) + ".bmp").c_str(), true);

            if(imagePos == 0)
              signs.back().mPos += -1.5 * up - 2 * right;
            else if(imagePos == 1)
              signs.back().mPos +=  1.5 * up + 2 * right;
            else if(imagePos == 2)
              signs.back().mPos +=  1.5 * up - 2 * right;
            else if(imagePos == 3)
              signs.back().mPos += -1.5 * up + 2 * right;
            else if(imagePos == 4)
              signs.back().mPos += -3 * right;
            else if(imagePos == 5)
              signs.back().mPos += 3 * right - 0.5 * up;

            imagePos++;
          } 

          /*if(t > 8 && imagePos == 5) {
            signs.resize(signs.size() + 1);

            signs.back().mPos = pos + dir * dist + up * 8;
            signs.back().mUp = up * 2;
            signs.back().mRight = right * 4;
            signs.back().mTexture = loadTexture("gray.bmp", ("1" + boost::lexical_cast<std::string>(imagePos) + ".bmp").c_str(), true);

            imagePos++;
          }*/
        } else if(k < 8) {
          if(t > 5)
            break;

          if(t > 3.0 && !logoPlaced) {
            logoPlaced = true;

            arx::Vector3d logoRight = right * 14;
            arx::Vector3d logoUp = -up * 7;
            arx::Vector3d logoDir = dir * 0.5;

            arx::Matrix4d imgToWorld = arx::Matrix4d::Identity();
            imgToWorld.row(0).block<1, 3>(0, 0) = 2 * logoRight;
            imgToWorld.row(1).block<1, 3>(0, 0) = 2 * logoUp;
            imgToWorld.row(2).block<1, 3>(0, 0) = 2 * logoDir;

            arx::Vector3d logoCenter = pos + dir * (dist + 5) + up * 10 + right;
            imgToWorld = translate(logoCenter - logoRight - logoUp - logoDir) * imgToWorld;

            linedText = new LinedText(arx::Image3f::loadFromFile("logo.bmp"), /*realT*/40.5, imgToWorld);
            for(int i = 0; i < 30; i++)
              linedText->addLine(pos);
          }
        } else if(k < 10) {
          if(t > 3.0)
            break;

          if(!signPlaced && t > 2) {
            signPlaced = true;

            signs.resize(signs.size() + 1);

            signs.back().mPos = pos + dir * dist + up * 8;
            signs.back().mUp = up * 3;
            signs.back().mRight = (dir.cross(up)).normalized() * 6;
            signs.back().mTexture = loadTexture("grayred.bmp", ("2" + boost::lexical_cast<std::string>(k - 8) + ".bmp").c_str(), true);
          }
        }
      }

      camera->addMoveEvent(Event(EVENT_POS, pos + dir * dist * 0.25 + up * 8));
      camera->addMoveEvent(Event(EVENT_POS, pos + dir * dist * 0.75 + up * 8));
      if(k == 6)
        camera->addEvent(Event(EVENT_SPEED, 4));
      if(k == 7)
        camera->addEvent(Event(EVENT_SPEED, 10));


      pos = prevTarget;

      dir = (dir + 0.4 * randomDirection()).normalized();

      if(k == 0)
        dir = arx::Vector3d(0, -1, -1).normalized();

      if(k == 4)
        dir = (arx::Vector3d(0, 0, -1) + prevDir).normalized();

      if(k == 5)
        dir = (arx::Vector3d(0, 0, -1) + prevDir).normalized();

      if(k == 6)
        dir = arx::Vector3d(0, 0, -1);

      if(k >= 8 && k < 12)
        dir = (dir + 0.25 * randomDirection()).normalized();

      arx::Vector3d newUp = dir.cross(up.cross(prevDir)).normalized();
      if(up.dot(newUp) < 0)
        up = -newUp;
      else
        up = newUp;
      
      if(k == 6)
        up = arx::Vector3d(0, 1, 0);

      camera->addMoveEvent(Event(EVENT_UP, up));
      camera->addMoveEvent(Event(EVENT_LOOKDIR, dir));

      prevDir = dir;
    }

    arx::Vector3d right = (dir.cross(up)).normalized();
    arx::Vector3d camPos = pos + dir * 5 + up * 8;

    double barrier = 55.1;

    for(int i = 0; i < 10; i++)
      mainLine->addMoveEvent(Event(EVENT_POS, pos + dir * i * 3 + 1 * randomDirection() + up * i * 0.5f));
    mainLine->addMoveEvent(Event(EVENT_SETBBASE, 0));
    mainLine->addMoveEvent(Event(EVENT_TIMEBARRIER, barrier - 7));
    
    mainLine->addMoveEvent(Event(EVENT_SETSPEED, 10));
    mainLine->addMoveEvent(Event(EVENT_SETTURNSPEED, 1000));
    mainLine->addMoveEvent(Event(EVENT_SETPOS, camPos + dir * 55 - right * 30 - up * 5));
    mainLine->addMoveEvent(Event(EVENT_POS, camPos + dir * 10 - right * 30 - up * 5));
    mainLine->addMoveEvent(Event(EVENT_POS, camPos + dir * 10 - right * 30));
    mainLine->addMoveEvent(Event(EVENT_TOUCH, 0.01));
    mainLine->addMoveEvent(Event(EVENT_SKIPPOS, camPos + dir * 10 + right * 150));

    double beats[][2] = {
      {55.9, 1},
      {56.4, 0.4},
      {56.9, 0.4},
      {57.9, 0.6},
      {58.4, 0.9},
      {58.9, 0.4},
      {59.4, 0.4},
      {60.6, 0.3},
      {61.3, 0.8},
      {61.9, 0.2},
      {62.4, 0.1},
      {62.9, 0.1},
      {63.4, 0.1},
      {64.2, 0.1},
      {64.8, 0.6},
      {65.6, 0.1},
      {66.8, 0.2},
      {0.0, 0.0}
    };

    mainLine->addMoveEvent(Event(EVENT_SEGMENTATION, 0.02));
    mainLine->addMoveEvent(Event(EVENT_TOUCH, 0.02));

    for(int i = 0; beats[i][0] != 0; i++) {
      mainLine->addMoveEvent(Event(EVENT_SETBBASE, 0));
      mainLine->addMoveEvent(Event(EVENT_TIMEBARRIER, beats[i][0]));
      mainLine->addMoveEvent(Event(EVENT_SETTURNSPEED, 5000));
      mainLine->addMoveEvent(Event(EVENT_SETSPEED, 80));

      double p = beats[i][1] * 0.60;

      mainLine->addMoveEvent(Event(EVENT_BARRIERBASE));
      mainLine->addMoveEvent(Event(EVENT_RELPOS, 30 * right - 150 * up));
      mainLine->addMoveEvent(Event(EVENT_TIMEBARRIER, 0.03 * p));

      mainLine->addMoveEvent(Event(EVENT_BARRIERBASE));
      mainLine->addMoveEvent(Event(EVENT_RELPOS, 15 * right + 150 * up));
      mainLine->addMoveEvent(Event(EVENT_TIMEBARRIER, 0.13 * p));

      mainLine->addMoveEvent(Event(EVENT_BARRIERBASE));
      mainLine->addMoveEvent(Event(EVENT_RELPOS, 15 * right - 150 * up));
      mainLine->addMoveEvent(Event(EVENT_TIMEBARRIER, 0.15 * p));

      mainLine->addMoveEvent(Event(EVENT_BARRIERBASE));
      mainLine->addMoveEvent(Event(EVENT_RELPOS, 20 * right + 150 * up));
      mainLine->addMoveEvent(Event(EVENT_TIMEBARRIER, 0.05 * p));

      mainLine->addMoveEvent(Event(EVENT_BARRIERBASE));
      mainLine->addMoveEvent(Event(EVENT_RELPOS, 100 * right));

      mainLine->addMoveEvent(Event(EVENT_SETSPEED, 8.3));
    }

    mainLine->addMoveEvent(Event(EVENT_BARRIERBASE));
    mainLine->addMoveEvent(Event(EVENT_TIMEBARRIER, 0.5));

    for(int i = 0; i < 51; i++) {
      mainLine->addMoveEvent(Event(EVENT_BARRIERBASE));
      mainLine->addMoveEvent(Event(EVENT_TIMEBARRIER, 1.0f / 20));
      mainLine->addMoveEvent(Event(EVENT_SETCOLOR, arx::Vector3d(1.0f - i / 50.0f, 0, 0)));
    }

    camera->addMoveEvent(Event(EVENT_LOOKDIR, dir));
    camera->addMoveEvent(Event(EVENT_POS, camPos));
    camera->addMoveEvent(Event(EVENT_SETBBASE, 0));
    camera->addMoveEvent(Event(EVENT_TIMEBARRIER, barrier));

    camera->addMoveEvent(Event(EVENT_SETMOVEDIR, right));
    camera->addMoveEvent(Event(EVENT_SETSPEED, 8));
    camera->addMoveEvent(Event(EVENT_TOUCH, 0.1));
    camera->addMoveEvent(Event(EVENT_SETTURNSPEED, 1000));
    camera->addMoveEvent(Event(EVENT_POS, camPos + right * 150));

    mainLine->addMoveEvent(Event(EVENT_TIMEBARRIER, 10000));
    camera->addMoveEvent(Event(EVENT_TIMEBARRIER, 10000));


    /* Init music */
    BASS_Init(-1, 44100, BASS_DEVICE_NOTHREAD, 0);
    music = BASS_StreamCreateFile(false, "1.wav", 0, 0, 0); 
    BASS_Start();
    BASS_ChannelSetPosition(music, (QWORD) MAKELONG(0, 0));
    BASS_SetVolume(100);


    /* GO! */
    glutMainLoop();
    return 0;
  } catch (std::runtime_error e) {
    std::cout << e.what();
    return -1;
  }
}
















