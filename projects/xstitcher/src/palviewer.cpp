#include "config.h"
#include <gl/glut.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <Eigen/Dense>
#include <arx/Foreach.h>
#include "xs/Palette.h"

#ifndef M_PI
#  define M_PI 3.14159265358979
#endif

#define FULLSCREEN_X 640
#define FULLSCREEN_Y 480

using namespace Eigen;

xs::Palette palette;

Vector3f camera, direction;

bool motionF, motionB, motionL, motionR;

float t, dt;

float mousePhi, mousePsi;

int windowWidth, windowHeight;

void displayHandler() {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(110.0, static_cast<double>(windowWidth) / windowHeight, 0.01, 200);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(camera.x(), camera.y(), camera.z(), camera.x() + direction.x(), camera.y() + direction.y(), camera.z() + direction.z(), 0, 0, 1);

  glBegin(GL_POINTS);
  foreach(const xs::Color& color, palette.colors()) {
    glColor3f(color.red() / 255.0f, color.green() / 255.0f, color.blue() / 255.0f);
    glVertex3f(color.red() / 255.0f, color.green() / 255.0f, color.blue() / 255.0f);
  }
  glEnd();

  glColor3f(1, 1, 1);
  glBegin(GL_LINE_LOOP);
  glVertex3f(0, 0, 0);
  glVertex3f(0, 1, 0);
  glVertex3f(0, 1, 1);
  glVertex3f(0, 0, 1);
  glEnd();
  glBegin(GL_LINE_LOOP);
  glVertex3f(1, 0, 0);
  glVertex3f(1, 1, 0);
  glVertex3f(1, 1, 1);
  glVertex3f(1, 0, 1);
  glEnd();
  glBegin(GL_LINES);
  glVertex3f(1, 0, 0);
  glVertex3f(0, 0, 0);
  glVertex3f(1, 1, 0);
  glVertex3f(0, 1, 0);
  glVertex3f(1, 1, 1);
  glVertex3f(0, 1, 1);
  glVertex3f(1, 0, 1);
  glVertex3f(0, 0, 1);
  glEnd();

  glutSwapBuffers();
}

void idleHandler() {
  static float lastt = glutGet(GLUT_ELAPSED_TIME) / 1000.0f;
  dt = glutGet(GLUT_ELAPSED_TIME) / 1000.0f - lastt;
  t += dt;
  lastt = lastt + dt;
  glutPostRedisplay();

  const float speed = 1;
  if(motionF)
    camera += Vector3f(sin(mousePhi) * cos(mousePsi), cos(mousePhi) * cos(mousePsi), sin(mousePsi)) * dt * speed;
  if(motionB)
    camera -= Vector3f(sin(mousePhi) * cos(mousePsi), cos(mousePhi) * cos(mousePsi), sin(mousePsi)) * dt * speed;
  if(motionL)
    camera -= Vector3f(cos(mousePhi), -sin(mousePhi), 0) * dt * speed;
  if(motionR)
    camera += Vector3f(cos(mousePhi), -sin(mousePhi), 0) * dt * speed;
}

void keyboardHandler(unsigned char key, int /*x*/, int /*y*/) {
  if(key == 27)
    exit(0);
  if(key == 'w' || key == 'W')
    motionF = true;
  if(key == 's' || key == 'S')
    motionB = true;
  if(key == 'a' || key == 'A')
    motionL = true;
  if(key == 'd' || key == 'D')
    motionR = true;
}

void keyboardUpHandler(unsigned char key, int /*x*/, int /*y*/) {
  if(key == 'w' || key == 'W')
    motionF = false;
  if(key == 's' || key == 'S')
    motionB = false;
  if(key == 'a' || key == 'A')
    motionL = false;
  if(key == 'd' || key == 'D')
    motionR = false;
}

void reshapeHandler(int w, int h) {
  glViewport(0, 0, w, h);
  windowWidth = w;
  windowHeight = h;
}

void passiveMotionHandler(int x, int y) {
  const float sense = 0.006f;
  if(x != windowWidth / 2 || y != windowHeight / 2) {
    mousePhi += (x - windowWidth / 2) * sense;
    mousePsi -= (y - windowHeight / 2) * sense;
    if(mousePsi >  M_PI * 0.49)
      mousePsi =  M_PI * 0.49;
    if(mousePsi < -M_PI * 0.49)
      mousePsi = -M_PI * 0.49;
    glutWarpPointer(windowWidth / 2, windowHeight / 2);
    
    direction = Vector3f(sin(mousePhi) * cos(mousePsi), cos(mousePhi) * cos(mousePsi), sin(mousePsi));
  }
}

int main(int argc, char** argv) {
  using namespace std;

  if(argc != 2) {
    cout << "palviever - palette viewer, version " << XSTITCHER_VERSION << "." << endl;
    cout << endl;
    cout << "USAGE:" << endl;
    cout << "  palviever <input.palette>" << endl;
    return 1;
  }

  palette = xs::Palette(argv[1]);

  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_DEPTH | GLUT_RGBA);
  glutInitWindowSize(FULLSCREEN_X, FULLSCREEN_Y);
  glutCreateWindow("");

  glutIdleFunc(idleHandler);
  glutDisplayFunc(displayHandler);
  glutKeyboardFunc(keyboardHandler);
  glutKeyboardUpFunc(keyboardUpHandler);
  glutPassiveMotionFunc(passiveMotionHandler);
  glutReshapeFunc(reshapeHandler);

  glutSetCursor(GLUT_CURSOR_NONE);
  glutIgnoreKeyRepeat(1);
  glutWarpPointer(FULLSCREEN_X / 2, FULLSCREEN_Y / 2);

  camera = Vector3f(0, 0, 0);

  glClearColor(0, 0, 0, 0);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_POINT_SMOOTH);
  glPointSize(10);
  glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);

  glutMainLoop();
  return 0;
}