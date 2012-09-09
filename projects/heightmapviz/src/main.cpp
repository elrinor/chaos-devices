#define NOMINMAX
#define _CRT_SECURE_NO_DEPRECATE
#define _USE_MATH_DEFINES

#include <Windows.h>
#include <gl/glew.h>
#include <gl/gl.h>
#include <gl/glut.h>

#include <fstream>
#include <utility>
#include <limits>
#include <map>
#include <ctime>
#include <sstream>
#include <vector>
#include <cmath>

#include "arx/Collections.h"
#include "arx/Image.h"
#include "arx/LinearAlgebra.h"

#pragma comment(lib, "glew32.lib")


using namespace std;
using namespace arx;

class Tile {
public:
  Tile(Image1f& im, int xOffset, int yOffset, int xSize, int ySize) {
    mPos = Vector3f(xOffset + xSize * 0.5f, yOffset + ySize * 0.5f, 0);

    int divisor = 1;
    while(divisor <= xSize && divisor <= ySize) {
      float dx = (1.0 * xSize) / (xSize / divisor);
      float dy = (1.0 * ySize) / (ySize / divisor);

      glNewList(sIslandListId, GL_COMPILE);
      glBegin(GL_QUADS);
      for(float x = 0; x < xSize; x += dx) {
        for(float y = 0; y < ySize; y += dy) {
          glVertex3f(xOffset + x     , yOffset + y     , im.getPixelInterpolated(xOffset + x     , yOffset + y     ));
          glVertex3f(xOffset + x + dx, yOffset + y     , im.getPixelInterpolated(xOffset + x + dx, yOffset + y     ));
          glVertex3f(xOffset + x + dx, yOffset + y + dy, im.getPixelInterpolated(xOffset + x + dx, yOffset + y + dy));
          glVertex3f(xOffset + x     , yOffset + y + dy, im.getPixelInterpolated(xOffset + x     , yOffset + y + dy));
        }
      }
      glEnd();
      glEndList();
      mLists.push_back(sIslandListId);
      sIslandListId++;
      divisor *= 2;
    }
  }

  void draw(float distance) {
    assert(distance >= 0 && distance <= 1.0);
    glCallList(mLists[(int) (mLists.size() * (distance - 1.0e-7))]);
  }

  Vector3f getPos() { return mPos; }

private:
  static GLuint sIslandListId;

  Vector3f mPos;

  vector<GLuint> mLists;
};

GLuint Tile::sIslandListId = 0;

Image1f medianFilter(Image1f im) {
  Image1f result = im.clone();
  for(int y = 1; y < im.getHeight() - 1; y++) {
    for(int x = 1; x < im.getWidth() - 1; x++) {
      float copy[5];
      copy[0] = im.getPixel(x, y);
      copy[1] = im.getPixel(x + 1, y);
      copy[2] = im.getPixel(x - 1, y);
      copy[3] = im.getPixel(x, y + 1);
      copy[4] = im.getPixel(x, y + 1);

      nth_element(copy, copy + 2, copy + 5);
      result.setPixel(x, y, copy[2]);
    }
  }
  return result;
}

char *readFile(const char *fileName) {
  FILE *f;
  char *result = NULL;
  int count = 0;

  f = fopen(fileName, "rt");
  fseek(f, 0, SEEK_END);
  count = ftell(f);
  rewind(f);

  if (count > 0) {
    result = (char *) malloc(sizeof(char) * (count + 1));
    count = (int) fread(result, sizeof(char), count, f);
    result[count] = '\0';
  }
  fclose(f);
  return result;
}

void checkShaderStatus(GLuint shader) {
  GLint status;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
  if (status == GL_FALSE) {
    GLsizei len;
    static GLchar buf[4096];
    glGetShaderInfoLog(shader, 4096, &len, buf);
    buf[len] = '\0';
    MessageBox(NULL, buf, "Error", MB_OK);
    exit(0);
  }
}

void checkProgramStatus(GLuint program) {
  GLint status;
  glGetShaderiv(program, GL_LINK_STATUS, &status);
  if (status == GL_FALSE) {
    GLsizei len;
    static GLchar buf[4096];
    glGetProgramInfoLog(program, 4096, &len, buf);
    buf[len] = '\0';
    MessageBox(NULL, buf, "Error", MB_OK);
    exit(0);
  }
}

GLuint loadShader(const char* pixelShaderFileName, const char* vertexShaderFileName) {
  GLuint shaderV = glCreateShader(GL_VERTEX_SHADER);
  GLuint shaderP = glCreateShader(GL_FRAGMENT_SHADER);
  char* v = readFile(vertexShaderFileName);
  char* p = readFile(pixelShaderFileName);
  glShaderSource(shaderV, 1, (const char**) &v, NULL);
  glShaderSource(shaderP, 1, (const char**) &p, NULL);
  glCompileShader(shaderV);
  glCompileShader(shaderP);
  GLuint result = glCreateProgram();
  glAttachShader(result, shaderV);
  glAttachShader(result, shaderP);
  glLinkProgram(result);

  checkShaderStatus(shaderV);
  checkShaderStatus(shaderP);
  checkProgramStatus(result);

  return result;
}


GLuint shader;
CheckedFastArray<Tile> tiles;
int windowWidth, windowHeight;
float cameraPhi, cameraPsi;
Vector3f cameraPos;
Vector3f cameraDir;
array<bool, 256> pressedKeys;
float maxZ;

float displayZMin, displayZMax;

static void display() {
  /* Time management. */
  static clock_t lastFrameClock = clock();
  clock_t currentFrameClock = clock();
  float dt = (currentFrameClock - lastFrameClock) * (1.0f / CLOCKS_PER_SEC);
  lastFrameClock = currentFrameClock;

  /* FPS. */
  static clock_t lastFpsDisplayClock = clock();
  static int framesFromLastFpsDisplay = 0;
  if(currentFrameClock - lastFpsDisplayClock > CLOCKS_PER_SEC && framesFromLastFpsDisplay > 0) {
    std::stringstream sStream;
    sStream << "Glut: " << 
      framesFromLastFpsDisplay * (1.0f * CLOCKS_PER_SEC) / (currentFrameClock - lastFpsDisplayClock) << " fps";
    glutSetWindowTitle(sStream.str().c_str());
    framesFromLastFpsDisplay = 0;
    lastFpsDisplayClock = currentFrameClock;
  } else
    framesFromLastFpsDisplay++;

  /* Move camera. */
  const float speed = 50.0f;
  if(pressedKeys['w'] || pressedKeys['W'])
    cameraPos += speed * dt * arx::Vector3f(sin(cameraPhi) * cos(cameraPsi), cos(cameraPhi) * cos(cameraPsi), sin(cameraPsi));
  if(pressedKeys['s'] || pressedKeys['S'])
    cameraPos -= speed * dt * arx::Vector3f(sin(cameraPhi) * cos(cameraPsi), cos(cameraPhi) * cos(cameraPsi), sin(cameraPsi));
  if(pressedKeys['a'] || pressedKeys['A'])
    cameraPos -= speed * dt * arx::Vector3f(cos(cameraPhi), -sin(cameraPhi), 0);
  if(pressedKeys['d'] || pressedKeys['D'])
    cameraPos += speed * dt * arx::Vector3f(cos(cameraPhi), -sin(cameraPhi), 0);

  if(pressedKeys['q'] || pressedKeys['Q'])
    displayZMin += speed * dt;
  if(pressedKeys['z'] || pressedKeys['Z'])
    displayZMin -= speed * dt;
  if(pressedKeys['e'] || pressedKeys['E'])
    displayZMax += speed * dt;
  if(pressedKeys['c'] || pressedKeys['C'])
    displayZMax -= speed * dt;

  /* Set modelview matrix. */
  cameraDir = Vector3f(sin(cameraPhi) * cos(cameraPsi), cos(cameraPhi) * cos(cameraPsi), sin(cameraPsi));
  glLoadIdentity(); 
  gluLookAt(cameraPos[0], cameraPos[1], cameraPos[2], 
    cameraPos[0] + cameraDir[0], 
    cameraPos[1] + cameraDir[1], 
    cameraPos[2] + cameraDir[2], 0, 0, 1);

  /* Render. */
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
  
  glPushMatrix();
  //glScalef(1, 1, 20);

  glUseProgram(shader);

  GLint displayZMinLoc = glGetUniformLocation(shader, "displayZMin");
  glUniform1f(displayZMinLoc, displayZMin);
  GLint displayZMaxLoc = glGetUniformLocation(shader, "displayZMax");
  glUniform1f(displayZMaxLoc, displayZMax);
  GLint maxZLoc = glGetUniformLocation(shader, "maxZ");
  glUniform1f(maxZLoc, maxZ);

  for(int i = 0; i < tiles.size(); i++) {
    if((cameraPos - tiles[i].getPos()).dot(cameraDir) > 35)
      continue;
    float dist = (cameraPos - tiles[i].getPos()).norm() / 1000;
    if(dist > 1.0f)
      dist = 1.0f;
    tiles[i].draw(dist);
  }
  glUseProgram(0);

  glPopMatrix();

  glutSwapBuffers();
}

static void idle() {
  glutPostRedisplay();
}

static void reshape(int w, int h) {
  /* Store window size. */
  windowWidth = w;
  windowHeight = h;

  /* Change viewport size. */ 
  glViewport(0, 0, w, h);

  /* Set projection matrix. */
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(120, (float) w / h, 0.1, 3000);
  glMatrixMode(GL_MODELVIEW);
}

static void keyboard(unsigned char c, int x, int y) {
  if(c == 27)
    exit(0);

  pressedKeys[c] = true;
}

static void keyboardUp(unsigned char c, int x, int y) {
  pressedKeys[c] = false;
}

static void passiveMotion(int x, int y) {
  const float sensitivity = 0.01f;
  const float lt05 = 0.5f - 1.0e-7;
  if(x != windowWidth / 2 || y != windowHeight / 2) {
    cameraPhi += (x - windowWidth / 2) * sensitivity;
    cameraPsi -= (y - windowHeight / 2) * sensitivity;
    if(cameraPsi >  M_PI * lt05)
      cameraPsi =  M_PI * lt05;
    if(cameraPsi < -M_PI * lt05)
      cameraPsi = -M_PI * lt05;
    glutWarpPointer(windowWidth / 2, windowHeight / 2);
  }
}

int main(int argc, char** argv) {
  if(argc < 2)
    return 1;
  else {
    ifstream f(argv[1]);

    if(!f.is_open())
      return 1;

    map<pair<int, int>, float> heightMap;
    int minX, minY = minX = numeric_limits<int>::max(), maxX, maxY = maxX = numeric_limits<int>::min();
    float minZ = numeric_limits<float>::max();
    maxZ = -numeric_limits<float>::max();

    while(!f.eof()) {
      float x, y, z;
      f >> x >> y >> z;

      if(z < 0) 
        z = 0;

      heightMap.insert(make_pair(make_pair((int) x, (int) y), z));

      minX = min((int) x, minX);
      minY = min((int) y, minY);
      maxX = max((int) x, maxX);
      maxY = max((int) y, maxY);
    }

    f.close();

    Image1f im(maxX - minX + 1, maxY - minY + 1);

    /*
    ifstream f1(argv[1]);
    while(!f1.eof()) {
      float x, y, z;
      f1 >> x >> y >> z;
      im.setPixel((int) x - minX, (int) y - minY, z);
    }
    f1.close();
    */
///*
    for(int y = minY; y <= maxY; y++)
      for(int x = minX; x <= maxX; x++)
        im.setPixel(x - minX, y - minY, heightMap[make_pair(x, y)]);
//*/
    im = medianFilter(im);
    im = medianFilter(im);

    for(int y = 0; y < im.getHeight(); y++) {
      for(int x = 0; x < im.getWidth(); x++) {
        float z = im.getPixel(x, y);
        minZ = min(z, minZ);
        maxZ = max(z, maxZ);
      }
    }
/*
    for(int y = 0; y < im.getHeight(); y++)
      for(int x = 0; x < im.getWidth(); x++)
        im.setPixel(x, y, (im.getPixel(x, y) - minZ) / (maxZ - minZ));
*/
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
    glutInitWindowSize(640, 480);
    glutCreateWindow("");
    glutSetCursor(GLUT_CURSOR_NONE);

    glutDisplayFunc(display);
    glutIdleFunc(idle);
    glutReshapeFunc(reshape);
    glutKeyboardFunc(keyboard);
    glutKeyboardUpFunc(keyboardUp);
    glutPassiveMotionFunc(passiveMotion);

    glewInit();

    shader = loadShader("shader.p", "shader.v");

    for(int y = 0; y < im.getHeight() - 1; y += 64)
      for(int x = 0; x < im.getWidth() - 1; x += 64)
        tiles.push_back(Tile(im, x, y, min(64, im.getWidth() - 2 - x), min(64, im.getHeight() - 2 - y)));

    cameraPos = Vector3f(0, 0, 0);


    displayZMax = 256;
    displayZMin = 0;

    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);

    glutMainLoop();

    return 0;
  }
}