#ifndef __UTILITY_H__
#define __UTILITY_H__

#include <cstdlib>
#include <cmath>
#include <gl/gl.h>
#include <arx/LinearAlgebra.h>

#define EPS 1.0e-6f

#ifndef M_PI
#  define M_PI 3.14159265358979323846
#endif

// ------------------------------------------------------------------------- //
// OpenGL Helpers
// ------------------------------------------------------------------------- //
inline void glColor(const arx::Vector3d& c) {
  glColor3f(c[0], c[1], c[2]);
}

inline void glNormal(const arx::Vector3d& n) {
  glNormal3f(n[0], n[1], n[2]);
}

inline void glVertex(const arx::Vector3d& v) {
  glVertex3f(v[0], v[1], v[2]);
}

inline void glTexCoord(const arx::Vector3d& t) {
  glTexCoord3f(t[0], t[1], t[2]);
}

inline void gluLookAt(const arx::Vector3d& eye, const arx::Vector3d& center, const arx::Vector3d& up) {
  gluLookAt(eye[0], eye[1], eye[2], center[0], center[1], center[2], up[0], up[1], up[2]);
}

inline float random(float min, float max) {
  return (max - min) * rand() / RAND_MAX + min;
}

inline arx::Vector3d randomDirection() {
  return arx::Vector3d(random(-1, 1), random(-1, 1), random(-1, 1)).normalized();
}

inline arx::Vector3d transform4(const arx::Vector4d& v, const arx::Matrix4d& m) {
  /* Start reciprocal calculation first. */
  float rec = 1.0f / v.dot(m.row(3));

  /* Manually calculate product. */
  return arx::Vector3d(v.dot(m.row(0)), v.dot(m.row(1)), v.dot(m.row(2))) * rec;
}

inline arx::Vector3d transform3(const arx::Vector3d& v, const arx::Matrix4d& m) {
  return transform4(arx::Vector4d(v[0], v[1], v[2], 1.0f), m);
}

inline arx::Matrix4d scale(float x, float y, float z) {
  arx::Matrix4d result = arx::Matrix4d::Identity();

  result(0, 0) = x;
  result(1, 1) = y;
  result(2, 2) = z;

  return result;
}

inline arx::Matrix4d translate(float x, float y, float z) {
  arx::Matrix4d result = arx::Matrix4d::Identity();
  
  result(0, 3) = x;
  result(1, 3) = y;
  result(2, 3) = z;
  
  return result;
}

inline arx::Matrix4d translate(arx::Vector3d v) {
  return translate(v[0], v[1], v[2]);
}

inline arx::Matrix4d rotate(float angle, float x, float y, float z) {
  float angleRadians = angle * (static_cast<float>(M_PI) / 180.0f);

  arx::Vector3d axis(x, y, z);
  axis.normalize();

  arx::Matrix4d hat = arx::Matrix4d::Zero();
  hat(1, 0) =  axis[2];
  hat(2, 0) = -axis[1];
  hat(2, 1) =  axis[0];
  hat(0, 1) = -axis[2];
  hat(0, 2) =  axis[1];
  hat(1, 2) = -axis[0];

  return arx::Matrix4d::Identity() + hat * sin(angleRadians) + hat * hat * (1 - cos(angleRadians));
}



#endif

