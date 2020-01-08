#include "types.h"

MV::MV()
{
  x = y = 0;
  error = MAXLONG;
}

MV MV::operator +=(MV vector)
{
  x += vector.x;
  y += vector.y;
  return *this;
}
MV MV::operator -=(MV vector)
{
  x -= vector.x;
  y -= vector.y;
  return *this;
}
MV MV::operator *=( int mul )
{
  x *= mul;
  y *= mul;
  return *this;
}
MV MV::operator /=( int mul )
{
  x /= mul;
  y /= mul;
  return *this;
}
MV MV::operator +(MV vector)
{
  MV res;
  res.x = x + vector.x;
  res.y = y + vector.y;
  return res;
}
MV MV::operator -(MV vector)
{
  MV res;
  res.x = x - vector.x;
  res.y = y - vector.y;
  return res;
}

