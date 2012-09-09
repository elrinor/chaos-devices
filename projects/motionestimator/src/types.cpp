#include "types.h"

MV::MV()
{
	splitted = false;
	//sub[0] = sub[1] = sub[2] = sub[3] = NULL;
	x = y = 0;
	error = MAXLONG;
	dir = sd_up;
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

/*
MV& MV::operator = (const MV& vector)
{
  x = vector.x;
  y = vector.y;
  dir = vector.dir;
  error = vector.error;
  splitted = vector.splitted;
  if(splitted) {
    for(int h = 0; h < 4; h++) if(vector.sub[h] != NULL) {
      if(sub[h] == NULL)
        sub[h] = new MV;
      sub[h] = vector.sub[h];
    }
  }
  return *this;
}*/