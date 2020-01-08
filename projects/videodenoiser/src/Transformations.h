#ifndef __TRANSFORMATIONS_H__
#define __TRANSFORMATIONS_H__
#include "Types.h"

void InitYUV(MFD *mfd, Pixel32* src_ref, int pitch);

__inline BYTE GetYFromRGB(Pixel32 rgb)
{
  return  (BYTE)(((rgb & 0xff0000) >> 16) * 0.299 
    +((rgb & 0xff00) >> 8) * 0.587 
    +(rgb & 0xff) * 0.114 + 0.5);
}

__inline short GetUFromRGB(Pixel32 rgb)
{
  return (int)(((rgb & 0xff0000)>>16) * (-0.147)
    +((rgb & 0xff00)>>8) * (-0.289)
    +(rgb & 0xff) * 0.436 + 255.5) - 255;
}

__inline short GetVFromRGB(Pixel32 rgb)
{
  return (int)(((rgb & 0xff0000)>>16) * 0.615
    +((rgb & 0xff00)>>8) * (-0.515)
    +(rgb & 0xff) * (-0.100) + 255.5) - 255;
}

#endif