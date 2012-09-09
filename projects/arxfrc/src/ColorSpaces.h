#ifndef __COLORSPACES_H__
#define __COLORSPACES_H__

#include "SimpleBitmap.h"

bool IsGrayScale(DSimpleBitmap* bmp);
unsigned char* GetGrayScale(DSimpleBitmap* bmp);
unsigned char** GetYUV(DSimpleBitmap* bmp);
DSimpleBitmap* GetBMPFromYUV(unsigned char** yuv, int w, int h);
DSimpleBitmap* GetBMPFromGrayScale(unsigned char* y, int w, int h);
DSimpleBitmap* GetBMPFromGrayScale(float* gray, int w, int h);

inline unsigned char Clamp(float u)
{
	if(u < 0)
		return 0;
	if(u > 255)
		return 255;
	return (unsigned char)u;
}

#endif