#include "ColorSpaces.h"
#include "SimpleBitmap.h"

bool IsGrayScale(DSimpleBitmap* bmp)
{
	int wb = (bmp->GetWidth() * 3 + 3) & -4;
	int w = bmp->GetWidth(), h = bmp->GetHeight();
	unsigned char *p = (unsigned char *) bmp->GetLinePointer(0);

	long long sum = 0;

	for(int y = 0; y < h; y++) for(int x = 0; x < w; x++)
	{
		sum += max(p[y * wb + x * 3 + 0], max(p[y * wb + x * 3 + 1], p[y * wb + x * 3 + 2]));
		sum -= min(p[y * wb + x * 3 + 0], min(p[y * wb + x * 3 + 1], p[y * wb + x * 3 + 2]));
	}

	if(sum > w * h * 3)
		return false;
	else
		return true;
}

unsigned char* GetGrayScale(DSimpleBitmap* bmp)
{
	int w = bmp->GetWidth(), h = bmp->GetHeight(), wb = (bmp->GetWidth()*3+3)&-4;
	unsigned char* p = (unsigned char *) bmp->GetLinePointer(0);
	unsigned char* grayscale = new unsigned char[w * h];
	for(int y = 0; y < h; y++) for(int x = 0; x < w; x++)
		grayscale[y * w + x] = (p[y * wb + x * 3 + 0] + p[y * wb + x * 3 + 1] + p[y * wb + x * 3 + 2]) / 3;
	return grayscale;
}

unsigned char** GetYUV(DSimpleBitmap* bmp)
{
	int w = bmp->GetWidth(), h = bmp->GetHeight(), wb = (bmp->GetWidth()*3+3)&-4;
	unsigned char* p = (unsigned char *) bmp->GetLinePointer(0);
	unsigned char** yuv = new unsigned char* [3];
	yuv[0] = new unsigned char [w * h];
	yuv[1] = new unsigned char [w * h];
	yuv[2] = new unsigned char [w * h];
	for(int y = 0; y < h; y++) for(int x = 0; x < w; x++)
	{
		/*
		Y' =       + 0.299    * R'd + 0.587    * G'd + 0.114    * B'd
		Cb = 128   - 0.168736 * R'd - 0.331264 * G'd + 0.5      * B'd
		Cr = 128   + 0.5      * R'd - 0.418688 * G'd - 0.081312 * B'd
		*/
		yuv[0][y * w + x] = (unsigned char)(  0.299f    * p[y * wb + x * 3 + 2] + 0.587f    * p[y * wb + x * 3 + 1] + 0.114f    * p[y * wb + x * 3 + 0]);
		yuv[1][y * w + x] = (unsigned char)(- 0.168736f * p[y * wb + x * 3 + 2] - 0.331264f * p[y * wb + x * 3 + 1] + 0.5f      * p[y * wb + x * 3 + 0] + 128);
		yuv[2][y * w + x] = (unsigned char)(  0.5f      * p[y * wb + x * 3 + 2] - 0.418688f * p[y * wb + x * 3 + 1] - 0.081312f * p[y * wb + x * 3 + 0] + 128);
	}
	return yuv;
}

DSimpleBitmap* GetBMPFromYUV(unsigned char** yuv, int w, int h)
{
	int wb = (w * 3 + 3) & -4;
	DSimpleBitmap* bmp = new DSimpleBitmap();
	bmp->CreateImage(w, h);
	unsigned char* p = (unsigned char *) bmp->GetLinePointer(0);
	for(int y = 0; y < h; y++) for(int x = 0; x < w; x++)
	{
		/*
		R = Y                    + 1.402 (Cr-128)
		G = Y - 0.34414 (Cb-128) - 0.71414 (Cr-128)
		B = Y + 1.772 (Cb-128)
		*/
		p[y * wb + x * 3 + 2] = Clamp((yuv[0][y * w + x]                                        + 1.402f   * (yuv[2][y * w + x] - 128)));
		p[y * wb + x * 3 + 1] = Clamp((yuv[0][y * w + x] - 0.34414f * (yuv[1][y * w + x] - 128) - 0.71414f * (yuv[2][y * w + x] - 128)));
		p[y * wb + x * 3 + 0] = Clamp((yuv[0][y * w + x] + 1.772f   * (yuv[1][y * w + x] - 128)));
	}
	return bmp;
}

DSimpleBitmap* GetBMPFromGrayScale(unsigned char* gray, int w, int h)
{
	int wb = (w * 3 + 3) & -4;
	DSimpleBitmap* bmp = new DSimpleBitmap();
	bmp->CreateImage(w, h);
	unsigned char* p = (unsigned char *) bmp->GetLinePointer(0);
	for(int y = 0; y < h; y++) for(int x = 0; x < w; x++)
	{
		p[y * wb + x * 3 + 2] = gray[y * w + x];
		p[y * wb + x * 3 + 1] = gray[y * w + x];
		p[y * wb + x * 3 + 0] = gray[y * w + x];
	}
	return bmp;
}

DSimpleBitmap* GetBMPFromGrayScale(float* gray, int w, int h)
{
	int wb = (w * 3 + 3) & -4;
	DSimpleBitmap* bmp = new DSimpleBitmap();
	bmp->CreateImage(w, h);
	unsigned char* p = (unsigned char *) bmp->GetLinePointer(0);
	for(int y = 0; y < h; y++) for(int x = 0; x < w; x++)
	{
		p[y * wb + x * 3 + 2] = (unsigned char) gray[y * w + x];
		p[y * wb + x * 3 + 1] = (unsigned char) gray[y * w + x];
		p[y * wb + x * 3 + 0] = (unsigned char) gray[y * w + x];
	}
	return bmp;
}
