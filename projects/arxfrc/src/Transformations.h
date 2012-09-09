#ifndef __TRANSFORMATIONS_H__
#define __TRANSFORMATIONS_H__

unsigned char* Transform(unsigned char* Data, int Size, unsigned char Transformation);
unsigned char* DownSample2x(unsigned char* Data, int Size);
unsigned char* UpSample2x(unsigned char* Data, int Size);
float* PlaneByteToFloat(unsigned char* pSrc, unsigned int Size);
void PlaneFreeConvertedToFloat(float* p);

inline void TransformRegion(unsigned int inX, unsigned int inY, unsigned int& outX, unsigned int& outY, unsigned int Size, unsigned int RegionSize, unsigned int Transformation)
{
	switch(Transformation) 
	{
	case 0:
		outX = inX;
		outY = inY;
		break;
	case 1:
		outX = Size - inX - RegionSize;
		outY = inY;
		break;
	case 2:
		outX = inX;
		outY = Size - inY - RegionSize;
		break;
	case 3:
		outX = Size - inX - RegionSize;
		outY = Size - inY - RegionSize;
		break;
	case 4:
		outX = inY;
		outY = inX;
		break;
	case 5:
		outX = inY;
		outY = Size - inX - RegionSize;
		break;
	case 6:
		outX = Size - inY - RegionSize;
		outY = inX;
		break;
	case 7:
		outX = Size - inY - RegionSize;
		outY = Size - inX - RegionSize;
		break;
	default:
		break;
	}
	return;
}

#endif