#include <string.h>
#include <malloc.h>
#include "Transformations.h"
#include "arxmath.h"

unsigned char* DownSample2x(unsigned char* Data, int Size)
{
	unsigned char* result = new unsigned char[Size * Size / 4];
	unsigned char LSize = intlog2(Size);
	for(int y = 0; y < Size; y += 2) for(int x = 0; x < Size; x += 2) 
	{
		result[(y << (LSize - 2)) + (x >> 1)] = 
			(Data[((y    ) << LSize) + x    ] +
			 Data[((y + 1) << LSize) + x    ] +
			 Data[((y    ) << LSize) + x + 1] +
			 Data[((y + 1) << LSize) + x + 1] + 2) >> 2;
	}
	return result;
}

unsigned char* UpSample2x(unsigned char* Data, int Size)
{
	unsigned char* result = new unsigned char[Size * Size * 4];
	unsigned char LSize = intlog2(Size) + 1;
	for(int y = 0; y < 2 * Size; y++) for(int x = 0; x < 2 * Size; x++) 
		result[(y << LSize) + x] = Data[((y >> 1) << (LSize - 1)) + (x >> 1)];
	return result;
}

unsigned char* Transform(unsigned char* Data, int Size, unsigned char Transformation)
{
	// 0..7 - 3 bits
	// 0x1  - 1 - horizontal flip
	// 0x2  - 2 - vertical flip
	// 0x4  - 3 - xy flip
	unsigned char* p = new unsigned char [Size * Size];
	int LSize = intlog2(Size);

	switch(Transformation) 
	{
	case 0:
		memcpy((void*)p, (void*)Data, Size * Size);
		break;
	case 1:
		for(int y = 0; y < Size; y++) for(int x = 0; x < Size; x++)
			p[(y << LSize) + x] = Data[(y << LSize) + (Size - x - 1)];
		break;
	case 2:
		for(int y = 0; y < Size; y++) for(int x = 0; x < Size; x++)
			p[(y << LSize) + x] = Data[((Size - y - 1) << LSize) + x];
		break;
	case 3:
		for(int y = 0; y < Size; y++) for(int x = 0; x < Size; x++)
			p[(y << LSize) + x] = Data[((Size - y - 1) << LSize) + (Size - x - 1)];
		break;
	case 4:
		for(int y = 0; y < Size; y++) for(int x = 0; x < Size; x++)
			p[(y << LSize) + x] = Data[(x << LSize) + y];
		break;
	case 5:
		for(int y = 0; y < Size; y++) for(int x = 0; x < Size; x++)
			p[(y << LSize) + x] = Data[(x << LSize) + (Size - y - 1)];
		break;
	case 6:
		for(int y = 0; y < Size; y++) for(int x = 0; x < Size; x++)
			p[(y << LSize) + x] = Data[((Size - x - 1) << LSize) + y];
		break;
	case 7:
		for(int y = 0; y < Size; y++) for(int x = 0; x < Size; x++)
			p[(y << LSize) + x] = Data[((Size - x - 1) << LSize) + (Size - y - 1)];
		break;
	default:
		break;
	}
	return p;
}

float* PlaneByteToFloat(unsigned char* pSrc, unsigned int Size)
{
	float* pDst = (float*) _aligned_malloc(Size * Size * sizeof(float), 16);
	unsigned int LSize = intlog2(Size);
	for(unsigned int y = 0; y < Size; y++) for(unsigned int x = 0; x < Size; x++)
		pDst[(y << LSize) + x] = pSrc[(y << LSize) + x];
	return pDst;
}

void PlaneFreeConvertedToFloat(float* p)
{
	_aligned_free(p);
	return;
}