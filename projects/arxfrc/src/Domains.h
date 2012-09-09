#ifndef __DOMAINS_H__
#define __DOMAINS_H__

#include "KDTree.h"

#define INDEX_DIMENSIONS 4

typedef struct TDomain{
	unsigned int x, y; // X and Y coordinated in x2 downscaled image
	unsigned int Size; // The real size of domain is Size*2, we store it's size in x2 downscaled image
	unsigned int Transformation;

	// Invariants are calculated for x2 downscaled image
	float Average; // SUM(Aij) / Size^2
	float Sum; // SUM(Aij)
	float SqrSum; // SUM(Aij^2)
	float Div; // Divider = SqrSum*Size^2 - Sum^2
	// float Inv[4];
	// 0-3 - 2x2 downsample (*)
	// 4 - cluster - edges - count
	// 5 - cluster - count
	//
	// (*) chunks:
	// 01
	// 23
} TDomain;

typedef TKDTree<float, INDEX_DIMENSIONS, TDomain> KDTreeD;

typedef struct TDomainPool{
	int DomainSize; // The real size of domain is Size*2, we store it's size in x2 downscaled image
	KDTreeD* Tree;
} TDomainPool;

class TSlice{
private:
public:
	TSlice(unsigned char* _Data, int _Size);
	~TSlice();
	void CreateDomainPools(int MaxDomainSize);
	void CreateTranslatedPlanes();


	unsigned int Size; // Width and Height of image
	unsigned int LSize;// log2(Size)
	unsigned int CoordBitCount; // bits to store packet coord

	// Data:
	// 8 - transformations
	// 2 - original size and x2 downsample
	unsigned char* Data[8][2]; 

	// FData:
	// 8 - transformations
	// 2 - original size and x2 downsample
	float* FData[8][2];

	// DomainPool:
	// 0 - 8x8 Domains
	// 1 - 16x16 Domains
	// 2 - 32x32 Domains
	// 3 - 64x64 Domains
	TDomainPool DomainPool[4];
	
	// RangePool:
	// 0 - 4x4 Range Blocks
	// 1 - 8x8 Range Blocks
	// 2 - 16x16 Range Blocks
	// 3 - 32x32 Range Blocks
	// TRangePool RangePool[4];
};

inline void CalculateRegionInvariants(KDTreeD::KDTreeItem& Region, unsigned int RegionSize, float* p, unsigned int LSize)
{
	Region.Data.Sum = 0;
	Region.Data.SqrSum = 0;
	unsigned int InvK = 0;
	for(unsigned int dy = 0; dy < RegionSize; dy += RegionSize / 2) for(unsigned int dx = 0; dx < RegionSize; dx += RegionSize / 2)
	{
		Region.Index[InvK] = 0;
		for(unsigned int y1 = dy; y1 < dy + RegionSize / 2; y1++) for(unsigned int x1 = dx; x1 < dx + RegionSize / 2; x1++)
		{
			Region.Data.SqrSum += sqr(p[(y1 << LSize) + x1]);
			Region.Index[InvK] += p[(y1 << LSize) + x1];
		}
		Region.Data.Sum += Region.Index[InvK];
		InvK++;
	}

	Region.Data.Average = Region.Data.Sum / sqr(RegionSize);
	
	//not needed
	//Region.Data.Div = Region.Data.SqrSum * sqr(RegionSize) - sqr(Region.Data.Sum);

	/*
	for(int i = 0; i < 4; i++)
		Region.Index[i] -= Region.Data.Sum / 4;
	float MaxIndex = max(max(abs(Region.Index[0]),abs(Region.Index[1])),max(abs(Region.Index[2]),abs(Region.Index[3])));
	for(int i = 0; i < 4; i++)
		Region.Index[i] /= MaxIndex;
	*/

	float DispSqrt = sqrt(Region.Data.SqrSum / sqr(RegionSize) - sqr(Region.Data.Average));
	for(int i = 0; i < 4; i++)
	{
		Region.Index[i] /= sqr(RegionSize / 2);
		Region.Index[i] -= Region.Data.Average;
		Region.Index[i] /= DispSqrt;
	}
	//Region.Index[4] = Region.Data.Average / 1024.0;

	return;
}

#endif
