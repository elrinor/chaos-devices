#include <math.h>
#include "FCompress.h"
#include "SimpleBitmap.h"
#include "ColorSpaces.h"
#include "Domains.h"
#include "FastOps.h"
#include "BitIO.h"
#include "Format.h"
#include "Transformations.h"

#define KD_LOOKUP_COUNT_8X8_2 4000
#define KD_RESEARCH_BORDER_8X8 (64*4000)

#define KD_LOOKUP_COUNT_32X32 40
#define KD_LOOKUP_COUNT_16X16 80
#define KD_LOOKUP_COUNT_8X8 250
#define KD_LOOKUP_COUNT_4X4 500
#define MONOTONOUS_BORDER 20
#define QUALITY_MUL 1024
#define GOOD_DIST_BORDER 2


int FractalCompress8x8(TSlice& Slice, TBitWriter& Writer)
{
	unsigned int CoordBitCount = GetCoordBitCount(Slice.Size);
	Slice.CreateDomainPools(16);
	Slice.CreateTranslatedPlanes();

	int LSize = intlog2(Slice.Size);
	for(unsigned int y = 0; y < Slice.Size; y += 8) for(unsigned int x = 0; x < Slice.Size; x += 8)
	{
		KDTreeD::KDTreeItem Region;
		CalculateRegionInvariants(Region, 8, &Slice.FData[0][0][(y << LSize) + x], LSize);
		if(abs(64 * sqr(Region.Data.Average) - Region.Data.SqrSum) < MONOTONOUS_BORDER)
		{
			Writer.WriteBits(0xFF, CoordBitCount);
			Writer.WriteBits((unsigned char)Region.Data.Average, 8);
			continue;
		}
		
		Slice.DomainPool[1].Tree->FindClosestPoints(Region.Index, KD_LOOKUP_COUNT_8X8);
		while(true)
		{
			float BestDist = FLOAT_PLUS_INF;
			float BestK = 0;
			float BestC = 0;
			TDomain* BestDomain = NULL;

			for(int i = 0; i < Slice.DomainPool[1].Tree->FoundCount; i++)
			{
				TDomain& CurrentDomain = Slice.DomainPool[1].Tree->Found[i]->Data;

				float ScalarMul = SCP8x8(&Slice.FData[CurrentDomain.Transformation][1][(CurrentDomain.y << (LSize - 1)) + CurrentDomain.x], &Slice.FData[0][0][(y << LSize) + x], Slice.Size << 1 /*(Size*sizeof(float)/2)*/, Slice.Size << 2 /*(Size*sizeof(float))*/);
				float K = (64 * ScalarMul - Region.Data.Sum * CurrentDomain.Sum) / CurrentDomain.Div;
				if(abs(K) > K_MAX)
					continue;
				float C = (Region.Data.Sum - CurrentDomain.Sum * K) / 64;
				if(abs(C) > C_MAX)
					continue;
				float Dist = 
					sqr(K) * CurrentDomain.SqrSum + 64 * sqr(C) + Region.Data.SqrSum + 
					2 * (C * K * CurrentDomain.Sum - K * ScalarMul - C * Region.Data.Sum);

				if(Dist < BestDist)
				{
					BestDist = Dist;
					BestK = K;
					BestC = C;
					BestDomain = &CurrentDomain;
				}
			}
			
			if(Slice.DomainPool[1].Tree->FoundCount <= KD_LOOKUP_COUNT_8X8)
			{
				if(BestDomain == NULL || BestDist > KD_RESEARCH_BORDER_8X8)
				{
					Slice.DomainPool[1].Tree->FindClosestPoints(Region.Index, KD_LOOKUP_COUNT_8X8_2);
					continue;
				}
			}


			if(BestDomain != NULL)
			{
				Writer.WriteBits(BestDomain->x, CoordBitCount);
				Writer.WriteBits(BestDomain->y, CoordBitCount);
				Writer.WriteBits(BestDomain->Transformation, 3);
		
				unsigned int intK = (int)((BestK + K_ADD) * K_MUL);
				if(intK > K_PACKED_MAX)
					intK = K_PACKED_MAX;
				Writer.WriteBits(intK, K_BITS);
				unsigned int intC = (int)((BestC + C_ADD) * C_MUL);
				if(intC > C_PACKED_MAX)
					intC = C_PACKED_MAX;
				Writer.WriteBits(intC, C_BITS);
				break;
			}
			else
			{
				Writer.WriteBits(0xFF, CoordBitCount);
				Writer.WriteBits((unsigned char)Region.Data.Average, 8);
				break;
			}
		}

		//fprintf(out, "%d %d %d %f %f\n", BestDomain->Transformation,  BestDomain->x, BestDomain->y, BestK, BestC);
		//fprintf(out, "%d %d %d %d %d %f %f\n", x, y, BestDomain->Transformation,  BestDomain->x, BestDomain->y, BestK, BestC);
	}

	return 0;
}

inline int GetLookupCount(const unsigned int Width)
{
	switch(Width)
	{
	case 4:
		return KD_LOOKUP_COUNT_4X4;
	case 8:
		return KD_LOOKUP_COUNT_8X8;
	case 16:
		return KD_LOOKUP_COUNT_16X16;
	case 32:
		return KD_LOOKUP_COUNT_32X32;
	default:
		return 1;
	}
}

int CompressBlockTree(TSlice& Slice, TBitWriter& Writer, const unsigned int x, const unsigned int y, const unsigned int Width, const unsigned int Quality)
{
	unsigned int DomainPoolN = intlog2(Width) - 2;
	KDTreeD::KDTreeItem Region;
	CalculateRegionInvariants(Region, Width, &Slice.FData[0][0][(y << Slice.LSize) + x], Slice.LSize);
	if(abs(sqr(Width * Region.Data.Average) - Region.Data.SqrSum) < MONOTONOUS_BORDER)
	{
		Writer.WriteBit(0);
		Writer.WriteBits(0xFF, Slice.CoordBitCount);
		Writer.WriteBits((unsigned char)Region.Data.Average, 8);
		return 0;
	}

	Slice.DomainPool[DomainPoolN].Tree->FindClosestPoints(Region.Index, GetLookupCount(Width));
	float BestDist = FLOAT_PLUS_INF;
	float BestK = 0;
	float BestC = 0;
	TDomain* BestDomain = NULL;

	for(int i = 0; i < Slice.DomainPool[DomainPoolN].Tree->FoundCount; i++)
	{
		TDomain& CurrentDomain = Slice.DomainPool[DomainPoolN].Tree->Found[i]->Data;
		
		float ScalarMul;
		switch(Width)
		{
		case 4:
			ScalarMul = SCP4x4(&Slice.FData[CurrentDomain.Transformation][1][(CurrentDomain.y << (Slice.LSize - 1)) + CurrentDomain.x], &Slice.FData[0][0][(y << Slice.LSize) + x], Slice.Size << 1 /*(Size*sizeof(float)/2)*/, Slice.Size << 2 /*(Size*sizeof(float))*/);
			break;
		case 8:
			ScalarMul = SCP8x8(&Slice.FData[CurrentDomain.Transformation][1][(CurrentDomain.y << (Slice.LSize - 1)) + CurrentDomain.x], &Slice.FData[0][0][(y << Slice.LSize) + x], Slice.Size << 1 /*(Size*sizeof(float)/2)*/, Slice.Size << 2 /*(Size*sizeof(float))*/);
			break;
		case 16:
			ScalarMul = SCP16x16(&Slice.FData[CurrentDomain.Transformation][1][(CurrentDomain.y << (Slice.LSize - 1)) + CurrentDomain.x], &Slice.FData[0][0][(y << Slice.LSize) + x], Slice.Size << 1 /*(Size*sizeof(float)/2)*/, Slice.Size << 2 /*(Size*sizeof(float))*/);
			break;
		case 32:
			ScalarMul = SCP32x32(&Slice.FData[CurrentDomain.Transformation][1][(CurrentDomain.y << (Slice.LSize - 1)) + CurrentDomain.x], &Slice.FData[0][0][(y << Slice.LSize) + x], Slice.Size << 1 /*(Size*sizeof(float)/2)*/, Slice.Size << 2 /*(Size*sizeof(float))*/);
			break;
		default:
			break;
		}
		float K = (sqr(Width) * ScalarMul - Region.Data.Sum * CurrentDomain.Sum) / CurrentDomain.Div;
		if(abs(K) > K_MAX)
			continue;
		float C = (Region.Data.Sum - CurrentDomain.Sum * K) / sqr(Width);
		if(abs(C) > C_MAX)
			continue;
		float Dist = 
			sqr(K) * CurrentDomain.SqrSum + sqr(C * Width) + Region.Data.SqrSum + 
			2 * (C * K * CurrentDomain.Sum - K * ScalarMul - C * Region.Data.Sum);

		if(Dist < BestDist)
		{
			BestDist = Dist;
			BestK = K;
			BestC = C;
			BestDomain = &CurrentDomain;
		}
	}

	if(BestDomain != NULL && BestDist <= sqr(Width) * (GOOD_DIST_BORDER + (100 - Quality) * QUALITY_MUL / sqr(Width)))
	{
		Writer.WriteBit(0);
		Writer.WriteBits(BestDomain->x, Slice.CoordBitCount);
		Writer.WriteBits(BestDomain->y, Slice.CoordBitCount);
		Writer.WriteBits(BestDomain->Transformation, 3);

		unsigned int intK = (int)((BestK + K_ADD) * K_MUL);
		if(intK > K_PACKED_MAX)
			intK = K_PACKED_MAX;
		Writer.WriteBits(intK, K_BITS);
		unsigned int intC = (int)((BestC + C_ADD) * C_MUL);
		if(intC > C_PACKED_MAX)
			intC = C_PACKED_MAX;
		Writer.WriteBits(intC, C_BITS);
	}
	else
	{
		Writer.WriteBit(1);
		if(Width != 4)
		{
			CompressBlockTree(Slice, Writer, x, y, Width >> 1, Quality);
			CompressBlockTree(Slice, Writer, x + (Width >> 1), y, Width >> 1, Quality);
			CompressBlockTree(Slice, Writer, x, y + (Width >> 1), Width >> 1, Quality);
			CompressBlockTree(Slice, Writer, x + (Width >> 1), y + (Width >> 1), Width >> 1, Quality);
		}
		else
		{
			Writer.WriteBits(Slice.Data[0][0][((y  ) << Slice.LSize) + (x  )], 8);
			Writer.WriteBits(Slice.Data[0][0][((y  ) << Slice.LSize) + (x+1)], 8);
			Writer.WriteBits(Slice.Data[0][0][((y  ) << Slice.LSize) + (x+2)], 8);
			Writer.WriteBits(Slice.Data[0][0][((y  ) << Slice.LSize) + (x+3)], 8);
			Writer.WriteBits(Slice.Data[0][0][((y+1) << Slice.LSize) + (x  )], 8);
			Writer.WriteBits(Slice.Data[0][0][((y+1) << Slice.LSize) + (x+1)], 8);
			Writer.WriteBits(Slice.Data[0][0][((y+1) << Slice.LSize) + (x+2)], 8);
			Writer.WriteBits(Slice.Data[0][0][((y+1) << Slice.LSize) + (x+3)], 8);
			Writer.WriteBits(Slice.Data[0][0][((y+2) << Slice.LSize) + (x  )], 8);
			Writer.WriteBits(Slice.Data[0][0][((y+2) << Slice.LSize) + (x+1)], 8);
			Writer.WriteBits(Slice.Data[0][0][((y+2) << Slice.LSize) + (x+2)], 8);
			Writer.WriteBits(Slice.Data[0][0][((y+2) << Slice.LSize) + (x+3)], 8);
			Writer.WriteBits(Slice.Data[0][0][((y+3) << Slice.LSize) + (x  )], 8);
			Writer.WriteBits(Slice.Data[0][0][((y+3) << Slice.LSize) + (x+1)], 8);
			Writer.WriteBits(Slice.Data[0][0][((y+3) << Slice.LSize) + (x+2)], 8);
			Writer.WriteBits(Slice.Data[0][0][((y+3) << Slice.LSize) + (x+3)], 8);
		}
	}

	return 0;
}

int FractalCompressTree(TSlice& Slice, TBitWriter& Writer, const unsigned int Quality)
{
	Slice.CreateDomainPools(64);
	Slice.CreateTranslatedPlanes();
	Slice.LSize = intlog2(Slice.Size);
	Slice.CoordBitCount = GetCoordBitCount(Slice.Size);

	for(unsigned int y = 0; y < Slice.Size; y += MAXBLOCKSIZE) for(unsigned int x = 0; x < Slice.Size; x += MAXBLOCKSIZE)
		CompressBlockTree(Slice, Writer, x, y, MAXBLOCKSIZE, Quality);

	return 0;
}

int FractalCompress(char* InFile, char* OutFile, bool FixedSize, int Quality)
{
	if(!FixedSize)
		Quality = (unsigned int) sqrt(100.0f * Quality);

	DSimpleBitmap* bmp = new DSimpleBitmap();
	if(!bmp->LoadBitmap(InFile))
	{
		printf("Failed to load bmp image %s\n", InFile);
		return -1;
	}

	int w = bmp->GetWidth(), h = bmp->GetHeight();

	if((h != w) || !(h == 256 || h == 512))
	{
		printf("Only 256*256 and 512*512 images are allowed.\n");
		return -1;
	}

	FILE* out;
	if((out = fopen(OutFile, "wb")) == NULL)
	{
		printf("Failed to open file %s.", OutFile);
		return -1;
	}

	bool GrayScale = IsGrayScale(bmp);

	TBitWriter* Writer = new TBitWriter(out);
	if(FixedSize)
		WriteHeader(Writer, w, GrayScale, false, 8);
	else
		WriteHeader(Writer, w, GrayScale, true, 32);

	unsigned char** yuv = GetYUV(bmp);

	TSlice* Slice = new TSlice(yuv[0], w);
	if(FixedSize)
		FractalCompress8x8(*Slice, *Writer);
	else
		FractalCompressTree(*Slice, *Writer, Quality);
	delete Slice;
	
	if(!GrayScale)
	{
		unsigned char* u = DownSample2x(yuv[1], w);
		Slice = new TSlice(u, w / 2);
		Writer->WriteBit(1);
		if(FixedSize)
			FractalCompress8x8(*Slice, *Writer);
		else
			FractalCompressTree(*Slice, *Writer, Quality);
		delete Slice;

		unsigned char* v = DownSample2x(yuv[2], w);
		Slice = new TSlice(v, w / 2);
		Writer->WriteBit(1);
		if(FixedSize)
			FractalCompress8x8(*Slice, *Writer);
		else
			FractalCompressTree(*Slice, *Writer, Quality);
		delete Slice;
		
		delete[] u;
		delete[] v;
	}

	delete[] yuv[0];
	delete[] yuv[1];
	delete[] yuv[2];
	delete[] yuv;
	delete Writer;
	return 0;
}
