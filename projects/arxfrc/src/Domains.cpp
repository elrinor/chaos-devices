#include <math.h>
#include <stdlib.h>
#include "Domains.h"
#include "Transformations.h"
#include "SimpleBitmap.h"
#include "ColorSpaces.h"
#include "FastOps.h"
#include "ArXMath.h"

TSlice::TSlice(unsigned char* _Data, int _Size)
{
	Size = _Size;
	Data[0][0] = Transform(_Data, Size, 0);
	Data[0][1] = DownSample2x(Data[0][0], Size);
	FData[0][0] = PlaneByteToFloat(Data[0][0], Size);
	FData[0][1] = PlaneByteToFloat(Data[0][1], Size >> 1);
	for(int i = 1; i < 8; i++) for(int j = 0; j < 2; j++)
	{
		Data[i][j] = NULL;
		FData[i][j] = NULL;
	}
	for(int i = 0; i < 4; i++)
		DomainPool[i].Tree = NULL;
	//CreateTranslatedPlanes();
	//DSimpleBitmap* bmp;	bmp = GetBMPFromGrayScale(Data[0][0], _Size, _Size); bmp->SaveBitmap("0_0.bmp");	bmp = GetBMPFromGrayScale(Data[1][0], _Size, _Size); bmp->SaveBitmap("1_0.bmp");	bmp = GetBMPFromGrayScale(Data[2][0], _Size, _Size); bmp->SaveBitmap("2_0.bmp");	bmp = GetBMPFromGrayScale(Data[3][0], _Size, _Size); bmp->SaveBitmap("3_0.bmp");	bmp = GetBMPFromGrayScale(Data[4][0], _Size, _Size); bmp->SaveBitmap("4_0.bmp");	bmp = GetBMPFromGrayScale(Data[5][0], _Size, _Size); bmp->SaveBitmap("5_0.bmp");	bmp = GetBMPFromGrayScale(Data[6][0], _Size, _Size); bmp->SaveBitmap("6_0.bmp");	bmp = GetBMPFromGrayScale(Data[7][0], _Size, _Size); bmp->SaveBitmap("7_0.bmp");	_Size >>= 1;	bmp = GetBMPFromGrayScale(Data[0][1], _Size, _Size); bmp->SaveBitmap("0_1.bmp");	bmp = GetBMPFromGrayScale(Data[1][1], _Size, _Size); bmp->SaveBitmap("1_1.bmp");	bmp = GetBMPFromGrayScale(Data[2][1], _Size, _Size); bmp->SaveBitmap("2_1.bmp");	bmp = GetBMPFromGrayScale(Data[3][1], _Size, _Size); bmp->SaveBitmap("3_1.bmp");	bmp = GetBMPFromGrayScale(Data[4][1], _Size, _Size); bmp->SaveBitmap("4_1.bmp");	bmp = GetBMPFromGrayScale(Data[5][1], _Size, _Size); bmp->SaveBitmap("5_1.bmp");	bmp = GetBMPFromGrayScale(Data[6][1], _Size, _Size); bmp->SaveBitmap("6_1.bmp");	bmp = GetBMPFromGrayScale(Data[7][1], _Size, _Size); bmp->SaveBitmap("7_1.bmp");//*/
	return;
}

TSlice::~TSlice()
{
	for(int i = 0; i < 8; i++) for(int j = 0; j < 2; j++)
	{
		if(Data[i][j] != NULL)
			delete Data[i][j];
		if(FData[i][j] != NULL)
			PlaneFreeConvertedToFloat(FData[i][j]);
	}
	for(int i = 0; i < 4; i++) if(DomainPool[i].Tree != NULL)
		delete DomainPool[i].Tree;
	return;
}

void TSlice::CreateTranslatedPlanes()
{
	for(int i = 1; i < 8; i++)
		Data[i][0] = Transform(Data[0][0], Size, i);
	for(int i = 1; i < 8; i++)
		Data[i][1] = DownSample2x(Data[i][0], Size);
	for(int i = 1; i < 8; i++)
	{
		FData[i][0] = PlaneByteToFloat(Data[i][0], Size);
		FData[i][1] = PlaneByteToFloat(Data[i][1], Size >> 1);
	}
	return;
}

void TSlice::CreateDomainPools(int MaxDomainSize)
{
	unsigned int LSize = intlog2(Size);
	unsigned int LSizeDec = LSize - 1;
	unsigned int MaxN = intlog2(MaxDomainSize) - 3;
	KDTreeD::KDTreeItem* Domains = new KDTreeD::KDTreeItem[8 * sqr(Size/2)];
	float* SumMatrix = new float [sqr(Size/2)];
	float* SqrSumMatrix = new float [sqr(Size/2)];

	// Calculate 4x4 Sums
	for(unsigned int y = 0; y <= (Size - 4) >> 1; y++) for(unsigned int x = 0; x <= (Size - 4) >> 1; x++)
	{
		// LWidth(SumMatrix) == LSize - 1
		// LWidth(FData[0][1]) == LSize - 1
		SumMatrix[(y << LSizeDec) + x] = (
			FData[0][1][(y << LSizeDec) + x] + 
			FData[0][1][(y << LSizeDec) + x + 1] + 
			FData[0][1][((y + 1) << LSizeDec) + x] + 
			FData[0][1][((y + 1) << LSizeDec) + x + 1]);
		SqrSumMatrix[(y << LSizeDec) + x] = (
			sqr(FData[0][1][(y << LSizeDec) + x]) + 
			sqr(FData[0][1][(y << LSizeDec) + x + 1]) + 
			sqr(FData[0][1][((y + 1) << LSizeDec) + x]) + 
			sqr(FData[0][1][((y + 1) << LSizeDec) + x + 1]));
	}

	for(unsigned int n = 0; n <= MaxN; n++)
	{
		unsigned int LBlockSize = n + 3;
		unsigned int BlockSizeDiv4 = pow2(LBlockSize - 2);
		unsigned int BlockSize = pow2(LBlockSize); // REAL Block Size (8x8 .. 64x64)

		unsigned int k = 0;
		for(unsigned int y = 0; y <= (Size - BlockSize) >> 1; y++) for(unsigned int x = 0; x <= (Size - BlockSize) >> 1; x++)
		{
			KDTreeD::KDTreeItem Region;
			CalculateRegionInvariants(Region, BlockSize >> 1, &FData[0][1][(y << (LSize - 1)) + x], LSize - 1);

			Domains[k].Index[0] = SumMatrix[(y << LSizeDec) + x];
			Domains[k].Index[1] = SumMatrix[(y << LSizeDec) + x + BlockSizeDiv4];
			Domains[k].Index[2] = SumMatrix[((y + BlockSizeDiv4) << LSizeDec) + x];
			Domains[k].Index[3] = SumMatrix[((y + BlockSizeDiv4) << LSizeDec) + x + BlockSizeDiv4];
			Domains[k].Data.Sum = 
				Domains[k].Index[0] + Domains[k].Index[1] + Domains[k].Index[2] + Domains[k].Index[3];
			Domains[k].Data.SqrSum = 
				SqrSumMatrix[(y << LSizeDec) + x] + 
				SqrSumMatrix[(y << LSizeDec) + x + BlockSizeDiv4] + 
				SqrSumMatrix[((y + BlockSizeDiv4) << LSizeDec) + x] + 
				SqrSumMatrix[((y + BlockSizeDiv4) << LSizeDec) + (x + BlockSizeDiv4)];
			Domains[k].Data.Average = Domains[k].Data.Sum / sqr(BlockSize / 2);
			Domains[k].Data.Size = BlockSize / 2;
			Domains[k].Data.Transformation = 0;
			Domains[k].Data.x = x;
			Domains[k].Data.y = y;
			Domains[k].Data.Div = Domains[k].Data.SqrSum * sqr(Domains[k].Data.Size) - sqr(Domains[k].Data.Sum);

			/*
			for(int i = 0; i < 4; i++)
				Domains[k].Index[i] -= Domains[k].Data.Sum / 4;
			float MaxIndex = max(max(abs(Domains[k].Index[0]),abs(Domains[k].Index[1])),max(abs(Domains[k].Index[2]),abs(Domains[k].Index[3])));
			for(int i = 0; i < 4; i++)
				Domains[k].Index[i] /= MaxIndex;
			*/

			float DispSqrt = sqrt(Domains[k].Data.SqrSum / sqr(BlockSize / 2) - sqr(Domains[k].Data.Average));
			for(int i = 0; i < 4; i++)
			{
				Domains[k].Index[i] /= sqr(BlockSize / 4);
				Domains[k].Index[i] -= Domains[k].Data.Average;
				Domains[k].Index[i] /= DispSqrt;
			}
			//Domains[k].Index[4] = Domains[k].Data.Average / 1024.0;

			SumMatrix[(y << LSizeDec) + x] = Domains[k].Data.Sum;
			SqrSumMatrix[(y << LSizeDec) + x] = Domains[k].Data.SqrSum;
			k++;
		}
		
		
		for(int i = 1; i < 8; i++)
			memcpy(&Domains[i * k], Domains, k * sizeof(KDTreeD::KDTreeItem));
		
		for(unsigned int q = 0; q < k; q++)
		{
			Domains[1 * k + q].Data.Transformation = 1;
			Domains[1 * k + q].Index[0] = Domains[q].Index[1];
			Domains[1 * k + q].Index[1] = Domains[q].Index[0];
			Domains[1 * k + q].Index[2] = Domains[q].Index[3];
			Domains[1 * k + q].Index[3] = Domains[q].Index[2];
			TransformRegion(Domains[q].Data.x, Domains[q].Data.y, Domains[1 * k + q].Data.x, Domains[1 * k + q].Data.y, Size >> 1, Domains[k].Data.Size, 1);

			Domains[2 * k + q].Data.Transformation = 2;
			Domains[2 * k + q].Index[0] = Domains[q].Index[2];
			Domains[2 * k + q].Index[1] = Domains[q].Index[3];
			Domains[2 * k + q].Index[2] = Domains[q].Index[0];
			Domains[2 * k + q].Index[3] = Domains[q].Index[1];
			TransformRegion(Domains[q].Data.x, Domains[q].Data.y, Domains[2 * k + q].Data.x, Domains[2 * k + q].Data.y, Size >> 1, Domains[k].Data.Size, 2);

			Domains[3 * k + q].Data.Transformation = 3;
			Domains[3 * k + q].Index[0] = Domains[q].Index[3];
			Domains[3 * k + q].Index[1] = Domains[q].Index[2];
			Domains[3 * k + q].Index[2] = Domains[q].Index[1];
			Domains[3 * k + q].Index[3] = Domains[q].Index[0];
			TransformRegion(Domains[q].Data.x, Domains[q].Data.y, Domains[3 * k + q].Data.x, Domains[3 * k + q].Data.y, Size >> 1, Domains[k].Data.Size, 3);

			Domains[4 * k + q].Data.Transformation = 4;
			Domains[4 * k + q].Index[0] = Domains[q].Index[0];
			Domains[4 * k + q].Index[1] = Domains[q].Index[2];
			Domains[4 * k + q].Index[2] = Domains[q].Index[1];
			Domains[4 * k + q].Index[3] = Domains[q].Index[3];
			TransformRegion(Domains[q].Data.x, Domains[q].Data.y, Domains[4 * k + q].Data.x, Domains[4 * k + q].Data.y, Size >> 1, Domains[k].Data.Size, 4);

			Domains[5 * k + q].Data.Transformation = 5;
			Domains[5 * k + q].Index[0] = Domains[q].Index[1];
			Domains[5 * k + q].Index[1] = Domains[q].Index[3];
			Domains[5 * k + q].Index[2] = Domains[q].Index[0];
			Domains[5 * k + q].Index[3] = Domains[q].Index[2];
			TransformRegion(Domains[q].Data.x, Domains[q].Data.y, Domains[5 * k + q].Data.x, Domains[5 * k + q].Data.y, Size >> 1, Domains[k].Data.Size, 5);

			Domains[6 * k + q].Data.Transformation = 6;
			Domains[6 * k + q].Index[0] = Domains[q].Index[2];
			Domains[6 * k + q].Index[1] = Domains[q].Index[0];
			Domains[6 * k + q].Index[2] = Domains[q].Index[3];
			Domains[6 * k + q].Index[3] = Domains[q].Index[1];
			TransformRegion(Domains[q].Data.x, Domains[q].Data.y, Domains[6 * k + q].Data.x, Domains[6 * k + q].Data.y, Size >> 1, Domains[k].Data.Size, 6);

			Domains[7 * k + q].Data.Transformation = 7;
			Domains[7 * k + q].Index[0] = Domains[q].Index[3];
			Domains[7 * k + q].Index[1] = Domains[q].Index[1];
			Domains[7 * k + q].Index[2] = Domains[q].Index[2];
			Domains[7 * k + q].Index[3] = Domains[q].Index[0];
			TransformRegion(Domains[q].Data.x, Domains[q].Data.y, Domains[7 * k + q].Data.x, Domains[7 * k + q].Data.y, Size >> 1, Domains[k].Data.Size, 7);
		}

		DomainPool[n].DomainSize = BlockSize / 2;
		DomainPool[n].Tree = new KDTreeD(8 * k, Domains);
	}

	delete[] Domains;
	delete[] SumMatrix;
	delete[] SqrSumMatrix;
	return;
}
