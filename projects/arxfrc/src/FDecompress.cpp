#include "FDecompress.h"
#include "SimpleBitmap.h"
#include "Domains.h"
#include "ColorSpaces.h"
#include "BitIO.h"
#include "Format.h"
#include "Transformations.h"

typedef struct TIteratingFunction{
	int T;
	int ToSize;
	int ToX;
	int ToY;
	int FromX;
	int FromY;
	float K;
	float C;
	bool Monotonous;
	bool Colored;
	unsigned char Color[16];
} TIteratingFunction;

unsigned char* RunIterativeFunctions(TIteratingFunction* F, int FCount, unsigned int Size)
{
	unsigned int LSize = intlog2(Size);
	unsigned char* p = new unsigned char[Size * Size];
	memset(p, 128, Size * Size);
	for(int iter = 0; iter < 20; iter++)
	{
		TSlice* Slice = new TSlice(p, Size);
		Slice->CreateTranslatedPlanes();
		for(int k = 0; k < FCount; k++)
		{
			if(F[k].Monotonous)
			{
				for(int y1 = 0; y1 < F[k].ToSize; y1++) for(int x1 = 0; x1 < F[k].ToSize; x1++)
					p[((F[k].ToY + y1) << LSize) + F[k].ToX + x1] = F[k].Color[0];
			}
			else if(F[k].Colored)
			{
				if(F[k].ToSize == 4)
				{
					p[((F[k].ToY + 0) << LSize) + F[k].ToX + 0] = F[k].Color[0];
					p[((F[k].ToY + 0) << LSize) + F[k].ToX + 1] = F[k].Color[1];
					p[((F[k].ToY + 0) << LSize) + F[k].ToX + 2] = F[k].Color[2];
					p[((F[k].ToY + 0) << LSize) + F[k].ToX + 3] = F[k].Color[3];
					p[((F[k].ToY + 1) << LSize) + F[k].ToX + 0] = F[k].Color[4];
					p[((F[k].ToY + 1) << LSize) + F[k].ToX + 1] = F[k].Color[5];
					p[((F[k].ToY + 1) << LSize) + F[k].ToX + 2] = F[k].Color[6];
					p[((F[k].ToY + 1) << LSize) + F[k].ToX + 3] = F[k].Color[7];
					p[((F[k].ToY + 2) << LSize) + F[k].ToX + 0] = F[k].Color[8];
					p[((F[k].ToY + 2) << LSize) + F[k].ToX + 1] = F[k].Color[9];
					p[((F[k].ToY + 2) << LSize) + F[k].ToX + 2] = F[k].Color[10];
					p[((F[k].ToY + 2) << LSize) + F[k].ToX + 3] = F[k].Color[11];
					p[((F[k].ToY + 3) << LSize) + F[k].ToX + 0] = F[k].Color[12];
					p[((F[k].ToY + 3) << LSize) + F[k].ToX + 1] = F[k].Color[13];
					p[((F[k].ToY + 3) << LSize) + F[k].ToX + 2] = F[k].Color[14];
					p[((F[k].ToY + 3) << LSize) + F[k].ToX + 3] = F[k].Color[15];
				}
				else if(F[k].ToSize == 8)
				{
					for(int x = 0; x < 8; x++) for (int y = 0; y < 8; y++) 
						p[((F[k].ToY + y) << LSize) + F[k].ToX + x] = F[k].Color[((y >> 1) << 2) + (x >> 1)];
				}
			}
			else
			{
				for(int y1 = 0; y1 < F[k].ToSize; y1++) for(int x1 = 0; x1 < F[k].ToSize; x1++)
					p[((F[k].ToY + y1) << LSize) + F[k].ToX + x1] = 
						Clamp(Slice->Data[F[k].T][1][((F[k].FromY + y1) << (LSize - 1)) + F[k].FromX + x1] * F[k].K + F[k].C);
			}
		}
		delete Slice;
	}
	return p;
}

int ReadIteratingFunctionTree(TBitReader& Reader, TIteratingFunction* F, const unsigned int x, const unsigned int y, const unsigned int Width, const unsigned int CoordBitCount, const unsigned int Size)
{
	if(Reader.ReadBit() == 0)
	{
		F[0].Colored = false;
		F[0].ToSize = Width;
		F[0].ToX = x;
		F[0].ToY = y;
		F[0].FromX = Reader.ReadBits(CoordBitCount);
		if(F[0].FromX == (Size >> 1) - 1)
		{
			F[0].Monotonous = true;
			F[0].Color[0] = Reader.ReadBits(8);
		}
		else
		{
			F[0].Monotonous = false;
			F[0].FromY = Reader.ReadBits(CoordBitCount);
			F[0].T = Reader.ReadBits(3);
			F[0].K = (Reader.ReadBits(K_BITS)) / K_MUL - K_ADD;
			F[0].C = (Reader.ReadBits(C_BITS)) / C_MUL - C_ADD;
		}
		return 1;
	}
	else
	{
		if(Width == 4)
		{
			F[0].Colored = true;
			F[0].Monotonous = false;
			F[0].ToX = x;
			F[0].ToY = y;
			F[0].ToSize = Width;
			F[0].Color[0] = Reader.ReadBits(8);
			F[0].Color[1] = Reader.ReadBits(8);
			F[0].Color[2] = Reader.ReadBits(8);
			F[0].Color[3] = Reader.ReadBits(8);
			F[0].Color[4] = Reader.ReadBits(8);
			F[0].Color[5] = Reader.ReadBits(8);
			F[0].Color[6] = Reader.ReadBits(8);
			F[0].Color[7] = Reader.ReadBits(8);
			F[0].Color[8] = Reader.ReadBits(8);
			F[0].Color[9] = Reader.ReadBits(8);
			F[0].Color[10] = Reader.ReadBits(8);
			F[0].Color[11] = Reader.ReadBits(8);
			F[0].Color[12] = Reader.ReadBits(8);
			F[0].Color[13] = Reader.ReadBits(8);
			F[0].Color[14] = Reader.ReadBits(8);
			F[0].Color[15] = Reader.ReadBits(8);
			return 1;
		}
		else
		{
			int k = 0;
			k += ReadIteratingFunctionTree(Reader, &F[k], x, y, Width >> 1, CoordBitCount, Size);
			k += ReadIteratingFunctionTree(Reader, &F[k], x + (Width >> 1), y, Width >> 1, CoordBitCount, Size);
			k += ReadIteratingFunctionTree(Reader, &F[k], x, y + (Width >> 1), Width >> 1, CoordBitCount, Size);
			k += ReadIteratingFunctionTree(Reader, &F[k], x + (Width >> 1), y + (Width >> 1), Width >> 1, CoordBitCount, Size);
			return k;
		}
	}
}

unsigned char* DecompressColorPlaneTree(TBitReader& Reader, unsigned int Size, unsigned int MaxDomainSize, bool MayBeDownScaled)
{
	bool DownScaled = false;
	if(MayBeDownScaled)
	{
		if(Reader.ReadBit() == 0)
			DownScaled = false;
		else
			DownScaled = true;
	}
	if(DownScaled)
		Size /= 2;

	unsigned int CoordBitCount = GetCoordBitCount(Size);

	TIteratingFunction* F = new TIteratingFunction[sqr(Size / 4)];
	int k = 0;
	for(unsigned int y = 0; y < Size; y += MaxDomainSize) for(unsigned int x = 0; x < Size; x += MaxDomainSize)
		k += ReadIteratingFunctionTree(Reader, &F[k], x, y, MaxDomainSize, CoordBitCount, Size);

	if(DownScaled)
	{
		for(int i = 0; i < k; i++)
		{
			F[i].FromX <<= 1;
			F[i].FromY <<= 1;
			F[i].ToX <<= 1;
			F[i].ToY <<= 1;
			F[i].ToSize <<= 1;
		}
		Size <<= 1;
	}

	unsigned char* p = RunIterativeFunctions(F, k, Size);
	delete F;
	return p;
}


unsigned char* DecompressColorPlane(TBitReader& Reader, unsigned int Size, unsigned int MaxDomainSize, bool UsesTree, bool MayBeDownScaled)
{
	if(UsesTree)
		return DecompressColorPlaneTree(Reader, Size, MaxDomainSize, MayBeDownScaled);
	else
	{
		bool DownScaled = false;
		if(MayBeDownScaled)
		{
			if(Reader.ReadBit() == 0)
				DownScaled = false;
			else
				DownScaled = true;
		}
		if(DownScaled)
			Size /= 2;

		unsigned int CoordBitCount = GetCoordBitCount(Size);

		TIteratingFunction* F = new TIteratingFunction[sqr(Size / MaxDomainSize)];
		int k = 0;
		for(unsigned int y = 0; y < Size; y += MaxDomainSize) for(unsigned int x = 0; x < Size; x += MaxDomainSize)
		{
			F[k].Colored = false;
			F[k].ToSize = MaxDomainSize;
			F[k].ToX = x;
			F[k].ToY = y;
			F[k].FromX = Reader.ReadBits(CoordBitCount);
			if(F[k].FromX == (Size >> 1) - 1)
			{
				F[k].Monotonous = true;
				F[k].Color[0] = Reader.ReadBits(8);
			}
			else
			{
				F[k].Monotonous = false;
				F[k].FromY = Reader.ReadBits(CoordBitCount);
				F[k].T = Reader.ReadBits(3);
				F[k].K = (Reader.ReadBits(K_BITS)) / K_MUL - K_ADD;
				F[k].C = (Reader.ReadBits(C_BITS)) / C_MUL - C_ADD;
			}
			k++;
		}

		if(DownScaled)
		{
			for(int i = 0; i < k; i++)
			{
				F[i].FromX <<= 1;
				F[i].FromY <<= 1;
				F[i].ToX <<= 1;
				F[i].ToY <<= 1;
				F[i].ToSize <<= 1;
			}
			Size <<= 1;
		}

		unsigned char* p = RunIterativeFunctions(F, k, Size);
		delete[] F;
		return p;
	}
}


int FractalDecompress(char* InFile, char* OutFile)
{
	FILE* in;
	if((in = fopen(InFile, "r")) == NULL)
	{
		printf("Failed to open file %s.", InFile);
		return -1;
	}

	TBitReader* Reader = new TBitReader(in);

	TFrHeader Header;
	ReadHeader(Reader, Header);
	
	unsigned char* yuv[3];
	yuv[0] = new unsigned char[sqr(Header.Size)];
	
	DSimpleBitmap* bmp;
	yuv[0] = DecompressColorPlane(*Reader, Header.Size, Header.MaxDomainSize, Header.UsesTree, false);
	if(!Header.GrayScale)
	{
		yuv[1] = DecompressColorPlane(*Reader, Header.Size, Header.MaxDomainSize, Header.UsesTree, true);
		//bmp = GetBMPFromGrayScale(yuv[1], Header.Size, Header.Size);bmp->SaveBitmap("u.bmp");

		yuv[2] = DecompressColorPlane(*Reader, Header.Size, Header.MaxDomainSize, Header.UsesTree, true);
		//bmp = GetBMPFromGrayScale(yuv[2], Header.Size, Header.Size);bmp->SaveBitmap("v.bmp");

		bmp = GetBMPFromYUV(yuv, Header.Size, Header.Size);
	}
	else
		bmp = GetBMPFromGrayScale(yuv[0], Header.Size, Header.Size);
	bmp->SaveBitmap(OutFile);
	return 0;
}