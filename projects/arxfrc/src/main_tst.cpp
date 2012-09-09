#include <Windows.h>
#include "ColorSpaces.h"
#include "KDTree.h"
#include "FastOps.h"
#include "ArXMath.h"
#include "BitIO.h"
#include "Transformations.h"

/*
// YUV Test
int main()
{
	DSimpleBitmap* bmp = new DSimpleBitmap();
	bmp->LoadBitmap("in.bmp");
	unsigned char** yuv = GetYUV(bmp);
	DSimpleBitmap* bmp2 = GetBMPFromYUV(yuv, bmp->GetWidth(), bmp->GetHeight());
	bmp2->SaveBitmap("out.bmp");
	return 0;
}
//*/

/*
// KDTree Test
typedef TKDTree<float, 2, void*> _KDTree;
int main()
{
	_KDTree::KDTreeItem* Items = new _KDTree::KDTreeItem[40];

	for(int i = 0; i < 20; i++)
	{
		Items[i].Data = 0;
		Items[i].Index[0] = 20 - i;
		Items[i].Index[1] = 20 - i;
	}

	for(int i = 0; i < 20; i++)
	{
		Items[i+20].Data = 0;
		Items[i+20].Index[0] = i;
		Items[i+20].Index[1] = 10 - i;
	}


	_KDTree* Tree = new _KDTree(40, Items);

	delete Items;

	float Point[2] = {3, 5};

	Tree->FindClosestPoints(Point, 6);
	
	for (int i = 0; i < Tree->FoundCount; i++) 
		printf("%f %f\n", Tree->Found[i]->Index[0], Tree->Found[i]->Index[1]);

	//Tree->FindClosestPoint(Point);
	//printf("%f %f\n", Tree->Found[0]->Index[0], Tree->Found[0]->Index[1]);


	getc(stdin);

	return 0;
}
//*/

/*
// SAD Test
int main()
{
	unsigned char t1[16] = {1,1,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
	unsigned char t2[16] = {0,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
	unsigned char lt1[64] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
	unsigned char lt2[64] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};

	unsigned char* p1;
	unsigned char* p2;
	unsigned char* p3;

	printf("%d\n", SAD8x8(lt1, lt2, 8));
	printf("%d\n", SAD4x4(t1, t2, 4));
	
	unsigned int sum = 0;

	p1 = t1;
	p2 = t2;
	unsigned int TickCount;
	TickCount = GetTickCount();
	for(int i = 0; i < 1000000000; i++)
	{
		p3=p1;p1=p2;p2=p3;
		sum += SAD4x4(p1, p2, 4);
	}
	printf("MMX: %d\n", GetTickCount() - TickCount);

	TickCount = GetTickCount();
	for(int i = 0; i < 1000000000; i++)
	{
		p3=p1;p1=p2;p2=p3;
		sum += SAD4x4_C(t1, t2, 4);
	}
	printf("C++: %d\n", GetTickCount() - TickCount);

	getc(stdin);

	return 0;
}
//*/

/*
// SUM Test
int main()
{
	//float t1[16] = {10001.0f,10010.0f,10100.0f,11000.0f,10001.0f,10010.0f,10100.0f,11000.0f,10001.0f,10010.0f,10100.0f,11000.0f,10001.0f,10010.0f,10100.0f,11000.0f};
	float t1[16] = {1.0f,2.0f,3.0f,4.0f,1.0f,2.0f,3.0f,4.0f,1.0f,2.0f,3.0f,4.0f,1.0f,2.0f,3.0f,4.0f};
	TSums Sum;

	unsigned int TickCount;
	TickCount = GetTickCount();
	for(int i = 0; i < 1000000; i++)
	{
		Sum = SUM4x4(t1, 4 * sizeof(float));
	}
	printf("XMM: %d = %f %f\n", GetTickCount() - TickCount, Sum.Sum, Sum.SqrSum);

	TickCount = GetTickCount();
	for(int i = 0; i < 1000000; i++)
	{
		Sum.Sum = 0;
		Sum.SqrSum = 0;
		Sum.Sum += t1[0];
		Sum.Sum += t1[1];
		Sum.Sum += t1[2];
		Sum.Sum += t1[3];
		Sum.Sum += t1[4];
		Sum.Sum += t1[5];
		Sum.Sum += t1[6];
		Sum.Sum += t1[7];
		Sum.Sum += t1[8];
		Sum.Sum += t1[9];
		Sum.Sum += t1[10];
		Sum.Sum += t1[11];
		Sum.Sum += t1[12];
		Sum.Sum += t1[13];
		Sum.Sum += t1[14];
		Sum.Sum += t1[15];

		Sum.SqrSum += t1[0] * t1[0];
		Sum.SqrSum += t1[1] * t1[1];
		Sum.SqrSum += t1[2] * t1[2];
		Sum.SqrSum += t1[3] * t1[3];
		Sum.SqrSum += t1[4] * t1[4];
		Sum.SqrSum += t1[5] * t1[5];
		Sum.SqrSum += t1[6] * t1[6];
		Sum.SqrSum += t1[7] * t1[7];
		Sum.SqrSum += t1[8] * t1[8];
		Sum.SqrSum += t1[9] * t1[9];
		Sum.SqrSum += t1[10] * t1[10];
		Sum.SqrSum += t1[11] * t1[11];
		Sum.SqrSum += t1[12] * t1[12];
		Sum.SqrSum += t1[13] * t1[13];
		Sum.SqrSum += t1[14] * t1[14];
		Sum.SqrSum += t1[15] * t1[15];

	}
	printf("C++: %d = %f %f\n", GetTickCount() - TickCount, Sum.Sum, Sum.SqrSum);

	getc(stdin);

	return 0;
}
//*/

/*
// BitIO Test
int main()
{
	TBitWriter* Writer = new TBitWriter("out.tst");
	Writer->WriteBits(0, 6);
	Writer->WriteBits(456, 17);
	Writer->WriteBits(6, 3);
	Writer->WriteBit(1);
	Writer->WriteBits(123123123, 32);
	Writer->WriteBits(12323, 18);
	delete Writer;

	TBitReader* Reader = new TBitReader("out.tst");
	printf("%d\n", Reader->ReadBits(6));
	printf("%d\n", Reader->ReadBits(17));
	printf("%d\n", Reader->ReadBits(3));
	printf("%d\n", Reader->ReadBit());
	printf("%d\n", Reader->ReadBits(32));
	printf("%d\n", Reader->ReadBits(18));
	delete Reader;

	getc(stdin);
	return 0;
}
//*/


// UpSample2x
int main()
{
	DSimpleBitmap* bmp = new DSimpleBitmap();
	bmp->LoadBitmap("in.bmp");
	unsigned char* y = GetGrayScale(bmp);
	unsigned char* y2 = UpSample2x(y, bmp->GetWidth());
	bmp = GetBMPFromGrayScale(y2, 2 * bmp->GetWidth(), 2 * bmp->GetHeight());
	bmp->SaveBitmap("in_2.bmp");
	return 0;
}
//*/