#include "BitIO.h"

#define BUFSIZE (1024*1024)

void TBitWriter::Init()
{
	Buf = new unsigned char [BUFSIZE];
	BufPos = 0;
	BitPos = 0;
	Buf[0] = 0;
	return;
}


TBitWriter::TBitWriter(char* FileName)
{
	Alive = true;
	F = fopen(FileName, "wb");
	if(F == NULL)
	{
		Alive = false;
		return;
	}
	Init();
	return;
}

TBitWriter::TBitWriter(FILE* _F)
{
	Alive = true;
	F = _F;
	Init();
	return;
}

TBitWriter::~TBitWriter()
{
	if(Alive)
	{
		if(BitPos != 0)
			BufPos++;
		WriteBuf();
		fclose(F);
		delete[] Buf;
	}
	return;
}

void TBitWriter::WriteBuf()
{		
	unsigned char* p = Buf;
	unsigned int Written = 0;
	do{
		Written += fwrite(p, 1, BufPos - Written, F);
		p = &Buf[Written];
	}while(BufPos - Written != 0);
	Buf[0] = Buf[BufPos];
	BufPos = 0;
	return;
}

void TBitWriter::WriteBit(unsigned char Bit)
{
	Buf[BufPos] |= (Bit & 1) << BitPos;
	BitPos++;
	if((BitPos = (BitPos & 0x00000007)) == 0)
	{
		BufPos++;
		Buf[BufPos] = 0;
		if(BufPos > BUFSIZE - 16)
			WriteBuf();
	}
	return;
}

void TBitWriter::WriteBits(unsigned int Bits, unsigned char Count)
{
	/*
	static const unsigned int uMask[33] = {0x00000000,
		0x00000001, 0x00000003, 0x00000007, 0x0000000F, 0x0000001F, 0x0000003F, 0x0000007F, 0x000000FF,
		0x000001FF, 0x000003FF, 0x000007FF, 0x00000FFF, 0x00001FFF, 0x00003FFF, 0x00007FFF, 0x0000FFFF,
		0x0001FFFF, 0x0003FFFF, 0x0007FFFF, 0x000FFFFF, 0x001FFFFF, 0x003FFFFF, 0x007FFFFF, 0x00FFFFFF,
		0x01FFFFFF, 0x03FFFFFF, 0x07FFFFFF, 0x0FFFFFFF, 0x1FFFFFFF, 0x3FFFFFFF, 0x7FFFFFFF, 0xFFFFFFFF};
	if(Count > 8 - BitPos)
	{
		Buf[BufPos] |= (Bits & uMask[8 - BitPos]) << BitPos;
		Bits >>= 8 - BitPos;
		Count -= 8 - BitPos;
		BufPos++;
		BitPos = 0;
		while(Count > 8)
		{
			Buf[BufPos] = Bits & uMask[8];
			Bits >>= 8;
			Count -= 8;
			BufPos++;
		}
		Buf[BufPos] = 0;
		Buf[BufPos] |= (Bits & uMask[Count]);
		BitPos = Count;
	}
	else
	{
		Buf[BufPos] |= (Bits & uMask[8 - BitPos]) << BitPos;
		BitPos += Count;
		if((BitPos & 0x00000007) == 0)
		{
			BufPos++;
			Buf[BufPos] = 0;
		}
	}
	if(BufPos > BUFSIZE - 16)
		WriteBuf();
	*/
	for(int i = 0; i < Count; i++)
	{
		WriteBit(Bits & 1);
		Bits >>= 1;
	}
	return;
}


void TBitReader::Init()
{
	Buf = new unsigned char [BUFSIZE];
	BufPos = 0;
	BitPos = 0;
	ReadBuf();
	return;
}

TBitReader::TBitReader(char* FileName)
{
	Alive = true;
	F = fopen(FileName, "rb");
	if(F == NULL)
	{
		Alive = false;
		return;
	}
	Init();
	return;
}

TBitReader::TBitReader(FILE* _F)
{
	Alive = true;
	F = _F;
	Init();
	return;
}


TBitReader::~TBitReader()
{
	if(Alive)
	{
		fclose(F);
		delete[] Buf;
	}
	return;
}

void TBitReader::ReadBuf()
{		
	unsigned char* p = Buf;
	unsigned int TotalRead = 0, Read;
	do{
		Read = fread(p, 1, BUFSIZE - TotalRead, F);
		if(Read == 0)
			break;
		TotalRead += Read;
		p = &Buf[TotalRead];
	}while(TotalRead != BUFSIZE);
	BufPos = 0;
	BitPos = 0;
	return;
}

unsigned char TBitReader::ReadBit()
{
	unsigned char Bit = (Buf[BufPos] >> BitPos) & 0x01;
	BitPos++;
	if((BitPos = (BitPos & 0x00000007)) == 0)
	{
		BufPos++;
		if(BufPos == BUFSIZE)
			ReadBuf();
	}
	return Bit;
}

unsigned int TBitReader::ReadBits(unsigned char Count)
{
	unsigned int Bits = 0;
	for(int i = 0; i < Count; i++)
	{
		Bits |= (ReadBit() << i);
	}
	return Bits;
}