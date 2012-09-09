#ifndef __BITIO_H__
#define __BITIO_H__

#include <stdio.h>

class TBitWriter
{
public:
	TBitWriter(char* FileName);
	TBitWriter(FILE* _F);
	~TBitWriter();
	void WriteBit(unsigned char Bit);
	void WriteBits(unsigned int Bits, unsigned char Count);
	bool IsAlive() {return Alive;};
private:
	void WriteBuf();
	void Init();
	unsigned int BufPos;
	unsigned int BitPos;
	bool Alive;
	FILE* F;
	unsigned char* Buf;
};

class TBitReader
{
public:
	TBitReader(char* FileName);
	TBitReader(FILE* _F);
	~TBitReader();
	unsigned char ReadBit();
	unsigned int ReadBits(unsigned char Count);
	bool IsAlive() {return Alive;};
private:
	void Init();
	void ReadBuf();
	unsigned int BufPos;
	unsigned int BitPos;
	bool Alive;
	FILE* F;
	unsigned char* Buf;
};

#endif