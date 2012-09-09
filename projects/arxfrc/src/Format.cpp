#include "Format.h"

void WriteHeader(TBitWriter* Writer, unsigned int Size, bool GrayScale, bool UsesTree, unsigned int MaxDomainSize)
{
	if(Size == 256)
		Writer->WriteBit(0);
	else
		Writer->WriteBit(1);

	if(GrayScale)
		Writer->WriteBit(0);
	else
		Writer->WriteBit(1);

	if(UsesTree)
		Writer->WriteBit(1);
	else
		Writer->WriteBit(0);

	if(MaxDomainSize == 4)
		Writer->WriteBits(0x00, 2);
	else if(MaxDomainSize == 8)
		Writer->WriteBits(0x01, 2);
	else if(MaxDomainSize == 16)
		Writer->WriteBits(0x02, 2);
	else 
		Writer->WriteBits(0x03, 2);
	return;
}

void ReadHeader(TBitReader* Reader, TFrHeader& Header)
{
	if(Reader->ReadBit() == 0)	
		Header.Size = 256;
	else
		Header.Size = 512;

	if(Reader->ReadBit() == 0)	
		Header.GrayScale = true;
	else
		Header.GrayScale = false;

	if(Reader->ReadBit() == 0)	
		Header.UsesTree = false;
	else
		Header.UsesTree = true;

	unsigned int c = Reader->ReadBits(2);
	if(c == 0x00)
		Header.MaxDomainSize = 4;
	else if(c == 0x01)
		Header.MaxDomainSize = 8;
	else if(c == 0x02)
		Header.MaxDomainSize = 16;
	else 
		Header.MaxDomainSize = 32;
}

unsigned int GetCoordBitCount(unsigned int Size)
{
	if(Size == 128)
		return 6;
	else if(Size == 256)
		return 7;
	else
		return 8;
}