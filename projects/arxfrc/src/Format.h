#ifndef __FORMAT_H__
#define __FORMAT_H__

#include "BitIO.h"

/* HEADER STRUCTURE:
1 BIT     0 - 256x256                     1 - 512x512
1 BIT     0 - grayscale                   1 - color
1 BIT     0 - don't spawn children        1 - spawn'em
REM 2 BITS    start with 4x4, 8x8, 16x16, or 32x32 blocks (00, 01, 10, 11)

U/V COLOR PLANES START WITH:
1 BIT     1 - x2downscaled                0 - normal size

CHILDREN SPAWNING ORDER:
01
23

NODE STRUCTURE:
*if spawning enabled
	1 BIT       spawn children? 0 - no, 1 - yes (if in 4x4 block - store pixels explicitly?)
*endif

*if no child spawned
	6-8 BITS    x coordinate       
	*if x == 0xFF then it's monotonous block and we store:
		8 BITS    color
	*else
		6-8 BITS  y coordinate
		3 BITS    transformation
		9 BITS    K coefficient      *K = chr / 256.0 - 1.0;
		9 BITS    C coefficient      *C = chr - 256.0;
	*endif
*endif
*/

#define K_MUL 256.0f
#define K_ADD 1.0f
#define K_PACKED_MAX 511
#define K_MAX 1.0f
#define K_BITS 9
#define C_MUL 1.0f
#define C_ADD 256.0f
#define C_PACKED_MAX 511
#define C_MAX 256.0f
#define C_BITS 9

#define MAXBLOCKSIZE 32

typedef struct TFrHeader{
	unsigned int Size;
	bool UsesTree;
	bool GrayScale;
	unsigned int MaxDomainSize;
} TFrHeader;


void WriteHeader(TBitWriter* Writer, unsigned int Size, bool GrayScale, bool UsesTree, unsigned int MaxDomainSize);
unsigned int GetCoordBitCount(unsigned int Size);
void ReadHeader(TBitReader* Reader, TFrHeader& Header);

#endif