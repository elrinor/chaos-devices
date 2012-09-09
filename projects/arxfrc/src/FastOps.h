#ifndef __FASTOPS_H__
#define __FASTOPS_H__

#include <stdlib.h>
#include "ArXMath.h"

#define SAD4x4 SAD4x4_C

typedef struct TSums{
	float Sum;
	float SqrSum;
} TSums;

inline TSums SUM4x4(const float* pSrc, unsigned int uWidth)
{
	TSums Sum;
	__asm
	{
		mov     esi,  pSrc       ; ds:[esi] points to source block
		mov     ebx,  uWidth     ; ebx = uWidth
		mov     edx,  ebx
		shl     edx,  1          ; edx = uWidth * 2

		; Load source rows
		movups  xmm0, [esi]
		movups  xmm1, [esi+ebx]
		add     esi,  ebx
		movups  xmm2, [esi]
		movups  xmm3, [esi+ebx]

		; Compute Sum
		movaps  xmm4, xmm0
		addps   xmm4, xmm1
		addps   xmm4, xmm2
		addps   xmm4, xmm3       ; xmm4 = a, b, c, d

		movhlps xmm5, xmm4       ; xmm5 = c, d, ?, ?
		addps   xmm4, xmm5       ; xmm4 = a+c, b+d, ?, ?
		movss   xmm5, xmm4       ; xmm5 = a+c, d, ?, ?
		shufps  xmm4, xmm5, 0xDD ; xmm4 = b+d, ?, d, ?
		addss   xmm4, xmm5       ; xmm4 = a+c+b+d, ?, ?, ?
		movss   Sum.Sum, xmm4    ; Sum = a+b+c+d

		; Compute SqrSum
		mulps   xmm0, xmm0
		mulps   xmm1, xmm1
		mulps   xmm2, xmm2
		mulps   xmm3, xmm3
		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3       ; xmm0 = a, b, c, d

		movhlps xmm5, xmm0       ; xmm5 = c, d, ?, ?
		addps   xmm0, xmm5       ; xmm0 = a+c, b+d, ?, ?
		movss   xmm5, xmm0       ; xmm5 = a+c, d, ?, ?
		shufps  xmm0, xmm5, 0xDD ; xmm0 = b+d, ?, d, ?
		addss   xmm0, xmm5       ; xmm0 = a+c+b+d, ?, ?, ?
		movss   Sum.SqrSum, xmm0 ; Sum = a+b+c+d
	}
	return Sum;
}

inline unsigned int SAD4x4_C(const unsigned char *pSrc, const unsigned char *pDst, unsigned int uWidth)
{
	unsigned int uSum = 0;
	for(unsigned int i = 0; i < 4; i++) for(unsigned int j = 0; j < uWidth * 4; j += uWidth)
		uSum += abs(pSrc[i + j] - pDst[i + j]);
	return uSum;
}

inline unsigned int SAD4x4_MMX(const unsigned char *pSrc, const unsigned char *pDst, unsigned int uWidth)
{
	unsigned int uSum;
	__asm
	{
		mov		esi, pSrc		; ds:[esi] points to the source image block
		mov		edi, pDst		; ds:[edi] points to the destination image block
		mov		ebx, uWidth	; ebx = uWidth
		mov		edx, ebx		; edx = uWidth
		shl		edx, 1			; edx = uWidth * 2

		; Load source rows in mm registers
		movd	mm0, [esi]
		movd	mm1, [esi+ebx]
		add		esi, edx
		movd	mm2, [esi]
		movd	mm3, [esi+ebx]

		; Load destination rows in mm registers
		movd	mm4, [edi]
		movd	mm5, [edi+ebx]
		add		edi, edx
		movd	mm6, [edi]
		movd	mm7, [edi+ebx]

		; Calculate SADs
		psadbw	mm0, mm4
		psadbw	mm1, mm5
		psadbw	mm2, mm6
		psadbw	mm3, mm7

		; Sum all SADs
		paddusw	mm0, mm1
		paddusw	mm0, mm2
		paddusw	mm0, mm3

		movd	uSum, mm0		; store sum
		emms							; empty MMX state
		mov		eax, uSum		; function result: eax
	}
}

inline unsigned int SAD8x8(const unsigned char *pSrc, const unsigned char *pDst, unsigned int uWidth)
{
	unsigned int uSum;
	__asm
	{
		mov		esi, pSrc		; ds:[esi] points to the source image block
		mov		edi, pDst		; ds:[edi] points to the destination image block
		mov		ebx, uWidth	; ebx = uWidth
		mov		edx, ebx		; edx = uWidth
		shl		edx, 1			; edx = uWidth * 2

		; Load source rows in mm registers
		movq	mm0, [esi]
		movq	mm1, [esi+ebx]
		add		esi, edx
		movq	mm2, [esi]
		movq	mm3, [esi+ebx]
		add		esi, edx
		movq	mm4, [esi]
		movq	mm5, [esi+ebx]
		add		esi, edx
		movq	mm6, [esi]
		movq	mm7, [esi+ebx]

		; Calculate SADs with destination rows
		psadbw	mm0, [edi]
		psadbw	mm1, [edi+ebx]
		add			edi, edx
		psadbw	mm2, [edi]
		psadbw	mm3, [edi+ebx]
		add			edi, edx
		psadbw	mm4, [edi]
		psadbw	mm5, [edi+ebx]
		add			edi, edx
		psadbw	mm6, [edi]
		psadbw	mm7, [edi+ebx]

		; Sum all SADs
		paddusw	mm0, mm1
		paddusw	mm0, mm2
		paddusw	mm0, mm3
		paddusw	mm0, mm4
		paddusw	mm0, mm5
		paddusw	mm0, mm6
		paddusw	mm0, mm7

		movd	uSum, mm0		; store sum
		emms							; empty MMX state
		mov		eax, uSum		; function result: eax
	}
}

inline unsigned int SAD16x16(const unsigned char *pSrc, const unsigned char *pDst, unsigned int uWidth)
{
	unsigned int uSum = 0;
	uSum += SAD8x8(pSrc, pDst, uWidth);
	uSum += SAD8x8(pSrc + 8, pDst + 8, uWidth);
	uSum += SAD8x8(pSrc + uWidth * 8, pDst + uWidth * 8, uWidth);
	uSum += SAD8x8(pSrc + uWidth * 8 + 8, pDst + uWidth * 8 + 8, uWidth);
	return uSum;
}

inline float ADV_EUC8x8(const unsigned char *pSrc, const unsigned char *pDst, unsigned int LSrcSize, unsigned int LDstSize, float K, float C)
{
	float uSum = 0;
	for(int y = 0; y < 8; y++) for(int x = 0; x < 8; x++)
		uSum += sqr(pSrc[(y << LSrcSize) + x] * K + C - pDst[(y << LDstSize) + x]);
	return uSum;
}

inline float SCP(const unsigned char *pSrc, const unsigned char *pDst, unsigned int LSrcSize, unsigned int LDstSize, unsigned int Size)
{
	float uSum = 0;
	for(unsigned int y = 0; y < Size; y++) for(unsigned int x = 0; x < Size; x++)
		uSum += ((float)pSrc[(y << LSrcSize) + x]) * pDst[(y << LDstSize) + x];
	return uSum;
}

inline float SCP16x16(const float* pSrcUnAligned, const float* pDstAligned16, unsigned int uSrcWidth, unsigned int uDstWidth)
{
	float Sum;
	__asm
	{
		mov     esi,  pSrcUnAligned ; ds:[esi] points to source block
		mov     edi,  pDstAligned16 ; ds:[edi] points to the destination block

		mov     ebx,  uSrcWidth     ; ebx = uSrcWidth
		mov     eax,  ebx
		shl     eax,  1
		add     eax,  ebx           ; eax = uSrcWidth * 3

		mov     edx,  uDstWidth     ; edx = uDstWidth
		mov     ecx,  edx
		shl     ecx,  1
		add     ecx,  edx           ; ecx = uDstWidth * 3

		; rows 0-1
		movups  xmm0, [esi]
		movups  xmm1, [esi]+16
		movups  xmm2, [esi]+32
		movups  xmm3, [esi]+48
		movups  xmm4, [esi+ebx]
		movups  xmm5, [esi+ebx]+16
		movups  xmm6, [esi+ebx]+32
		movups  xmm7, [esi+ebx]+48

		mulps   xmm0, [edi]
		mulps   xmm1, [edi]+16
		mulps   xmm2, [edi]+32
		mulps   xmm3, [edi]+48
		mulps   xmm4, [edi+edx]
		mulps   xmm5, [edi+edx]+16
		mulps   xmm6, [edi+edx]+32
		mulps   xmm7, [edi+edx]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4
		addps   xmm0, xmm5
		addps   xmm0, xmm6
		addps   xmm0, xmm7

		; row 2
		movups  xmm1, [esi+2*ebx]
		movups  xmm2, [esi+2*ebx]+16
		movups  xmm3, [esi+2*ebx]+32
		movups  xmm4, [esi+2*ebx]+48

		mulps   xmm1, [edi+2*edx]
		mulps   xmm2, [edi+2*edx]+16
		mulps   xmm3, [edi+2*edx]+32
		mulps   xmm4, [edi+2*edx]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; shift by 3 rows  
		add     esi,  eax           
		add     edi,  ecx

		; row 3
		movups  xmm1, [esi]
		movups  xmm2, [esi]+16
		movups  xmm3, [esi]+32
		movups  xmm4, [esi]+48

		mulps   xmm1, [edi]
		mulps   xmm2, [edi]+16
		mulps   xmm3, [edi]+32
		mulps   xmm4, [edi]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; row 4
		movups  xmm1, [esi+ebx]
		movups  xmm2, [esi+ebx]+16
		movups  xmm3, [esi+ebx]+32
		movups  xmm4, [esi+ebx]+48

		mulps   xmm1, [edi+edx]
		mulps   xmm2, [edi+edx]+16
		mulps   xmm3, [edi+edx]+32
		mulps   xmm4, [edi+edx]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; row 5
		movups  xmm1, [esi+2*ebx]
		movups  xmm2, [esi+2*ebx]+16
		movups  xmm3, [esi+2*ebx]+32
		movups  xmm4, [esi+2*ebx]+48

		mulps   xmm1, [edi+2*edx]
		mulps   xmm2, [edi+2*edx]+16
		mulps   xmm3, [edi+2*edx]+32
		mulps   xmm4, [edi+2*edx]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; shift by 3 rows  
		add     esi,  eax           
		add     edi,  ecx

		; row 6
		movups  xmm1, [esi]
		movups  xmm2, [esi]+16
		movups  xmm3, [esi]+32
		movups  xmm4, [esi]+48

		mulps   xmm1, [edi]
		mulps   xmm2, [edi]+16
		mulps   xmm3, [edi]+32
		mulps   xmm4, [edi]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; row 7
		movups  xmm1, [esi+ebx]
		movups  xmm2, [esi+ebx]+16
		movups  xmm3, [esi+ebx]+32
		movups  xmm4, [esi+ebx]+48

		mulps   xmm1, [edi+edx]
		mulps   xmm2, [edi+edx]+16
		mulps   xmm3, [edi+edx]+32
		mulps   xmm4, [edi+edx]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; row 8
		movups  xmm1, [esi+2*ebx]
		movups  xmm2, [esi+2*ebx]+16
		movups  xmm3, [esi+2*ebx]+32
		movups  xmm4, [esi+2*ebx]+48

		mulps   xmm1, [edi+2*edx]
		mulps   xmm2, [edi+2*edx]+16
		mulps   xmm3, [edi+2*edx]+32
		mulps   xmm4, [edi+2*edx]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; shift by 3 rows  
		add     esi,  eax           
		add     edi,  ecx

		; row 9
		movups  xmm1, [esi]
		movups  xmm2, [esi]+16
		movups  xmm3, [esi]+32
		movups  xmm4, [esi]+48

		mulps   xmm1, [edi]
		mulps   xmm2, [edi]+16
		mulps   xmm3, [edi]+32
		mulps   xmm4, [edi]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; row 10
		movups  xmm1, [esi+ebx]
		movups  xmm2, [esi+ebx]+16
		movups  xmm3, [esi+ebx]+32
		movups  xmm4, [esi+ebx]+48

		mulps   xmm1, [edi+edx]
		mulps   xmm2, [edi+edx]+16
		mulps   xmm3, [edi+edx]+32
		mulps   xmm4, [edi+edx]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; row 11
		movups  xmm1, [esi+2*ebx]
		movups  xmm2, [esi+2*ebx]+16
		movups  xmm3, [esi+2*ebx]+32
		movups  xmm4, [esi+2*ebx]+48

		mulps   xmm1, [edi+2*edx]
		mulps   xmm2, [edi+2*edx]+16
		mulps   xmm3, [edi+2*edx]+32
		mulps   xmm4, [edi+2*edx]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; shift by 3 rows  
		add     esi,  eax           
		add     edi,  ecx

		; row 12
		movups  xmm1, [esi]
		movups  xmm2, [esi]+16
		movups  xmm3, [esi]+32
		movups  xmm4, [esi]+48

		mulps   xmm1, [edi]
		mulps   xmm2, [edi]+16
		mulps   xmm3, [edi]+32
		mulps   xmm4, [edi]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; row 13
		movups  xmm1, [esi+ebx]
		movups  xmm2, [esi+ebx]+16
		movups  xmm3, [esi+ebx]+32
		movups  xmm4, [esi+ebx]+48

		mulps   xmm1, [edi+edx]
		mulps   xmm2, [edi+edx]+16
		mulps   xmm3, [edi+edx]+32
		mulps   xmm4, [edi+edx]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; row 14
		movups  xmm1, [esi+2*ebx]
		movups  xmm2, [esi+2*ebx]+16
		movups  xmm3, [esi+2*ebx]+32
		movups  xmm4, [esi+2*ebx]+48

		mulps   xmm1, [edi+2*edx]
		mulps   xmm2, [edi+2*edx]+16
		mulps   xmm3, [edi+2*edx]+32
		mulps   xmm4, [edi+2*edx]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; shift by 3 rows  
		add     esi,  eax           
		add     edi,  ecx

		; row 15
		movups  xmm1, [esi]
		movups  xmm2, [esi]+16
		movups  xmm3, [esi]+32
		movups  xmm4, [esi]+48

		mulps   xmm1, [edi]
		mulps   xmm2, [edi]+16
		mulps   xmm3, [edi]+32
		mulps   xmm4, [edi]+48

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; horizontal sum in xmm0        = a, b, c, d
		movhlps xmm1, xmm0       ; xmm1 = c, d, ?, ?
		addps   xmm0, xmm1       ; xmm0 = a+c, b+d, ?, ?
		movss   xmm1, xmm0       ; xmm1 = a+c, d, ?, ?
		shufps  xmm0, xmm0, 0x99 ; xmm0 = b+d, ?, ?, ?
		addss   xmm0, xmm1       ; xmm0 = a+c+b+d, ?, ?, ?
		movss   Sum,  xmm0       ; Sum = a+b+c+d
	}
	return Sum;
}


inline float SCP8x8(const float* pSrcUnAligned, const float* pDstAligned16, unsigned int uSrcWidth, unsigned int uDstWidth)
{
	float Sum;
	__asm
	{
		mov     esi,  pSrcUnAligned ; ds:[esi] points to source block
		mov     edi,  pDstAligned16 ; ds:[edi] points to the destination block
		
		mov     ebx,  uSrcWidth     ; ebx = uSrcWidth
		mov     eax,  ebx
		shl     eax,  1
		add     eax,  ebx           ; eax = uSrcWidth * 3
		
		mov     edx,  uDstWidth     ; edx = uDstWidth
		mov     ecx,  edx
		shl     ecx,  1
		add     ecx,  edx           ; ecx = uDstWidth * 3

		; rows 0-2
		movups  xmm0, [esi]
		movups  xmm1, [esi]+16
		movups  xmm2, [esi+ebx]
		movups  xmm3, [esi+ebx]+16
		movups  xmm4, [esi+2*ebx]
		movups  xmm5, [esi+2*ebx]+16

		mulps   xmm0, [edi]
		mulps   xmm1, [edi]+16
		mulps   xmm2, [edi+edx]
		mulps   xmm3, [edi+edx]+16
		mulps   xmm4, [edi+2*edx]
		mulps   xmm5, [edi+2*edx]+16

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4
		addps   xmm0, xmm5
	
		; shift by 3 rows  
		add     esi,  eax           
		add     edi,  ecx

		; rows 3-5
		movups  xmm1, [esi]
		movups  xmm2, [esi]+16
		movups  xmm3, [esi+ebx]
		movups  xmm4, [esi+ebx]+16
		movups  xmm5, [esi+2*ebx]
		movups  xmm6, [esi+2*ebx]+16

		mulps   xmm1, [edi]
		mulps   xmm2, [edi]+16
		mulps   xmm3, [edi+edx]
		mulps   xmm4, [edi+edx]+16
		mulps   xmm5, [edi+2*edx]
		mulps   xmm6, [edi+2*edx]+16

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4
		addps   xmm0, xmm5
		addps   xmm0, xmm6

		; shift by 3 rows  
		add     esi,  eax           
		add     edi,  ecx

		; rows 6-7
		movups  xmm1, [esi]
		movups  xmm2, [esi]+16
		movups  xmm3, [esi+ebx]
		movups  xmm4, [esi+ebx]+16
		
		mulps   xmm1, [edi]
		mulps   xmm2, [edi]+16
		mulps   xmm3, [edi+edx]
		mulps   xmm4, [edi+edx]+16

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3
		addps   xmm0, xmm4

		; horizontal sum in xmm0        = a, b, c, d
		movhlps xmm1, xmm0       ; xmm1 = c, d, ?, ?
		addps   xmm0, xmm1       ; xmm0 = a+c, b+d, ?, ?
		movss   xmm1, xmm0       ; xmm1 = a+c, d, ?, ?
		shufps  xmm0, xmm0, 0x99 ; xmm0 = b+d, ?, ?, ?
		addss   xmm0, xmm1       ; xmm0 = a+c+b+d, ?, ?, ?
		movss   Sum,  xmm0       ; Sum = a+b+c+d
	}
	return Sum;
}

inline float SCP4x4(const float* pSrcUnAligned, const float* pDstAligned16, unsigned int uSrcWidth, unsigned int uDstWidth)
{
	float Sum;
	__asm
	{
		mov     esi,  pSrcUnAligned ; ds:[esi] points to source block
		mov     edi,  pDstAligned16 ; ds:[edi] points to the destination block

		mov     ebx,  uSrcWidth     ; ebx = uSrcWidth
		mov     eax,  ebx
		shl     eax,  1             ; eax = uSrcWidth * 2

		mov     edx,  uDstWidth     ; edx = uDstWidth
		mov     ecx,  edx
		shl     ecx,  1             ; ecx = uDstWidth * 2

		movups  xmm0, [esi]
		movups  xmm1, [esi+ebx]
		add     esi,  eax
		movups  xmm2, [esi]
		movups  xmm3, [esi+ebx]

		mulps   xmm0, [edi]
		mulps   xmm1, [edi+edx]
		add     edi,  ecx
		mulps   xmm2, [edi]
		mulps   xmm3, [edi+edx]

		addps   xmm0, xmm1
		addps   xmm0, xmm2
		addps   xmm0, xmm3

		; horizontal sum in xmm0        = a, b, c, d
		movhlps xmm1, xmm0       ; xmm1 = c, d, ?, ?
		addps   xmm0, xmm1       ; xmm0 = a+c, b+d, ?, ?
		movss   xmm1, xmm0       ; xmm1 = a+c, d, ?, ?
		shufps  xmm0, xmm0, 0x99 ; xmm0 = b+d, ?, ?, ?
		addss   xmm0, xmm1       ; xmm0 = a+c+b+d, ?, ?, ?
		movss   Sum,  xmm0       ; Sum = a+b+c+d
	}
	return Sum;
}

inline float SCP32x32(const float* pSrcUnAligned, const float* pDstAligned16, unsigned int uSrcWidth, unsigned int uDstWidth)
{
	return 
		SCP16x16(pSrcUnAligned, pDstAligned16, uSrcWidth, uDstWidth) + 
		SCP16x16(pSrcUnAligned + 16, pDstAligned16 + 16, uSrcWidth, uDstWidth) + 
		SCP16x16(pSrcUnAligned + (uSrcWidth << 2), pDstAligned16 + (uDstWidth << 2), uSrcWidth, uDstWidth) + 
		SCP16x16(pSrcUnAligned + (uSrcWidth << 2) + 16, pDstAligned16 + (uDstWidth << 2) + 16, uSrcWidth, uDstWidth);
}

inline float EUC16x16(const float* pSrcUnAligned, const float* pDstAligned16, unsigned int uSrcWidth, unsigned int uDstWidth, float SrcK, float SrcC)
{
	// SUM (SRCij * K + C - DSTij)^2
	float Sum;
	__asm
	{
		mov     esi,  pSrcUnAligned ; ds:[esi] points to source block
		mov     edi,  pDstAligned16 ; ds:[edi] points to the destination block

		mov     ebx,  uSrcWidth     ; ebx = uSrcWidth
		mov     eax,  ebx
		shl     eax,  1
		add     eax,  ebx           ; eax = uSrcWidth * 3

		mov     edx,  uDstWidth     ; edx = uDstWidth
		mov     ecx,  edx
		shl     ecx,  1
		add     ecx,  edx           ; ecx = uDstWidth * 3


		; load K to xmm0 and C to xmm1
		movss   xmm0, SrcK
		shufps  xmm0, xmm0, 0x00
		movss   xmm1, SrcC
		shufps  xmm1, xmm1, 0x00

		; row 0
		movups  xmm2, [esi]
		movups  xmm3, [esi]+16
		movups  xmm4, [esi]+32
		movups  xmm5, [esi]+48
		
		mulps   xmm2, xmm0
		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0

		addps   xmm2, xmm1
		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1

		subps   xmm2, [edi]
		subps   xmm3, [edi]+16
		subps   xmm4, [edi]+32
		subps   xmm5, [edi]+48

		mulps   xmm2, xmm2
		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5

		; row 1
		movups  xmm3, [esi+ebx]
		movups  xmm4, [esi+ebx]+16
		movups  xmm5, [esi+ebx]+32
		movups  xmm6, [esi+ebx]+48

		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi+edx]
		subps   xmm4, [edi+edx]+16
		subps   xmm5, [edi+edx]+32
		subps   xmm6, [edi+edx]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; row 2
		movups  xmm3, [esi+2*ebx]
		movups  xmm4, [esi+2*ebx]+16
		movups  xmm5, [esi+2*ebx]+32
		movups  xmm6, [esi+2*ebx]+48

		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi+2*edx]
		subps   xmm4, [edi+2*edx]+16
		subps   xmm5, [edi+2*edx]+32
		subps   xmm6, [edi+2*edx]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; shift by 3 rows  
		add     esi,  eax           
		add     edi,  ecx

		; row 3
		movups  xmm3, [esi]
		movups  xmm4, [esi]+16
		movups  xmm5, [esi]+32
		movups  xmm6, [esi]+48
		
		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi]
		subps   xmm4, [edi]+16
		subps   xmm5, [edi]+32
		subps   xmm6, [edi]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; row 4
		movups  xmm3, [esi+ebx]
		movups  xmm4, [esi+ebx]+16
		movups  xmm5, [esi+ebx]+32
		movups  xmm6, [esi+ebx]+48

		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi+edx]
		subps   xmm4, [edi+edx]+16
		subps   xmm5, [edi+edx]+32
		subps   xmm6, [edi+edx]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; row 5
		movups  xmm3, [esi+2*ebx]
		movups  xmm4, [esi+2*ebx]+16
		movups  xmm5, [esi+2*ebx]+32
		movups  xmm6, [esi+2*ebx]+48

		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi+2*edx]
		subps   xmm4, [edi+2*edx]+16
		subps   xmm5, [edi+2*edx]+32
		subps   xmm6, [edi+2*edx]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; shift by 3 rows  
		add     esi,  eax           
		add     edi,  ecx

		; row 6
		movups  xmm3, [esi]
		movups  xmm4, [esi]+16
		movups  xmm5, [esi]+32
		movups  xmm6, [esi]+48
		
		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi]
		subps   xmm4, [edi]+16
		subps   xmm5, [edi]+32
		subps   xmm6, [edi]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; row 7
		movups  xmm3, [esi+ebx]
		movups  xmm4, [esi+ebx]+16
		movups  xmm5, [esi+ebx]+32
		movups  xmm6, [esi+ebx]+48

		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi+edx]
		subps   xmm4, [edi+edx]+16
		subps   xmm5, [edi+edx]+32
		subps   xmm6, [edi+edx]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; row 8
		movups  xmm3, [esi+2*ebx]
		movups  xmm4, [esi+2*ebx]+16
		movups  xmm5, [esi+2*ebx]+32
		movups  xmm6, [esi+2*ebx]+48

		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi+2*edx]
		subps   xmm4, [edi+2*edx]+16
		subps   xmm5, [edi+2*edx]+32
		subps   xmm6, [edi+2*edx]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; shift by 3 rows  
		add     esi,  eax           
		add     edi,  ecx

		; row 9
		movups  xmm3, [esi]
		movups  xmm4, [esi]+16
		movups  xmm5, [esi]+32
		movups  xmm6, [esi]+48
		
		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi]
		subps   xmm4, [edi]+16
		subps   xmm5, [edi]+32
		subps   xmm6, [edi]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; row 10
		movups  xmm3, [esi+ebx]
		movups  xmm4, [esi+ebx]+16
		movups  xmm5, [esi+ebx]+32
		movups  xmm6, [esi+ebx]+48

		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi+edx]
		subps   xmm4, [edi+edx]+16
		subps   xmm5, [edi+edx]+32
		subps   xmm6, [edi+edx]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; row 11
		movups  xmm3, [esi+2*ebx]
		movups  xmm4, [esi+2*ebx]+16
		movups  xmm5, [esi+2*ebx]+32
		movups  xmm6, [esi+2*ebx]+48

		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi+2*edx]
		subps   xmm4, [edi+2*edx]+16
		subps   xmm5, [edi+2*edx]+32
		subps   xmm6, [edi+2*edx]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; shift by 3 rows  
		add     esi,  eax           
		add     edi,  ecx

		; row 12
		movups  xmm3, [esi]
		movups  xmm4, [esi]+16
		movups  xmm5, [esi]+32
		movups  xmm6, [esi]+48
		
		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi]
		subps   xmm4, [edi]+16
		subps   xmm5, [edi]+32
		subps   xmm6, [edi]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; row 13
		movups  xmm3, [esi+ebx]
		movups  xmm4, [esi+ebx]+16
		movups  xmm5, [esi+ebx]+32
		movups  xmm6, [esi+ebx]+48

		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi+edx]
		subps   xmm4, [edi+edx]+16
		subps   xmm5, [edi+edx]+32
		subps   xmm6, [edi+edx]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; row 14
		movups  xmm3, [esi+2*ebx]
		movups  xmm4, [esi+2*ebx]+16
		movups  xmm5, [esi+2*ebx]+32
		movups  xmm6, [esi+2*ebx]+48

		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi+2*edx]
		subps   xmm4, [edi+2*edx]+16
		subps   xmm5, [edi+2*edx]+32
		subps   xmm6, [edi+2*edx]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; shift by 3 rows  
		add     esi,  eax           
		add     edi,  ecx

		; row 15
		movups  xmm3, [esi]
		movups  xmm4, [esi]+16
		movups  xmm5, [esi]+32
		movups  xmm6, [esi]+48
		
		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi]
		subps   xmm4, [edi]+16
		subps   xmm5, [edi]+32
		subps   xmm6, [edi]+48

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; horizontal sum in xmm2        = a, b, c, d
		movhlps xmm1, xmm2       ; xmm1 = c, d, ?, ?
		addps   xmm2, xmm1       ; xmm2 = a+c, b+d, ?, ?
		movss   xmm1, xmm2       ; xmm1 = a+c, d, ?, ?
		shufps  xmm2, xmm2, 0x99 ; xmm2 = b+d, ?, ?, ?
		addss   xmm2, xmm1       ; xmm2 = a+c+b+d, ?, ?, ?
		movss   Sum,  xmm2       ; Sum = a+b+c+d
	}
	return Sum;
}

inline float EUC8x8(const float* pSrcUnAligned, const float* pDstAligned16, unsigned int uSrcWidth, unsigned int uDstWidth, float SrcK, float SrcC)
{
	// SUM (SRCij * K + C - DSTij)^2
	float Sum;
	__asm
	{
		mov     esi,  pSrcUnAligned ; ds:[esi] points to source block
		mov     edi,  pDstAligned16 ; ds:[edi] points to the destination block

		mov     ebx,  uSrcWidth     ; ebx = uSrcWidth
		mov     eax,  ebx
		shl     eax,  1             ; eax = uSrcWidth * 2

		mov     edx,  uDstWidth     ; edx = uDstWidth
		mov     ecx,  edx
		shl     ecx,  1             ; ecx = uDstWidth * 2

		; load K to xmm0 and C to xmm1
		movss   xmm0, SrcK
		shufps  xmm0, xmm0, 0x00
		movss   xmm1, SrcC
		shufps  xmm1, xmm1, 0x00

		; rows 0-1
		movups  xmm2, [esi]
		movups  xmm3, [esi]+16
		movups  xmm4, [esi+ebx]
		movups  xmm5, [esi+ebx]+16
		
		mulps   xmm2, xmm0
		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0

		addps   xmm2, xmm1
		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1

		subps   xmm2, [edi]
		subps   xmm3, [edi]+16
		subps   xmm4, [edi+edx]
		subps   xmm5, [edi+edx]+16

		mulps   xmm2, xmm2
		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5

		; rows 2-3
		add     esi,  eax
		add     edi,  ecx

		movups  xmm3, [esi]
		movups  xmm4, [esi]+16
		movups  xmm5, [esi+ebx]
		movups  xmm6, [esi+ebx]+16

		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi]
		subps   xmm4, [edi]+16
		subps   xmm5, [edi+edx]
		subps   xmm6, [edi+edx]+16

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; rows 4-5
		add     esi,  eax
		add     edi,  ecx

		movups  xmm3, [esi]
		movups  xmm4, [esi]+16
		movups  xmm5, [esi+ebx]
		movups  xmm6, [esi+ebx]+16

		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi]
		subps   xmm4, [edi]+16
		subps   xmm5, [edi+edx]
		subps   xmm6, [edi+edx]+16

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; rows 6-7
		add     esi,  eax
		add     edi,  ecx

		movups  xmm3, [esi]
		movups  xmm4, [esi]+16
		movups  xmm5, [esi+ebx]
		movups  xmm6, [esi+ebx]+16

		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0
		mulps   xmm6, xmm0

		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1
		addps   xmm6, xmm1

		subps   xmm3, [edi]
		subps   xmm4, [edi]+16
		subps   xmm5, [edi+edx]
		subps   xmm6, [edi+edx]+16

		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5
		mulps   xmm6, xmm6

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5
		addps   xmm2, xmm6

		; horizontal sum in xmm2        = a, b, c, d
		movhlps xmm1, xmm2       ; xmm1 = c, d, ?, ?
		addps   xmm2, xmm1       ; xmm2 = a+c, b+d, ?, ?
		movss   xmm1, xmm2       ; xmm1 = a+c, d, ?, ?
		shufps  xmm2, xmm2, 0x99 ; xmm2 = b+d, ?, ?, ?
		addss   xmm2, xmm1       ; xmm2 = a+c+b+d, ?, ?, ?
		movss   Sum,  xmm2       ; Sum = a+b+c+d
	}
	return Sum;
}

inline float EUC4x4(const float* pSrcUnAligned, const float* pDstAligned16, unsigned int uSrcWidth, unsigned int uDstWidth, float SrcK, float SrcC)
{
	// SUM (SRCij * K + C - DSTij)^2
	float Sum;
	__asm
	{
		mov     esi,  pSrcUnAligned ; ds:[esi] points to source block
		mov     edi,  pDstAligned16 ; ds:[edi] points to the destination block

		mov     ebx,  uSrcWidth     ; ebx = uSrcWidth
		mov     eax,  ebx
		shl     eax,  1             ; eax = uSrcWidth * 2

		mov     edx,  uDstWidth     ; edx = uDstWidth
		mov     ecx,  edx
		shl     ecx,  1             ; ecx = uDstWidth * 2

		; load K to xmm0 and C to xmm1
		movss   xmm0, SrcK
		shufps  xmm0, xmm0, 0x00
		movss   xmm1, SrcC
		shufps  xmm1, xmm1, 0x00

		; rows 0-3
		movups  xmm2, [esi]
		movups  xmm3, [esi+ebx]
		add     esi,  eax
		movups  xmm4, [esi]
		movups  xmm5, [esi+ebx]

		mulps   xmm2, xmm0
		mulps   xmm3, xmm0
		mulps   xmm4, xmm0
		mulps   xmm5, xmm0

		addps   xmm2, xmm1
		addps   xmm3, xmm1
		addps   xmm4, xmm1
		addps   xmm5, xmm1

		subps   xmm2, [edi]
		subps   xmm3, [edi+edx]
		add     edi,  ecx
		subps   xmm4, [edi]
		subps   xmm5, [edi+edx]

		mulps   xmm2, xmm2
		mulps   xmm3, xmm3
		mulps   xmm4, xmm4
		mulps   xmm5, xmm5

		addps   xmm2, xmm3
		addps   xmm2, xmm4
		addps   xmm2, xmm5

		; horizontal sum in xmm2        = a, b, c, d
		movhlps xmm1, xmm2       ; xmm1 = c, d, ?, ?
		addps   xmm2, xmm1       ; xmm2 = a+c, b+d, ?, ?
		movss   xmm1, xmm2       ; xmm1 = a+c, d, ?, ?
		shufps  xmm2, xmm2, 0x99 ; xmm2 = b+d, ?, ?, ?
		addss   xmm2, xmm1       ; xmm2 = a+c+b+d, ?, ?, ?
		movss   Sum,  xmm2       ; Sum = a+b+c+d
	}
	return Sum;
}

#endif