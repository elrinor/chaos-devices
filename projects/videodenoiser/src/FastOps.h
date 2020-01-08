#ifndef __FASTOPS_H__
#define __FASTOPS_H__

inline unsigned int SAD4x4w(const unsigned short *pSrc, const unsigned short *pDst, unsigned int uWidth)
{
  unsigned int uSum;
  __asm
  {
    mov      esi, pSrc      // ds:[esi] points to the source image block
    mov      edi, pDst      // ds:[edi] points to the destination image block
    mov      ebx, uWidth    // ebx = uWidth
    mov      edx, ebx       // edx = uWidth
    shl      edx, 1         // edx = uWidth * 2

    movq mm0, [esi]
    movq mm1, [esi+ebx] 
    movq mm2, [edi]
    movq mm3, [edi+ebx] 
    movq mm4, mm0 
    movq mm5, mm1
    psubusw mm0, mm2
    psubusw mm1, mm3
    psubusw mm2, mm4
    psubusw mm3, mm5
    por mm0, mm2         
    por mm1, mm3   

    add esi, edx
    add edi, edx

    movq mm2, [esi]
    movq mm3, [esi+ebx] 
    movq mm4, [edi]
    movq mm5, [edi+ebx] 
    movq mm6, mm2 
    movq mm7, mm3
    psubusw mm2, mm4
    psubusw mm3, mm5
    psubusw mm4, mm6
    psubusw mm5, mm7
    por mm2, mm4         
    por mm3, mm5   
    
    paddw mm0, mm1
    paddw mm0, mm2
    paddw mm0, mm3

    movq mm1, mm0
    pxor mm3, mm3
    punpcklbw mm0, mm3
    punpckhbw mm1, mm3
    paddusw mm0, mm1
    movq mm1, mm0
    psrlq mm1, 32 
    paddd mm0, mm1

    movd uSum, mm0
    emms
    mov eax, uSum
  }
}

inline unsigned int SAD8x8(const unsigned char *pSrc, const unsigned char *pDst, unsigned int uWidth)
{
  unsigned int uSum;
  __asm
  {
    mov    esi, pSrc      // ds:[esi] points to the source image block
    mov    edi, pDst      // ds:[edi] points to the destination image block
    mov    ebx, uWidth    // ebx = uWidth
    mov    edx, ebx       // edx = uWidth
    shl    edx, 1         // edx = uWidth * 2

    // Load source rows in mm registers
    movq   mm0, [esi]
    movq   mm1, [esi+ebx]
    add    esi, edx
    movq   mm2, [esi]
    movq   mm3, [esi+ebx]
    add    esi, edx
    movq   mm4, [esi]
    movq   mm5, [esi+ebx]
    add    esi, edx
    movq   mm6, [esi]
    movq   mm7, [esi+ebx]

    // Calculate SADs with destination rows
    psadbw mm0, [edi]
    psadbw mm1, [edi+ebx]
    add    edi, edx
    psadbw mm2, [edi]
    psadbw mm3, [edi+ebx]
    add    edi, edx
    psadbw mm4, [edi]
    psadbw mm5, [edi+ebx]
    add    edi, edx
    psadbw mm6, [edi]
    psadbw mm7, [edi+ebx]

    // Sum all SADs
    paddusw   mm0, mm1
    paddusw   mm0, mm2
    paddusw   mm0, mm3
    paddusw   mm0, mm4
    paddusw   mm0, mm5
    paddusw   mm0, mm6
    paddusw   mm0, mm7

    movd      uSum, mm0      // store sum
    emms                  // empty MMX state
    mov       eax, uSum      // function result: eax
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

inline unsigned int NoiseEstimate(const unsigned char *pSrc, unsigned int uWidth, unsigned int uHeight)
{
  unsigned int uSum = 0;
  unsigned int uDiv = 0;
  for(int y = (uHeight - 1) & 0xFFFFFFF0; y != 0; y -= 16)
  {
    const unsigned char* p1 = pSrc + y * uWidth;
    const unsigned char* p2 = pSrc + (y + 1) * uWidth;
    __asm
    {
      mov esi, p1              // ds:[esi] points to p1
      mov edi, p2              // ds:[edi] points to p2
      mov ebx, uWidth
      and ebx, 0xFFFFFFC0      
      sub ebx, 64              // (store Width & !63) - 64 in  ebx
      pxor mm0, mm0            // Sum in mm0
      xor ecx, ecx

      // Count Sum
Cycle:
      movq    mm1, [esi+ebx]
      psadbw  mm1, [edi+ebx]
      paddusw mm0, mm1
      movq    mm1, [esi+ebx+8]
      psadbw  mm1, [edi+ebx+8]
      paddusw mm0, mm1
      movq    mm1, [esi+ebx+16]
      psadbw  mm1, [edi+ebx+16]
      paddusw mm0, mm1
      movq    mm1, [esi+ebx+24]
      psadbw  mm1, [edi+ebx+24]
      paddusw mm0, mm1
      movq    mm1, [esi+ebx+32]
      psadbw  mm1, [edi+ebx+32]
      paddusw mm0, mm1
      movq    mm1, [esi+ebx+40]
      psadbw  mm1, [edi+ebx+40]
      paddusw mm0, mm1
      movq    mm1, [esi+ebx+48]
      psadbw  mm1, [edi+ebx+48]
      paddusw mm0, mm1
      movq    mm1, [esi+ebx+56]
      psadbw  mm1, [edi+ebx+56]
      paddusw mm0, mm1
    
      // Continue?
      inc ecx
      sub ebx, 64
      jnz Cycle

      // Store Sum
      movd eax, mm0
      add uSum, eax
      add uDiv, ecx
    }
  }
  __asm
  {
    emms 
  }
  return uSum / uDiv;
}

#endif