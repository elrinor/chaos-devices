#ifndef _TYPES_H_
#define _TYPES_H_

#include "filter.h"
#include "ScriptInterpreter.h"
#include "ScriptError.h"
#include "ScriptValue.h"
#include "resource.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <commctrl.h>
#include <io.h>
#include <fcntl.h>
#include <sys\stat.h>


#define BORDER 48
#define MAX_MOTION 32
#define SPLIT_TRESH 1000


struct Point {
	int x, y;
};

typedef enum {sd_none, sd_up, sd_l, sd_upl} Shift_Dir;

struct SubMV {
  int x, y;
  Shift_Dir dir;
  long error;
};

struct MV : public SubMV{
	MV();
	bool splitted;
	SubMV sub[4];
	MV operator += (MV vector);
	MV operator -= (MV vector);
	MV operator + (MV vector);
	MV operator - (MV vector);
	MV operator /= ( int );
	MV operator *= ( int );
  //MV& operator = (const MV& vector);
};

//Deinterlace functionality
void HalfpixelShift(BYTE *field, int width, int height, bool shift_up);
void HalfpixelShift(short *field, int width, int height, bool shift_up);
void HalfpixelShift(Pixel32 *field, int width, int height, bool shift_up);
void HalfpixelShiftHorz(BYTE *field, int width, int height, bool shift_up);
void HalfpixelShiftHorz(short *field, int width, int height, bool shift_up);
void HalfpixelShiftHorz(Pixel32 *field, int width, int height, bool shift_up);

//ME functionality
//void MEFunction(BYTE* cur_Y_frame, BYTE* prev_Y_frame, BYTE* prev_Y_frame_up, BYTE* prev_Y_frame_l, BYTE* prev_Y_frame_upl, int width, int height, MV *MVectors, BYTE quality, bool use_half_pixel);
void MEFunction(BYTE* cur_Y_frame, BYTE* prev_Y_frame, BYTE* prev_Y_frame_up, BYTE* prev_Y_frame_l, BYTE* prev_Y_frame_upl, int width, int height, MV *MVectors, MV* LastMVectors, BYTE quality, bool use_half_pixel);
void MEStart( int width, int heigth, BYTE quality );
void MEEnd();
long GetErrorSAD_16x16(const BYTE* block1, const BYTE* block2, const int stride);
long GetErrorSAD_8x8(const BYTE* block1, const BYTE* block2, const int stride);
//unsigned long GetErrorSAD_8x8(const unsigned char *pSrc, const unsigned char *pDst, unsigned int uWidth);
//unsigned long GetErrorSAD_16x16(const BYTE *pSrc, const BYTE *pDst, unsigned int uWidth);

/*
inline unsigned long GetErrorSAD_8x8(const unsigned char *pSrc, const unsigned char *pDst, unsigned int uWidth)
{
  unsigned long uSum;
  __asm
  {
    mov  esi, pSrc  ; ds:[esi] points to the source image block
      mov  edi, pDst  ; ds:[edi] points to the destination image block
      mov  ebx, uWidth ; ebx = uWidth
      mov  edx, ebx  ; edx = uWidth
      shl  edx, 1   ; edx = uWidth * 2

      ; Load source rows in mm registers
      movq mm0, [esi]
    movq mm1, [esi+ebx]
    add  esi, edx
      movq mm2, [esi]
    movq mm3, [esi+ebx]
    add  esi, edx
      movq mm4, [esi]
    movq mm5, [esi+ebx]
    add  esi, edx
      movq mm6, [esi]
    movq mm7, [esi+ebx]

    ; Calculate SADs with destination rows
      psadbw mm0, [edi]
    psadbw mm1, [edi+ebx]
    add   edi, edx
      psadbw mm2, [edi]
    psadbw mm3, [edi+ebx]
    add   edi, edx
      psadbw mm4, [edi]
    psadbw mm5, [edi+ebx]
    add   edi, edx
      psadbw mm6, [edi]
    psadbw mm7, [edi+ebx]

    ; Sum all SADs
      paddusw mm0, mm1
      paddusw mm0, mm2
      paddusw mm0, mm3
      paddusw mm0, mm4
      paddusw mm0, mm5
      paddusw mm0, mm6
      paddusw mm0, mm7

      movd uSum, mm0  ; store sum
      emms       ; empty MMX state
      mov  eax, uSum  ; function result: eax
  }
}

inline unsigned long GetErrorSAD_16x16(const BYTE *pSrc, const BYTE *pDst, unsigned int uWidth)
{
  unsigned long uSum = 0;
  uSum += GetErrorSAD_8x8(pSrc, pDst, uWidth);
  uSum += GetErrorSAD_8x8(pSrc + 8, pDst + 8, uWidth);
  uSum += GetErrorSAD_8x8(pSrc + uWidth * 8, pDst + uWidth * 8, uWidth);
  uSum += GetErrorSAD_8x8(pSrc + uWidth * 8 + 8, pDst + uWidth * 8 + 8, uWidth);
  return uSum;
}
*/

typedef enum { den } Method;
typedef unsigned char BYTE;

struct MFD {
	//specific
	bool show_vectors;
	bool show_res_after;
	bool show_res_before;
	bool show_MC;
	bool show_nothing;
	bool show_source;
	Method method;
	bool log_need;
	char log_file[50];
	bool log_vectors;
	char log_vectors_file[50];
	bool read_motions;

	//controls output
	BYTE quality;
	bool use_half_pixel;
	bool work_in_RGB;

	//buffers
	BYTE* prev_Y;
	BYTE* prev_Y_up;
	BYTE* prev_Y_upl;
	BYTE* prev_Y_l;
	short* prev_U;
	short* prev_U_up;
	short* prev_U_upl;
	short* prev_U_l;
	short* prev_U_MC;
	short* prev_V;
	short* prev_V_up;
	short* prev_V_upl;
	short* prev_V_l;
	short* prev_V_MC;
	BYTE* prev_Y_MC;
	BYTE* cur_Y;
	short* cur_U;
	short* cur_V;
	Pixel32* output_buf;
	Pixel32* prev_RGB;
	Pixel32* prev_RGB_up;
	Pixel32* prev_RGB_upl;
	Pixel32* prev_RGB_l;
  //unsigned int* LastBlockSums;
  //unsigned int* BlockSums;

	int offset;
	double noise;
	int frame_no;
	//direction of the halfpixel shift
	bool shift_up;
	//motion vectors for current frame (not determined before special function call)
	MV *MVectors;
  MV *LastMVectors;
	//dimensions of extended ( with BORDERs ) field
	int ext_w, ext_h, ext_size;
	int width, height, size;
	//size of frame in blocks
	int num_blocks_vert, num_blocks_hor;
};


#endif