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

struct MV
{
  int x, y;
  unsigned int error;
  MV();
  MV operator += (MV vector);
  MV operator -= (MV vector);
  MV operator + (MV vector);
  MV operator - (MV vector);
  MV operator /= (int);
  MV operator *= (int);
};

typedef enum { den } Method;
typedef unsigned char BYTE;
typedef unsigned int uint;

struct MFD {
	//specific
	bool show_MC;
	bool show_MC_next;
	bool show_nothing;
	bool show_source;
	bool show_denoised;
	Method method;
	bool log_need;
	char log_file[50];
	bool log_vectors;
	char log_vectors_file[50];
	bool read_motions;

	//controls output
	BYTE quality;

	//denoising
	int strength;

	//buffers
	BYTE* prev_Y;
	short* prev_U;
	short* prev_V;
	BYTE* prev_Y_MC;
	short* prev_U_MC;
	short* prev_V_MC;

	BYTE* cur_Y;
	short* cur_U;
	short* cur_V;
	BYTE* cur_Y_den;
	short* cur_U_den;
	short* cur_V_den;

	BYTE* next_Y;
	short* next_U;
	short* next_V;
	BYTE* next_Y_MC;
	short* next_U_MC;
	short* next_V_MC;

  BYTE* DotMask;
  BYTE* DotMask2;
  BYTE* DotMask3;

  float* VerticalSums;
  bool* Scratch;

	int offset;
	int frame_no;
	//direction of the halfpixel shift
	bool shift_up;
	//motion vectors for current frame (not determined before special function call)
	MV *MVectors_next;
	MV *MVectors_prev;
	//dimensions of extended ( with BORDERs ) field
	int ext_w, ext_h, ext_size;
	int width, height, size;
	//size of frame in blocks
	int num_blocks_vert, num_blocks_hor;
};

#endif