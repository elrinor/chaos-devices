#ifndef _DENOISE_H_
#define _DENOISE_H_
#include "Types.h"

void DenoiseStart( int width, int height, BYTE strength );

void DenoiseEnd();

void Denoising(BYTE *prev_Y_MC, BYTE *cur_Y, BYTE *next_Y_MC,
			   short *prev_U_MC, short *cur_U, short *next_U_MC,
			   short *prev_V_MC, short *cur_V, short *next_V_MC,
			   int width, int height, int strength);

int Denoise(const FilterActivation *fa, const FilterFunctions *ff);

#endif