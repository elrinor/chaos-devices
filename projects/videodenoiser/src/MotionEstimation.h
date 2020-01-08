#ifndef __MOTIONESTIMATION_H__
#define __MOTIONESTIMATION_H__
#include "Types.h"

void MotionEstimation(BYTE* cur_Y_frame, BYTE* prev_Y_frame, int width, int height, MV *MVectors, MV* LastMVectors, BYTE quality, bool use_half_pixel);

void MotionEstimateStart(int width, int height, BYTE quality);

void MotionEstimateEnd();

int MotionEstimate(const FilterActivation *fa, const FilterFunctions *ff);


#endif