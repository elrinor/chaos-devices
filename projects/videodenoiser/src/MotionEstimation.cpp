#include "Types.h"
#include "FastOps.h"
#include "MotionEstimation.h"

#define SPLITTING
#pragma warning(disable : 4533)

// Duh, Voodoo code :(

#define bmvContinue {if(bmv.error <= ErrorDesired) {MVectors[j * BlocksX + i] = bmv;goto continue_for;}}
#define mvCheckAndContinue {mv.error = SAD8x8(p, p1 + mv.y * w + mv.x, w);if(mv.error < bmv.error){bmv = mv;bmvContinue;}}

void MotionEstimation(BYTE* cur_Y_frame, BYTE* prev_Y_frame, int width, int height, MV *MVectors, MV* LastMVectors, BYTE quality, bool use_half_pixel)
{
  int BlocksY = (height + 7) >> 3, BlocksX = (width + 7) >> 3;
  int w = width + BORDER * 2;
  int OffsetX, OffsetY, Offset0 = w * BORDER + BORDER;
  int ErrorDesired, GlobalErrorDesired = 128 + (100 - (int)quality) / 100.0f * 15 * 256;
  MV mv, bmv, cmv, bcmv;
  BYTE *p, *p1;

  for(int j = 0; j < BlocksY; j++ )
  {
    for(int i = 0; i < BlocksX; i++ )
    {
      ErrorDesired = GlobalErrorDesired;
      if(LastMVectors[j * BlocksX + i].error != 0)
        ErrorDesired = (LastMVectors[j * BlocksX + i].error + ErrorDesired) / 2;

      OffsetX = (i << 3);
      OffsetY = (j << 3) * w + Offset0;
      p  = cur_Y_frame  + OffsetX + OffsetY;
      p1 = prev_Y_frame + OffsetX + OffsetY;

      // Check Zero
      bmv.x = 0;
      bmv.y = 0;
      bmv.error = SAD8x8(p, p1, w);
      bmvContinue;

      // Fill first vector for diamond search;
      bcmv.x = 0;
      bcmv.y = 0;
      bcmv.error = bmv.error;

      // Check Previous MVector
      mv = LastMVectors[j * BlocksX + i];
      mvCheckAndContinue;

      // Check Upper MVector
      if(j != 0) {
        mv = MVectors[(j - 1) * BlocksX + i];
        mvCheckAndContinue;
      }

      // Check Left MVector
      if(i != 0) {
        mv = MVectors[j * BlocksX + i - 1];
        mvCheckAndContinue;
      }

      // Check Temporal Lower MVector
      if(j != BlocksY - 1) {
        mv = LastMVectors[(j + 1) * BlocksX + i];
        mvCheckAndContinue;
      }

      // Check Temporal Right MVector
      if(i != BlocksX - 1) {
        mv = LastMVectors[j * BlocksX + i + 1];
        mvCheckAndContinue;
      }

      // Diamond Search
      int r = MAX_MOTION / 2;
      mv.x = 0;
      mv.y = 0;
      cmv.x = 0;
      cmv.y = 0;
      cmv.error = MAXLONG;

      do {
        mv.x = cmv.x + r;
        mv.y = cmv.y;
        mvCheckAndContinue;
        if(mv.error < bcmv.error)
          bcmv = mv;
        mv.x = cmv.x - r;
        mv.y = cmv.y;
        mvCheckAndContinue;
        if(mv.error < bcmv.error)
          bcmv = mv;
        mv.x = cmv.x;
        mv.y = cmv.y + r;
        mvCheckAndContinue;
        if(mv.error < bcmv.error)
          bcmv = mv;
        mv.x = cmv.x;
        mv.y = cmv.y - r;
        mvCheckAndContinue;
        if(mv.error < bcmv.error)
          bcmv = mv;
        cmv = bcmv;
        r /= 2;
      } while (r > 0);

      if(cmv.error > bmv.error + 256 * 0.25f) // Our diamond search found local min, that's bad
      {
        mv.x = bmv.x + 1;
        mv.y = bmv.y;
        if(mv.x >= MAX_MOTION) 
          mv.x = MAX_MOTION - 1;
        mvCheckAndContinue;
        mv.x = bmv.x - 1;
        mv.y = bmv.y;
        if(mv.x < -MAX_MOTION) 
          mv.x = -MAX_MOTION;
        mvCheckAndContinue;
        mv.x = bmv.x;
        mv.y = bmv.y + 1;
        if(mv.y >= MAX_MOTION) 
          mv.y = MAX_MOTION - 1;
        mvCheckAndContinue;
        mv.x = bmv.x;
        mv.y = bmv.y - 1;
        if(mv.y < -MAX_MOTION) 
          mv.y = -MAX_MOTION;
        mvCheckAndContinue;
      }

      MVectors[j * BlocksX + i] = bmv;
continue_for:;
    }
  }
}













/*

void MotionEstimation(BYTE* cur_Y_frame, BYTE* prev_Y_frame, BYTE* prev_Y_frame_up, BYTE* prev_Y_frame_l, BYTE* prev_Y_frame_upl, int width, int height, MV *MVectors, BYTE quality)
{
  int num_blocks_vert = (height + 15) >> 4, num_blocks_hor = (width + 15) >> 4;
  int wext = width + BORDER * 2;
  int i, j, k, l, temp;
  int vert_offset, hor_offset, first_row_offset = wext * BORDER + BORDER;
  long min_error, error;
  MV prob_motion_vector;
  BYTE *cur, *prev;
  int mm = MAX_MOTION;

  for( i = 0; i < num_blocks_vert; i++ )
  {
    for( j = 0; j < num_blocks_hor; j++ )
    {
      vert_offset = ( i << 4 ) * wext + first_row_offset;
      hor_offset = (j << 4);
      min_error = MAXLONG;
      mm = MAX_MOTION;
      cur = cur_Y_frame + vert_offset + hor_offset;
      prev = prev_Y_frame + vert_offset + hor_offset;
      for( k = -mm; k < mm; k++ )
      {
        temp = k * wext;
        for( l = -mm; l < mm; l++ )
        {
          //if( (l==0) && (k==0) ) continue;
          error = SAD16x16( cur, prev + temp + l, wext );
          if( error < min_error )
          {
            prob_motion_vector.x = l;
            prob_motion_vector.y = k;
            prob_motion_vector.dir = sd_none;
            prob_motion_vector.error = error;
            min_error = error;
          }
        }
      }
      MVectors[ i * num_blocks_hor + j ] = prob_motion_vector;
      if(prob_motion_vector.error > SPLIT_TRESH )// && 
        //(prob_motion_vector.dir == sd_up || prob_motion_vector.dir == sd_upl))
      {
        MVectors[ i * num_blocks_hor + j ].splitted = true;
        for(int h = 0; h < 4; h++)
        {
          if(MVectors[ i * num_blocks_hor + j ].sub[h] == NULL)
          {
            MVectors[ i * num_blocks_hor + j ].sub[h] = new MV;
          }
          prob_motion_vector.x = 0;
          prob_motion_vector.y = 0;
          //mm = MAX_MOTION/2;
          vert_offset = (( i << 4 ) + ((h > 1)? 8 : 0))* wext + first_row_offset;
          hor_offset = (j << 4) + ((h & 1)? 8 : 0);
          min_error = MAXLONG;
          cur = cur_Y_frame + vert_offset + hor_offset;
          prev = prev_Y_frame_up + vert_offset + hor_offset;
          for( k = -mm; k < mm; k++ )
          {
            temp = k * wext;
            for( l = -mm; l < mm; l++ )
            {
              error = SAD8x8( cur, prev + temp + l, wext );
              if( error < min_error )
              {
                prob_motion_vector.x = l;
                prob_motion_vector.y = k;
                prob_motion_vector.dir = sd_up;
                prob_motion_vector.error = error;
                min_error = error;
              }
            }
          }
          prev = prev_Y_frame_upl + vert_offset + hor_offset;
          for( k = -mm; k < mm; k++ )
          {
            temp = k * wext;
            for( l = -mm; l < mm; l++ )
            {
              error = SAD8x8( cur, prev + temp + l, wext );
              if( error < min_error )
              {
                prob_motion_vector.x = l;
                prob_motion_vector.y = k;
                prob_motion_vector.dir = sd_upl;
                prob_motion_vector.error = error;
                min_error = error;
              }
            }
          }
          MVectors[ i * num_blocks_hor + j ].sub[h]->x = prob_motion_vector.x;
          MVectors[ i * num_blocks_hor + j ].sub[h]->y = prob_motion_vector.y;
          MVectors[ i * num_blocks_hor + j ].sub[h]->dir = prob_motion_vector.dir;
          MVectors[ i * num_blocks_hor + j ].sub[h]->error = prob_motion_vector.error;
          MVectors[ i * num_blocks_hor + j ].sub[h]->splitted = false;
        }
        min_error = MVectors[ i * num_blocks_hor + j ].error;
        if((MVectors[ i * num_blocks_hor + j ].sub[0]->error
          +MVectors[ i * num_blocks_hor + j ].sub[1]->error
          +MVectors[ i * num_blocks_hor + j ].sub[2]->error
          +MVectors[ i * num_blocks_hor + j ].sub[3]->error > 
          MVectors[ i * num_blocks_hor + j ].error * 0.7))
        {
          MVectors[ i * num_blocks_hor + j ].splitted = false;
        }
      }
    }
  }

}*/

void MotionEstimateStart(int width, int height, BYTE quality)
{
  //this function is called once for filter chain startup
  //place here code for all memory allocations and initializations you want to do
}

void MotionEstimateEnd()
{
  //this function is opposite to YourStart()
  //place here code to release resources if necessary
}

int MotionEstimate(const FilterActivation *fa, const FilterFunctions *ff)
{
  MFD *mfd = (MFD*)fa->filter_data;
  MotionEstimation(mfd->cur_Y, mfd->prev_Y, mfd->width, mfd->height, mfd->MVectors_prev, mfd->MVectors_prev, mfd->quality, false);
  MotionEstimation(mfd->cur_Y, mfd->next_Y, mfd->width, mfd->height, mfd->MVectors_next, mfd->MVectors_next, mfd->quality, false);
  return 0;
}
