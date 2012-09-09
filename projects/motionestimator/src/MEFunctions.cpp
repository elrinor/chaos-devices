#include "types.h"

#pragma warning(disable : 4533)

// Duh, Voodoo code :(

#define GetError GetErrorSAD_16x16

#define bmvContinue {if(bmv.error <= ErrorDesired) {MVectors[j * BlocksX + i] = bmv;goto continue_for;}}
#define mvCheckAndContinue {mv.error = GetErrorSAD_16x16(p, p1[mv.dir] + mv.y * w + mv.x, w);if(mv.error < bmv.error){bmv = mv;bmvContinue;}}

#define bmvContinue8x8 {if(bmv.error <= ErrorDesired) {MVectors[j * BlocksX + i].sub[h].x = bmv.x;MVectors[j * BlocksX + i].sub[h].y = bmv.y;MVectors[j * BlocksX + i].sub[h].dir = bmv.dir;MVectors[j * BlocksX + i].sub[h].error = bmv.error;goto continue_for8x8;}}
#define mvCheckAndContinue8x8 {mv.error = GetErrorSAD_8x8(p, p1[mv.dir] + mv.y * w + mv.x, w);if(mv.error < bmv.error){bmv = mv;bmvContinue8x8;}}


void MEFunction(BYTE* cur_Y_frame, BYTE* prev_Y_frame, BYTE* prev_Y_frame_up, BYTE* prev_Y_frame_l, BYTE* prev_Y_frame_upl, int width, int height, MV *MVectors, MV* LastMVectors, BYTE quality, bool use_half_pixel)
{
  int BlocksY = (height + 15) >> 4, BlocksX = (width + 15) >> 4;
	int w = width + BORDER * 2;
	int OffsetX, OffsetY, Offset0 = w * BORDER + BORDER;
	int ErrorDesired, GlobalErrorDesired = 128 + (100 - (int)quality) / 100.0f * 15 * 256;
	MV mv, bmv, cmv, bcmv;
	BYTE *p, *p1[4];

  for(int j = 0; j < BlocksY; j++ )
	{
		for(int i = 0; i < BlocksX; i++ )
		{
      ErrorDesired = GlobalErrorDesired;
      if(LastMVectors[j * BlocksX + i].error != 0)
        ErrorDesired = (LastMVectors[j * BlocksX + i].error + ErrorDesired) / 2;

      OffsetX = (i << 4);
      OffsetY = (j << 4) * w + Offset0;
      p           = cur_Y_frame      + OffsetX + OffsetY;
      p1[sd_none] = prev_Y_frame     + OffsetX + OffsetY;
      p1[sd_up]   = prev_Y_frame_up  + OffsetX + OffsetY;
      p1[sd_l]    = prev_Y_frame_l   + OffsetX + OffsetY;
      p1[sd_upl]  = prev_Y_frame_upl + OffsetX + OffsetY;

      /*
      // Try to fix desired error
      if(i > 0 && j > 0 && !MVectors[(j - 1) * BlocksX + i].splitted && !MVectors[j * BlocksX + (i - 1)].splitted && abs(MVectors[(j - 1) * BlocksX + i].x - MVectors[j * BlocksX + (i - 1)].x) + abs(MVectors[(j - 1) * BlocksX + i].y - MVectors[j * BlocksX + (i - 1)].y) < 8) {
        ErrorDesired = max(MVectors[(j - 1) * BlocksX + i].error, MVectors[j * BlocksX + (i - 1)].error);
        // Diamond from best vector
        int r = 2;
        mv.x = 0;
        mv.y = 0;
        mv.dir = sd_none;
        bcmv.x = (MVectors[(j - 1) * BlocksX + i].x + MVectors[j * BlocksX + (i - 1)].x) / 2;
        bcmv.y = (MVectors[(j - 1) * BlocksX + i].y + MVectors[j * BlocksX + (i - 1)].y) / 2;
        bcmv.dir = sd_none;
        bcmv.error = GetErrorSAD_16x16(p, p1[bcmv.dir] + bcmv.y * w + bcmv.x, w);
        cmv = bcmv;

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

        // Half-pixel correction
        if(use_half_pixel)
        {
          mv = cmv;
          for(int n = 0; n < 4; n++)
          {
            mv.dir = (Shift_Dir)n;
            mvCheckAndContinue;
          }
        }
      }*/
      
      // Check Zero
      bmv.splitted = false;
      bmv.x = 0;
      bmv.y = 0;
      bmv.dir = sd_none;
      bmv.error = GetErrorSAD_16x16(p, p1[bmv.dir], w);
      bmvContinue;

      // Fill first vector for diamond search;
      bcmv.x = 0;
      bcmv.y = 0;
      bcmv.error = bmv.error;
      bcmv.dir = sd_none;

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
      mv.dir = sd_none;
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

      // Half-pixel correction
      if(use_half_pixel)
      {
        mv = cmv;
        for(int n = 0; n < 4; n++)
        {
          mv.dir = (Shift_Dir)n;
          mvCheckAndContinue;
        }
      }

      MVectors[j * BlocksX + i] = bmv;

      bmv.error = MAXLONG;

#ifdef SPLITTING

      if(abs(bmv.x) + abs(bmv.y) < 7)
        goto continue_for;
      
      if(bmv.error > ErrorDesired) // SPLIT!
      {
        MVectors[j * BlocksX + i].splitted = true;
        ErrorDesired /= 4;
        for(int h = 0; h < 4; h++)
        {
          if(LastMVectors[j * BlocksX + i].splitted && LastMVectors[j * BlocksX + i].sub[h].error < ErrorDesired)
            ErrorDesired = LastMVectors[j * BlocksX + i].sub[h].error;

          OffsetX = (i << 4) + ((h & 1)? 8 : 0);
          OffsetY = ((j << 4) + ((h > 1)? 8 : 0)) * w + Offset0;
          p           = cur_Y_frame      + OffsetX + OffsetY;
          p1[sd_none] = prev_Y_frame     + OffsetX + OffsetY;
          p1[sd_up]   = prev_Y_frame_up  + OffsetX + OffsetY;
          p1[sd_l]    = prev_Y_frame_l   + OffsetX + OffsetY;
          p1[sd_upl]  = prev_Y_frame_upl + OffsetX + OffsetY;

          // Check Last Best
          if (h > 0) {
            bmv.error = GetErrorSAD_8x8(p, p1[bmv.dir], w);
            bmvContinue8x8;
          }

          // Check Zero
          bmv.x = 0;
          bmv.y = 0;
          bmv.dir = sd_none;
          bmv.error = GetErrorSAD_8x8(p, p1[bmv.dir], w);
          bmvContinue8x8;

          // Fill first vector for diamond search;
          bcmv.x = 0;
          bcmv.y = 0;
          bcmv.error = bmv.error;
          bcmv.dir = sd_none;

          // Check Previous MVector
          if(LastMVectors[j * BlocksX + i].splitted) {
            mv.x = LastMVectors[j * BlocksX + i].sub[h].x;
            mv.y = LastMVectors[j * BlocksX + i].sub[h].y;
            mv.dir = LastMVectors[j * BlocksX + i].sub[h].dir;
            mv.error = LastMVectors[j * BlocksX + i].sub[h].error;
            mvCheckAndContinue8x8;
          }

          // Diamond Search
          int r = MAX_MOTION / 2 - 1;
          mv.x = 0;
          mv.y = 0;
          mv.dir = sd_none;
          cmv.x = 0;
          cmv.y = 0;
          cmv.error = MAXLONG;

          do {
            mv.x = cmv.x + r;
            mv.y = cmv.y;
            mvCheckAndContinue8x8;
            if(mv.error < bcmv.error)
              bcmv = mv;
            mv.x = cmv.x - r;
            mv.y = cmv.y;
            mvCheckAndContinue8x8;
            if(mv.error < bcmv.error)
              bcmv = mv;
            mv.x = cmv.x;
            mv.y = cmv.y + r;
            mvCheckAndContinue8x8;
            if(mv.error < bcmv.error)
              bcmv = mv;
            mv.x = cmv.x;
            mv.y = cmv.y - r;
            mvCheckAndContinue8x8;
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
            mvCheckAndContinue8x8;
            mv.x = bmv.x - 1;
            mv.y = bmv.y;
            if(mv.x < -MAX_MOTION) 
              mv.x = -MAX_MOTION;
            mvCheckAndContinue8x8;
            mv.x = bmv.x;
            mv.y = bmv.y + 1;
            if(mv.y >= MAX_MOTION) 
              mv.y = MAX_MOTION - 1;
            mvCheckAndContinue8x8;
            mv.x = bmv.x;
            mv.y = bmv.y - 1;
            if(mv.y < -MAX_MOTION) 
              mv.y = -MAX_MOTION;
            mvCheckAndContinue8x8;
          }

          //MessageBox(0, "!!", "!!", 0);

          // Half-pixel correction
          if(use_half_pixel)
          {
            mv = cmv;
            for(int n = 0; n < 4; n++)
            {
              mv.dir = (Shift_Dir)n;
              mvCheckAndContinue8x8;
            }
          }

          MVectors[j * BlocksX + i].sub[h].x = bmv.x;
          MVectors[j * BlocksX + i].sub[h].y = bmv.y;
          MVectors[j * BlocksX + i].sub[h].dir = bmv.dir;
          MVectors[j * BlocksX + i].sub[h].error = bmv.error;
continue_for8x8:
          ;
        }
        
        if((MVectors[j * BlocksX + i].sub[0].error + MVectors[j * BlocksX + i].sub[1].error + MVectors[j * BlocksX + i].sub[2].error + MVectors[j * BlocksX + i].sub[3].error > MVectors[j * BlocksX + i].error))
          MVectors[j * BlocksX + i].splitted = false;
        else
          MVectors[j * BlocksX + i].error = MVectors[j * BlocksX + i].sub[0].error + MVectors[j * BlocksX + i].sub[1].error + MVectors[j * BlocksX + i].sub[2].error + MVectors[j * BlocksX + i].sub[3].error;
        continue;
      }

      
      /*if(bmv.error > ErrorDesired)
      {
        MVectors[j * BlocksX + i].splitted = true;
        for(int h = 0; h < 4; h++)
        {
          mv.x = 0;
          mv.y = 0;
          OffsetY = ((j << 4) + ((h > 1)? 8 : 0))* w + Offset0;
          OffsetX = (i << 4) + ((h & 1)? 8 : 0);
          p = cur_Y_frame + OffsetY + OffsetX;
          p1[0] = prev_Y_frame + OffsetY + OffsetX;
          int mm = MAX_MOTION;
          int min_error = MAXLONG;
          for(int k = -mm; k < mm; k++ )
          {
            int temp = k * w;
            for(int l = -mm; l < mm; l++ )
            {
              int error = GetErrorSAD_8x8( p, p1[0] + temp + l, w );
              if( error < min_error )
              {
                mv.x = l;
                mv.y = k;
                mv.dir = sd_none;
                mv.error = error;
                min_error = error;
              }
            }
          }
          MVectors[j * BlocksX + i].sub[h].x = mv.x;
          MVectors[j * BlocksX + i].sub[h].y = mv.y;
          MVectors[j * BlocksX + i].sub[h].dir = mv.dir;
          MVectors[j * BlocksX + i].sub[h].error = mv.error;
        }
        continue;
      }*/

/*
      int num_blocks_vert = (height + 15) >> 4, num_blocks_hor = (width + 15) >> 4;
      int wext = width + BORDER * 2;
      int k, l, temp;
      int vert_offset, hor_offset, first_row_offset = wext * BORDER + BORDER;
      long min_error, error;
      MV prob_motion_vector;
      BYTE *cur, *prev;
      int mm = MAX_MOTION;
      prob_motion_vector.error = MAXLONG;
      if(prob_motion_vector.error > SPLIT_TRESH )
      {
        MVectors[ j * num_blocks_hor + i ].splitted = true;
        for(int h = 0; h < 4; h++)
        {
          prob_motion_vector.x = 0;
          prob_motion_vector.y = 0;
          vert_offset = ((j << 4 ) + ((h > 1)? 8 : 0))* wext + first_row_offset;
          hor_offset = (i << 4) + ((h & 1)? 8 : 0);
          min_error = MAXLONG;
          cur = cur_Y_frame + vert_offset + hor_offset;

          prev = prev_Y_frame + vert_offset + hor_offset;
          for( k = -mm; k < mm; k++ )
          {
            temp = k * wext;
            for( l = -mm; l < mm; l++ )
            {
              error = GetErrorSAD_8x8( cur, prev + temp + l, wext );
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

          MVectors[ j * num_blocks_hor + i ].sub[h].x = prob_motion_vector.x;
          MVectors[ j * num_blocks_hor + i ].sub[h].y = prob_motion_vector.y;
          MVectors[ j * num_blocks_hor + i ].sub[h].dir = prob_motion_vector.dir;
          MVectors[ j * num_blocks_hor + i ].sub[h].error = prob_motion_vector.error;
          //MVectors[ j * num_blocks_hor + i ].sub[h].splitted = false;
        }
        continue;
      }*/


#endif

continue_for:
      MVectors[j * BlocksX + i].splitted = false;
		}
	}

}


/*void MEFunction(BYTE* cur_Y_frame, BYTE* prev_Y_frame, BYTE* prev_Y_frame_up, BYTE* prev_Y_frame_l, BYTE* prev_Y_frame_upl, int width, int height, MV *MVectors, MV* LastMVectors, BYTE quality, bool use_half_pixel)
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
          error = GetError( cur, prev + temp + l, wext );
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
      if(use_half_pixel)
      {
        prev = prev_Y_frame_up + vert_offset + hor_offset;
        //searching for the most probable motion vector
        for( k = -mm; k < mm; k++ )
        {
          temp = k * wext;
          for( l = -mm; l < mm; l++ )
          {
            error = GetError( cur, prev + temp + l, wext );
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
        prev = prev_Y_frame_l + vert_offset + hor_offset;
        for( k = -mm; k < mm; k++ )
        {
          temp = k * wext;
          for( l = -mm; l < mm; l++ )
          {
            error = GetError( cur, prev + temp + l, wext );
            if( error < min_error )
            {
              prob_motion_vector.x = l;
              prob_motion_vector.y = k;
              prob_motion_vector.dir = sd_l;
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
            error = GetError( cur, prev + temp + l, wext );
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
      }
      MVectors[ i * num_blocks_hor + j ] = prob_motion_vector;
      if(prob_motion_vector.error > SPLIT_TRESH )
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
          vert_offset = (( i << 4 ) + ((h > 1)? 8 : 0))* wext + first_row_offset;
          hor_offset = (j << 4) + ((h & 1)? 8 : 0);
          min_error = MAXLONG;
          cur = cur_Y_frame + vert_offset + hor_offset;

          prev = prev_Y_frame + vert_offset + hor_offset;
          for( k = -mm; k < mm; k++ )
          {
            temp = k * wext;
            for( l = -mm; l < mm; l++ )
            {
              error = GetErrorSAD_8x8( cur, prev + temp + l, wext );
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

          if(use_half_pixel)
          {
            prev = prev_Y_frame_up + vert_offset + hor_offset;
            for( k = -mm; k < mm; k++ )
            {
              temp = k * wext;
              for( l = -mm; l < mm; l++ )
              {
                error = GetErrorSAD_8x8( cur, prev + temp + l, wext );
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

            prev = prev_Y_frame_l + vert_offset + hor_offset;
            for( k = -mm; k < mm; k++ )
            {
              temp = k * wext;
              for( l = -mm; l < mm; l++ )
              {
                error = GetErrorSAD_8x8( cur, prev + temp + l, wext );
                if( error < min_error )
                {
                  prob_motion_vector.x = l;
                  prob_motion_vector.y = k;
                  prob_motion_vector.dir = sd_l;
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
                error = GetErrorSAD_8x8( cur, prev + temp + l, wext );
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


void MEStart( int width, int height, BYTE quality )
{
	//this function is called once for filter chain startup
	//place here code for all memory allocations and initializations you want to do
}

void MEEnd()
{
	//this function is opposite to YourStart()
	//place here code to release resources if necessary
}

