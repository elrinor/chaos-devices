#include "Transformations.h"

void InitYUV(MFD *mfd, Pixel32* src_ref, int pitch)
{
  Pixel32* src = src_ref;
  BYTE *next = mfd->next_Y;
  short *next_U = mfd->next_U, *next_V = mfd->next_V;
  int wext = mfd->ext_w, hext = mfd->ext_h;
  int w = mfd->width, h = mfd->height, p = pitch;
  int first_row_offset = BORDER * wext, after_last_row_offset = (BORDER + h) * wext;
  int i, j;

  next = next + first_row_offset;
  for( i = 0; i < h; i++ )
  {
    //filling extra space to the left from picture
    memset( next, GetYFromRGB( src[0] ), BORDER + 1);
    next_U[0] = GetUFromRGB( src[0] );
    next_V[0] = GetVFromRGB( src[0] );
    for( j = 1; j < w - 1; j++ )
    {
      next[ j + BORDER ] = GetYFromRGB( src[j] );
      next_U[j] = GetUFromRGB( src[j] );
      next_V[j] = GetVFromRGB( src[j] );
    }
    //filling extra space to the right from picture
    memset(next + BORDER + w - 1, GetYFromRGB(src[w - 1]), BORDER+1);
    next_U[w - 1] = GetUFromRGB(src[w - 1]);
    next_V[w - 1] = GetVFromRGB(src[w - 1]);
    src = (Pixel32*)((BYTE*)src + p);
    next += wext;
    next_U += w;
    next_V += w;
  }

  //It's only left now to fulfill extra space above and below the picture
  BYTE *first_next = mfd->next_Y + first_row_offset;
  next = mfd->next_Y;
  for( i = 0; i < BORDER; i++ )
  {
    memcpy( next, first_next, wext );
    next += wext; 
  }
  next = mfd->next_Y + after_last_row_offset;
  first_next = mfd->next_Y + after_last_row_offset - wext;
  for( i = 0; i < BORDER; i++ )
  {
    memcpy( next, first_next, wext );
    next += wext; 
  }
}