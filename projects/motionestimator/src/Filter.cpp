#include "types.h"

//Global variables
static FilterDefinition *fd;
bool flag_first_frame;
char *MethodDesc[] = { "Not another ME method" };
char *MethodDescShort[] = { "ME" };

int initProc(FilterActivation *fa, const FilterFunctions *ff);

int startProc(FilterActivation *fa, const FilterFunctions *ff)
{
	MFD* mfd = (MFD*)fa->filter_data;
	//Allocating buffers of extra size ( will need in motion estimation )
	mfd->width = fa->dst.w;
	mfd->height = fa->dst.h;
	mfd->size = mfd->width * mfd->height;
	mfd->ext_w = mfd->width + BORDER * 2;
	mfd->ext_h = mfd->height + BORDER * 2;
	mfd->ext_size = mfd->ext_h * mfd->ext_w;
	mfd->num_blocks_vert = mfd->height + 15 >> 4;
	mfd->num_blocks_hor = mfd->width + 15 >> 4;
	mfd->prev_Y = new BYTE[ mfd->ext_size ];
	mfd->prev_Y_up = new BYTE[ mfd->ext_size ];
	mfd->prev_Y_upl = new BYTE[ mfd->ext_size ];
	mfd->prev_Y_l = new BYTE[ mfd->ext_size ];
	mfd->prev_U = new short[ mfd->width * mfd->height ];
	mfd->prev_U_up = new short[ mfd->width * mfd->height ];
	mfd->prev_U_upl = new short[ mfd->width * mfd->height ];
	mfd->prev_U_l = new short[ mfd->width * mfd->height ];
	mfd->prev_U_MC = new short[ mfd->width * mfd->height ];
	mfd->prev_V = new short[ mfd->width * mfd->height ];
	mfd->prev_V_up = new short[ mfd->width * mfd->height ];
	mfd->prev_V_upl = new short[ mfd->width * mfd->height ];
	mfd->prev_V_l = new short[ mfd->width * mfd->height ];
	mfd->prev_V_MC = new short[ mfd->width * mfd->height ];
	mfd->prev_Y_MC = new BYTE[ mfd->width * mfd->height ];
	mfd->cur_U = new short[ mfd->width * mfd->height ];
	mfd->cur_V = new short[ mfd->width * mfd->height ];
	mfd->cur_Y = new BYTE[ mfd->ext_size ];
	mfd->MVectors = new MV[ mfd->num_blocks_vert * mfd->num_blocks_hor ];
  mfd->LastMVectors = new MV[ mfd->num_blocks_vert * mfd->num_blocks_hor ];
  memset(mfd->LastMVectors, 0, sizeof(MV) * mfd->num_blocks_vert * mfd->num_blocks_hor);
  //mfd->LastBlockSums = new unsigned int[ mfd->width * mfd->height ];
  //mfd->BlockSums = new unsigned int[ mfd->width * mfd->height ];

	//if(mfd->raise_sharpness)
	//{
		mfd->output_buf = new Pixel32[mfd->size];
		mfd->prev_RGB = new Pixel32[mfd->size];
		mfd->prev_RGB_up = new Pixel32[mfd->size];
		mfd->prev_RGB_upl = new Pixel32[mfd->size];
		mfd->prev_RGB_l = new Pixel32[mfd->size];
	//}
	mfd->frame_no = 0;
	mfd->offset = 0;
	mfd->noise = 0.0;
	flag_first_frame = true;
	MEStart( mfd->width, mfd->height, mfd->quality );
	return 0;
}

BYTE GetYFromRGB( Pixel32 rgb )
{
	return  (BYTE)(((rgb & 0xff0000) >> 16) * 0.299 
					+ ((rgb & 0xff00) >> 8) * 0.587 
					+ (rgb & 0xff) * 0.114  + 0.5);
}

void DrawLine( Pixel32* canvas, int width, int line_offset, int x1, int y1, int x2, int y2 )
{
	int x, y;
	Pixel32 *cur;
	bool origin;
	bool point = x1 == x2 && y1 == y2;
	if( x1 == x2 )
	{
		for( y = min( y1, y2 ); y <= max( y2, y1 ); y++ )
		{
			x = x1;
			if( x < 0 || x >= width )
				continue;
			origin = y == y1;
			cur = (Pixel32*)((BYTE*)canvas + y * line_offset) + x;
			if( point )
			{
				*cur = 0x00ff00;
				return;
			}
			if( GetYFromRGB( *cur ) < 128 )
			{
				if( origin )
				{
					*cur = 0xff0000;
					origin = false;
				}
				else
					*cur = 0xffffff;
			}
			else
			{
				if( origin )
				{
					*cur = 0xff0000;
					origin = false;
				}
				else
					*cur = 0x00;
			}
		}
	}
	else if( abs( x2 - x1 ) >= abs( y2 - y1 ) )
	{
		for( x = min( x1, x2 ); x <= max( x2, x1 ); x++ )
		{
			if( x < 0 || x >= width )
				continue;
			origin = x == x1;
			y = ( x - x1 ) * ( y2 - y1 ) / ( x2 - x1 ) + y1;
			cur = (Pixel32*)((BYTE*)canvas + y * line_offset) + x;
			if( point )
			{
				*cur = 0x00ff00;
				return;
			}
			if( GetYFromRGB( *cur ) < 128 )
			{
				if( origin )
				{
					*cur = 0xff0000;
					origin = false;
				}
				else
					*cur = 0xffffff;
			}
			else
			{
				if( origin )
				{
					*cur = 0xff0000;
					origin = false;
				}
				else
					*cur = 0x000000;
			}
		}
	}
	else
	{
		for( y = min( y1, y2 ); y <= max( y2, y1 ); y++ )
		{
			origin = y == y1;
			x = ( y - y1 ) * ( x2 - x1 ) / ( y2 - y1 ) + x1;
			if( x < 0 || x >= width )
				continue;
			cur = (Pixel32*)((BYTE*)canvas + y * line_offset) + x;
			if( point )
			{
				*cur = 0x00ff00;
				return;
			}
			if( GetYFromRGB( *cur ) < 128 )
			{
				if( origin )
				{
					*cur = 0xff0000;
					origin = false;
				}
				else
					*cur = 0xffffff;
			}
			else
			{
				if( origin )
				{
					*cur = 0xff0000;
					origin = false;
				}
				else
					*cur = 0x000000;
			}
		}
	}
}
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


long GetErrorSAD_16x16(const BYTE* block1, const BYTE* block2, const int stride)
{
	long sum = 0;
	short m[4] = {1,1,1,1};
	int b1 = (int)block1;
	int b2 = (int)block2;
	__asm
	{
		push edi
		push esi

		mov esi, b1//[esp + 8 + 4]	; Current MB (Estimated MB)
		mov edi, b2//[esp + 8 + 12]	; reference MB
		mov eax, stride//[esp + 8 + 20]	; stride
		
		pxor mm7, mm7			; mm7 = sum = 0
		pxor mm6, mm6			; mm6 = 0

		movq mm1, [esi]			; 8 pixels for current MB
		movq mm3, [esi+8]		; another 8 pixels in the same row of current MB

		mov	ecx, 16
		//mov	ecx, 8

sad16_mmx_loop:
		movq mm0, [edi]			; 8 pixels for reference MB
		movq mm2, [edi+8]		; another 8 pixels in the same row of reference MB

		movq mm4, mm1
		movq mm5, mm3

		psubusb mm1, mm0
		psubusb mm3, mm2
		
		add	esi, eax
		add	edi, eax

		psubusb mm0, mm4
		psubusb mm2, mm5

		por mm0, mm1			; mm0 = |cur - ref|
		por mm2, mm3			; mm2 = |*(cur+8) - *(ref+8)|

		movq mm1,mm0
		movq mm3,mm2

		punpcklbw mm0,mm6
		punpckhbw mm1,mm6

		punpcklbw mm2,mm6
		punpckhbw mm3,mm6

		paddusw mm0,mm1
		paddusw mm2,mm3		

		paddusw mm7,mm0		; sum += mm01
		movq mm1, [esi]		; 8 pixels for current MB

		paddusw mm7,mm2		; sum += mm23
		movq mm3, [esi+8]	; another 8 pixels in the same row of current MB

		;// start next row's processing
		movq mm0, [edi]			; 8 pixels for reference MB
		movq mm2, [edi+8]		; another 8 pixels in the same row of reference MB

		movq mm4, mm1
		movq mm5, mm3

		psubusb mm1, mm0
		psubusb mm3, mm2
		
		add	esi, eax
		add	edi, eax

		psubusb mm0, mm4
		psubusb mm2, mm5

		por mm0, mm1			; mm0 = |cur - ref|
		por mm2, mm3			; mm2 = |*(cur+8) - *(ref+8)|

		movq mm1,mm0
		movq mm3,mm2

		punpcklbw mm0,mm6
		punpckhbw mm1,mm6

		punpcklbw mm2,mm6
		punpckhbw mm3,mm6

		paddusw mm0,mm1
		paddusw mm2,mm3		

		paddusw mm7,mm0		; sum += mm01
		movq mm1, [esi]		; 8 pixels for current MB

		paddusw mm7,mm2		; sum += mm23
		sub		ecx, 2		; unlooped two rows' processing

		movq mm3, [esi+8]	; another 8 pixels in the same row of current MB
		
		jnz	sad16_mmx_loop

		pmaddwd mm7, m	; merge sum
		pop 	esi

		movq mm0, mm7
		pop 	edi
		psrlq mm7, 32 

		paddd mm0, mm7
		movd sum, mm0

		emms
	}
	return sum;
}

long GetErrorSAD_8x8(const BYTE* block1, const BYTE* block2, const int stride)
{
	long sum = 0;
	short m[4] = {1,1,1,1};
	int b1 = (int)block1;
	int b2 = (int)block2;
	__asm
	{
		push edi
		push esi

		mov esi, b1//[esp + 8 + 4]	; Current MB (Estimated MB)
		mov edi, b2//[esp + 8 + 12]	; reference MB
		mov eax, stride//[esp + 8 + 20]	; stride
		
		pxor	mm6, mm6			; mm6 = sum = 0

		pxor	mm7, mm7			; mm7 = 0
		mov		ecx, 4

sad8_mmx_lp:

		movq mm0, [esi]	; ref
		movq mm1, [edi]	; cur

		add	esi, eax
		add	edi, eax

		movq mm2, [esi]	; ref2
		movq mm3, [edi]	; cur2

		movq mm4, mm0 
		movq mm5, mm2

		psubusb mm0, mm1
		psubusb mm2, mm3
		
		psubusb mm1, mm4
		psubusb mm3, mm5

		por mm0, mm1			; mm0 = |ref - cur|
		por mm2, mm3			; mm2 = |*(ref+stride) - *(cur+stride)|

		movq mm1,mm0
		movq mm3,mm2

		punpcklbw mm0,mm7
		punpcklbw mm2,mm7

		punpckhbw mm1,mm7
		punpckhbw mm3,mm7

		paddusw mm0,mm1
		paddusw mm2,mm3
		
		paddusw mm6,mm0			; sum += mm01
		add	esi, eax

		add	edi, eax
		paddusw mm6,mm2			; sum += mm23

		dec	ecx
		jnz	sad8_mmx_lp

		pmaddwd mm6, m	; merge sum
		pop 	esi

		movq mm7, mm6
		pop 	edi

		psrlq mm7, 32 

		paddd mm6, mm7

		movd sum, mm6

		emms
	}
	return sum;
}


int MotionEstimate(const FilterActivation *fa, const FilterFunctions *ff)
{
	MFD *mfd = (MFD*)fa->filter_data;
	//FullSearchSerial_16x16( mfd->cur_Y, mfd->prev_Y, mfd->width, mfd->height, mfd->MVectors);
	MEFunction( mfd->cur_Y, mfd->prev_Y, mfd->prev_Y_up, mfd->prev_Y_l, mfd->prev_Y_upl, mfd->width, mfd->height, mfd->MVectors, mfd->LastMVectors, mfd->quality, mfd->use_half_pixel);
	return 0;
}

void Log(const FilterActivation *fa, MV *mvectors)
{
	MFD *mfd = (MFD*)fa->filter_data;
	int i, h;
	int num_blocks_total = mfd->num_blocks_hor * mfd->num_blocks_vert;
	double sum_error, sum_length;
	static double all_sum_error, all_sum_length;
	MV mvector;
	FILE *f;
	/*if(mfd->frame_no == 0)
	{
		f = fopen(mfd->log_file,"w");
		all_sum_error = 0;
		all_sum_length = 0;
		fseek(f, 66, SEEK_SET);
	}
	else
	{*/
		f = fopen(mfd->log_file,"a");
		//fseek(f, 0, SEEK_END);
	//}
	sum_error = 0;
	sum_length = 0;
	for(i = 0; i < num_blocks_total; i++)
	{
		mvector = mvectors[i];
		if(mvector.splitted)
		{
			sum_error += mvector.sub[0].error;
			sum_length += sqrt((double)(mvector.sub[0].x) * mvector.sub[0].x + mvector.sub[0].y * mvector.sub[0].y);
			sum_error += mvector.sub[1].error;
			sum_length += sqrt((double)(mvector.sub[1].x) * mvector.sub[1].x + mvector.sub[1].y * mvector.sub[1].y);
			sum_error += mvector.sub[2].error;
			sum_length += sqrt((double)(mvector.sub[2].x) * mvector.sub[2].x + mvector.sub[2].y * mvector.sub[2].y);
			sum_error += mvector.sub[3].error;
			sum_length += sqrt((double)(mvector.sub[3].x) * mvector.sub[3].x + mvector.sub[3].y * mvector.sub[3].y);
		}
		else
		{
			sum_error += mvector.error;
			sum_length += sqrt((double)(mvector.x) * mvector.x + mvector.y * mvector.y);
		}
	}
	sum_error /= num_blocks_total;
	sum_length /= num_blocks_total;

	fprintf(f, "Frame #,%5d, average_error,%11.2f, average_length,%7.2f, absolute frame #,%5d\n",
			mfd->frame_no, sum_error, sum_length, fa->pfsi->lCurrentSourceFrame);
	all_sum_error += sum_error;
	all_sum_length += sum_length;
	//fseek(f, 0, SEEK_SET);
	fprintf(f, "All frames:,   Average_Error,%11.2f, Average_Length,%7.2f\n",all_sum_error/(mfd->frame_no + 1),all_sum_length/(mfd->frame_no + 1));
	fclose(f);
}

void DrawOutput(const FilterActivation *fa, MV *mvectors)
{
	MV mvector;
	MFD *mfd = (MFD*)fa->filter_data;
	Pixel32 *src = (Pixel32*)fa->src.data;
	Pixel32 *dst = (Pixel32*)fa->dst.data;
	int w = mfd->width, h = mfd->height, p = fa->src.pitch;
	int wext = mfd->ext_w, hext = mfd->ext_h;
	int first_row_offset = BORDER * wext, after_last_row_offset = (BORDER + h) * wext;
	BYTE *cur,  *prev;
	short *cur_U, *cur_V, *prev_U, *prev_V;
	Pixel32 *RGB_MC, src_pixel, mc_pixel, *RGB;
	int i, j, sh_x, sh_y;
	int y, u, v, r, g, b;

	if( mfd->show_res_after )
	{
		dst = (Pixel32*)fa->dst.data;
		src = (Pixel32*)fa->src.data;
		cur = mfd->cur_Y + first_row_offset + BORDER;
		cur_U = mfd->cur_U;
		cur_V = mfd->cur_V;
		prev = mfd->prev_Y_MC;
		prev_U = mfd->prev_U_MC;
		prev_V = mfd->prev_V_MC;
		RGB_MC = mfd->output_buf;
		for( i = 0; i < h; i++ )
		{
			for( j = 0; j < w; j++ )
			{
				if(!mfd->work_in_RGB)
				{
					y = (cur[j] - prev[j]) * 3;
					u = (cur_U[j] - prev_U[j]) * 3;
					v = (cur_V[j] - prev_V[j]) * 3;
					r = y + 1.14 * v + 0.5 + 128;
					g = y - 0.395 * u - 0.581 * v + 0.5 + 128;
					b = y + 2.032 * u + 0.5 + 128;
				}
				else
				{
					src_pixel = src[j];
					mc_pixel = RGB_MC[j];
					r = ((src_pixel >> 16 & 0xff) - (mc_pixel >> 16 & 0xff)) * 3 + 128;
					g = ((src_pixel >> 8 & 0xff) - (mc_pixel >> 8 & 0xff)) * 3 + 128;
					b = ((src_pixel & 0xff) - (mc_pixel & 0xff)) * 3 + 128;
				}
				if(r > 255) r = 255; else if(r < 0) r = 0;
				if(g > 255) g = 255; else if(g < 0) g = 0;
				if(b > 255) b = 255; else if(b < 0) b = 0;
				dst[j] = (r << 16) | (g << 8) | b;
			}
			cur += wext;
			cur_U += w;
			cur_V += w;
			prev += w;
			prev_U += w;
			prev_V += w;
			dst = (Pixel32*)((BYTE*)dst + p);
			src = (Pixel32*)((BYTE*)src + p);
			RGB_MC += w;
		}
	}
	else if( mfd->show_MC )
	{
		dst = (Pixel32*)fa->dst.data;
		prev = mfd->prev_Y_MC;
		prev_U = mfd->prev_U_MC;
		prev_V = mfd->prev_V_MC;
		RGB_MC = mfd->output_buf;
		for( i = 0; i < h; i++ )
		{
			for( j = 0; j < w; j++ )
			{
				if(!mfd->work_in_RGB)
				{
					y = prev[j];
					u = prev_U[j];
					v = prev_V[j];
					r = y + 1.14 * v + 0.5;
					g = y - 0.395 * u - 0.581 * v + 0.5;
					b = y + 2.032 * u + 0.5;
				}
				else
				{
					src_pixel = src[j];
					mc_pixel = RGB_MC[j];
					r = (mc_pixel >> 16 & 0xff);
					g = (mc_pixel >> 8 & 0xff);
					b = (mc_pixel & 0xff);
				}
				if(r > 255) r = 255; else if(r < 0) r = 0;
				if(g > 255) g = 255; else if(g < 0) g = 0;
				if(b > 255) b = 255; else if(b < 0) b = 0;
				dst[j] = (r << 16) | (g << 8) | b;
			}
			prev += w;
			prev_U += w;
			prev_V += w;
			dst = (Pixel32*)((BYTE*)dst + p);
			src = (Pixel32*)((BYTE*)src + p);
			RGB_MC += w;
		}
	}
	if( mfd->show_res_before )
	{
		dst = (Pixel32*)fa->dst.data;
		src = (Pixel32*)fa->src.data;
		cur = mfd->cur_Y + first_row_offset + BORDER;
		cur_U = mfd->cur_U;
		cur_V = mfd->cur_V;
		prev = mfd->prev_Y + first_row_offset + BORDER;
		prev_U = mfd->prev_U;
		prev_V = mfd->prev_V;
		RGB_MC = mfd->prev_RGB;
		for( i = 0; i < h; i++ )
		{
			for( j = 0; j < w; j++ )
			{
				if(!mfd->work_in_RGB)
				{
					y = (cur[j] - prev[j]) * 3;
					u = (cur_U[j] - prev_U[j]) * 3;
					v = (cur_V[j] - prev_V[j]) * 3;
					r = y + 1.14 * v + 0.5 + 128;
					g = y - 0.395 * u - 0.581 * v + 0.5 + 128;
					b = y + 2.032 * u + 0.5 + 128;
				}
				else
				{
					src_pixel = src[j];
					mc_pixel = RGB_MC[j];
					r = ((src_pixel >> 16 & 0xff) - (mc_pixel >> 16 & 0xff)) * 3 + 128;
					g = ((src_pixel >> 8 & 0xff) - (mc_pixel >> 8 & 0xff)) * 3 + 128;
					b = ((src_pixel & 0xff) - (mc_pixel & 0xff)) * 3 + 128;
				}
				if(r > 255) r = 255; else if(r < 0) r = 0;
				if(g > 255) g = 255; else if(g < 0) g = 0;
				if(b > 255) b = 255; else if(b < 0) b = 0;
				dst[j] = (r << 16) | (g << 8) | b;
			}
			cur += wext;
			cur_U += w;
			cur_V += w;
			prev += wext;
			prev_U += w;
			prev_V += w;
			dst = (Pixel32*)((BYTE*)dst + p);
			src = (Pixel32*)((BYTE*)src + p);
			RGB_MC += w;
		}
	}
	if( mfd->show_vectors )
	{
		dst = (Pixel32*)fa->dst.data;
		for( i = 0; i < mfd->num_blocks_vert; i++ )
		{
			for( j = 0; j < mfd->num_blocks_hor; j++ )
			{
				mvector = mfd->MVectors[ i * mfd->num_blocks_hor + j ];
				if(!mvector.splitted)
				{
					DrawLine(dst, w, p, (j << 4) + 8, ((i << 4) + 8), (j << 4) + mvector.x + 8, ((i << 4) + mvector.y + 8) );
				}
				else
				{
					for(int h = 0; h < 4; h++)
					{
						DrawLine(dst, w, p, (j << 4) + 8 + ((h & 1)? 4 : -4), ((i << 4) + 8 + ((h > 1)? 4 : -4)), (j << 4) + mvector.sub[h].x + 8 + ((h & 1)? 4 : -4), ((i << 4) + mvector.sub[h].y + 8 + ((h > 1)? 4 : -4)) );
					}
				}
			}
		}
	}
}

int SpatialNoiseLevel(const FilterActivation *fa, int _step, int _threshold, double *D_noise)
{
	BYTE *CurrMetering, *b_point;
	int src_pitch = fa->src.pitch , src_modulo = fa->src.modulo , next = src_pitch-7, b_step = 4*_step;
	int X_num_metering = fa->src.w/_step, Y_num_metering = fa->src.h/_step;
	int num_of_metering = X_num_metering*Y_num_metering, real_nop=num_of_metering;
	int sumR,sumG,sumB, R,G,B, r,g,b, count, semaphore;
	double temp;
	
	//////////////////////////////////////////////////////////////
	//

	CurrMetering = (BYTE*)fa->src.data + (_step/2-2)*src_pitch + _step*2;

	*D_noise = 0;
	count = semaphore = 0;
	while(count < num_of_metering)
	{
		sumR = sumG = sumB = 0;
		
		b_point = CurrMetering;
		sumR += *b_point++;	sumG += *b_point++;	sumB += *b_point++;
		////////////////////////////////////////////
		b_point += next;
		R = *b_point++;		G = *b_point++;		B = *b_point++;

		b_point++;
		r = *b_point++;		g = *b_point++;		b = *b_point++;
		if( (R-r<-_threshold) || (R-r>_threshold) || (G-g<-_threshold) || (G-g>_threshold) || (B-b<-_threshold) || (B-b>_threshold) || 
			(sumR-r<-_threshold) || (sumR-r>_threshold) || (sumG-g<-_threshold) || (sumG-g>_threshold) || (sumB-b<-_threshold) || (sumB-b>_threshold)	)
		{
			real_nop--;
			goto lable;
		}
		sumR += R+r*2;	sumG += G+g*2;	sumB += B+b*2;

		b_point++;
		R = *b_point++;	G = *b_point++;	B = *b_point++;
		if( (R-r<-_threshold) || (R-r>_threshold) || (G-g<-_threshold) || (G-g>_threshold) || (B-b<-_threshold) || (B-b>_threshold) )
		{
			real_nop--;
			goto lable;
		}
		sumR += R;	sumG += G;	sumB += B;
		////////////////////////////////////////////
		b_point += next;
		R = *b_point++;	G = *b_point++;	B = *b_point++;
		if( (R-r<-_threshold) || (R-r>_threshold) || (G-g<-_threshold) || (G-g>_threshold) || (B-b<-_threshold) || (B-b>_threshold) )
		{
			real_nop--;
			goto lable;
		}
		sumR += R;	sumG += G;	sumB += B;



		temp = 0.299*(sumR - r*6) + 0.587*(sumG - g*6) + 0.114*(sumB - b*6);
		*D_noise += temp*temp;
		
		
lable:	
		CurrMetering += b_step;
		count++;	semaphore++;
		if(semaphore==X_num_metering)
		{
			semaphore = 0;
			CurrMetering = (BYTE*)CurrMetering + src_pitch*(_step-1) + src_modulo;
		}	
	
	}

	//
	//////////////////////////////////////////////////////////////
	//

	*D_noise /= 36*(real_nop-1);
	
	
	//
	//////////////////////////////////////////////////////////////

	
	return (100*real_nop/num_of_metering);
}

void LogMotions(char *filename, MV *vectors, int count)
{
	int f;
	if((f = _open(filename, _O_WRONLY | _O_BINARY | _O_APPEND))==-1)
	{
		throw "Can't write motions to file!";
	}

	int i;
	for(int i = 0; i < count; i++)
	{
		_write(f, &(vectors[i]), sizeof(MV));
		/*if(vectors[i].splitted)
		{
			_write(f, (&vectors[i].sub[0]), sizeof(SubMV));
			_write(f, (&vectors[i].sub[1]), sizeof(SubMV));
			_write(f, (&vectors[i].sub[2]), sizeof(SubMV));
			_write(f, (&vectors[i].sub[3]), sizeof(SubMV));
		}*/
	}

	_close(f);
}

void ReadMotions(char *filename, int *offset, MV *vectors, int count)
{
	int f;
	if((f = _open(filename, _O_RDONLY | _O_BINARY))==-1)
	{
		throw "Can't read motions from file!";
	}

	_lseek(f, *offset, SEEK_SET);

	int i;
	for(int i = 0; i < count; i++)
	{
		_read(f, &(vectors[i]), sizeof(MV));
		/*if(vectors[i].splitted)
		{
      
			vectors[i].sub[0] = new MV;
			vectors[i].sub[1] = new MV;
			vectors[i].sub[2] = new MV;
			vectors[i].sub[3] = new MV;
			_read(f, (&vectors[i].sub[0]), sizeof(SubMV));
			_read(f, (&vectors[i].sub[1]), sizeof(SubMV));
			_read(f, (&vectors[i].sub[2]), sizeof(SubMV));
			_read(f, (&vectors[i].sub[3]), sizeof(SubMV));
		}*/
	}

	*offset = _tell(f);
	_close(f);
}

int runProc(const FilterActivation *fa, const FilterFunctions *ff)
{
	Pixel32 *src = (Pixel32*)fa->src.data, *dst = (Pixel32*)fa->dst.data;
	MFD *mfd = (MFD*) fa->filter_data;
	int i, j, block_id;
	MV mvector;
	BYTE temp, *prev = mfd->prev_Y, *cur = mfd->cur_Y, *prev_MC = mfd->prev_Y_MC;
	short *cur_U = mfd->cur_U, *cur_V = mfd->cur_V;
	short *prev_U = mfd->prev_U, *prev_V = mfd->prev_V;
	short *prev_U_MC = mfd->prev_U_MC, *prev_V_MC = mfd->prev_V_MC;
	int w = mfd->width, h = mfd->height, p = fa->src.pitch;
	int num_blocks_hor = (w + 15) >> 4;
	int wext = mfd->ext_w, hext = mfd->ext_h, sh_x, sh_y;
	int first_row_offset = BORDER * wext, after_last_row_offset = (BORDER + h) * wext;
	bool base_line, frame_with_even_lines;

	if((mfd->frame_no & 0xf) == 0)
	{
		SpatialNoiseLevel(fa, 4, 20, &mfd->noise);
		mfd->noise = sqrt(mfd->noise);
	}

	src = fa->src.data;
	dst = fa->dst.data;
	cur = cur + first_row_offset;
	for( i = 0; i < h; i++ )
	{
		//filling extra space to the left from picture
		memset( cur, GetYFromRGB( src[0] ), BORDER + 1);
		cur_U[0] = (int)(-0.147*((src[0]&0xff0000)>>16)
						-0.289*((src[0]&0xff00)>>8)
						+0.436*(src[0]&0xff) + 0.5);
		cur_V[0] = (int)(0.615*((src[0]&0xff0000)>>16)
						-0.515*((src[0]&0xff00)>>8)
						-0.100*(src[0]&0xff) + 0.5);
		dst[0] = src[0];
		for( j = 1; j < w - 1; j++ )
		{
			cur[ j + BORDER ] = GetYFromRGB( src[j] );
			cur_U[j] = (int)(-0.147*((src[j]&0xff0000)>>16)
							-0.289*((src[j]&0xff00)>>8)
							+0.436*(src[j]&0xff) + 0.5);
			cur_V[j] = (int)(0.615*((src[j]&0xff0000)>>16)
							-0.515*((src[j]&0xff00)>>8)
							-0.100*(src[j]&0xff) + 0.5);
			dst[j] = src[j];
		}
		//filling extra space to the right from picture
		memset(cur + BORDER + w - 1, GetYFromRGB(src[w - 1]), BORDER+1);
		cur_U[w - 1] = (int)(-0.147*((src[w - 1]&0xff0000)>>16)
						-0.289*((src[w - 1]&0xff00)>>8)
						+0.436*(src[w - 1]&0xff) + 0.5);
		cur_V[w - 1] = (int)(0.615*((src[w - 1]&0xff0000)>>16)
						-0.515*((src[w - 1]&0xff00)>>8)
						-0.100*(src[w - 1]&0xff) + 0.5);
		dst[w - 1] = src[w - 1];
		src = (Pixel32*)((BYTE*)src + p);
		dst = (Pixel32*)((BYTE*)dst + p);
		cur += wext;
		cur_U += w;
		cur_V += w;
	}

	//It's only left now to fulfill extra space above and below the picture
	BYTE *first_cur = mfd->cur_Y + first_row_offset;
	cur = mfd->cur_Y;
	for( i = 0; i < BORDER; i++ )
	{
		memcpy( cur, first_cur, wext );
		cur += wext; 
	}
	cur = mfd->cur_Y + after_last_row_offset;
	first_cur = mfd->cur_Y + after_last_row_offset - wext;
	for( i = 0; i < BORDER; i++ )
	{
		memcpy( cur, first_cur, wext );
		cur += wext; 
	}

	if(mfd->frame_no == 0)
	{
    //memset(mfd->LastMVectors, 0, sizeof(MV));
		memcpy(mfd->prev_Y, mfd->cur_Y, mfd->ext_size);
		memcpy(mfd->prev_U, mfd->cur_U, mfd->size * sizeof(short));
		memcpy(mfd->prev_V, mfd->cur_V, mfd->size * sizeof(short));
		for(i = 0; i < h; i++)
		{
			memcpy((BYTE*)mfd->prev_RGB + i * 4 * w, (BYTE*)fa->src.data + i * p, w * 4);
		}
		if(mfd->log_vectors && !mfd->read_motions)
		{
			_creat( mfd->log_vectors_file, _S_IREAD | _S_IWRITE);
		}
	}

	//halfpixel shift
	if(mfd->read_motions || mfd->use_half_pixel)
	{
		memcpy(mfd->prev_Y_l, mfd->prev_Y, mfd->ext_size);
		memcpy(mfd->prev_Y_up, mfd->prev_Y, mfd->ext_size);
		memcpy(mfd->prev_Y_upl, mfd->prev_Y, mfd->ext_size);

		HalfpixelShiftHorz( mfd->prev_Y_l, mfd->ext_w, mfd->ext_h, mfd->shift_up );
		HalfpixelShift( mfd->prev_Y_up, mfd->ext_w, mfd->ext_h, mfd->shift_up );
		HalfpixelShift( mfd->prev_Y_upl, mfd->ext_w, mfd->ext_h, mfd->shift_up );
		HalfpixelShiftHorz( mfd->prev_Y_upl, mfd->ext_w, mfd->ext_h, mfd->shift_up );

		memcpy(mfd->prev_U_l, mfd->prev_U, mfd->size*sizeof(short));
		memcpy(mfd->prev_U_up, mfd->prev_U, mfd->size*sizeof(short));
		memcpy(mfd->prev_U_upl, mfd->prev_U, mfd->size*sizeof(short));

		HalfpixelShiftHorz( mfd->prev_U_l, mfd->width, mfd->height, mfd->shift_up );
		HalfpixelShift( mfd->prev_U_up, mfd->width, mfd->height, mfd->shift_up );
		HalfpixelShift( mfd->prev_U_upl, mfd->width, mfd->height, mfd->shift_up );
		HalfpixelShiftHorz( mfd->prev_U_upl, mfd->width, mfd->height, mfd->shift_up );

		memcpy(mfd->prev_V_l, mfd->prev_V, mfd->size*sizeof(short));
		memcpy(mfd->prev_V_up, mfd->prev_V, mfd->size*sizeof(short));
		memcpy(mfd->prev_V_upl, mfd->prev_V, mfd->size*sizeof(short));

		HalfpixelShiftHorz( mfd->prev_V_l, mfd->width, mfd->height, mfd->shift_up );
		HalfpixelShift( mfd->prev_V_up, mfd->width, mfd->height, mfd->shift_up );
		HalfpixelShift( mfd->prev_V_upl, mfd->width, mfd->height, mfd->shift_up );
		HalfpixelShiftHorz( mfd->prev_V_upl, mfd->width, mfd->height, mfd->shift_up );

		memcpy(mfd->prev_RGB_l, mfd->prev_RGB, mfd->size*sizeof(Pixel32));
		memcpy(mfd->prev_RGB_upl, mfd->prev_RGB, mfd->size*sizeof(Pixel32));
		memcpy(mfd->prev_RGB_up, mfd->prev_RGB, mfd->size*sizeof(Pixel32));

		HalfpixelShiftHorz( mfd->prev_RGB_l, mfd->width, mfd->height, mfd->shift_up );
		HalfpixelShift( mfd->prev_RGB_up, mfd->width, mfd->height, mfd->shift_up );
		HalfpixelShift( mfd->prev_RGB_upl, mfd->width, mfd->height, mfd->shift_up );
		HalfpixelShiftHorz( mfd->prev_RGB_upl, mfd->width, mfd->height, mfd->shift_up );
	}
	
	if(mfd->log_vectors && mfd->read_motions)
	{
		ReadMotions(mfd->log_vectors_file, &mfd->offset, mfd->MVectors, mfd->num_blocks_hor * mfd->num_blocks_vert);
	}
	else
	{
		//here all is ready to call motion estimation algorithm
		MotionEstimate(fa, ff);
	}

	if(mfd->log_vectors && !mfd->read_motions)
	{
		LogMotions(mfd->log_vectors_file, mfd->MVectors, mfd->num_blocks_hor * mfd->num_blocks_vert);
	}

	//constructing MC frames
	cur = mfd->cur_Y + first_row_offset + BORDER;
	cur_U = mfd->cur_U;
	cur_V = mfd->cur_V;
	Pixel32* rgb_MC = mfd->output_buf, *prev_RGB;
	for( i = 0; i < h; i++ )
	{
		for( j = 0; j < w; j++ )
		{
			mvector = mfd->MVectors[ ( i >> 4 ) * num_blocks_hor + ( j >> 4 ) ];
			if(mvector.splitted)
			{
				block_id = (((i % 16) > 7)? 0x2 : 0x0) + (((j % 16) > 7)? 0x1 : 0x0);
				mvector.x = mvector.sub[block_id].x;
        mvector.y = mvector.sub[block_id].y;
        mvector.dir = mvector.sub[block_id].dir;
        mvector.error = mvector.sub[block_id].error;
			}
			switch(mvector.dir)
			{
				case sd_none:
					prev = mfd->prev_Y + first_row_offset + BORDER + i * wext + j;
					prev_U = mfd->prev_U + i * w + j;
					prev_V = mfd->prev_V + i * w + j;
					prev_RGB = mfd->prev_RGB + i * w + j;
					break;
				case sd_up:
					prev = mfd->prev_Y_up + first_row_offset + BORDER + i * wext + j;
					prev_U = mfd->prev_U_up + i * w + j;
					prev_V = mfd->prev_V_up + i * w + j;
					prev_RGB = mfd->prev_RGB_up + i * w + j;
					break;
				case sd_l:
					prev = mfd->prev_Y_l + first_row_offset + BORDER + i * wext + j;
					prev_U = mfd->prev_U_l + i * w + j;
					prev_V = mfd->prev_V_l + i * w + j;
					prev_RGB = mfd->prev_RGB_l + i * w + j;
					break;
				case sd_upl:
					prev = mfd->prev_Y_upl + first_row_offset + BORDER + i * wext + j;
					prev_U = mfd->prev_U_upl + i * w + j;
					prev_V = mfd->prev_V_upl + i * w + j;
					prev_RGB = mfd->prev_RGB_upl + i * w + j;
					break;
			}
			if(j + mvector.x >= 0 && j + mvector.x <= w - 1)
				sh_x = mvector.x;
			else if(j + mvector.x < 0)
				sh_x = -j;
			else
				sh_x = w - 1 - j; 
			if(mvector.y + i >= 0 && mvector.y + i <= h - 1)
				sh_y = mvector.y;
			else if(mvector.y + i < 0)
				sh_y = -i;
			else
				sh_y = h - 1 - i; 
			prev_MC[j] = prev[sh_y * wext + sh_x];
			prev_U_MC[j] = prev_U[sh_y * w + sh_x];
			prev_V_MC[j] = prev_V[sh_y * w + sh_x];
			rgb_MC[j] = prev_RGB[sh_y * w + sh_x];
		}
		prev_MC += w;
		prev_U_MC += w;
		prev_V_MC += w;
		rgb_MC += w;
	}
	
	if( !mfd->show_nothing )
	{
		DrawOutput( fa, mfd->MVectors );
	}

	if( mfd->log_need )
	{
		Log(fa, mfd->MVectors);
	}

	//preparing to the next frame
	memcpy(mfd->prev_Y, mfd->cur_Y, mfd->ext_size);
	memcpy(mfd->prev_U, mfd->cur_U, mfd->size * sizeof(short));
	memcpy(mfd->prev_V, mfd->cur_V, mfd->size * sizeof(short));
	for(i = 0; i < h; i++)
	{
		memcpy((BYTE*)mfd->prev_RGB + i * 4 * w, (BYTE*)fa->src.data + i * p, w * 4);
	}
  MV* tmp = mfd->MVectors;
  mfd->MVectors = mfd->LastMVectors;
  mfd->LastMVectors = tmp;

	mfd->frame_no++;
	return 0;
}

BOOL CALLBACK ConfigDlgProc(HWND hdlg, UINT msg, WPARAM wParam, LPARAM lParam) {
    MFD *mfd = (MFD *)GetWindowLong(hdlg, DWL_USER);
	static bool sem = false;
	int temp;

    switch(msg) {
        case WM_INITDIALOG:
            SetWindowLong(hdlg, DWL_USER, lParam);
            mfd = (MFD *)lParam;
			SendMessage( GetDlgItem( hdlg, IDC_QUALITYSLIDER ), TBM_SETRANGEMIN, true, 0);
			SendMessage( GetDlgItem( hdlg, IDC_QUALITYSLIDER ), TBM_SETRANGEMAX, true, 100);
			SendMessage( GetDlgItem( hdlg, IDC_QUALITYSLIDER ), TBM_SETLINESIZE, 0, 1);
			SendMessage( GetDlgItem( hdlg, IDC_QUALITYSLIDER ), TBM_SETPAGESIZE, 0, 5);
			SendMessage( GetDlgItem( hdlg, IDC_QUALITYSLIDER ), TBM_SETTICFREQ, 5, 0);
			SendMessage( GetDlgItem( hdlg, IDC_QUALITYSPIN ), UDM_SETRANGE32, 0, 100);
			CheckRadioButton( hdlg, IDC_SHOWSOURCE, IDC_SHOWMC, mfd->show_res_after?IDC_SHOWRESAFTER:(mfd->show_MC?IDC_SHOWMC:(mfd->show_res_before?IDC_SHOWRESBEFORE:IDC_SHOWSOURCE)));
			CheckDlgButton( hdlg, IDC_SHOWMV, mfd->show_vectors?BST_CHECKED:BST_UNCHECKED);
			CheckDlgButton( hdlg, IDC_RGB, mfd->work_in_RGB?BST_CHECKED:BST_UNCHECKED);
			CheckDlgButton( hdlg, IDC_DRAWNOTHING, mfd->show_nothing?BST_CHECKED:BST_UNCHECKED);
			CheckDlgButton( hdlg, IDC_LOGNEED, mfd->log_need?BST_CHECKED:BST_UNCHECKED);
			CheckDlgButton( hdlg, IDC_HALFPIXEL, mfd->use_half_pixel?BST_CHECKED:BST_UNCHECKED);
			SetDlgItemInt( hdlg, IDC_QUALITY, mfd->quality, false );
			SetDlgItemText( hdlg, IDC_LOGFILENAME, (LPCSTR)mfd->log_file );
			CheckDlgButton( hdlg, IDC_LOG_VECTORS, mfd->log_vectors?BST_CHECKED:BST_UNCHECKED);
			SetDlgItemText( hdlg, IDC_LOG_VECTORS_FILE, (LPCSTR)mfd->log_vectors_file );
			CheckDlgButton( hdlg, IDC_LOG_READ, mfd->read_motions?BST_CHECKED:BST_UNCHECKED);
            return TRUE;

        case WM_COMMAND:
            switch(LOWORD(wParam)) {
            case IDOK:
				mfd->show_res_after = !!IsDlgButtonChecked( hdlg, IDC_SHOWRESAFTER );
				mfd->show_res_before = !!IsDlgButtonChecked( hdlg, IDC_SHOWRESBEFORE );
				mfd->show_MC = !!IsDlgButtonChecked( hdlg, IDC_SHOWMC );
				mfd->show_source = !!IsDlgButtonChecked( hdlg, IDC_SHOWSOURCE );
				mfd->show_vectors = !!IsDlgButtonChecked( hdlg, IDC_SHOWMV );
				mfd->quality = GetDlgItemInt(hdlg, IDC_QUALITY, NULL, false);
				mfd->work_in_RGB = !!IsDlgButtonChecked( hdlg, IDC_RGB );
				mfd->log_need = !!IsDlgButtonChecked( hdlg, IDC_LOGNEED );
				mfd->show_nothing = !!IsDlgButtonChecked( hdlg, IDC_DRAWNOTHING );
				mfd->use_half_pixel = !!IsDlgButtonChecked( hdlg, IDC_HALFPIXEL );
				GetDlgItemText(hdlg, IDC_LOGFILENAME, mfd->log_file, 50);
				mfd->log_vectors = !!IsDlgButtonChecked( hdlg, IDC_LOG_VECTORS );
				GetDlgItemText(hdlg, IDC_LOG_VECTORS_FILE, mfd->log_vectors_file, 50);
				mfd->read_motions = !!IsDlgButtonChecked( hdlg, IDC_LOG_READ );
                EndDialog(hdlg, 0);
                return TRUE;
            case IDCANCEL:
                EndDialog(hdlg, 1);
                return FALSE;
			case IDC_QUALITY:
				if(HIWORD(wParam) == EN_UPDATE)
				{
					temp = GetDlgItemInt( hdlg, IDC_QUALITY, NULL, false);
					if( temp > 100 )
					{
						temp = 100;
						SetDlgItemInt( hdlg, IDC_QUALITY, temp, false);
					}
					else if( temp < 0 )
					{
						temp = 0;
						SetDlgItemInt( hdlg, IDC_QUALITY, temp, false);
					}
					sem = true;
					SendMessage( GetDlgItem( hdlg, IDC_QUALITYSLIDER), TBM_SETPOS, true, GetDlgItemInt( hdlg, IDC_QUALITY, NULL, false) );
					sem = false;
				}
				return TRUE;
			default:
				return TRUE;
			}
            break;
		case WM_NOTIFY:
			if( LOWORD(wParam) == IDC_QUALITYSLIDER )
			{
				if( !sem )
					SetDlgItemInt( hdlg, IDC_QUALITY, SendMessage( GetDlgItem( hdlg, IDC_QUALITYSLIDER), TBM_GETPOS, 0, 0 ), false );
			}
			break;
    }

    return FALSE;
}

int configProc(FilterActivation *fa, const FilterFunctions *ff, HWND hwnd) {
	return DialogBoxParam(fa->filter->module->hInstModule,
            MAKEINTRESOURCE(IDDIALOG), hwnd,
            ConfigDlgProc, (LPARAM)fa->filter_data);
}

void stringProc(const FilterActivation *fa, const FilterFunctions *ff, char *str) 
{
    MFD *mfd = (MFD *)fa->filter_data;
	sprintf( str, "" );
}

long paramProc(FilterActivation *fa, const FilterFunctions *ff)
{
	return FILTERPARAM_NEEDS_LAST | FILTERPARAM_SWAP_BUFFERS;
}

int endProc(FilterActivation *fa, const FilterFunctions *ff)
{
	MFD* mfd = (MFD*)fa->filter_data;
	MEEnd();
	delete mfd->prev_Y;
	delete mfd->cur_Y;
	delete mfd->prev_Y_MC;
	delete mfd->MVectors;
  delete mfd->LastMVectors;
  //delete mfd->BlockSums;
  //delete mfd->LastBlockSums;
	return 0;
}


//job support
bool fssProc(FilterActivation *fa, const FilterFunctions *ff, char *buf, int buflen) 
{
    MFD *mfd = (MFD *)fa->filter_data;

    _snprintf(buf, buflen, "Config(%d, %d, %d, %d, \"%s\", %d, %d, %d, %d, %d, %d, %d, \"%s\", %d)", 
								mfd->quality,
								mfd->use_half_pixel,
								mfd->show_nothing,
								mfd->log_need,
								mfd->log_file,
								mfd->work_in_RGB,
								mfd->show_vectors,
								mfd->show_source,
								mfd->show_MC,
								mfd->show_res_before,
								mfd->show_res_after,
								mfd->log_vectors,
								mfd->log_vectors_file,
								mfd->read_motions
								);
    return true;
}

void scriptConfig(IScriptInterpreter *isi, void *lpVoid, CScriptValue *argv, int argc) 
{
    FilterActivation *fa = (FilterActivation *)lpVoid;
    MFD *mfd = (MFD *)fa->filter_data;

	mfd->quality = argv[0].asInt();
	mfd->use_half_pixel = !!(argv[1].asInt());
	mfd->show_nothing = !!(argv[2].asInt());
	mfd->log_need = !!(argv[3].asInt());
	strcpy(mfd->log_file, *argv[4].asString());
	mfd->work_in_RGB = !!(argv[5].asInt());
	mfd->show_vectors = !!(argv[6].asInt());
	mfd->show_source = !!(argv[7].asInt());
	mfd->show_MC = !!(argv[8].asInt());
	mfd->show_res_before = !!(argv[9].asInt());
	mfd->show_res_after = !!(argv[10].asInt());
	mfd->log_vectors = !!(argv[11].asInt());
	strcpy(mfd->log_vectors_file, *argv[12].asString());
	mfd->read_motions = !!(argv[13].asInt());
}

ScriptFunctionDef script_func_defs[]={
    { (ScriptFunctionPtr)scriptConfig, "Config", "0iiiisiiiiiiisi" },
    { NULL },
};

CScriptObject script_obj={
    NULL, script_func_defs
};

struct FilterDefinition filterDef = {

    NULL, NULL, NULL,				// next, prev, module

#ifdef SPLITTING
    "ME_Fokin8x8",				// name
#else
    "ME_Fokin16x16",				// name
#endif
    "ME task filter",				// desc
    "Alexander Fokin",			// maker
    NULL,							// private_data
    sizeof(MFD),                    // inst_data_size

    initProc,						// initProc
    NULL,							// deinitProc
    runProc,						// runProc
    paramProc,						// paramProc
    configProc,						// configProc
    stringProc,						// stringProc
    startProc,						// startProc
    endProc,						// endProc

    &script_obj,                    // script_obj
    fssProc,                        // fssProc
};

int initProc(FilterActivation *fa, const FilterFunctions *ff)
{
	MFD *mfd = (MFD*)fa->filter_data;
	mfd->show_res_after = true;
	mfd->show_vectors = false;
	mfd->show_nothing = false;
	mfd->show_res_before = false;
	mfd->show_source = false;
	mfd->show_MC = false;
	mfd->work_in_RGB = false;
	mfd->quality = 100;
	mfd->use_half_pixel = false;
	mfd->method = den;
	mfd->log_need = false;
	strcat(strcpy(mfd->log_file, filterDef.name), ".txt");
	mfd->log_need = false;
	strcat(strcpy(mfd->log_vectors_file, filterDef.name), ".mv");
	mfd->read_motions = false;
	mfd->offset = 0;
	return 0;
}

extern "C" int __declspec(dllexport) __cdecl VirtualdubFilterModuleInit2(FilterModule *fm, const FilterFunctions *ff, int& vdfd_ver, int& vdfd_compat);
extern "C" void __declspec(dllexport) __cdecl VirtualdubFilterModuleDeinit(FilterModule *fm, const FilterFunctions *ff);


int __declspec(dllexport) __cdecl VirtualdubFilterModuleInit2(FilterModule *fm, const FilterFunctions *ff, int& vdfd_ver, int& vdfd_compat) {
    if (!(fd = ff->addFilter(fm, &filterDef, sizeof(FilterDefinition))))
        return 1;

    vdfd_ver    = VIRTUALDUB_FILTERDEF_VERSION;
    vdfd_compat = VIRTUALDUB_FILTERDEF_COMPATIBLE;

    return 0;
}

void __declspec(dllexport) __cdecl VirtualdubFilterModuleDeinit(FilterModule *fm, const FilterFunctions *ff) {
    ff->removeFilter(fd);
}
