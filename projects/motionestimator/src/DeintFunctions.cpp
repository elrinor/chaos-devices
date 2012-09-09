#ifndef _DEINT_H_
#define _DEINT_H_
#include "types.h"

void HalfpixelShift(BYTE *field, int width, int height, bool shift_up)
{
	int i, j, width2 = width * 2, temp;
	BYTE *new_field = new BYTE[width*height];
	BYTE *old_ptr = field, *new_ptr = new_field;
	old_ptr += width;
	for( j = 0; j < width; j++ )
	{
		temp = (old_ptr[j-width] + old_ptr[j])>>1;
		if( temp < 255 )
			if( temp >= 0);
			else
				temp = 0;
		else
			temp = 255;
		new_ptr[j] = temp;
	}
	old_ptr += width;
	new_ptr += width;
	for( i = 2; i < height - 1; i++ )
	{
		for( j = 0; j < width; j++ )
		{
			temp = (5*(old_ptr[j-width] + old_ptr[j]) - 
							(old_ptr[j-width2] + old_ptr[j+width]))>>3;
			if( temp < 255 )
				if( temp >= 0);
				else
					temp = 0;
			else
				temp = 255;
			new_ptr[j] = temp;
		}
		old_ptr += width;
		new_ptr += width;
	}
	for( j = 0; j < width; j++ )
	{
		temp = (old_ptr[j-width] + old_ptr[j])>>1;
		if( temp < 255 )
			if( temp >= 0);
			else
				temp = 0;
		else
			temp = 255;
		new_ptr[j] = temp;
	}
	if( shift_up )
	{
		temp = width;
	}
	else
	{
		temp = 0;
	}
	memcpy( (void*)(field + temp), (void*)new_field, width * ( height - 1 ) * sizeof(BYTE) );
	delete new_field;
}

void HalfpixelShiftHorz(BYTE *field, int width, int height, bool shift_right)
{
	int i, j, width2 = width * 2, temp;
	BYTE *new_field = new BYTE[width*height];
	BYTE *old_ptr = field, *new_ptr = new_field;

	old_ptr += 1;
	for( j = 0; j < height; j++ )
	{
		temp = (old_ptr[j*width - 1] + old_ptr[j*width])>>1;
		if( temp < 255 )
			if( temp >= 0);
			else
				temp = 0;
		else
			temp = 255;
		new_ptr[j*width] = temp;
	}
	old_ptr += 1;
	new_ptr += 1;
	for( i = 2; i < width - 1; i++ )
	{
		for( j = 0; j < height; j++ )
		{
			temp = (5*(old_ptr[j*width-1] + old_ptr[j*width]) - 
							(old_ptr[j*width-2] + old_ptr[j*width+1]))>>3;
			if( temp < 255 )
				if( temp >= 0);
				else
					temp = 0;
			else
				temp = 255;
			new_ptr[j*width] = temp;
		}
		old_ptr += 1;
		new_ptr += 1;
	}
	for( j = 0; j < height; j++ )
	{
		temp = (old_ptr[j*width-1] + old_ptr[j*width])>>1;
		if( temp < 255 )
			if( temp >= 0);
			else
				temp = 0;
		else
			temp = 255;
		new_ptr[j*width] = temp;
	}
	if( shift_right )
	{
		new_ptr = new_field;
		old_ptr = field;
		for( j = 0; j < height; j++ )
		{
			new_ptr[j*width] = old_ptr[j*width];
		}
	}
	else
	{
		new_ptr += 1;
		for( j = 0; j < height; j++ )
		{
			new_ptr[j*width] = old_ptr[j*width];
		}
	}
	memcpy( field, (void*)new_field, width * height * sizeof(BYTE) );
	delete new_field;
}

void HalfpixelShift(Pixel32 *field, int width, int height, bool shift_up)
{
	int i, j, width2 = width * 2, temp;
	int r, g, b;
	Pixel32 *new_field = new Pixel32[width*height];
	Pixel32 *old_ptr = field, *new_ptr = new_field;
	old_ptr += width;
	for( j = 0; j < width; j++ )
	{
		r = (((old_ptr[j-width] & 0xff0000) + (old_ptr[j] & 0xff0000))>>17);
		g = (((old_ptr[j-width] & 0xff00) + (old_ptr[j] & 0xff00))>>9);
		b = (((old_ptr[j-width] & 0xff) + (old_ptr[j] & 0xff))>>1);
		if(r > 255) r = 255;
		if(g > 255) g = 255;
		if(b > 255) b = 255;
		if(r < 0) r = 0;
		if(g < 0) g = 0;
		if(b < 0) b = 0;
		new_ptr[j] = (r<<16) | (g<<8) | b;
	}
	old_ptr += width;
	new_ptr += width;
	for( i = 2; i < height - 1; i++ )
	{
		for( j = 0; j < width; j++ )
		{
			r = (((old_ptr[j-width] & 0xff0000) + (old_ptr[j] & 0xff0000))>>16)*5;
			g = (((old_ptr[j-width] & 0xff00) + (old_ptr[j] & 0xff00))>>8)*5;
			b = ((old_ptr[j-width] & 0xff) + (old_ptr[j] & 0xff))*5;
			r -= (((old_ptr[j-width2] & 0xff0000) + (old_ptr[j+width] & 0xff0000))>>16);
			g -= (((old_ptr[j-width2] & 0xff00) + (old_ptr[j+width] & 0xff00))>>8);
			b -= ((old_ptr[j-width2] & 0xff) + (old_ptr[j+width] & 0xff));
			//temp = (5*(old_ptr[j-width] + old_ptr[j]) - 
			//				(old_ptr[j-width2] + old_ptr[j+width]))>>3;
			r >>= 3;
			g >>= 3;
			b >>= 3;
			if(r > 255) r = 255;
			if(g > 255) g = 255;
			if(b > 255) b = 255;
			if(r < 0) r = 0;
			if(g < 0) g = 0;
			if(b < 0) b = 0;
			new_ptr[j] = (r<<16) | (g<<8) | b;
		}
		old_ptr += width;
		new_ptr += width;
	}
	for( j = 0; j < width; j++ )
	{
		r = (((old_ptr[j-width] & 0xff0000) + (old_ptr[j] & 0xff0000))>>17);
		g = (((old_ptr[j-width] & 0xff00) + (old_ptr[j] & 0xff00))>>9);
		b = (((old_ptr[j-width] & 0xff) + (old_ptr[j] & 0xff))>>1);
		if(r > 255) r = 255;
		if(g > 255) g = 255;
		if(b > 255) b = 255;
		if(r < 0) r = 0;
		if(g < 0) g = 0;
		if(b < 0) b = 0;
		new_ptr[j] = (r<<16) | (g<<8) | b;
	}
	if( shift_up )
	{
		temp = width * 4;
	}
	else
	{
		temp = 0;
	}
	memcpy( (void*)(field + temp), (void*)new_field, width * ( height - 1 ) * sizeof(Pixel32) );
	delete new_field;
}


void HalfpixelShiftHorz(Pixel32 *field, int width, int height, bool shift_right)
{
	int i, j, width2 = width * 2, temp;
	int r, g, b;
	Pixel32 *new_field = new Pixel32[width*height];
	Pixel32 *old_ptr = field, *new_ptr = new_field;

	old_ptr += 1;
	for( j = 0; j < height; j++ )
	{
		r = (((old_ptr[j*width-1] & 0xff0000) + (old_ptr[j*width] & 0xff0000))>>17);
		g = (((old_ptr[j*width-1] & 0xff00) + (old_ptr[j*width] & 0xff00))>>9);
		b = (((old_ptr[j*width-1] & 0xff) + (old_ptr[j*width] & 0xff))>>1);
		if(r > 255) r = 255;
		if(g > 255) g = 255;
		if(b > 255) b = 255;
		if(r < 0) r = 0;
		if(g < 0) g = 0;
		if(b < 0) b = 0;
		new_ptr[j*width] = (r<<16) | (g<<8) | b;
	}
	old_ptr += 1;
	new_ptr += 1;
	for( i = 2; i < width - 1; i++ )
	{
		for( j = 0; j < height; j++ )
		{
			r = (((old_ptr[j*width-1] & 0xff0000) + (old_ptr[j*width] & 0xff0000))>>16)*5;
			g = (((old_ptr[j*width-1] & 0xff00) + (old_ptr[j*width] & 0xff00))>>8)*5;
			b = ((old_ptr[j*width-1] & 0xff) + (old_ptr[j*width] & 0xff))*5;
			r -= (((old_ptr[j*width-2] & 0xff0000) + (old_ptr[j*width+1] & 0xff0000))>>16);
			g -= (((old_ptr[j*width-2] & 0xff00) + (old_ptr[j*width+1] & 0xff00))>>8);
			b -= ((old_ptr[j*width-2] & 0xff) + (old_ptr[j*width+1] & 0xff));
			//temp = (5*(old_ptr[j-width] + old_ptr[j]) - 
			//				(old_ptr[j-width2] + old_ptr[j+width]))>>3;
			r >>= 3;
			g >>= 3;
			b >>= 3;
			if(r > 255) r = 255;
			if(g > 255) g = 255;
			if(b > 255) b = 255;
			if(r < 0) r = 0;
			if(g < 0) g = 0;
			if(b < 0) b = 0;
			new_ptr[j*width] = (r<<16) | (g<<8) | b;
		}
		old_ptr += 1;
		new_ptr += 1;
	}
	for( j = 0; j < height; j++ )
	{
		r = (((old_ptr[j*width-1] & 0xff0000) + (old_ptr[j*width] & 0xff0000))>>17);
		g = (((old_ptr[j*width-1] & 0xff00) + (old_ptr[j*width] & 0xff00))>>9);
		b = (((old_ptr[j*width-1] & 0xff) + (old_ptr[j*width] & 0xff))>>1);
		if(r > 255) r = 255;
		if(g > 255) g = 255;
		if(b > 255) b = 255;
		if(r < 0) r = 0;
		if(g < 0) g = 0;
		if(b < 0) b = 0;
		new_ptr[j*width] = (r<<16) | (g<<8) | b;
	}
	if( shift_right )
	{
		new_ptr = new_field;
		old_ptr = field;
		for( j = 0; j < height; j++ )
		{
			new_ptr[j*width] = old_ptr[j*width];
		}
	}
	else
	{
		new_ptr += 1;
		for( j = 0; j < height; j++ )
		{
			new_ptr[j*width] = old_ptr[j*width];
		}
	}
	memcpy( field, (void*)new_field, width * height * sizeof(Pixel32) );
	delete new_field;
}

void HalfpixelShift(short *field, int width, int height, bool shift_up)
{
	int i, j, width2 = width * 2, temp;
	short *new_field = new short[width*height];
	short *old_ptr = field, *new_ptr = new_field;
	old_ptr += width;
	for( j = 0; j < width; j++ )
	{
		temp = (old_ptr[j-width] + old_ptr[j])>>1;
		new_ptr[j] = temp;
	}
	old_ptr += width;
	new_ptr += width;
	for( i = 2; i < height - 1; i++ )
	{
		for( j = 0; j < width; j++ )
		{
			temp = (5*(old_ptr[j-width] + old_ptr[j]) - 
							(old_ptr[j-width2] + old_ptr[j+width]))>>3;
			new_ptr[j] = temp;
		}
		old_ptr += width;
		new_ptr += width;
	}
	for( j = 0; j < width; j++ )
	{
		temp = (old_ptr[j-width] + old_ptr[j])>>1;
		new_ptr[j] = temp;
	}
	if( shift_up )
	{
		temp = width;
	}
	else
	{
		temp = 0;
	}
	memcpy( (void*)(field + temp), (void*)new_field, width * ( height - 1 ) * sizeof(short) );
	delete new_field;
}

void HalfpixelShiftHorz(short *field, int width, int height, bool shift_right)
{
	int i, j, width2 = width * 2, temp;
	short *new_field = new short[width*height];
	short *old_ptr = field, *new_ptr = new_field;

	old_ptr += 1;
	for( j = 0; j < height; j++ )
	{
		temp = (old_ptr[j*width - 1] + old_ptr[j*width])>>1;
		new_ptr[j*width] = temp;
	}
	old_ptr += 1;
	new_ptr += 1;
	for( i = 2; i < width - 1; i++ )
	{
		for( j = 0; j < height; j++ )
		{
			temp = (5*(old_ptr[j*width-1] + old_ptr[j*width]) - 
							(old_ptr[j*width-2] + old_ptr[j*width+1]))>>3;
			new_ptr[j*width] = temp;
		}
		old_ptr += 1;
		new_ptr += 1;
	}
	for( j = 0; j < height; j++ )
	{
		temp = (old_ptr[j*width-1] + old_ptr[j*width])>>1;
		new_ptr[j*width] = temp;
	}
	if( shift_right )
	{
		new_ptr = new_field;
		old_ptr = field;
		for( j = 0; j < height; j++ )
		{
			new_ptr[j*width] = old_ptr[j*width];
		}
	}
	else
	{
		new_ptr += 1;
		for( j = 0; j < height; j++ )
		{
			new_ptr[j*width] = old_ptr[j*width];
		}
	}
	memcpy( field, (void*)new_field, width * height * sizeof(short) );
	delete new_field;
}

void Deinterlace(const FilterActivation *fa)
{
	MFD *mfd = (MFD*)fa->filter_data;
	int w = mfd->width, h = mfd->height, w2 = w * 2;
	int wext = w + 2*BORDER;
	int first_row_offset = BORDER * wext, after_last_row_offset = (BORDER + h) * wext;
	BYTE *prev = mfd->prev_Y_MC;
	short *prev_U = mfd->prev_U_MC;
	short *prev_V = mfd->prev_V_MC;
	BYTE *cur = mfd->cur_Y + first_row_offset + BORDER;
	BYTE *map_ = new BYTE[w * h];
	BYTE *map = map_;
	double diff;
	int p = fa->dst.pitch, p2 = p * 2;
	int num_blocks_vert = mfd->num_blocks_vert, num_blocks_hor = mfd->num_blocks_hor;
	int num_blocks_total = num_blocks_hor * num_blocks_vert;
	MV mvector;
	Pixel32 *dst = (Pixel32*)fa->dst.data, *dst_ptr = dst;
	Pixel32 *src = (Pixel32*)fa->src.data, *src_ptr = src;
	Pixel32* buf = mfd->output_buf;
	int i, j;
	short r, g, b;
	BYTE ym1, ym2, yp1, yp2, yp3, ym3;
	short um1, um2, um3, up1, up2, up3, vm1, vm2, vm3, vp1, vp2, vp3;
	short Y, U, V, y, u, v;
	short rm1, rm2, rm3, rp1, rp2, rp3, gm1, gm2, gm3, gp1, gp2, gp3, bm1, bm2, bm3, bp1, bp2, bp3;
	//interpolation coefficients
	double k1 = 4;
	double k2 = -1;
	double ksum = (k1 + k2)*2;
	//measure coefficients
	double km1 = 4;
	double km2 = -1;
	double kmsum = (km1 + km2)*2;
	double ycur, ypred, ynext, ucur, upred, unext, vcur, vpred, vnext;
	double rcur, rpred, rnext, gcur, gpred, gnext, bcur, bpred, bnext;
	double yinterp, uinterp, vinterp;
	double rinterp, ginterp, binterp;
	double measure;
	double def_treshold = (100 - mfd->quality) * 3.5, treshold;
	bool work_RGB = mfd->work_in_RGB;
	int w3 = w * 3;
	bool base_line = false;
	bool halfpel_shift;
	double noise_16x8 = mfd->noise * 16 * 8;

	for(i = 0; i < h; i++)
	{
		for(j = 0; j < w; j++)
		{

			if(!work_RGB)
			{
				r = prev[j] + 1.14 * prev_V[j];
				g = prev[j] - 0.395 * prev_U[j] - 0.581 * prev_V[j];
				b = prev[j] + 2.032 * prev_U[j];
				if(r > 255) r = 255; else if(r < 0) r = 0;
				if(g > 255) g = 255; else if(g < 0) g = 0;
				if(b > 255) b = 255; else if(b < 0) b = 0;
				dst_ptr[j] = (r << 16) + (g << 8) + b;
			}
			else
			{
				dst_ptr[j] = buf[j];
			}
		}
		dst_ptr = (Pixel32*)((BYTE*)dst_ptr + p);
		buf += w;
		prev += w;
		prev_U += w;
		prev_V += w;

	}

	//Creating interpolation mask
	map = map_;
	memset(map, 0, w * h);
	prev = mfd->prev_Y_MC;
	prev_U = mfd->prev_U_MC;
	prev_V = mfd->prev_V_MC;
	dst_ptr = dst;
	buf = mfd->output_buf;
	for(i = 0; i < h; i++)
	{
		if (!base_line)
		{
			y = prev[0];
			u = prev_U[0];
			v = prev_V[0];
			if(i > 0)
			{
				ym1 = prev[-w];
				um1 = prev_U[-w];
				vm1 = prev_V[-w];
			}
			else
			{
				ym1 = prev[w];
				um1 = prev_U[w];
				vm1 = prev_V[w];
			}
			if(i < h - 1)
			{
				yp1 = prev[w];
				up1 = prev_U[w];
				vp1 = prev_V[w];
			}
			else
			{
				yp1 = ym1;
				up1 = um1;
				vp1 = vm1;
			}
			if(i > 1)
			{
				ym2 = prev[-w2];
				um2 = prev_U[-w2];
				vm2 = prev_V[-w2];
			}
			else
			{
				ym2 = prev[w2];
				um2 = prev_U[w2];
				vm2 = prev_V[w2];
			}
			if(i < h - 2)
			{
				yp2 = prev[w2];
				up2 = prev_U[w2];
				vp2 = prev_V[w2];
			}
			else
			{
				yp2 = ym2;
				up2 = um2;
				vp2 = vm2;
			}
			ycur = fabs( y * kmsum - ((ym1 + yp1) * km1 + (ym2 + yp2) * km2) ) / kmsum;
			ucur = fabs( u * kmsum - ((um1 + up1) * km1 + (um2 + up2) * km2) ) / kmsum;
			vcur = fabs( v * kmsum - ((vm1 + vp1) * km1 + (vm2 + vp2) * km2) ) / kmsum;
			if(i > 2)
			{
				ym3 = prev[-w3];
				um3 = prev_U[-w3];
				vm3 = prev_V[-w3];
			}
			else
			{
				ym3 = prev[w3];
				um3 = prev_U[w3];
				vm3 = prev_V[w3];
			}
			if(i < h - 3)
			{
				yp3 = prev[w3];
				up3 = prev_U[w3];
				vp3 = prev_V[w3];
			}
			else
			{
				yp3 = ym3;
				up3 = um3;
				vp3 = vm3;
			}
			yinterp = ( (ym1 + yp1) * k1 + (ym3 + yp3) * k2 ) / ksum;
			uinterp = ( (um1 + up1) * k1 + (um3 + up3) * k2 ) / ksum;
			vinterp = ( (vm1 + vp1) * k1 + (vm3 + vp3) * k2 ) / ksum;
			if(work_RGB)
			{
				r = (buf[0] & 0xff0000)>>16;
				g = (buf[0] & 0xff00)>>8;
				b = (buf[0] & 0xff);
				if(i > 0)
				{
					rm1 = (buf[-w] & 0xff0000)>>16;
					gm1 = (buf[-w] & 0xff00)>>8;
					bm1 = (buf[-w] & 0xff);
				}
				else
				{
					rm1 = (buf[w] & 0xff0000)>>16;
					gm1 = (buf[w] & 0xff00)>>8;
					bm1 = (buf[w] & 0xff);
				}

				if(i < h - 1)
				{
					rp1 = (buf[w] & 0xff0000)>>16;
					gp1 = (buf[w] & 0xff00)>>8;
					bp1 = (buf[w] & 0xff);
				}
				else
				{
					rp1 = rm1;
					gp1 = gm1;
					bp1 = bm1;
				}

				if(i > 1)
				{
					rm2 = (buf[-w2] & 0xff0000)>>16;
					gm2 = (buf[-w2] & 0xff00)>>8;
					bm2 = (buf[-w2] & 0xff);
				}
				else
				{
					rm2 = (buf[w2] & 0xff0000)>>16;
					gm2 = (buf[w2] & 0xff00)>>8;
					bm2 = (buf[w2] & 0xff);
				}

				if(i < h - 2)
				{
					rp2 = (buf[w2] & 0xff0000)>>16;
					gp2 = (buf[w2] & 0xff00)>>8;
					bp2 = (buf[w2] & 0xff);
				}
				else
				{
					rp2 = rm2;
					gp2 = gm2;
					bp2 = bm2;
				}
				rcur = fabs( r * kmsum - ((rm1 + rp1) * km1 + (rm2 + rp2) * km2) ) / kmsum;
				gcur = fabs( g * kmsum - ((gm1 + gp1) * km1 + (gm2 + gp2) * km2) ) / kmsum;
				bcur = fabs( b * kmsum - ((bm1 + bp1) * km1 + (bm2 + bp2) * km2) ) / kmsum;
				if(i > 2)
				{
					rm3 = (buf[-w3] & 0xff0000)>>16;
					gm3 = (buf[-w3] & 0xff00)>>8;
					bm3 = (buf[-w3] & 0xff);
				}
				else
				{
					rm3 = (buf[w3] & 0xff0000)>>16;
					gm3 = (buf[w3] & 0xff00)>>8;
					bm3 = (buf[w3] & 0xff);
				}
				if(i < h - 3)
				{
					rp3 = (buf[w3] & 0xff0000)>>16;
					gp3 = (buf[w3] & 0xff00)>>8;
					bp3 = (buf[w3] & 0xff);
				}
				else
				{
					rp3 = rm3;
					gp3 = gm3;
					bp3 = bm3;
				}
				rinterp = ( (rm1 + rp1) * k1 + (rm3 + rp3) * k2 ) / ksum;
				ginterp = ( (gm1 + gp1) * k1 + (gm3 + gp3) * k2 ) / ksum;
				binterp = ( (bm1 + bp1) * k1 + (bm3 + bp3) * k2 ) / ksum;
			}
			for(j = 0; j < w; j++)
			{
				if(i != w - 1)
				{
					y = prev[j + 1];
					u = prev_U[j + 1];
					v = prev_V[j + 1];

					if(i > 0)
					{
						ym1 = prev[j + 1 - w];
						um1 = prev_U[j + 1 - w];
						vm1 = prev_V[j + 1 - w];
					}
					else
					{
						ym1 = prev[j + 1 + w];
						um1 = prev_U[j + 1 + w];
						vm1 = prev_V[j + 1 + w];
					}

					if(i < h - 1)
					{
						yp1 = prev[j + 1 + w];
						up1 = prev_U[j + 1 + w];
						vp1 = prev_V[j + 1 + w];
					}
					else
					{
						yp1 = ym1;
						up1 = um1;
						vp1 = vm1;
					}

					if(i > 1)
					{
						ym2 = prev[j + 1 - w2];
						um2 = prev_U[j + 1 - w2];
						vm2 = prev_V[j + 1 - w2];
					}
					else
					{
						ym2 = prev[j + 1 + w2];
						um2 = prev_U[j + 1 + w2];
						vm2 = prev_V[j + 1 + w2];
					}

					if(i < h - 2)
					{
						yp2 = prev[j + 1 + w2];
						up2 = prev_U[j + 1 + w2];
						vp2 = prev_V[j + 1 + w2];
					}
					else
					{
						yp2 = ym2;
						up2 = um2;
						vp2 = vm2;
					}
					ynext = fabs( y * kmsum - ((ym1 + yp1) * km1 + (ym2 + yp2) * km2) ) / kmsum;
					unext = fabs( u * kmsum - ((um1 + up1) * km1 + (um2 + up2) * km2) ) / kmsum;
					vnext = fabs( v * kmsum - ((vm1 + vp1) * km1 + (vm2 + vp2) * km2) ) / kmsum;
				}
				else
				{
					ynext = ypred;
					unext = upred;
					vnext = vpred;
				}
				if(i == 0)
				{
					ypred = ynext;
					upred = unext;
					vpred = vnext;
				}
				if(work_RGB)
				{
					if(i != w - 1)
					{
						r = (buf[j + 1] & 0xff0000)>>16;
						g = (buf[j + 1] & 0xff00)>>8;
						b = (buf[j + 1] & 0xff);
						if(i > 0)
						{
							rm1 = (buf[j + 1-w] & 0xff0000)>>16;
							gm1 = (buf[j + 1-w] & 0xff00)>>8;
							bm1 = (buf[j + 1-w] & 0xff);
						}
						else
						{
							rm1 = (buf[j + 1+w] & 0xff0000)>>16;
							gm1 = (buf[j + 1+w] & 0xff00)>>8;
							bm1 = (buf[j + 1+w] & 0xff);
						}
						if(i < h - 1)
						{
							rp1 = (buf[j + 1+w] & 0xff0000)>>16;
							gp1 = (buf[j + 1+w] & 0xff00)>>8;
							bp1 = (buf[j + 1+w] & 0xff);
						}
						else
						{
							rp1 = rm1;
							gp1 = gm1;
							bp1 = bm1;
						}
						if(i > 1)
						{
							rm2 = (buf[j + 1-w2] & 0xff0000)>>16;
							gm2 = (buf[j + 1-w2] & 0xff00)>>8;
							bm2 = (buf[j + 1-w2] & 0xff);
						}
						else
						{
							rm2 = (buf[j + 1+w2] & 0xff0000)>>16;
							gm2 = (buf[j + 1+w2] & 0xff00)>>8;
							bm2 = (buf[j + 1+w2] & 0xff);
						}
						if(i < h - 2)
						{
							rp2 = (buf[j + 1+w2] & 0xff0000)>>16;
							gp2 = (buf[j + 1+w2] & 0xff00)>>8;
							bp2 = (buf[j + 1+w2] & 0xff);
						}
						else
						{
							rp2 = rm2;
							gp2 = gm2;
							bp2 = bm2;
						}
						rnext = fabs( r * kmsum - ((rm1 + rp1) * km1 + (rm2 + rp2) * km2) ) / kmsum;
						gnext = fabs( g * kmsum - ((gm1 + gp1) * km1 + (gm2 + gp2) * km2) ) / kmsum;
						bnext = fabs( b * kmsum - ((bm1 + bp1) * km1 + (bm2 + bp2) * km2) ) / kmsum;
					}
					else
					{
						rnext = rpred;
						gnext = gpred;
						bnext = bpred;
					}
					if(i == 0)
					{
						rpred = rnext;
						gpred = gnext;
						bpred = bnext;
					}
					measure = (rpred * rpred + 2 * rcur * rcur + rnext * rnext)/4.0;
					measure += (gpred * gpred + 2 * gcur * gcur + gnext * gnext)/4.0;
					measure += (bpred * bpred + 2 * bcur * bcur + bnext * bnext)/4.0;
					measure *= 0.7;
				}
				else
				{
					//work in YUV
					measure = (ypred * ypred + 2 * ycur * ycur + ynext * ynext)/4.0;
					measure += (upred * upred + 2 * ucur * ucur + unext * unext)/4.0;
					measure += (vpred * vpred + 2 * vcur * vcur + vnext * vnext)/4.0;
				}

				//interpolation start
				if(true/*measure > treshold*/)
				{
					if(yinterp > 255) yinterp = 255; else if(yinterp < 0) yinterp = 0;
					prev[j] = yinterp;
					prev_U[j] = uinterp;
					prev_V[j] = vinterp;
					if(!work_RGB)
					{
						r = prev[j] + 1.14 * prev_V[j] + 0.5;
						g = prev[j] - 0.395 * prev_U[j] - 0.581 * prev_V[j] + 0.5;
						b = prev[j] + 2.032 * prev_U[j] + 0.5;
					}
					else
					{
						r = rinterp + 0.5;
						g = ginterp + 0.5;
						b = binterp + 0.5;
					}
					if(r > 255) r = 255; else if(r < 0) r = 0;
					if(g > 255) g = 255; else if(g < 0) g = 0;
					if(b > 255) b = 255; else if(b < 0) b = 0;
					buf[j] = (r << 16) | (g << 8) | b;
					dst_ptr[j] = buf[j];
					map[j] = 1;
				}
				//interpolation end
				if(i != w - 1)
				{
					if(i > 2)
					{
						ym3 = prev[j + 1-w3];
						um3 = prev_U[j + 1-w3];
						vm3 = prev_V[j + 1-w3];
					}
					else
					{
						ym3 = prev[j + 1+w3];
						um3 = prev_U[j + 1+w3];
						vm3 = prev_V[j + 1+w3];
					}
					if(i < h - 3)
					{
						yp3 = prev[j + 1+w3];
						up3 = prev_U[j + 1+w3];
						vp3 = prev_V[j + 1+w3];
					}
					else
					{
						yp3 = ym3;
						up3 = um3;
						vp3 = vm3;
					}
					yinterp = ( (ym1 + yp1) * k1 + (ym3 + yp3) * k2 ) / ksum;
					uinterp = ( (um1 + up1) * k1 + (um3 + up3) * k2 ) / ksum;
					vinterp = ( (vm1 + vp1) * k1 + (vm3 + vp3) * k2 ) / ksum;
				}
				ypred = ycur;
				upred = ucur;
				vpred = vcur;
				ycur = ynext;
				ucur = unext;
				vcur = vnext;
				if(work_RGB)
				{
					if(i != w - 1)
					{
						if(i > 2)
						{
							rm3 = (buf[j + 1-w3] & 0xff0000)>>16;
							gm3 = (buf[j + 1-w3] & 0xff00)>>8;
							bm3 = (buf[j + 1-w3] & 0xff);
						}
						else
						{
							rm3 = (buf[j + 1+w3] & 0xff0000)>>16;
							gm3 = (buf[j + 1+w3] & 0xff00)>>8;
							bm3 = (buf[j + 1+w3] & 0xff);
						}
						if(i < h - 3)
						{
							rp3 = (buf[j + 1+w3] & 0xff0000)>>16;
							gp3 = (buf[j + 1+w3] & 0xff00)>>8;
							bp3 = (buf[j + 1+w3] & 0xff);
						}
						else
						{
							rp3 = rm3;
							gp3 = gm3;
							bp3 = bm3;
						}
						rinterp = ( (rm1 + rp1) * k1 + (rm3 + rp3) * k2 ) / ksum;
						ginterp = ( (gm1 + gp1) * k1 + (gm3 + gp3) * k2 ) / ksum;
						binterp = ( (bm1 + bp1) * k1 + (bm3 + bp3) * k2 ) / ksum;
					}
					rpred = rcur;
					gpred = gcur;
					bpred = bcur;
					rcur = rnext;
					gcur = gnext;
					bcur = bnext;
				}
			}
		}
		prev += w;
		prev_U += w;
		prev_V += w;
		dst_ptr = (Pixel32*)((BYTE*)dst_ptr + p);
		buf += w;
		map += w;
	}

	delete map_;
}

#endif
