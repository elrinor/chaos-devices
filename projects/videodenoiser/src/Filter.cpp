#include "Types.h"
#include "MotionEstimation.h"
#include "Transformations.h"
#include "Denoising.h"

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
	mfd->num_blocks_vert = (mfd->height + 7) >> 3;
	mfd->num_blocks_hor = (mfd->width + 7) >> 3;
	mfd->prev_Y = new BYTE[ mfd->ext_size ];
	mfd->prev_U = new short[ mfd->width * mfd->height ];
	mfd->prev_V = new short[ mfd->width * mfd->height ];
	mfd->prev_Y_MC = new BYTE[ mfd->ext_size ];
	mfd->prev_U_MC = new short[ mfd->width * mfd->height ];
	mfd->prev_V_MC = new short[ mfd->width * mfd->height ];

	mfd->cur_Y = new BYTE[ mfd->ext_size ];
	mfd->cur_U = new short[ mfd->width * mfd->height ];
	mfd->cur_V = new short[ mfd->width * mfd->height ];
	mfd->cur_Y_den = new BYTE[ mfd->ext_size ];
	mfd->cur_U_den = new short[ mfd->width * mfd->height ];
	mfd->cur_V_den = new short[ mfd->width * mfd->height ];

	mfd->next_Y = new BYTE[ mfd->ext_size ];
	mfd->next_U = new short[ mfd->width * mfd->height ];
	mfd->next_V = new short[ mfd->width * mfd->height ];
	mfd->next_Y_MC = new BYTE[ mfd->ext_size ];
	mfd->next_U_MC = new short[ mfd->width * mfd->height ];
	mfd->next_V_MC = new short[ mfd->width * mfd->height ];

	mfd->MVectors_prev = new MV[ mfd->num_blocks_vert * mfd->num_blocks_hor ];
	mfd->MVectors_next = new MV[ mfd->num_blocks_vert * mfd->num_blocks_hor ];

  mfd->DotMask = new BYTE[ mfd->width * mfd->height ];
  mfd->DotMask2 = new BYTE[ mfd->width * mfd->height ];
  mfd->DotMask3 = new BYTE[ mfd->width * mfd->height ];

  
  mfd->VerticalSums = new float[mfd->width];
  mfd->Scratch = new bool[mfd->width];


	mfd->frame_no = 0;
	mfd->offset = 0;
	flag_first_frame = true;
	MotionEstimateStart( mfd->width, mfd->height, mfd->quality );
	DenoiseStart( mfd->width, mfd->height, mfd->strength );
	return 0;
}

void DrawLine( Pixel32* canvas, int width, int height, int line_offset, int x1, int y1, int x2, int y2 )
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
			origin = y == y1;
      if( x < 0 || x >= width || y >= height || y < 0)
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
					*cur = 0x00;
			}
		}
	}
	else if( abs( x2 - x1 ) >= abs( y2 - y1 ) )
	{
		for( x = min( x1, x2 ); x <= max( x2, x1 ); x++ )
		{
			origin = x == x1;
			y = ( x - x1 ) * ( y2 - y1 ) / ( x2 - x1 ) + y1;
      if( x < 0 || x >= width || y >= height || y < 0)
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
	else
	{
		for( y = min( y1, y2 ); y <= max( y2, y1 ); y++ )
		{
			origin = y == y1;
			x = ( y - y1 ) * ( x2 - x1 ) / ( y2 - y1 ) + x1;
			if( x < 0 || x >= width || y >= height || y < 0)
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

void Log(const FilterActivation *fa, MV *mvectors)
{
	MFD *mfd = (MFD*)fa->filter_data;
	int i;
	int num_blocks_total = mfd->num_blocks_hor * mfd->num_blocks_vert;
	double sum_error, sum_length;
	static double all_sum_error, all_sum_length;
	MV mvector;
	FILE *f;
	if(mfd->frame_no == 0)
	{
		f = fopen(mfd->log_file,"w");
		all_sum_error = 0;
		all_sum_length = 0;
		fseek(f, 66, SEEK_SET);
	}
	else
	{
		f = fopen(mfd->log_file,"r+");
		fseek(f, 0, SEEK_END);
	}
	sum_error = 0;
	sum_length = 0;
	for(i = 0; i < num_blocks_total; i++)
	{
		mvector = mvectors[i];
  	sum_error += mvector.error;
		sum_length += sqrtf(mvector.x * mvector.x + mvector.y * mvector.y);
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
	BYTE *cur,  *prev, *next;
	short *cur_U, *cur_V, *prev_U, *prev_V, *next_U, *next_V;
	int i, j;
	int y, u, v, r, g, b;
	int num_blocks_hor = (w + 7) >> 3;

	if(mfd->show_source) 
		memcpy(fa->dst.data, fa->src.data, fa->src.pitch * fa->src.h);

	if( mfd->show_MC )
	{
		dst = (Pixel32*)fa->dst.data;
		prev = mfd->prev_Y_MC + first_row_offset + BORDER;
		prev_U = mfd->prev_U_MC;
		prev_V = mfd->prev_V_MC;
		next = mfd->next_Y_MC + first_row_offset + BORDER;
		next_U = mfd->next_U_MC;
		next_V = mfd->next_V_MC;
		for( i = 0; i < h; i++ )
		{
			for( j = 0; j < w; j++ )
			{
				MV mvector_prev = mfd->MVectors_prev[(i >> 3) * num_blocks_hor + (j >> 3)];
				long error_prev = mvector_prev.error;
				MV mvector_next = mfd->MVectors_next[(i >> 3) * num_blocks_hor + (j >> 3)];
				long error_next = mvector_next.error;
				if (error_prev < error_next)
				{
					y = prev[j];
					u = prev_U[j];
					v = prev_V[j];
				}
				else
				{
					y = next[j];
					u = next_U[j];
					v = next_V[j];
				}
				r = y + 1.14 * v + 0.5;
				g = y - 0.395 * u - 0.581 * v + 0.5;
				b = y + 2.032 * u + 0.5;
				if(r > 255) r = 255; else if(r < 0) r = 0;
				if(g > 255) g = 255; else if(g < 0) g = 0;
				if(b > 255) b = 255; else if(b < 0) b = 0;
				dst[j] = (r << 16) | (g << 8) | b;
			}
			prev += wext;
			prev_U += w;
			prev_V += w;
			next += wext;
			next_U += w;
			next_V += w;
			dst = (Pixel32*)((BYTE*)dst + p);
			src = (Pixel32*)((BYTE*)src + p);
		}
	}
	else if( mfd->show_denoised )
	{
		dst = (Pixel32*)fa->dst.data;
		cur = mfd->cur_Y_den + first_row_offset + BORDER;
		cur_U = mfd->cur_U_den;
		cur_V = mfd->cur_V_den;
		for( i = 0; i < h; i++ )
		{
			for( j = 0; j < w; j++ )
			{
				y = cur[j];
				u = cur_U[j];
				v = cur_V[j];
				r = y + 1.14 * v + 0.5;
				g = y - 0.395 * u - 0.581 * v + 0.5;
				b = y + 2.032 * u + 0.5;
				if(r > 255) r = 255; else if(r < 0) r = 0;
				if(g > 255) g = 255; else if(g < 0) g = 0;
				if(b > 255) b = 255; else if(b < 0) b = 0;
				dst[j] = (r << 16) | (g << 8) | b;
			}
			cur += wext;
			cur_U += w;
			cur_V += w;
			dst = (Pixel32*)((BYTE*)dst + p);
		}
	}
 /*
  if(1)
  {
    dst = (Pixel32*)fa->dst.data;
    for( i = 0; i < mfd->num_blocks_vert; i++ )
    {
      for( j = 0; j < mfd->num_blocks_hor; j++ )
      {
        mvector = mfd->MVectors_next[i * mfd->num_blocks_hor + j];
        DrawLine(dst, w, h, p, (j << 3) + 4, ((i << 3) + 4), (j << 3) + mvector.x + 4, ((i << 3) + mvector.y + 4));
      }
    }
  }
 */
}

void LogMotions(char *filename, MV *vectors, int count)
{
	int f;
	if((f = _open(filename, _O_WRONLY | _O_BINARY | _O_APPEND))==-1)
		throw "Can't write motions to file!";
	for(int i = 0; i < count; i++)
		_write(f, &(vectors[i]), sizeof(MV));
	_close(f);
}

void ReadMotions(char *filename, int *offset, MV *vectors, int count)
{
	int f;
	if((f = _open(filename, _O_RDONLY | _O_BINARY))==-1)
		throw "Can't read motions from file!";
	_lseek(f, *offset, SEEK_SET);
	for(int i = 0; i < count; i++)
		_read(f, &(vectors[i]), sizeof(MV));
	*offset = _tell(f);
	_close(f);
}

_inline void CycleSwap(void **a1, void **a2, void **a3)
{
	void *temp;
	temp = *a1;
	*a1 = *a2;
	*a2 = *a3;
	*a3 = temp;
}

__inline void Swap(void **a1, void **a2)
{
  void *temp;
  temp = *a1;
  *a1 = *a2;
  *a2 = temp;
}

int runProc(const FilterActivation *fa, const FilterFunctions *ff)
{
	Pixel32 *src = (Pixel32*)fa->src.data, *dst = (Pixel32*)fa->dst.data;
	MFD *mfd = (MFD*) fa->filter_data;
	int i, j;
	MV mvector_prev, mvector_next;
	BYTE *prev = mfd->prev_Y, *cur = mfd->cur_Y, *prev_MC = mfd->prev_Y_MC; 
	BYTE *next_MC = mfd->next_Y_MC, *next = mfd->next_Y;
	short *cur_U = mfd->cur_U, *cur_V = mfd->cur_V;
	short *prev_U = mfd->prev_U, *prev_V = mfd->prev_V;
	short *next_U = mfd->next_U, *next_V = mfd->next_V;
	short *prev_U_MC = mfd->prev_U_MC, *prev_V_MC = mfd->prev_V_MC;
	short *next_U_MC = mfd->next_U_MC, *next_V_MC = mfd->next_V_MC;
	int w = mfd->width, h = mfd->height, p = fa->src.pitch;
	int num_blocks_hor = (w + 7) >> 3;
	int wext = mfd->ext_w, hext = mfd->ext_h, sh_x, sh_y;
	int first_row_offset = BORDER * wext, after_last_row_offset = (BORDER + h) * wext;

	InitYUV(mfd, (Pixel32*)fa->src.data, fa->src.pitch);

	if(mfd->frame_no == 0)
	{
		memcpy(mfd->cur_Y, mfd->next_Y, mfd->ext_size);
		memcpy(mfd->cur_U, mfd->next_U, mfd->size * sizeof(short));
		memcpy(mfd->cur_V, mfd->next_V, mfd->size * sizeof(short));
		memcpy(mfd->prev_Y, mfd->cur_Y, mfd->ext_size);
		memcpy(mfd->prev_U, mfd->cur_U, mfd->size * sizeof(short));
		memcpy(mfd->prev_V, mfd->cur_V, mfd->size * sizeof(short));
		int fl;
		if(mfd->log_vectors)
		if(!mfd->read_motions)
			fl = _creat( mfd->log_vectors_file, _S_IREAD | _S_IWRITE);
		else 
			fl = _open( mfd->log_vectors_file, _O_RDONLY);
	}

	if(mfd->log_vectors && mfd->read_motions)
	{
		ReadMotions(mfd->log_vectors_file, &mfd->offset, mfd->MVectors_prev, mfd->num_blocks_hor * mfd->num_blocks_vert);
		ReadMotions(mfd->log_vectors_file, &mfd->offset, mfd->MVectors_next, mfd->num_blocks_hor * mfd->num_blocks_vert);
	}
	else
		MotionEstimate(fa, ff);

	if(mfd->log_vectors && !mfd->read_motions)
	{
		LogMotions(mfd->log_vectors_file, mfd->MVectors_prev, mfd->num_blocks_hor * mfd->num_blocks_vert);
		LogMotions(mfd->log_vectors_file, mfd->MVectors_next, mfd->num_blocks_hor * mfd->num_blocks_vert);
	}

	//constructing MC frames
	cur = mfd->cur_Y + first_row_offset + BORDER;
	cur_U = mfd->cur_U;
	cur_V = mfd->cur_V;
  next_MC += first_row_offset + BORDER;
  prev_MC += first_row_offset + BORDER;
	for(i = 0; i < h; i++)
	{
		for(j = 0; j < w; j++)
		{
			mvector_prev = mfd->MVectors_prev[(i >> 3) * num_blocks_hor + (j >> 3)];
  		prev = mfd->prev_Y + first_row_offset + BORDER + i * wext + j;
			prev_U = mfd->prev_U + i * w + j;
			prev_V = mfd->prev_V + i * w + j;
      if(j + mvector_prev.x >= 0 && j + mvector_prev.x <= w - 1)
				sh_x = mvector_prev.x;
			else if(j + mvector_prev.x < 0)
				sh_x = -j;
			else
				sh_x = w - 1 - j; 
			if(mvector_prev.y + i >= 0 && mvector_prev.y + i <= h - 1)
				sh_y = mvector_prev.y;
			else if(mvector_prev.y + i < 0)
				sh_y = -i;
			else
				sh_y = h - 1 - i; 
			prev_MC[j] = prev[sh_y * wext + sh_x];
			prev_U_MC[j] = prev_U[sh_y * w + sh_x];
			prev_V_MC[j] = prev_V[sh_y * w + sh_x];

			mvector_next = mfd->MVectors_next[(i >> 3) * num_blocks_hor + (j >> 3)];
			next = mfd->next_Y + first_row_offset + BORDER + i * wext + j;
			next_U = mfd->next_U + i * w + j;
			next_V = mfd->next_V + i * w + j;
			if(j + mvector_next.x >= 0 && j + mvector_next.x <= w - 1)
				sh_x = mvector_next.x;
			else if(j + mvector_next.x < 0)
				sh_x = -j;
			else
				sh_x = w - 1 - j; 
			if(mvector_next.y + i >= 0 && mvector_next.y + i <= h - 1)
				sh_y = mvector_next.y;
			else if(mvector_next.y + i < 0)
				sh_y = -i;
			else
				sh_y = h - 1 - i; 
			next_MC[j] = next[sh_y * wext + sh_x];
			next_U_MC[j] = next_U[sh_y * w + sh_x];
			next_V_MC[j] = next_V[sh_y * w + sh_x];
		}
		prev_MC += wext;
		prev_U_MC += w;
		prev_V_MC += w;
		next_MC += wext;
		next_U_MC += w;
		next_V_MC += w;
	}

	Denoise(fa, ff);
	
	if( !mfd->show_nothing )
	{
		DrawOutput( fa, mfd->MVectors_prev );
	}

	if( mfd->log_need )
	{
		Log(fa, mfd->MVectors_prev);
	}

	//preparing to the next frame
  Swap((void**)&mfd->cur_Y, (void**)&mfd->cur_Y_den);
  Swap((void**)&mfd->cur_U, (void**)&mfd->cur_U_den);
  Swap((void**)&mfd->cur_V, (void**)&mfd->cur_V_den);
	CycleSwap((void**)&mfd->prev_Y, (void**)&mfd->cur_Y, (void**)&mfd->next_Y);
	CycleSwap((void**)&mfd->prev_U, (void**)&mfd->cur_U, (void**)&mfd->next_U);
	CycleSwap((void**)&mfd->prev_V, (void**)&mfd->cur_V, (void**)&mfd->next_V);

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
			SendMessage( GetDlgItem( hdlg, IDC_STRENGTHSLIDER ), TBM_SETRANGEMIN, true, 0);
			SendMessage( GetDlgItem( hdlg, IDC_STRENGTHSLIDER ), TBM_SETRANGEMAX, true, 100);
			SendMessage( GetDlgItem( hdlg, IDC_STRENGTHSLIDER ), TBM_SETLINESIZE, 0, 1);
			SendMessage( GetDlgItem( hdlg, IDC_STRENGTHSLIDER ), TBM_SETPAGESIZE, 0, 5);
			SendMessage( GetDlgItem( hdlg, IDC_STRENGTHSLIDER ), TBM_SETTICFREQ, 5, 0);
			SendMessage( GetDlgItem( hdlg, IDC_STRENGTHSPIN ), UDM_SETRANGE32, 0, 100);
			CheckRadioButton( hdlg, IDC_SHOWSOURCE, IDC_SHOWDEN, mfd->show_MC_next?IDC_SHOWMCNEXT:(mfd->show_MC?IDC_SHOWMC:(mfd->show_denoised?IDC_SHOWDEN:IDC_SHOWSOURCE)));
			CheckDlgButton( hdlg, IDC_DRAWNOTHING, mfd->show_nothing?BST_CHECKED:BST_UNCHECKED);
			CheckDlgButton( hdlg, IDC_LOGNEED, mfd->log_need?BST_CHECKED:BST_UNCHECKED);
			SetDlgItemInt( hdlg, IDC_QUALITY, mfd->quality, false );
			SetDlgItemInt( hdlg, IDC_STRENGTH, mfd->strength, false );
			SetDlgItemText( hdlg, IDC_LOGFILENAME, (LPCSTR)mfd->log_file );
			CheckDlgButton( hdlg, IDC_LOG_VECTORS, mfd->log_vectors?BST_CHECKED:BST_UNCHECKED);
			SetDlgItemText( hdlg, IDC_LOG_VECTORS_FILE, (LPCSTR)mfd->log_vectors_file );
			CheckDlgButton( hdlg, IDC_LOG_READ, mfd->read_motions?BST_CHECKED:BST_UNCHECKED);
            return TRUE;

        case WM_COMMAND:
            switch(LOWORD(wParam)) {
            case IDOK:
				mfd->show_MC = !!IsDlgButtonChecked( hdlg, IDC_SHOWMC );
				mfd->show_MC_next = !!IsDlgButtonChecked( hdlg, IDC_SHOWMCNEXT );
				mfd->show_source = !!IsDlgButtonChecked( hdlg, IDC_SHOWSOURCE );
				mfd->show_denoised = !!IsDlgButtonChecked( hdlg, IDC_SHOWDEN );
				mfd->quality = GetDlgItemInt(hdlg, IDC_QUALITY, NULL, false);
				mfd->strength = GetDlgItemInt(hdlg, IDC_STRENGTH, NULL, false);
				mfd->log_need = !!IsDlgButtonChecked( hdlg, IDC_LOGNEED );
				mfd->show_nothing = !!IsDlgButtonChecked( hdlg, IDC_DRAWNOTHING );
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
			case IDC_STRENGTH:
				if(HIWORD(wParam) == EN_UPDATE)
				{
					temp = GetDlgItemInt( hdlg, IDC_STRENGTH, NULL, false);
					if( temp > 100 )
					{
						temp = 100;
						SetDlgItemInt( hdlg, IDC_STRENGTH, temp, false);
					}
					else if( temp < 0 )
					{
						temp = 0;
						SetDlgItemInt( hdlg, IDC_STRENGTH, temp, false);
					}
					sem = true;
					SendMessage( GetDlgItem( hdlg, IDC_STRENGTHSLIDER), TBM_SETPOS, true, GetDlgItemInt( hdlg, IDC_STRENGTH, NULL, false) );
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
			if( LOWORD(wParam) == IDC_STRENGTHSLIDER )
			{
				if( !sem )
					SetDlgItemInt( hdlg, IDC_STRENGTH, SendMessage( GetDlgItem( hdlg, IDC_STRENGTHSLIDER), TBM_GETPOS, 0, 0 ), false );
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
	DenoiseEnd();
	MotionEstimateEnd();
	delete mfd->prev_Y;
	delete mfd->cur_Y;
	delete mfd->prev_Y_MC;
	delete mfd->MVectors_prev;
	delete mfd->MVectors_next;
  //delete mfd->DotMask;
  //delete mfd->DotMask2;
	return 0;
}


//job support
bool fssProc(FilterActivation *fa, const FilterFunctions *ff, char *buf, int buflen) 
{
    MFD *mfd = (MFD *)fa->filter_data;

    _snprintf(buf, buflen, "Config(%d, %d, %d, \"%s\", %d, %d, %d, %d, \"%s\", %d, %d)", 
								mfd->quality,
								mfd->show_nothing,
								mfd->log_need,
								mfd->log_file,
								mfd->show_source,
								mfd->show_MC,
								mfd->show_MC_next,
								mfd->log_vectors,
								mfd->log_vectors_file,
								mfd->read_motions,
								mfd->strength
								);
    return true;
}

void scriptConfig(IScriptInterpreter *isi, void *lpVoid, CScriptValue *argv, int argc) 
{
  FilterActivation *fa = (FilterActivation *)lpVoid;
  MFD *mfd = (MFD *)fa->filter_data;

	mfd->quality = argv[0].asInt();
	mfd->show_nothing = !!(argv[1].asInt());
	mfd->log_need = !!(argv[2].asInt());
	strcpy(mfd->log_file, *argv[3].asString());
	mfd->show_source = !!(argv[4].asInt());
	mfd->show_MC = !!(argv[5].asInt());
	mfd->show_MC_next = !!(argv[6].asInt());
	mfd->log_vectors = !!(argv[7].asInt());
	strcpy(mfd->log_vectors_file, *argv[8].asString());
	mfd->read_motions = !!(argv[9].asInt());
	mfd->strength = argv[10].asInt();
}

ScriptFunctionDef script_func_defs[]={
    { (ScriptFunctionPtr)scriptConfig, "Config", "0iiisiiiisii" },
    { NULL },
};

CScriptObject script_obj={
    NULL, script_func_defs
};

struct FilterDefinition filterDef = {

    NULL, NULL, NULL,				// next, prev, module
#ifndef _DEBUG
    "Denoise_Fokin",					// name
#else
    "Denoise_Fokin_dbg",					// name
#endif
    "Denoising task filter",		// desc
    "Fokin Alexander",				// maker
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
	mfd->show_denoised = true;
	mfd->show_nothing = false;
	mfd->show_source = false;
	mfd->show_MC = false;
	mfd->show_MC_next = false;
	mfd->quality = 100;
	mfd->strength = 100;
	mfd->method = den;
	mfd->log_need = false;
	strcat(strcpy(mfd->log_file, filterDef.name), ".txt");
	mfd->log_need = false;
	strcat(strcpy(mfd->log_vectors_file, filterDef.name), ".mvd");
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
