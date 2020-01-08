#include "Denoising.h"
#include "FastOps.h"

#define sqr(x) ((x)*(x))

#define KNN_STRENGTH 1500
#define NLM_STRENGTH 70


void DenoiseStart( int width, int height, BYTE strength )
{
	//this function is called once for filter chain startup
	//place here code for all memory allocations and initializations you want to do
}

void DenoiseEnd()
{
	//this function is opposite to DenoiseStart()
	//place here code to release resources if necessary
}

void DenoisingNLMKNN(BYTE *prev_Y_MC, BYTE *cur_Y, BYTE *next_Y_MC,
                     short *prev_U_MC, short *cur_U, short *next_U_MC,
                     short *prev_V_MC, short *cur_V, short *next_V_MC,
                     int width, int height, int radius, float strength)
{
  // strength in [0, 1]
  int w = width, h = height, wext = width + 2 * BORDER;

  prev_Y_MC += BORDER * (wext + 1);
  cur_Y     += BORDER * (wext + 1);
  next_Y_MC += BORDER * (wext + 1);

  //BYTE* p = new BYTE[w * h];
  //memset(p, 1, w * h);

  static float Exponent[512];
  for(int k = 0; k < 512; k ++)
    Exponent[k] = exp(-(k * k) / (strength * strength * KNN_STRENGTH));
  static float NExponent[256];
  for(int k = 0; k < 256; k ++)
    NExponent[k] = exp(-(k * k) / (strength * strength * NLM_STRENGTH));

  static float BorderK = 1.1;
  float NewBorderK = 0;
  //int knnc = 0;

  for(int y = 0; y < h; y++) 
  for(int x = 0; x < w; x++)
  {
    float k, s[3] = {0, 0, 0}, c[3] = {0, 0, 0};
    int xD = max(0, x - radius), xU = min(w - 1, x + radius), yD = max(0, y - radius), yU = min(h - 1, y + radius);
    for(int x1 = xD; x1 <= xU; x1++) for(int y1 = yD; y1 <= yU; y1++)
    {
      int similarity;
      similarity = SAD8x8(&cur_Y[(y - 4) * wext + (x - 4)], &cur_Y[(y1 - 4) * wext + (x1 - 4)], wext) / 64;
      k = NExponent[similarity];
      s[0] += k;
      c[0] += cur_Y[y1 * wext + x1] * k;
      k = NExponent[similarity] * Exponent[abs(cur_U[y1 * w + x1] - cur_U[y * w + x])];
      s[1] += k;
      c[1] += cur_U[y1 * w    + x1] * k;
      k = NExponent[similarity] * Exponent[abs(cur_V[y1 * w + x1] - cur_V[y * w + x])];
      s[2] += k;
      c[2] += cur_V[y1 * w    + x1] * k;
    }
    NewBorderK += s[0];
    if(s[0] < BorderK && x > radius && x < w - radius && y > radius && y < h - radius)
    {
      // KNN!
      //p[y * w + x] = 0;
      /*c[0] = 0;
      c[1] = 0;
      c[2] = 0;*/
      for(int x1 = xD; x1 <= xU; x1++) for(int y1 = yD; y1 <= yU; y1++)
      {
        k = Exponent[abs(cur_Y[y * wext + x] - cur_Y[y1 * wext + x1])];
        s[0] += k;
        c[0] += cur_Y[y1 * wext + x1] * k;
        k = Exponent[abs(cur_U[y * w    + x] - cur_U[y1 * w    + x1])];
        s[1] += k;
        c[1] += cur_U[y1 * w    + x1] * k;
        k = Exponent[abs(cur_V[y * w    + x] - cur_V[y1 * w    + x1])];
        s[2] += k;
        c[2] += cur_V[y1 * w    + x1] * k;
      }
      //knnc++;
    }
    cur_Y[y * wext + x] = (BYTE) (c[0] / s[0]);
    cur_U[y * w    + x] = (short)(c[1] / s[1]);
    cur_V[y * w    + x] = (short)(c[2] / s[2]);
  }
  NewBorderK /= w * h;
  BorderK = NewBorderK * 0.25;

  //for(int y = 0; y < h; y++) for(int x = 0; x < w; x++)
  //  cur_Y[y * wext + x] *= p[y * w + x];
}


void DenoisingNLM(BYTE *prev_Y_MC, BYTE *cur_Y, BYTE *next_Y_MC,
                  short *prev_U_MC, short *cur_U, short *next_U_MC,
                  short *prev_V_MC, short *cur_V, short *next_V_MC,
                  int width, int height, int radius, float strength)
{
  // strength in [0, 1]
  int w = width, h = height, wext = width + 2 * BORDER;
  
  prev_Y_MC += BORDER * (wext + 1);
  cur_Y     += BORDER * (wext + 1);
  next_Y_MC += BORDER * (wext + 1);

  static float Exponent[256];
  for(int k = 0; k < 256; k ++)
    Exponent[k] = exp(-(k * k) / (strength * strength * NLM_STRENGTH));

  for(int y = 0; y < h; y++) for(int x = 0; x < w; x++)
  {
    float k, s[3] = {0, 0, 0}, c[3] = {0, 0, 0};
    int xD = max(0, x - radius), xU = min(w - 1, x + radius), yD = max(0, y - radius), yU = min(h - 1, y + radius);
    for(int x1 = xD; x1 <= xU; x1++) for(int y1 = yD; y1 <= yU; y1++)
    {
      int similarity;
      similarity = SAD8x8(&cur_Y[(y - 4) * wext + (x - 4)], &cur_Y[(y1 - 4) * wext + (x1 - 4)], wext) / 64;
      k = Exponent[similarity];
      s[0] += k;
      c[0] += cur_Y[y1 * wext + x1] * k;
      k = Exponent[similarity] * Exponent[min(abs(cur_U[y1 * w + x1] - cur_U[y * w + x]), 255)];
      s[1] += k;
      c[1] += cur_U[y1 * w    + x1] * k;
      k = Exponent[similarity] * Exponent[min(abs(cur_V[y1 * w + x1] - cur_V[y * w + x]), 255)];
      s[2] += k;
      c[2] += cur_V[y1 * w    + x1] * k;
/*
      similarity = SAD8x8(&cur_Y[(y - 4) * wext + (x - 4)], &prev_Y_MC[(y1 - 4) * wext + (x1 - 4)], wext) / 64;
      k = Exponent[similarity];
      s[0] += k;
      c[0] += prev_Y_MC[y1 * wext + x1] * k;
      k = Exponent[similarity] * Exponent[min(abs(prev_U_MC[y1 * w + x1] - cur_U[y * w + x]), 255)];
      s[1] += k;
      c[1] += prev_U_MC[y1 * w    + x1] * k;
      k = Exponent[similarity] * Exponent[min(abs(prev_V_MC[y1 * w + x1] - cur_V[y * w + x]), 255)];
      s[2] += k;
      c[2] += prev_V_MC[y1 * w    + x1] * k;

      similarity = SAD8x8(&cur_Y[(y - 4) * wext + (x - 4)], &next_Y_MC[(y1 - 4) * wext + (x1 - 4)], wext) / 64;
      k = Exponent[similarity];
      s[0] += k;
      c[0] += next_Y_MC[y1 * wext + x1] * k;
      k = Exponent[similarity] * Exponent[min(abs(next_U_MC[y1 * w + x1] - cur_U[y * w + x]), 255)];
      s[1] += k;
      c[1] += next_U_MC[y1 * w    + x1] * k;
      k = Exponent[similarity] * Exponent[min(abs(next_V_MC[y1 * w + x1] - cur_V[y * w + x]), 255)];
      s[2] += k;
      c[2] += next_V_MC[y1 * w    + x1] * k;
      */
    }
    cur_Y[y * wext + x] = (BYTE) (c[0] / s[0]);
    cur_U[y * w    + x] = (short)(c[1] / s[1]);
    cur_V[y * w    + x] = (short)(c[2] / s[2]);
  }
}


void DenoisingKNN(BYTE *prev_Y_MC, BYTE *cur_Y, BYTE *next_Y_MC,
                  short *prev_U_MC, short *cur_U, short *next_U_MC,
                  short *prev_V_MC, short *cur_V, short *next_V_MC,
                  int width, int height, int radius, float strength)
{
  // strength in [0, 1]
  int w = width, h = height, wext = width + 2 * BORDER;

  prev_Y_MC += BORDER * (wext + 1);
  cur_Y     += BORDER * (wext + 1);
  next_Y_MC += BORDER * (wext + 1);

  static float Exponent[256];
  for(int k = 0; k < 256; k ++)
    Exponent[k] = exp(-(k * k) / (strength * strength * KNN_STRENGTH));

  for(int y = 0; y < h; y++) for(int x = 0; x < w; x++)
  {
    float k, s[3] = {0, 0, 0}, c[3] = {0, 0, 0};
    int xD = max(0, x - radius), xU = min(w - 1, x + radius), yD = max(0, y - radius), yU = min(h - 1, y + radius);
    for(int x1 = xD; x1 <= xU; x1++) for(int y1 = yD; y1 <= yU; y1++)
    {
      k = Exponent[abs(cur_Y[y * wext + x] - cur_Y[y1 * wext + x1])];
      s[0] += k;
      c[0] += cur_Y[y1 * wext + x1] * k;
      k = Exponent[abs(cur_U[y * w    + x] - cur_U[y1 * w    + x1])];
      s[1] += k;
      c[1] += cur_U[y1 * w    + x1] * k;
      k = Exponent[abs(cur_V[y * w    + x] - cur_V[y1 * w    + x1])];
      s[2] += k;
      c[2] += cur_V[y1 * w    + x1] * k;
/*
      k = Exponent[abs(cur_Y[y * wext + x] - prev_Y_MC[y1 * wext + x1])];
      s[0] += k;
      c[0] += prev_Y_MC[y1 * wext + x1] * k;
      k = Exponent[abs(cur_U[y * w    + x] - prev_U_MC[y1 * w    + x1])];
      s[1] += k;
      c[1] += prev_U_MC[y1 * w    + x1] * k;
      k = Exponent[abs(cur_V[y * w    + x] - prev_V_MC[y1 * w    + x1])];
      s[2] += k;
      c[2] += prev_V_MC[y1 * w    + x1] * k;

      k = Exponent[abs(cur_Y[y * wext + x] - next_Y_MC[y1 * wext + x1])];
      s[0] += k;
      c[0] += next_Y_MC[y1 * wext + x1] * k;
      k = Exponent[abs(cur_U[y * w    + x] - next_U_MC[y1 * w    + x1])];
      s[1] += k;
      c[1] += next_U_MC[y1 * w    + x1] * k;
      k = Exponent[abs(cur_V[y * w    + x] - next_V_MC[y1 * w    + x1])];
      s[2] += k;
      c[2] += next_V_MC[y1 * w    + x1] * k;*/
    }
    cur_Y[y * wext + x] = (BYTE) (c[0] / s[0]);
    cur_U[y * w    + x] = (short)(c[1] / s[1]);
    cur_V[y * w    + x] = (short)(c[2] / s[2]);
  }
}

void Denoising(BYTE *prev_Y_MC, BYTE *cur_Y, BYTE *next_Y_MC,
			   short *prev_U_MC, short *cur_U, short *next_U_MC,
			   short *prev_V_MC, short *cur_V, short *next_V_MC,
			   int width, int height, int strength)
{
	int i, j;
	int w = width, h = height, wext = width + 2 * BORDER;

	int thresh = strength / 7 + 5;

	int yp, yc, yn;
	int up, uc, un;
	int vp, vc, vn;
	int y, u, v;

  prev_Y_MC += BORDER * (wext + 1);
  cur_Y     += BORDER * (wext + 1);
  next_Y_MC += BORDER * (wext + 1);

	for(i = 0; i < h; i++)
		for(j = 0; j < w; j++)
		{
			yp = prev_Y_MC[i * wext + j];
			up = prev_U_MC[i * w + j];
			vp = prev_V_MC[i * w + j];
			yc = cur_Y[i * wext + j];
			uc = cur_U[i * w + j];
			vc = cur_V[i * w + j];
			yn = next_Y_MC[i * wext + j];
			un = next_U_MC[i * w + j];
			vn = next_V_MC[i * w + j];

			//median temporal filtering
			if(abs(yp - yc) + abs(yc - yn) + abs(up - uc) + abs(uc - un) + abs(vp - vc) + abs(vc - vn) < thresh * 2)
			if (yp < yc) 
			{
				if (yc < yn) 
				{
					y = yc;
					u = uc;
					v = vc;
				} 
				else 
				{
					if (yp > yn) 
					{
						y = yp;
						u = up;
						v = vp;
					} 
					else 
					{
						y = yn;
						u = un;
						v = vn;
					}
				}
			} 
			else 
			{
				if (yc > yn) 
				{
					y = yc;
					u = uc;
					v = vc;
				} 
				else 
				{
					if (yp < yn) 
					{
						y = yp;
						u = up;
						v = vp;
					} 
					else 
					{
						y = yn;
						u = un;
						v = vn;
					}
				}
			}
			else
			{
				y = yc;
				u = uc;
				v = vc;
			}

			//y = yc;
			//u = uc;
			//v = vc;

			cur_Y[i * wext + j] = y;
			cur_U[i * w + j] = u;
			cur_V[i * w + j] = v;
		}
}


inline void Expand(BYTE* p, BYTE* p1, int w, int h, int r)
{
  memcpy(p1, p, w * h);
  for(int y = r; y < h - r; y++) for(int x = r; x < w - r; x++)
  {
    if(p[y * w + x])
      for(int i = -r; i <= r; i++)
        p1[y * w + x + i] = 1;
  }
  memcpy(p, p1, w * h);
  for(int y = r; y < h - r; y++) for(int x = r; x < w - r; x++)
  {
    if(p1[y * w + x])
      for(int i = -r; i <= r; i++)
        p[(y + i) * w + x] = 1;
  }
}

inline void Shrink(BYTE* p, BYTE* p1, int w, int h, int r)
{
  memcpy(p1, p, w * h);
  for(int y = r; y < h - r; y++) for(int x = r; x < w - r; x++)
  {
    if(!p[y * w + x])
    for(int i = -r; i <= r; i++)
      p1[y * w + x + i] = 0;
  }
  memcpy(p, p1, w * h);
  for(int y = r; y < h - r; y++) for(int x = r; x < w - r; x++)
  {
    if(!p1[y * w + x])
      for(int i = -r; i <= r; i++)
        p[(y + i) * w + x] = 0;
  }
}

void RemoveOldCinemaPass1(MFD* p)
{
  int w = p->width, h = p->height, wext = p->ext_w;
  BYTE* cur_Y = p->cur_Y_den + BORDER * (wext + 1);
  BYTE* prev_Y = p->prev_Y_MC + BORDER * (wext + 1);
  BYTE* next_Y = p->next_Y_MC + BORDER * (wext + 1);
  short* cur_U = p->cur_U_den;
  short* cur_V = p->cur_V_den;
  float* VerticalSums = p->VerticalSums;
  bool* Scratches = p->Scratch;

  memset(VerticalSums, 0, sizeof(unsigned int) * w);

  for(int y = 0; y < h; y++) for(int x = 0; x < w; x++)
    VerticalSums[x] += cur_Y[y * wext + x];

  for(int x = 0; x < w; x++)
    VerticalSums[x] /= h;

  /*
  // Smoothing
  for(int x = 0; x < w - 1; x++)
    VerticalSums[x] = (VerticalSums[x] + VerticalSums[x + 1]) / 2;
  for(int x = w - 1; x > 0; x--)
    VerticalSums[x] = (VerticalSums[x] + VerticalSums[x - 1]) / 2;
    */

  float MeanDY = 0;
  for(int x = 0; x < w - 1; x++)
    MeanDY += abs(VerticalSums[x] - VerticalSums[x + 1]);
  MeanDY /= (w - 1);
  float BorderDY = MeanDY * 3;

  memset(Scratches, 0, sizeof(bool) * w);

  for(int x = 0; x < w - 2; x++)
  {
    if(max(abs(VerticalSums[x + 1] - VerticalSums[x]), abs(VerticalSums[x + 2] - VerticalSums[x])) > BorderDY)
    {
      bool Found = false;
      int x1;
      float PrevY = 0;
      int MidX;
      for(x1 = max(0, x - 2); x1 <= x; x1++)
        PrevY += VerticalSums[x1];
      PrevY /= x - max(0, x - 2) + 1;
      if(VerticalSums[x + 1] - VerticalSums[x] < -BorderDY)
      {
        // Black Scratch
        float MinY = 255;
        for(x1 = x + 1; x1 < min(w - 1, x + 10); x1++)
        {
          if(VerticalSums[x1] < MinY)
          {
            MidX = x1;
            MinY = VerticalSums[x1];
          }
          if(Found && !(VerticalSums[x1 + 1] - VerticalSums[x1] > BorderDY / 3))
            break;
          if(VerticalSums[x1 + 1] - MinY > BorderDY || abs(VerticalSums[x1 + 1] - PrevY) < MeanDY * 3)
            Found = true;
        }
      }
      if(VerticalSums[x + 1] - VerticalSums[x] > BorderDY)
      {
        // White Scratch
        float MaxY = 0;
        for(x1 = x + 1; x1 < min(w - 1, x + 10); x1++)
        {
          if(VerticalSums[x1] > MaxY)
          {
            MidX = x1;
            MaxY = VerticalSums[x1];
          }
          if(Found && !(VerticalSums[x1 + 1] - VerticalSums[x1] < -BorderDY / 3))
            break;
          if(MaxY - VerticalSums[x1] < -BorderDY || abs(VerticalSums[x1 + 1] - PrevY) < MeanDY * 3)
            Found = true;
        }
      }
      if(Found) 
      {
        bool Good = true;
        if(p->frame_no >= 2)
        {
          Good = false;
          float Diff = 0;
          for(int x2 = x + 1; x2 < x1; x2++) for(int y = 0; y < h; y++)
            Diff += abs(cur_Y[y * wext + x2] - prev_Y[y * wext + x2]);
          Diff /= h;
          Diff /= (x1 - (x + 1));
          if(Diff > 1)
            Good = true;
          else 
            Good = false;
        }

        // Check Dispersion;
        /*float Mean = VerticalSums[MidX], Dispersion = 0;
        for(int y = 0; y < h; y++)
          Dispersion += sqr(cur_Y[y * wext + MidX] - Mean);
        Dispersion /= h;
        Dispersion = sqrt(Dispersion);
        if(Dispersion < 96)*/
        if(Good)
          for(int x2 = x + 1; x2 < x1; x2++)
            Scratches[x2] = true;
      }
      x = x1 - 1;
    }
  }

  for(int x1 = 1; x1 < w; x1++) if(Scratches[x1]) for(int x2 = x1; x2 < w; x2++) if(!Scratches[x2])
  {
    x1 = max(1, x1 - 1);
    x2 = min(w - 1, x2 + 1);
    for(int x = x1; x < x2; x++)
    {
      float K2 = ((float)(x - x1 + 1)) / (x2 - x1 + 1);
      float K1 = ((float)(x2 - x)) / (x2 - x1 + 1);
      for(int y = 0; y < h; y++)
      {
        cur_Y[y * wext + x] = cur_Y[y * wext + x1 - 1] * K1 + cur_Y[y * wext + x2] * K2;
        cur_U[y * w + x] = cur_U[y * w + x1 - 1] * K1 + cur_U[y * w + x2] * K2;
        cur_V[y * w + x] = cur_V[y * w + x1 - 1] * K1 + cur_V[y * w + x2] * K2;
        //cur_Y[y * wext + x] = prev_Y[y * wext + x];
        //cur_U[y * w + x] = 255;
      }
    }
    x1 = x2;
    break;
  }
  else if(x2 == w - 1)
    x1 = w;
/*
  for(int x = 0; x < w; x++) if(Scratches[x]) for(int y = 0; y < h; y++)
    cur_U[y * w + x] = 255;
  */
}

void RemoveOldCinemaPass2(MFD* p)
{
  int w = p->width, h = p->height, wext = p->width + 2 * BORDER;
  BYTE* prev_Y_MC = p->prev_Y_MC + BORDER * (wext + 1);
  BYTE* cur_Y     = p->cur_Y_den + BORDER * (wext + 1);
  BYTE* next_Y_MC = p->next_Y_MC + BORDER * (wext + 1);
  BYTE* DotMask   = p->DotMask;
  BYTE* DotMask2  = p->DotMask2;
  BYTE* DotMask3  = p->DotMask3;
  short* cur_U    = p->cur_U_den;
  short* cur_V    = p->cur_V_den;

  float MeanDY = 0;
  for(int y = 0; y < h; y++) for(int x = 0; x < w; x++)
    MeanDY += abs(prev_Y_MC[y * wext + x] - cur_Y[y * wext + x]);
  MeanDY /= w * h;
  float BorderDY = max(MeanDY, 3) * 6;

  memset(DotMask, 0, sizeof(BYTE) * w * h);
  for(int y = 1; y < h - 1; y++) for(int x = 1; x < w - 1; x++)
  {
    if(abs(prev_Y_MC[y * wext + x] - cur_Y[y * wext + x]) > BorderDY)
    {
      DotMask[y * w + x] = 1;
      //cur_U[y * w + x] = 255;
    }
  }

  // Remove Singles
  for(int y = 1; y < h - 1; y++) for(int x = 1; x < w - 1; x++) 
  {
    if(DotMask[y * w + x] && !DotMask[y * w + x + 1] && !DotMask[y * w + x - 1])
      DotMask[y * w + x] = 0;
    else if(DotMask[y * w + x] && !DotMask[(y - 1) * w + x] && !DotMask[(y + 1) * w + x])
      DotMask[y * w + x] = 0;
  }

  memcpy(DotMask3, DotMask, w * h);

  Expand(DotMask, DotMask2, w, h, 10);
  Shrink(DotMask, DotMask2, w, h, 15);
  Expand(DotMask, DotMask2, w, h, 10);

  for(int y = 1; y < h - 1; y++) for(int x = 1; x < w - 1; x++)
    DotMask[y * w + x] = ~DotMask[y * w + x] & DotMask3[y * w + x];

  // Remove Singles
  for(int y = 1; y < h - 1; y++) for(int x = 1; x < w - 1; x++) 
  {
    if(DotMask[y * w + x] && !DotMask[y * w + x + 1] && !DotMask[y * w + x - 1])
      DotMask[y * w + x] = 0;
    else if(DotMask[y * w + x] && !DotMask[(y - 1) * w + x] && !DotMask[(y + 1) * w + x])
      DotMask[y * w + x] = 0;
  }
  
  Expand(DotMask, DotMask2, w, h, 1);
  
  // Expand(0.5)
  memcpy(DotMask2, DotMask, w * h);
  for(int y = 1; y < h - 1; y++) for(int x = 1; x < w - 1; x++)
  {
    if(DotMask[y * w + x])
    {
      DotMask2[y * w + x + 1] = 1;
      DotMask2[y * w + x - 1] = 1;
      DotMask2[(y + 1) * w + x] = 1;
      DotMask2[(y - 1) * w + x] = 1;
    }
  }
  memcpy(DotMask, DotMask2, w * h);

  for(int y = 0; y < h; y++) 
    for(int x1 = 1; x1 < w; x1++) if(DotMask[y * w + x1]) for(int x2 = x1; x2 < w; x2++) if(!DotMask[y * w + x2])
    {
      for(int x = x1; x < x2; x++)
      {
        float K2 = ((float)(x - x1 + 1)) / (x2 - x1 + 1);
        float K1 = ((float)(x2 - x)) / (x2 - x1 + 1);
        cur_Y[y * wext + x] = cur_Y[y * wext + x1 - 1] * K1 + cur_Y[y * wext + x2] * K2;
        cur_U[y * w + x] = cur_U[y * w + x1 - 1] * K1 + cur_U[y * w + x2] * K2;
        cur_V[y * w + x] = cur_V[y * w + x1 - 1] * K1 + cur_V[y * w + x2] * K2;
      }
      x1 = x2;
      break;
    }
  
  /*for(int y = 1; y < h - 1; y++) for(int x = 1; x < w - 1; x++) if(DotMask[y * w + x])
    cur_U[y * w + x] = 255;*/
}

int Denoise(const FilterActivation *fa, const FilterFunctions *ff)
{
  MFD *mfd = (MFD*)fa->filter_data;

  /*
  for(int i = 0; i < mfd->height; i++)
  {
    memcpy(mfd->cur_Y_den + i * mfd->width, mfd->cur_Y + (i + BORDER) * mfd->ext_w + BORDER, mfd->width * sizeof(BYTE));
  }
  */
  memcpy(mfd->cur_Y_den, mfd->cur_Y, mfd->ext_size);
  memcpy(mfd->cur_U_den, mfd->cur_U, mfd->size * sizeof(short));
  memcpy(mfd->cur_V_den, mfd->cur_V, mfd->size * sizeof(short));

  if(mfd->strength == 100)
  {
    //DenoisingNLMKNN(mfd->prev_Y_MC, mfd->cur_Y_den, mfd->next_Y_MC, mfd->prev_U_MC, mfd->cur_U_den, mfd->next_U_MC, mfd->prev_V_MC, mfd->cur_V_den, mfd->next_V_MC, mfd->width, mfd->height, 1, 0.5f);
    Denoising(mfd->prev_Y_MC, mfd->cur_Y_den, mfd->next_Y_MC, mfd->prev_U_MC, mfd->cur_U_den, mfd->next_U_MC, mfd->prev_V_MC, mfd->cur_V_den, mfd->next_V_MC, mfd->width, mfd->height, 300);
    RemoveOldCinemaPass1(mfd);
    if(!mfd->frame_no >= 2)
      Denoising(mfd->prev_Y_MC, mfd->cur_Y_den, mfd->next_Y_MC, mfd->prev_U_MC, mfd->cur_U_den, mfd->next_U_MC, mfd->prev_V_MC, mfd->cur_V_den, mfd->next_V_MC, mfd->width, mfd->height, 300);
    RemoveOldCinemaPass2(mfd);
    DenoisingNLMKNN(mfd->prev_Y_MC, mfd->cur_Y_den, mfd->next_Y_MC, mfd->prev_U_MC, mfd->cur_U_den, mfd->next_U_MC, mfd->prev_V_MC, mfd->cur_V_den, mfd->next_V_MC, mfd->width, mfd->height, 3, 0.5f);
  }
  else
  {
    // Temporal
    Denoising(mfd->prev_Y_MC, mfd->cur_Y_den, mfd->next_Y_MC, mfd->prev_U_MC, mfd->cur_U_den, mfd->next_U_MC, mfd->prev_V_MC, mfd->cur_V_den, mfd->next_V_MC, mfd->width, mfd->height, mfd->strength);

    // Spatial

    // 3.4 fps
    DenoisingNLMKNN(mfd->prev_Y_MC, mfd->cur_Y_den, mfd->next_Y_MC, mfd->prev_U_MC, mfd->cur_U_den, mfd->next_U_MC, mfd->prev_V_MC, mfd->cur_V_den, mfd->next_V_MC, mfd->width, mfd->height, 3, max(1, mfd->strength) / 100.0f);

    // 14.3 fps
    // DenoisingKNN(mfd->prev_Y_MC, mfd->cur_Y_den, mfd->next_Y_MC, mfd->prev_U_MC, mfd->cur_U_den, mfd->next_U_MC, mfd->prev_V_MC, mfd->cur_V_den, mfd->next_V_MC, mfd->width, mfd->height, 2, max(1, mfd->strength) / 100.0f);
    
    // 9.4 fps
    // DenoisingKNN(mfd->prev_Y_MC, mfd->cur_Y_den, mfd->next_Y_MC, mfd->prev_U_MC, mfd->cur_U_den, mfd->next_U_MC, mfd->prev_V_MC, mfd->cur_V_den, mfd->next_V_MC, mfd->width, mfd->height, 3, max(1, mfd->strength) / 100.0f);

    // 4.3 fps
    // DenoisingKNN(mfd->prev_Y_MC, mfd->cur_Y_den, mfd->next_Y_MC, mfd->prev_U_MC, mfd->cur_U_den, mfd->next_U_MC, mfd->prev_V_MC, mfd->cur_V_den, mfd->next_V_MC, mfd->width, mfd->height, 5, max(1, mfd->strength) / 100.0f);
  }

  return 0;
}
