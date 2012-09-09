#include "a_compression.h"
#include <stdio.h>
#include <stdlib.h>
#include <map>
#define min(x,y) (((x)>(y))?(y):(x))

#define CODE_BITS 16
#define VALUE_MAX (((unsigned int)1 << CODE_BITS) - 1)
#define VALUE_1QR (VALUE_MAX / 4 + 1)
#define VALUE_MID (VALUE_1QR * 2)
#define VALUE_3QR (VALUE_1QR * 3)
#define CHAR_NUMBER 257
#define CHAR_EOF 256
#define FREQ_MAX (((unsigned int)1 << (CODE_BITS - 2)) - 1)
#define BUFSIZE 65536
#define PPM_AGGRESSIVITY 2048

FILE* in, *out;
unsigned short* sum;
std::map<int, unsigned short*> ppm;
unsigned int ppmkey;
unsigned int ppmcount;
unsigned int ppmcleanupcount;


unsigned int bitchar;
unsigned int bitcount;
unsigned int bitfollow;
unsigned int l,h,v;
unsigned int lastchar;
unsigned int beginfreq;
int seqfreqincsum;
int seqfreqinc;

unsigned int bitout(unsigned int bit)
{
	bitchar <<= 1;
	bitchar |= bit;
  bitcount++;
	if(bitcount == 8)
	{
		putc(bitchar, out);
		bitcount = 0;
		bitchar = 0;
	}
	//printf("%d", bit);
	return 0;
}

unsigned int bitin()
{
	if(bitcount == 0)
	{
		bitchar = getc(in);
		bitcount = 8;
	}
	bitchar <<= 1;
	bitcount--;
	if(bitchar & 0x100)
		return 1;
	else
		return 0;
}

unsigned int bitfollowout(unsigned int bit)
{
	bitout(bit);
	for(; bitfollow>0; bitfollow--)
		bitout(bit ^ 1);
	return 0;
}

unsigned int bit_init()
{
	bitchar = 0;
	bitcount = 0;
	bitfollow = 0;
	return 0;
}

unsigned int bitout_close()
{
	bitfollow++;
	if(l < VALUE_1QR)
		bitfollowout(0);
	else
		bitfollowout(1);
	if(bitcount != 0)
	{
		bitchar <<= (8 - bitcount);
		putc(bitchar, out);
	}
	return 0;
}


unsigned int model_init()
{
	for(unsigned int i = 0; i < CHAR_NUMBER + 1; i++)
		sum[i] = i;
	lastchar = 0;
	seqfreqinc = -2;
	seqfreqincsum = 0;
	return 0;
}

unsigned int model_modify(unsigned int c)
{
	unsigned int df = 0;
	if(c == lastchar)
	{
		seqfreqinc++;
		if(seqfreqinc >= 64)
			seqfreqinc = 64;
		if(seqfreqinc == 1)
			beginfreq = sum[c + 1] - sum[c];
		if(seqfreqinc >= 1)
		{
			seqfreqincsum += seqfreqinc;
			df = seqfreqinc;
		}
	}
	else
	{
		if(seqfreqincsum > 0)
		{
			int dfreq;
			if (sum[lastchar + 1] - sum[lastchar] > seqfreqincsum)
				dfreq = -seqfreqincsum;
			else
				dfreq = beginfreq - (sum[lastchar + 1] - sum[lastchar]);
			for(int i = lastchar + 1; i <= CHAR_NUMBER; i++)
				sum[i] += dfreq;
		}
		lastchar = c;
		seqfreqinc = -2;
		seqfreqincsum = 0;
	}

	df += 2;
	for(int i = c + 1; i <= CHAR_NUMBER; i++)
		sum[i] += df;

	while(sum[CHAR_NUMBER] >= FREQ_MAX)
	{
		unsigned int lastsumdec = 0;
		unsigned int newsumi;
		for(unsigned int i = 1; i <= CHAR_NUMBER; i++)
		{
			newsumi = sum[i - 1] + (sum[i] - (sum[i - 1] + lastsumdec)) / 2 + 1;
			lastsumdec = sum[i] - newsumi;
			sum[i] = newsumi;
		}
	}
	return 0;
}

unsigned int compress_symbol(unsigned int c)
{
	unsigned int r = h - l + 1;
	h = l + r * sum[c + 1] / sum[CHAR_NUMBER] - 1;
	l = l + r * sum[c] / sum[CHAR_NUMBER];
	while(true)
	{
		if(h < VALUE_MID)
			bitfollowout(0);
		else if(l >= VALUE_MID)
		{
			l -= VALUE_MID;
			h -= VALUE_MID;
			bitfollowout(1);
		}
		else if(l >= VALUE_1QR && h < VALUE_3QR)
		{
			bitfollow++;
			l -= VALUE_1QR;
			h -= VALUE_1QR;
		}
		else 
			break;
		l = 2 * l;
		h = 2 * h + 1;
	}
	return 0;
}

unsigned int decompress_symbol()
{
	unsigned int c;
	unsigned int r = h - l + 1;
	unsigned int f = ((v - l + 1) * sum[CHAR_NUMBER] - 1) / r;
	
	for(c = CHAR_NUMBER; sum[c] > f; c--);

	h = l + r * sum[c + 1] / sum[CHAR_NUMBER] - 1;
	l = l + r * sum[c] / sum[CHAR_NUMBER];
	
	while(true)
	{
		if(l >= VALUE_MID)
		{
			l -= VALUE_MID;
			h -= VALUE_MID;
			v -= VALUE_MID;
		}
		else if(l >= VALUE_1QR && h < VALUE_3QR)
		{
			l -= VALUE_1QR;
			h -= VALUE_1QR;
			v -= VALUE_1QR;
		}
		else if(!(h < VALUE_MID))
			break;
		l = 2 * l;
		h = 2 * h + 1;
		v = 2 * v + bitin();
	}

	return c;
}

unsigned int a_compress(FILE* infile, FILE* outfile)
{
	unsigned int c;
	sum = (unsigned short*)malloc(sizeof(short)*(CHAR_NUMBER+1));
	in = infile;
	out = outfile;
	l = 0;
	h = VALUE_MAX;
	bitfollow = 0;
	putc('a',out);
	bit_init();
	model_init();
	while((c = getc(in)) != EOF)
	{
		compress_symbol(c);
		model_modify(c);
	}
	compress_symbol(CHAR_EOF);
	bitout_close();
	free(sum);
	return 0;
}

unsigned int a_decompress(FILE* infile, FILE* outfile)
{
	unsigned int c;
	sum = (unsigned short*)malloc(sizeof(short)*(CHAR_NUMBER+1));
	in = infile;
	out = outfile;
	l = 0;
	h = VALUE_MAX;
	getc(in);
	bit_init();
	model_init();
	v = 0;
	for(unsigned int i = 0; i < CODE_BITS; i++)
		v = v * 2 + bitin();
	while(true)
	{
		c = decompress_symbol();
		if(c == CHAR_EOF)
			break;
		putc(c, out);
		model_modify(c);
	}
	free(sum);
	return 0;
}

unsigned int ppm_init()
{
	ppmcount = 1;
	ppmkey = 0;
	ppm[ppmkey] = sum = (unsigned short*)malloc(sizeof(short)*(CHAR_NUMBER+1));
	for(unsigned int i = 0; i < CHAR_NUMBER + 1; i++)
		sum[i] = i;
	return 0;
}

unsigned int ppm_modify(unsigned int c)
{
	if(ppmcleanupcount < 16)
	{
		for(int i = c + 1; i <= CHAR_NUMBER; i++)
			sum[i] += PPM_AGGRESSIVITY;
	}
	else
	{
		for(int i = c + 1; i <= CHAR_NUMBER; i++)
			sum[i] += 1;
	}

	while(sum[CHAR_NUMBER] >= FREQ_MAX)
	{
		unsigned int lastsumdec = 0;
		unsigned int newsumi;
		for(unsigned int i = 1; i <= CHAR_NUMBER; i++)
		{
			newsumi = sum[i - 1] + (sum[i] - (sum[i - 1] + lastsumdec)) / 2 + 1;
			lastsumdec = sum[i] - newsumi;
			sum[i] = newsumi;
		}
	}

	if(ppmcleanupcount < 16)
	{
		ppmkey = (ppmkey << 8) | c;
		if((sum = ppm[ppmkey]) == NULL)
		{
			if(ppmcount > 128*1024)
			{
				ppmcleanupcount++;
				std::map<int, unsigned short*>::iterator i = ppm.begin(), j;
				while(i != ppm.end())
				{
					if(i->second != NULL && i->second[CHAR_NUMBER] <= 257 + 2 * PPM_AGGRESSIVITY)
					{
						ppmcount--;
						free(i->second);
						i->second = NULL;
						j = i;
						i++;
						ppm.erase(j);
					}
					else
						i++;
				}
				if(ppmcount > 64*1024)
				{
					while(i != ppm.end())
					{
						if(i->second != NULL && i->second[CHAR_NUMBER] <= 257 + 16 * PPM_AGGRESSIVITY)
						{
							ppmcount--;
							free(i->second);
							i->second = NULL;
							j = i;
							i++;
							ppm.erase(j);
						}
						else
							i++;
					}
				}
			}
			ppmcount++;
			ppm[ppmkey] = sum = (unsigned short*)malloc(sizeof(short)*(CHAR_NUMBER+1));
			for(unsigned int i = 0; i < CHAR_NUMBER + 1; i++)
				sum[i] = i;
		}
	}
	else 
		sum = ppm[0];
	return 0;
}

unsigned int ppm_compress(FILE* infile, FILE* outfile)
{
	unsigned int c;
	in = infile;
	out = outfile;
	l = 0;
	h = VALUE_MAX;
	bitfollow = 0;
	ppmcleanupcount = 0;
	putc('p',out);
	bit_init();
	ppm_init();
	while((c = getc(in)) != EOF)
	{
		compress_symbol(c);
		ppm_modify(c);
	}
	compress_symbol(CHAR_EOF);
	bitout_close();
	ppm.clear();
	return 0;
}

unsigned int ppm_decompress(FILE* infile, FILE* outfile)
{
	unsigned int c;
	in = infile;
	out = outfile;
	l = 0;
	h = VALUE_MAX;
	getc(in);
	bit_init();
	ppm_init();
	v = 0;
	for(unsigned int i = 0; i < CODE_BITS; i++)
		v = v * 2 + bitin();
	while(true)
	{
		c = decompress_symbol();
		if(c == CHAR_EOF)
			break;
		putc(c, out);
		ppm_modify(c);
	}
	ppm.clear();
	return 0;
}

