#define _CRT_SECURE_NO_DEPRECATE
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "a_compression.h"

int main(int argc, char** argv)
{
	if(argc<2)
	{
		printf("ArX Compressor v1.0\n");
		printf("USAGE: arxc [c|d] infile outfile [ppm]\n");
		exit(0);
	}

	FILE* in = fopen(argv[2],"rb");
	FILE* out = fopen(argv[3],"wb");
	if (argc >= 5 && strcmp(argv[4], "ppm") == 0)
	{
		if(argv[1][0] == 'c')
			ppm_compress(in, out);
		else if(argv[1][0] == 'd')
			ppm_decompress(in, out);
	}
	else
	{
		if(argv[1][0] == 'c')
			a_compress(in, out);
		else if(argv[1][0] == 'd')
		{
			unsigned char c;
			c = getc(in);
			fclose(in);
			in = fopen(argv[2],"rb");
			if(c=='p')
				ppm_decompress(in, out);
			else
				a_decompress(in, out);
		}
	}
	fclose(in);
	fclose(out);
	return 0;
	

	/*
	FILE* in = fopen("_in","rb");
	FILE* out = fopen("_comp","wb");
	a_compress(in, out);
	fclose(in);
	fclose(out);

	in = fopen("_comp","rb");
	out = fopen("_out","wb");
	a_decompress(in, out);
	fclose(in);
	fclose(out);*/

}