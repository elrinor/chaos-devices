#ifndef __A_COMPRESSION_H__
#define __A_COMPRESSION_H__

#include <stdio.h>

unsigned int a_compress(FILE* in, FILE* out);
unsigned int a_decompress(FILE* in, FILE* out);
unsigned int ppm_compress(FILE* infile, FILE* outfile);
unsigned int ppm_decompress(FILE* infile, FILE* outfile);

#endif