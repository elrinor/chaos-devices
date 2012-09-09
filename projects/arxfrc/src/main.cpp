#include <stdio.h>
#include <stdlib.h>
#include "FCompress.h"
#include "FDecompress.h"

int synopsis()
{
	printf("%s", "arxfrc - ArX Fractal Image Compressor\n");
	printf("%s", "\n");
	printf("%s", "USAGE:\n");
	printf("%s", "arxfrc [-c|-d] infile outfile [options]\n");
	printf("%s", "Possible options:\n");
	printf("%s", "-c       Compress\n");
	printf("%s", "-d       Decompress\n");
	printf("%s", "-s SIZE  Use blocks of fixed size = SIZE. Off by default.\n");
	printf("%s", "-q QUAL  Use quadtree based compression with quality = QUAL. Quality must be between 1 and 100. 70 by default.\n");
	return 0;
}


int main(int argc, char** argv)
{
	
	/*FractalDecompress("out.afr", "res.bmp");
	return 0;
	//*/


	char* InFile = NULL;
	char* OutFile = NULL;
	int quality = 70;
	bool fixedsize = false;
	bool compress = true;

	if (argc < 4)
	{
		synopsis();
	}
	else
	{
		for(int i = 1; i < argc; i++)
			if(argv[i][0] == '-')
			{
				if(argv[i][1] == 'c')
					compress = true;
				else if(argv[i][1] == 'd')
					compress = false;
				else if(argv[i][1] == 's')
				{
					fixedsize = true;
					i++;
				}
				else if(argv[i][1] == 'q')
				{
					fixedsize = false;
					i++;
					quality = atoi(argv[i]);
				}
				else
					printf("Unknown option \"%s\"\n", argv[i]);
			}
			else
			{
				if(InFile == NULL)
					InFile = argv[i];
				else if(OutFile == NULL)
					OutFile = argv[i];
			}
		if(InFile != NULL && OutFile != NULL)
		{
			if(compress)
				FractalCompress(InFile, OutFile, fixedsize, quality);
			else
				FractalDecompress(InFile, OutFile);
		}
		else
			synopsis();
	}


	//FractalDecompress("out.afr", "res.bmp");

	return 0;
}