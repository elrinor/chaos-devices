#define _CRT_SECURE_NO_DEPRECATE
#include <stdio.h>
#include <stdlib.h>
#include <io.h>
#include "gost40hash.h"

#define BUFSIZE 1024

char* HexDump(unsigned char* Data, unsigned int Size)
{
  static char result[1024]; // static is bad idea, but...
  for(int i = 0; i < Size; i++)
    sprintf(&result[i * 2], "%02x", Data[i]);
  return result;
}

int main(int argc, char** argv)
{
  if(argc < 2)
  {
    printf("USAGE: gost40hash filename\n");
  }
  else
  {
    GostHashCtx ctx;
    gost40hash_init();
    gost40hash_reset(&ctx);
    FILE* f = fopen(argv[1], "rb");
    if(f == NULL)
    {
      printf("Could not open file \"%s\"\n", argv[1]);
      return 1;
    }

    while(true)
    {
      unsigned char buf[BUFSIZE];
      unsigned int bytesRead = _read(f->_file, buf, BUFSIZE);
      if(bytesRead == 0)
      {
        unsigned char digest[32];
        gost40hash_final(&ctx, digest);
        printf("%s\n", HexDump(digest, 32));
        //scanf("%s", digest);
        return 0;
      }
      else
        gost40hash_update(&ctx, buf, bytesRead);
    }
  }
}