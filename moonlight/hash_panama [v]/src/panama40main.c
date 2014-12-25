#define _CRT_SECURE_NO_DEPRECATE
#include <stdio.h>
#include <string.h>
#include "panama40.h"

void synopsis() {
    printf("panama40 - panama40 hash calculator\n");
    printf("USAGE:\n");
    printf("  panama40 -s [string]\n");
    printf("  panama40 -f [file]\n");
}

int main(int argc, char** argv) {
    if(argc < 3) {
        synopsis();
    } else if(strcmp(argv[1], "-s") == 0) {
        int i;
        unsigned char hashcode[PANAMA40_BIT_LEN / 8];
        Panama40_Hash(PANAMA40_BIT_LEN, argv[2], 8 * strlen(argv[2]), hashcode);
        for(i = 0; i < PANAMA40_BIT_LEN / 8; i++)
            printf("%02x", (unsigned int)hashcode[i]);
    } else if(strcmp(argv[1], "-f") == 0) {
        int i;
        unsigned char hashcode[PANAMA40_BIT_LEN / 8];
        unsigned int readlen;
        Panama40_State ctx;
        unsigned char buffer[1024];
        FILE* f = fopen(argv[2], "rb");
        if(f == NULL)
            return 1;
        Panama40_Init(&ctx, PANAMA40_BIT_LEN);
        while((readlen = fread(buffer, 1, 1024, f)) != 0)
            Panama40_Update(&ctx, buffer, readlen * 8);
        Panama40_Final(&ctx, hashcode);
        for(i = 0; i < PANAMA40_BIT_LEN / 8; i++)
            printf("%02x", (unsigned int)hashcode[i]);
    } else 
        synopsis();
    return 0;
}