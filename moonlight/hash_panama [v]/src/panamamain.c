#define _CRT_SECURE_NO_DEPRECATE
#include <stdio.h>
#include <string.h>
#include "panama.h"

void synopsis() {
    printf("panama - panama hash calculator\n");
    printf("USAGE:\n");
    printf("  panama -s [string]\n");
    printf("  panama -f [file]\n");
}

/* Test Data:
 *
 * Message: ""
 * Digest: aa0cc954d757d7ac7779ca3342334ca471abd47d5952ac91ed837ecd5b16922b
 *
 * Message: "The quick brown fox jumps over the lazy dog"
 * Digest: 5f5ca355b90ac622b0aa7e654ef5f27e9e75111415b48b8afe3add1c6b89cba1
 */

int main(int argc, char** argv) {
    if(argc < 3) {
        synopsis();
    } else if(strcmp(argv[1], "-s") == 0) {
        int i;
        unsigned char hashcode[PANAMA_BIT_LEN / 8];
        Panama_Hash(PANAMA_BIT_LEN, argv[2], 8 * strlen(argv[2]), hashcode);
        for(i = 0; i < PANAMA_BIT_LEN / 8; i++)
            printf("%02x", (unsigned int)hashcode[i]);
    } else if(strcmp(argv[1], "-f") == 0) {
        int i;
        unsigned char hashcode[PANAMA_BIT_LEN / 8];
        unsigned int readlen;
        Panama_State ctx;
        unsigned char buffer[1024];
        FILE* f = fopen(argv[2], "rb");
        if(f == NULL)
            return 1;
        Panama_Init(&ctx, PANAMA_BIT_LEN);
        while((readlen = fread(buffer, 1, 1024, f)) != 0)
            Panama_Update(&ctx, buffer, readlen * 8);
        Panama_Final(&ctx, hashcode);
        for(i = 0; i < PANAMA_BIT_LEN / 8; i++)
            printf("%02x", (unsigned int)hashcode[i]);
    } else 
        synopsis();
    return 0;
}