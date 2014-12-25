#ifndef __SHA224_H__
#define __SHA224_H__
#ifdef __cplusplus
extern "C" {
#endif

#define SHA224_DIGEST_LENGTH 28

typedef enum { SUCCESS = 0, FAIL = 1, BAD_HASHLEN = 2 } HashReturn;
typedef unsigned char BitSequence;
typedef unsigned long long DataLength;

typedef struct {
    unsigned char state[32];
    unsigned char buffer[64];
    unsigned long long bitcount;
    unsigned int hashbitlen;
} hashState;

HashReturn SHA224_Init(hashState* ctx, int hashbitlen);

HashReturn SHA224_Update(hashState* ctx, const BitSequence *data, DataLength databitlen);

HashReturn SHA224_Final(hashState* ctx, BitSequence *hashval);

HashReturn SHA224_Hash(int hashbitlen, const BitSequence *data, DataLength databitlen, BitSequence *hashval);

#ifdef __cplusplus
}
#endif
#endif