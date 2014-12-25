#ifndef __PANAMA40_H__
#define __PANAMA40_H__

typedef  unsigned int   uint32;

#define PANAMA40_STAGE_SIZE   8
#define PANAMA40_STAGES       32
#define PANAMA40_BIT_LEN      40
#define PANAMA40_BLOCK_SIZE   32

typedef unsigned char BitSequence;
typedef unsigned long long DataLength;
typedef enum { SUCCESS = 0, FAIL = 1, BAD_HASHLEN = 2 } HashReturn;

typedef struct {
    uint32 word[PANAMA40_STAGE_SIZE];
} Panama40_Stage;

typedef struct {
    unsigned char buffer[PANAMA40_BLOCK_SIZE];
    unsigned int buffered_bytes;
    unsigned int hashbitlen;
    DataLength message_bitlen;

    uint32 state[17];
    Panama40_Stage stage[PANAMA40_STAGES];
    int tap_0;
} Panama40_State;

HashReturn Panama40_Init(Panama40_State *ctx, int hashbitlen);
HashReturn Panama40_Update(Panama40_State *ctx, const BitSequence *data, DataLength databitlen);
HashReturn Panama40_Final(Panama40_State *ctx, BitSequence *hashval);
HashReturn Panama40_Hash(int hashbitlen, const BitSequence *data, DataLength databitlen, BitSequence *hashval);

#endif