#ifndef __PANAMA_H__
#define __PANAMA_H__

typedef  unsigned int   uint32;

#define PANAMA_STAGE_SIZE   8
#define PANAMA_STAGES       32
#define PANAMA_BIT_LEN      256
#define PANAMA_BLOCK_SIZE   32

typedef unsigned char BitSequence;
typedef unsigned long long DataLength;
typedef enum { SUCCESS = 0, FAIL = 1, BAD_HASHLEN = 2 } HashReturn;

typedef struct {
    uint32 word[PANAMA_STAGE_SIZE];
} Panama_Stage;

typedef struct {
    unsigned char buffer[PANAMA_BLOCK_SIZE];
    unsigned int buffered_bytes;
    unsigned int hashbitlen;
    DataLength message_bitlen;
    
    uint32 state[17];
    Panama_Stage stage[PANAMA_STAGES];
    int tap_0;
} Panama_State;

HashReturn Panama_Init(Panama_State *ctx, int hashbitlen);
HashReturn Panama_Update(Panama_State *ctx, const BitSequence *data, DataLength databitlen);
HashReturn Panama_Final(Panama_State *ctx, BitSequence *hashval);
HashReturn Panama_Hash(int hashbitlen, const BitSequence *data, DataLength databitlen, BitSequence *hashval);

#endif