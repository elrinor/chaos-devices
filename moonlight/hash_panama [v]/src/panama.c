#include <stdlib.h>
#include "panama.h"

/* циклический сдвиг, в msvs есть встроенный */
#if defined(_MSC_VER)
#  define ROTL32(a,shift) _lrotl(a,shift)
#else
#  define ROTL32(a,shift)  (((a) << (shift)) | ((a) >> (32 - (shift))))
#endif


void Panama_Push(Panama_State* ctx, uint32* in, int blocks) {
    int i, j, k;

    uint32 state[17];
    uint32 gamma[17];
    uint32 theta[17];
    uint32 pi[17];

    int tap_0;
    Panama_Stage *ptap_0, *ptap_25;
    Panama_Stage *l, *b;

    /* скопируем состояние и tap_0 */
    tap_0 = ctx->tap_0;
    for(i = 0; i < 17; i++)
        state[i] = ctx->state[i];

    l = (Panama_Stage*)in;

    /* операции обновления состояния */
    for(i = 0; i < blocks; i++) {
        /* non-linearity stage */
        for(j = 0; j < 17; j++)
            gamma[j] = state[j] ^ (state[(j + 1) % 17] | ~state[(j + 2) % 17]);

        /* bit-dispersion stage */
        pi[0] = gamma[0];
        k = 0;
        for(j = 1; j < 17; j++) {
            k += j;
            pi[j] = ROTL32(gamma[(j * 7) % 17], k % 32);
        }

        /* diffusion stage */
        for(j = 0; j < 17; j++)
            theta[j] = pi[j] ^ pi[(j + 1) % 17] ^ pi[(j + 4) % 17];

        b = &ctx->stage[(tap_0 + 16) & (PANAMA_STAGES - 1)];

        tap_0   = (tap_0 - 1) & (PANAMA_STAGES - 1);

        ptap_0  = &ctx->stage[tap_0];
        ptap_25 = &ctx->stage[(tap_0 + 25) & (PANAMA_STAGES - 1)];

        /* обновляем LFSR */
        for(j = 0; j < 8; j++)
            ptap_25->word[j] = ptap_25->word[j] ^ ptap_0->word[(j+2) & (PANAMA_STAGE_SIZE-1)];
        for(j = 0; j < 8; j++)
            ptap_0->word[j] = l->word[j] ^ ptap_0->word[j];

        /* buffer injection stage */
        state[0] = theta[0] ^ 0x00000001L; 
        for(j = 1; j < 9; j++)
            state[j] = theta[j] ^ l->word[j - 1];
        for(j = 9; j < 17; j++)
            state[j] = theta[j] ^ b->word[j - 9];

        l++;
    }

    /* запишем состояние и tap_0 обратно */
    ctx->tap_0 = tap_0;
    for(i = 0; i < 17; i++)
        ctx->state[i] = state[i];
}


void Panama_Pull(Panama_State* ctx, uint32 *in, uint32* out, int blocks) {
    int i, j, k;

    uint32 state[17];
    uint32 gamma[17];
    uint32 theta[17];
    uint32 pi[17];

    int tap_0;
    Panama_Stage *ptap_0, *ptap_25;
    Panama_Stage *l, *b;

    /* смотрим, в каком режиме был вызван PULL и настраваем параметры в соответствии с этим */
    static uint32 in_none[PANAMA_STAGE_SIZE] = {0, 0, 0, 0, 0, 0, 0, 0};
    uint32 out_none[PANAMA_STAGE_SIZE];
    int in_step, out_step;

    in_step = out_step = PANAMA_STAGE_SIZE;

    if(in == NULL || out == NULL) {
        in = in_none;
        in_step = 0;
    }

    if(out == NULL) {
        out = out_none;
        out_step = 0;
    }

    /* скопируем состояние и tap_0 */
    tap_0 = ctx->tap_0;
    for(i = 0; i < 17; i++)
        state[i] = ctx->state[i];

    /* операции обновления состояния */
    for(i = 0; i < blocks; i++) {
        /* apply stage */
        out[0] = in[0] ^ state[9];
        out[1] = in[1] ^ state[10];
        out[2] = in[2] ^ state[11];
        out[3] = in[3] ^ state[12];
        out[4] = in[4] ^ state[13];
        out[5] = in[5] ^ state[14];
        out[6] = in[6] ^ state[15];
        out[7] = in[7] ^ state[16];

        out += out_step;
        in  += in_step;

        /* non-linearity stage */
        for(j = 0; j < 17; j++)
            gamma[j] = state[j] ^ (state[(j + 1) % 17] | ~state[(j + 2) % 17]);


        /* bit-dispersion stage */
        pi[0] = gamma[0];
        k = 0;
        for(j = 1; j < 17; j++) {
            k += j;
            pi[j] = ROTL32(gamma[(j * 7) % 17], k % 32);
        }

        /* diffusion stage */
        for(j = 0; j < 17; j++)
            theta[j] = pi[j] ^ pi[(j + 1) % 17] ^ pi[(j + 4) % 17];

        l = &ctx->stage[(tap_0 +  4) & (PANAMA_STAGES - 1)];
        b = &ctx->stage[(tap_0 + 16) & (PANAMA_STAGES - 1)];

        tap_0   = (tap_0 - 1) & (PANAMA_STAGES - 1);

        ptap_0  = &ctx->stage[tap_0];
        ptap_25 = &ctx->stage[(tap_0 + 25) & (PANAMA_STAGES - 1)];

        /* обновляем LFSR */
        for(j = 0; j < 8; j++)
            ptap_25->word[j] = ptap_25->word[j] ^ ptap_0->word[(j+2) & (PANAMA_STAGE_SIZE-1)];
        for(j = 0; j < 8; j++)
            ptap_0->word[j] = state[j + 1] ^ ptap_0->word[j];

        /* buffer injection stage */
        state[0] = theta[0] ^ 0x00000001L; 
        for(j = 1; j < 9; j++)
            state[j] = theta[j] ^ l->word[j - 1];
        for(j = 9; j < 17; j++)
            state[j] = theta[j] ^ b->word[j - 9];
    }

    /* запишем состояние и tap_0 обратно */
    ctx->tap_0 = tap_0;
    for(i = 0; i < 17; i++)
        ctx->state[i] = state[i];
}


HashReturn Panama_Init(Panama_State *ctx, int hashbitlen) {
    int i, j;

    if(hashbitlen != PANAMA_BIT_LEN)
        return FAIL;

    /* инициализация нулями */
    ctx->tap_0 = 0;

    for(j = 0; j < PANAMA_STAGES; j++)
        for(i = 0; i < PANAMA_STAGE_SIZE; i++)
            ctx->stage[j].word[i] = 0L;

    for(i = 0; i < 17; i++)
        ctx->state[i] = 0L;

    ctx->hashbitlen = hashbitlen;
    ctx->buffered_bytes = 0;
    ctx->message_bitlen = 0;
    return SUCCESS;
}

HashReturn Panama_Update(Panama_State *ctx, const BitSequence *data, DataLength databitlen) {
    if(databitlen == 0)
        return SUCCESS;
    ctx->message_bitlen += databitlen;
    while(1) {
        ctx->buffer[ctx->buffered_bytes++] = *data++;
        if(ctx->buffered_bytes == PANAMA_BLOCK_SIZE) {
            Panama_Push(ctx, (uint32*) ctx->buffer, 1);
            ctx->buffered_bytes = 0;
        }
        databitlen -= 8;
        if(databitlen <= 0)
            break;
    }
    return SUCCESS;
}

HashReturn Panama_Final(Panama_State *ctx, BitSequence *hashval) {
    int i;
    uint32* remnant = (uint32*) ctx->buffer;
    unsigned int rembits = (unsigned int) (ctx->message_bitlen % (PANAMA_BLOCK_SIZE * 8));

    /* заполним нулями остатки буфера */
    for(i = ctx->buffered_bytes; i < PANAMA_BLOCK_SIZE; i++)
        ctx->buffer[i] = 0;

    /* допишем бит '1' */
    remnant[rembits / 32] &= ~(~0L << (rembits % 32));
    remnant[rembits / 32] |= 1L << (rembits % 32);

    /* обработка последнего блока */
    Panama_Push(ctx, remnant, 1);

    /* 32 операции PULL */
    Panama_Pull(ctx, NULL, NULL, 32);

    /* последняя операция PULL */
    Panama_Pull(ctx, NULL, (uint32*) hashval, 1);

    return SUCCESS;
}

HashReturn Panama_Hash(int hashbitlen, const BitSequence *data, DataLength databitlen, BitSequence *hashval) {
    Panama_State ctx;
    HashReturn result;
    result = Panama_Init(&ctx, PANAMA_BIT_LEN);
    if(result != SUCCESS)
        return result;

    result = Panama_Update(&ctx, data, databitlen);
    if(result != SUCCESS)
        return result;

    return Panama_Final(&ctx, hashval);
}
























