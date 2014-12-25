#include <string.h>
#include "sha224.h"

static const unsigned int K256[64] = {
    0x428a2f98U, 0x71374491U, 0xb5c0fbcfU, 0xe9b5dba5U,
    0x3956c25bU, 0x59f111f1U, 0x923f82a4U, 0xab1c5ed5U,
    0xd807aa98U, 0x12835b01U, 0x243185beU, 0x550c7dc3U,
    0x72be5d74U, 0x80deb1feU, 0x9bdc06a7U, 0xc19bf174U,
    0xe49b69c1U, 0xefbe4786U, 0x0fc19dc6U, 0x240ca1ccU,
    0x2de92c6fU, 0x4a7484aaU, 0x5cb0a9dcU, 0x76f988daU,
    0x983e5152U, 0xa831c66dU, 0xb00327c8U, 0xbf597fc7U,
    0xc6e00bf3U, 0xd5a79147U, 0x06ca6351U, 0x14292967U,
    0x27b70a85U, 0x2e1b2138U, 0x4d2c6dfcU, 0x53380d13U,
    0x650a7354U, 0x766a0abbU, 0x81c2c92eU, 0x92722c85U,
    0xa2bfe8a1U, 0xa81a664bU, 0xc24b8b70U, 0xc76c51a3U,
    0xd192e819U, 0xd6990624U, 0xf40e3585U, 0x106aa070U,
    0x19a4c116U, 0x1e376c08U, 0x2748774cU, 0x34b0bcb5U,
    0x391c0cb3U, 0x4ed8aa4aU, 0x5b9cca4fU, 0x682e6ff3U,
    0x748f82eeU, 0x78a5636fU, 0x84c87814U, 0x8cc70208U,
    0x90befffaU, 0xa4506cebU, 0xbef9a3f7U, 0xc67178f2U
};

static const unsigned int sha224_initial_hash_value[8] = {
    0xc1059ed8U, 0x367cd507U, 0x3070dd17U, 0xf70e5939U,
    0xffc00b31U, 0x68581511U, 0x64f98fa7U, 0xbefa4fa4U
};

unsigned long long REVERSE64(unsigned long long w)  {
    w = (w >> 32) | (w << 32);
    w = ((w & 0xff00ff00ff00ff00ULL) >> 8) | ((w & 0x00ff00ff00ff00ffULL) << 8);
    /*x =*/return ((w & 0xffff0000ffff0000ULL) >> 16) | ((w & 0x0000ffff0000ffffULL) << 16);
}

unsigned int REVERSE32(unsigned int w)  {
    w = (w >> 16) | (w << 16);
    /*x =*/return ((w & 0xff00ff00UL) >> 8) | ((w & 0x00ff00ffUL) << 8);
}


unsigned int SHR(unsigned int b,unsigned int x){return (x >> b);};
unsigned int ROTR32(unsigned int b,unsigned int x){return ((x >> b) | (x << (32 - b)));};
unsigned int Ch(unsigned int x,unsigned int y,unsigned int z){return ((x & y) ^ ((~x) & z));};
unsigned int Maj(unsigned int x,unsigned int y,unsigned int z){return ((x & y) ^ (x & z) ^ (y & z));};

unsigned int Sigma0_256(unsigned int x){return (ROTR32(2, x) ^ ROTR32(13, x) ^ ROTR32(22, x));};
unsigned int Sigma1_256(unsigned int x){return (ROTR32(6, x) ^ ROTR32(11, x) ^ ROTR32(25, x));};
unsigned int sigma0_256(unsigned int x){return (ROTR32(7, x) ^ ROTR32(18, x) ^ SHR(   3 , x));};
unsigned int sigma1_256(unsigned int x){return (ROTR32(17,x) ^ ROTR32(19, x) ^ SHR(   10, x));};

void SHA224_Transform(hashState* ctx, const unsigned int* data) {
    unsigned int  a, b, c, d, e, f, g, h, s0, s1;
    unsigned int *state = (unsigned int*) ctx->state;
    unsigned int  T1, T2, *W256 = (unsigned int*) ctx->buffer;
    int j;

    a = state[0];
    b = state[1];
    c = state[2];
    d = state[3];
    e = state[4];
    f = state[5];
    g = state[6];
    h = state[7];

    j = 0;
    do {
        W256[j] = REVERSE32(*data++/*, W256[j]*/);

        T1 = h + Sigma1_256(e) + Ch(e, f, g) + K256[j] + W256[j];
        T2 = Sigma0_256(a) + Maj(a, b, c);
        h = g;
        g = f;
        f = e;
        e = d + T1;
        d = c;
        c = b;
        b = a;
        a = T1 + T2;

        j++;
    } while (j < 16);

    do {
        s0 = W256[(j + 1) & 0x0f];
        s0 = sigma0_256(s0);
        s1 = W256[(j + 14) & 0x0f];
        s1 = sigma1_256(s1);

        T1 = h + Sigma1_256(e) + Ch(e, f, g) + K256[j] + (W256[j & 0x0f] += s1 + W256[(j + 9) & 0x0f] + s0);
        T2 = Sigma0_256(a) + Maj(a, b, c);
        h = g;
        g = f;
        f = e;
        e = d + T1;
        d = c;
        c = b;
        b = a;
        a = T1 + T2;

        j++;
    } while (j < 64);

    state[0] += a;
    state[1] += b;
    state[2] += c;
    state[3] += d;
    state[4] += e;
    state[5] += f;
    state[6] += g;
    state[7] += h;
}


HashReturn SHA224_Init(hashState* ctx, int hashbitlen) {
    if(hashbitlen != 224)
        return BAD_HASHLEN;
    memset(ctx, 0, sizeof(hashState));
    memcpy(ctx->state, sha224_initial_hash_value, sizeof(sha224_initial_hash_value));
    ctx->hashbitlen = hashbitlen;
    return SUCCESS;
}

HashReturn SHA224_Update(hashState* ctx, const BitSequence *data, DataLength databitlen) {
    unsigned int freespace, usedspace;
    unsigned int len = (databitlen + 7) / 8;

    if(databitlen < 1)
        return SUCCESS;

    /* If incremental hashing is being performed, all calls to
     * update will contain data lengths that are divisible by 8, except, possibly, the last call. */
    usedspace = (unsigned int) ((ctx->bitcount >> 3) % 64);
    if(usedspace > 0) {
        freespace = 64 - usedspace;
        if(len >= freespace) {
            memcpy(&ctx->buffer[usedspace], data, freespace);
            len -= freespace;
            data += freespace;
            SHA224_Transform(ctx, (const unsigned int*) ctx->buffer);
        } else {
            memcpy(&ctx->buffer[usedspace], data, len);
            ctx->bitcount += databitlen;
            return SUCCESS;
        }
    }
    while(len >= 64) {
        SHA224_Transform(ctx, (const unsigned int*) data);
        len -= 64;
        data += 64;
    }
    if(len > 0)
        memcpy(&ctx->buffer, data, len);
    ctx->bitcount += databitlen;
    return SUCCESS;
}

HashReturn SHA224_Final(hashState* ctx, BitSequence *hashval) {
    unsigned int *state32 = (unsigned int *)ctx->state;
    int i, j;
    unsigned int usedspace, usedbits;

    usedspace = (unsigned int)(ctx->bitcount >> 3) % 64;
    usedbits = ctx->bitcount % 8;
    ctx->buffer[usedspace] &= (((1 << usedbits) - 1) << (8 - usedbits));
    ctx->buffer[usedspace] |= 1 << (7 - usedbits);
    usedspace++;
    if(usedspace <= 56) {
        memset(&ctx->buffer[usedspace], 0, 56 - usedspace);
    } else {
        if(usedspace < 64)
            memset(&ctx->buffer[usedspace], 0, 64 - usedspace);
        SHA224_Transform(ctx, (unsigned int*)ctx->buffer);
        memset(ctx->buffer,0, 56);
    }

    ctx->bitcount = REVERSE64(ctx->bitcount);
    *(unsigned long long*) &ctx->buffer[56] = ctx->bitcount;
    SHA224_Transform(ctx, (unsigned int*) ctx->buffer);

    for(i = 0; i < 7; i++)
        for(j = 3; j > -1; --j) 
            *hashval++ = *((unsigned char*) &state32[i] + j);

    return SUCCESS;
}

HashReturn SHA224_Hash(int hashbitlen, const BitSequence *data, DataLength databitlen, BitSequence *hashval) {
    hashState ctx;
    HashReturn result;
    result = SHA224_Init(&ctx, 224);
    if(result != SUCCESS)
        return result;

    result = SHA224_Update(&ctx, data, databitlen);
    if(result != SUCCESS)
        return result;

    return SHA224_Final(&ctx, hashval);
}