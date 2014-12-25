#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>

#include "sha224.h"

using namespace std;

typedef unsigned long long ULL;

const unsigned long long HASH_INIT_H = 0xDEADF0000DULL;

void REVERSE64(unsigned long long w, unsigned long long &x)  {
    w = (w >> 32) | (w << 32);
    w = ((w & 0xff00ff00ff00ff00ULL) >> 8) | ((w & 0x00ff00ff00ff00ffULL) << 8);
    x = ((w & 0xffff0000ffff0000ULL) >> 16) | ((w & 0x0000ffff0000ffffULL) << 16);
}

ULL h0(ULL val) { // 60-bit val -> 20-bit hash
    val <<= 4;
    /*val |= 0x8;*/
    REVERSE64(val, val);
    
    hashState ctx;
    unsigned char digest[SHA224_DIGEST_LENGTH];
    SHA224_Init(&ctx, 224);
    SHA224_Update(&ctx, (const BitSequence*) &val, 60);
    SHA224_Final(&ctx, digest/*, false, 60*/);
    REVERSE64(*(ULL*)digest, *(ULL*)digest);
    return *(ULL*)digest >> (64 - 20);
}

ULL E(ULL K, ULL P) { // 40-bit K, 40-bit P -> 40-bit H
    ULL A = (P >> 20) & ((1 << 20) - 1);
    ULL B = P & ((1 << 20) - 1);
    for(int r = 0; r < 4; r++) {
        ULL Ai = h0((K << 20) | A) ^ B;
        ULL Bi = A;
        A = Ai;
        B = Bi;
    }
    return (A << 20) | B;
}

ULL E_1(ULL K, ULL H) { // 40-bit K, 40-bit H -> 40-bit P
    ULL A = (H >> 20) & ((1 << 20) - 1);
    ULL B = H & ((1 << 20) - 1);
    for(int r = 0; r < 4; r++) {
        ULL Ai = B;
        ULL Bi = h0((K << 20) | Ai) ^ A;
        A = Ai;
        B = Bi;
    }
    return (A << 20) | B;
}

struct CTX {
    union {
        ULL x; // 40-bit chunk
        unsigned char x_c[8];
    };
    int x_size;
    ULL h; // 40-bit state
};

void Init(CTX* ctx) {
    ctx->x = 0;
    ctx->x_size = 0;
    ctx->h = HASH_INIT_H;
}

void Update(CTX* ctx, const unsigned char* data, unsigned int size) {
    while(size > 0) {
        ctx->x_c[4 - ctx->x_size] = *data;
        data++;
        ctx->x_size++;
        size--;
        if(ctx->x_size == 5) {
            ctx->h = E(ctx->h, ctx->x ^ ctx->h);
            ctx->x_size = 0;
        }
    }
}

ULL End(CTX* ctx) {
    if(ctx->x_size > 0) {
        ULL zero = 0;
        Update(ctx, (unsigned char*) &zero, 5 - ctx->x_size);
    }
    return ctx->h;
}

ULL Hash(const unsigned char* data, unsigned int size) {
    CTX ctx;
    Init(&ctx);
    Update(&ctx, data, size);
    return End(&ctx);
}

void DeHash(ULL h, unsigned char* data, unsigned int* size) {
    ULL X = E_1(HASH_INIT_H, h) ^ HASH_INIT_H;
    for(int i = 0; i < 5; i++)
        data[i] = ((unsigned char*)&X)[4 - i];
    *size = 5;
    return;
}


void usage() {
    cout << "USAGE: " << endl;
    cout << "  hash -f FILENAME" << endl;
    cout << "  hash -s STRING" << endl;
    cout << "  hash -x HEXSTRING" << endl;
    cout << "  hash -r HEXHASH" << endl;
}

void fromHex(char* str, unsigned char* data, int len) {
    string s(str);
    for(int i = 0; i < len; i++) {
        istringstream ss(s.substr(i * 2, 2));
        int val;
        ss >> hex >> val;
        data[i] = (unsigned char) val;
    }
}

int main(int argc, char** argv) {
    if(argc != 3) {
        usage();
    } else if(string(argv[1]) == "-f") {
        unsigned char buf[1024];
        ifstream f;
        f.open(argv[2], ios_base::in | ios_base::binary);
        if(!f.is_open()) {
            cout << "Cannot open file \"" << argv[2] << "\"";
            return 1;
        }

        CTX ctx;
        Init(&ctx);

        while(!f.eof() && !f.fail()) {
            f.read((char*) buf, 1024);
            Update(&ctx, buf, f.gcount());
        }

        cout << setbase(16) << setw(10) << setfill('0') << End(&ctx);
    } else if(string(argv[1]) == "-s") {
        cout << setbase(16) << setw(10) << setfill('0') << Hash((unsigned char*)argv[2], (unsigned int) strlen(argv[2]));
    } else if(string(argv[1]) == "-x") {
        unsigned int len = (unsigned int) strlen(argv[2]) / 2;
        unsigned char *data = new unsigned char[len];
        fromHex(argv[2], data, len);
        cout << setbase(16) << setw(10) << setfill('0') << Hash(data, len);
    } else if(string(argv[1]) == "-r") {
        istringstream ss(argv[2]);
        ULL h;
        ss >> hex >> h;
        unsigned char buf[1024];
        unsigned int len;
        DeHash(h, buf, &len);
        for(unsigned int i = 0; i < len; i++)
            cout << setbase(16) << setw(2) << setfill('0') << (int) buf[i];
    } else {
        usage();
    }

    // Hash('abcde') == 64e10fe3a6
    // Hash('abc')   == ad69d4bc7e

    return 0;
}