#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include "sha224.h"

using namespace std;


string SHA224_Final(hashState* ctx) {
    unsigned char digest[SHA224_DIGEST_LENGTH];
    SHA224_Final(ctx, digest);
    ostringstream ss;
    ss << setbase(16);
    for(int i = 0; i < SHA224_DIGEST_LENGTH; i++)
        ss << setw(2) << setfill('0') << (int) digest[i];
    return ss.str();
}

void usage() {
    cout << "USAGE: " << endl;
    cout << "  sha224test -f FILENAME" << endl;
    cout << "  sha224test -s STRING" << endl;
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

        hashState ctx;
        SHA224_Init(&ctx, 224);

        while(!f.eof() && !f.fail()) {
            f.read((char*) buf, 1024);
            SHA224_Update(&ctx, buf, 8 * f.gcount());
        }

        cout << SHA224_Final(&ctx);
    } else if(string(argv[1]) == "-s") {
        hashState ctx;
        SHA224_Init(&ctx, 224);
        SHA224_Update(&ctx, (unsigned char*) argv[2], 8 * strlen(argv[2]));
        cout << SHA224_Final(&ctx);
    } else {
        usage();
    }

    return 0;
}