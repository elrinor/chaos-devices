#define _CRT_SECURE_NO_DEPRECATE
#include <Windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <io.h>
#include "gost40hash.h"
#include "hackexp.h"

#define BUFSIZE 1024
#define MINSIZE 32768
#define ARRAYSIZE 1024*1024

// Data block - 256 bits
class Block
{
public:
  unsigned char data[32];
  void Init()
  {
    memset(data, 0, 32);
  }
  void Init(unsigned long n)
  {
    Init();
    ((unsigned long*)data)[0] = n;
  }
  Block()
  {
    Init();
  }
  Block(unsigned long n)
  {
    Init(n);
  }
  void next()
  {
    ((unsigned long*)data)[0]++;
  }
};

// Hash - 40 bits
class Hash{
public:
  unsigned long hash[2];
  bool operator== (const Hash& h) const 
  {
    return (this->hash[0] == h.hash[0] && this->hash[1] == h.hash[1]);
  }
  bool operator!= (const Hash& h) const 
  {
    return (this->hash[0] != h.hash[0] || this->hash[1] != h.hash[1]);
  }
  Hash(GostHashCtx& ctx)
  {
    hash[0] = ctx.hash[0];
    hash[1] = ctx.hash[1];
  }
  Hash()
  {
    hash[0] = hash[1] = 0;
  }
  void compress(Block block)
  {
    GostHashCtx ctx;
    gost40hash_reset(&ctx);
    ctx.hash[0] = hash[0];
    ctx.hash[1] = hash[1];
    gost40hash_update(&ctx, block.data, 32);
    hash[0] = ctx.hash[0];
    hash[1] = ctx.hash[1];
  }
};

// Used in HashArray
class HashValue{
public:
  Hash hash;
  unsigned int pos;
  HashValue* next;
  HashValue(Hash& hash, unsigned int pos, HashValue* next) 
  {
    this->hash = hash;
    this->pos = pos;
    this->next = next;
  }
  ~HashValue()
  {
    if(this->next != NULL)
      delete this->next;
  }
};

// Array of 1024*1024 hashes for fast collision search
class HashArray{
public:
  HashValue** hashes;

  unsigned int getIndex(Hash& hash)
  {
    return hash.hash[0] & 0x000FFFFF; // not more than 1024*1024
  }

  void add(Hash& hash, unsigned long pos)
  {
    HashValue** p = &hashes[getIndex(hash)];
    while(*p != NULL)
      p = &(*p)->next;
    *p = new HashValue(hash, pos, NULL);
  }

  HashValue* find(Hash& hash)
  {
    HashValue* p = hashes[getIndex(hash)];
    if(p == NULL)
      return NULL;
    while(p->hash != hash && p->next != NULL)
      p = p->next;
    if(p->hash != hash)
      return NULL;
    else
      return p;
  }

  HashArray()
  {
    hashes = new HashValue*[ARRAYSIZE];
    memset(hashes, 0, ARRAYSIZE * sizeof(HashValue*));
  }
  ~HashArray()
  {
    for(int i = 0; i < ARRAYSIZE; i++)
      if(hashes[i] != NULL)
        delete hashes[i];
    delete[] hashes;
  }
};

class ExpandableMessage
{
public:
  int preZeroBlocks;
  Block singleBlock, pastZeroBlock;
  Hash afterHash;
  void construct(Hash startHash, int preZeroBlocks)
  {
    this->preZeroBlocks = preZeroBlocks;
    HashArray a;
    Block block;
    for(int i = 0; i < 1024*1024; i++)
    {
      Hash afterHash = startHash;
      afterHash.compress(block);
      a.add(afterHash, i);
      block.next();
    }
    block.Init();
    for(int i = 0; i < preZeroBlocks; i++)
      startHash.compress(block);
    while(true)
    {
      Hash afterHash = startHash;
      afterHash.compress(block);
      HashValue* v = a.find(afterHash);
      if(v != NULL)
      {
        singleBlock.Init(v->pos);
        pastZeroBlock = block;
        this->afterHash = afterHash;
        return;
      }
      block.next();
    }
  }
};

ExpandableMessage expandable[20];

unsigned long long GetFileSize(char* FileName)
{
  WIN32_FIND_DATA FD;
  HANDLE hSearch;
  if((hSearch = FindFirstFile(FileName, &FD)) == INVALID_HANDLE_VALUE)
    return 0;
  SYSTEMTIME tModifyTime;
  if(FileTimeToSystemTime(&FD.ftLastWriteTime, &tModifyTime) == 0)
    return 0;
  FindClose(hSearch);
  return ((unsigned long long)FD.nFileSizeHigh << 32) + FD.nFileSizeLow;
}

char* HexDump(unsigned char* Data, unsigned int Size)
{
  static char result[1024]; // static is bad idea, but...
  for(int i = 0; i < Size; i++)
    sprintf(&result[i * 2], "%02x", Data[i]);
  return result;
}

int writeBytes(FILE* f, unsigned char* buf, int bytesToWrite)
{
  unsigned int bytesProcessed = 0;
  while(true)
  {
    unsigned int bytesWritten = _write(f->_file, buf, bytesToWrite - bytesProcessed);
    bytesProcessed += bytesWritten;
    if(bytesProcessed == bytesToWrite)
      return bytesProcessed;
    if(bytesWritten == 0)
      return 1;
  }
}

int readBytes(FILE* f, unsigned char* buf, int bytesToRead)
{
  unsigned int bytesProcessed = 0;
  while(true)
  {
    unsigned int bytesRead = _read(f->_file, buf, bytesToRead - bytesProcessed);
    bytesProcessed += bytesRead;
    if(bytesProcessed == bytesToRead)
      return bytesProcessed;
    if(bytesRead == 0)
      return bytesProcessed;
  }
}

int hashBytes(FILE* f, GostHashCtx* ctx, int bytesToHash)
{
  static unsigned char buf[1024];
  unsigned int bytesProcessed = readBytes(f, buf, bytesToHash);
  gost40hash_update(ctx, buf, bytesProcessed);
  if(bytesProcessed != bytesToHash)
  {
    unsigned char digest[32];
    gost40hash_final(ctx, digest);
    return 1;
  }
  return 0;
}

int main(int argc, char** argv)
{
  if(argc < 3)
  {
    printf("USAGE: gost40hack infile outfile [--reconstruct]\n");
  }
  else
  {
    FILE* f = fopen(argv[1], "rb");
    if(f == NULL)
    {
      printf("Could not open file \"%s\"\n", argv[1]);
      return 1;
    }
    if(GetFileSize(argv[1]) < MINSIZE)
    {
      printf("Trying to create a second preimage for files with size < %d is useless.\n", MINSIZE);
      return 1;
    }

    GostHashCtx ctx;
    gost40hash_init();
    gost40hash_reset(&ctx);
    Hash hash;

    // construct expandable messages
    if(argc == 4 && strcmp(argv[3], "--reconstruct") == 0)
    {
      int p = 0, preZero = 1;
      printf("Constructing expandable messages\n");
      for(; p < 20; p++)
      {
        //printf("Constructing (1, %d) message...\n", preZero + 1);
        expandable[p].construct(hash, preZero);
        hash = expandable[p].afterHash;
        preZero *= 2;

        // write it
        printf("1-msg: ");
        //printf("{{");
        for(int i = 0; i < 32; i++)
        {
          printf("0x%02x", expandable[p].singleBlock.data[i]);
          //if(i != 31)
            //printf(" ,");
        }
        //printf("},{");
        printf("\n");
        printf("n-msg: ");
        for(int i = 0; i < 32; i++)
        {
          printf("0x%02x", expandable[p].pastZeroBlock.data[i]);
          //if(i != 31)
            //printf(" ,");
        }
        //printf("}},\n");
        printf("\n");
      }
      //printf("0x%08x 0x%08x", expandable[19].afterHash.hash[0], expandable[19].afterHash.hash[1]);
    }
    else
    {
      printf("Using preconstructed expandable messages\n");
      int p = 0, preZero = 1;
      for(; p < 20; p++)
      {
        expandable[p].preZeroBlocks = preZero;
        memcpy(expandable[p].singleBlock.data, expTable[p][0], 32);
        memcpy(expandable[p].pastZeroBlock.data, expTable[p][1], 32);
        preZero *= 2;
      }
      expandable[19].afterHash.hash[0] = expLastHash[0];
      expandable[19].afterHash.hash[1] = expLastHash[1];
    }

    // hash file
    printf("Hashing file...\n");
    HashArray fileHashes;

    // skip 20 blocks
    hashBytes(f, &ctx, 20*32);
    int filePos = 20;
    for(int k = 0; k < ARRAYSIZE && hashBytes(f, &ctx, 32) == 0; k++) 
    {
      fileHashes.add(Hash(ctx), filePos); // here filePos is "how many blocks are placed before that one?"
      filePos++; 
    }

    // find linking block
    printf("Constructing link block...\n");
    Block block;
    block.Init();
    //*((unsigned long*)block.data) = 24000000; // 4 debugging
    while(true)
    {
      hash = expandable[19].afterHash;
      hash.compress(block);
      HashValue* v = fileHashes.find(hash);
      if(v != NULL)
      {
        printf("Writing to output file...\n");
        // gocha! */
        unsigned int preBlocks = /*123412;*/v->pos;
        unsigned int mask = 1;
        preBlocks -= 21;
        // write expandable blocks
        FILE* fout = fopen(argv[2], "wb");
        for(int i = 0; i < 20; i++)
        {
          if((preBlocks & mask) != 0)
          {
            // expanded
            Block zeroBlock;
            for(int j = 0; j < expandable[i].preZeroBlocks; j++)
              writeBytes(fout, zeroBlock.data, 32);
            writeBytes(fout, expandable[i].pastZeroBlock.data, 32);
          }
          else
          {
            // small
            writeBytes(fout, expandable[i].singleBlock.data, 32);
          }
          mask <<= 1;
        }
        
        // write link block
        writeBytes(fout, block.data, 32);
        
        // write file end
        fseek(f, v->pos * 32, SEEK_SET);
        while(true)
        {
          unsigned char buf[1024];
          int bytesRead = readBytes(f, buf, 1024);
          if(bytesRead != 0)
            writeBytes(fout, buf, bytesRead);
          if(bytesRead < 1024)
          {
            fclose(f);
            fclose(fout);
            break;
          }
        }
        break;
      }
      block.next();
    }//*/

    printf("Done.\n");
  }
}