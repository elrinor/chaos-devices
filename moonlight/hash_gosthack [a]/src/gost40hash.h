#ifndef __GOSTHASH_H__
#define __GOSTHASH_H__

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct 
  {
    unsigned long sum[8];
    unsigned long hash[8];
    unsigned long len[8];
    unsigned char partial[32];
    size_t partial_bytes;  
  } GostHashCtx;

  void gost40hash_init();     
  void gost40hash_reset(GostHashCtx *ctx);  
  void gost40hash_update(GostHashCtx *ctx, const unsigned char *buf, size_t len);
  void gost40hash_final(GostHashCtx *ctx, unsigned char *digest);

#ifdef __cplusplus
}
#endif


#endif
