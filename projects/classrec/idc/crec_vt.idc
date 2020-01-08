#ifndef __ARX_VT_IDC__
#define __ARX_VT_IDC__

#include <idc.idc>
#include "crec_util.idc"
#include "crec_vtm.idc"

/** Dumps the vtable of size size at pos. */
static dumpVTable(f, pos, vtSize, mangledClassName, dumpedMethodArray, vtSizeArray, vtAccessFuncArray, visitedFuncArray) {
  auto i;
  for(i = 0; i < vtSize; i++) {
    auto funcAddr; 
    funcAddr = Dword(pos + i * 4);
    analyzeMethod(f, Dword(pos + i * 4), pos, i, mangledClassName, dumpedMethodArray, vtSizeArray, vtAccessFuncArray, visitedFuncArray);
  }
}

/** @return possible vtable size at pos. */
static computeVTableSize(pos) {
  auto vtableSize, p, pf;
  
  for(vtableSize = 0; ;vtableSize = vtableSize + 1) {
    auto funcAddr, funcFlags;
    p = pos + vtableSize * 4;
    pf = GetFlags(p);
    funcAddr = Dword(p);
    funcFlags = GetFlags(funcAddr);
    
    /* If it's a first one... */
    if(vtableSize == 0) {
      auto dref, leaving;
      
      /* ..., then it must be referenced... */
      if(!isRef(pf))
        break;
        
      /* and named (auto or manually). */
      if(!hasName(pf) && !(pf & FF_LABL))
        break;
        
      /* All the references must be "offset" references. */
      leaving = 0;
      for(dref = DfirstB(p); dref != BADADDR; dref = DnextB(p, dref)) {
        if(XrefType() != 1) {
          leaving = 1;
          break;
        }
      }
      if(leaving)
        break;
    }
    
    /* If it's nth one, then it shouldn't be referenced. */
    if(vtableSize > 0 && isRef(pf))
      break;

     
    /* It must be a pointer to a function. */
    if(!isPtrToCode(p) || !(funcFlags & FF_FUNC)) {
      /* Well... it may have not been recognized as a function yet. */
      
      /* It must point into code segment. */
      if(!(GetSegmentAttr(funcAddr, SEGATTR_TYPE) == SEG_CODE || SegName(funcAddr) == ".text"))
        break;
      
      /* If it is code, but not a function start - then we're inside a function of function chunk. 
       * Check previous instruction. */
      if(isCode(funcFlags) && isCode(GetFlags(funcAddr - 1)))
        break;
      
      /* Ok, it seems to be a function start - first unmark it. */
      MakeUnkn(funcAddr, 1);
      Wait();
      
      /* Then try to turn it into code. */
      if(MakeCode(funcAddr) == 0)
        break;
      
      /* It may be a thunk or a proc... Anyway, it will be marked as a proc because of AU_PROC. */
      AutoMark(funcAddr, AU_PROC);
      Wait();
      funcFlags = GetFlags(funcAddr);
      
      /* Check whether it is a proc. */
      if(!(funcFlags & FF_FUNC)) {
        /* It's not a proc, but maybe it's IDA's false positive? TODO: parse and check for ret? */
        MakeUnkn(funcAddr, 1);
        Wait();
        break;
      }
    }
  }
  
  return vtableSize;
}


#endif
