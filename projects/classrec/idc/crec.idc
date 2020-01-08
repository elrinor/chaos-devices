#include <idc.idc>
#include "crec_util.idc"
#include "crec_vt.idc"
#include "crec_rtti_msvc.idc"

/** Dumps into f all CHR data. */
static dumpChr(f, outputProgress) {
  auto pos, minAddr, maxAddr, nextProgressOutput, dumpedMethodArray, vtSizeArray, vtAccessFuncArray, vtSize, visitedFuncArray;
  
  /* Create an array for dumped methods. */
  DeleteArray(GetArrayId("dumpedMethodArray"));
  dumpedMethodArray = CreateArray("dumpedMethodArray");
  
  /* Create an array for vtable sizes. */
  DeleteArray(GetArrayId("vtSizeArray"));
  vtSizeArray = CreateArray("vtSizeArray");
  
  /* Create an array for functions which work with vtables. */
  DeleteArray(GetArrayId("vtAccessFuncArray"));
  vtAccessFuncArray = CreateArray("vtAccessFuncArray");
  
  /* Create intermediate visited functions array. */
  DeleteArray(GetArrayId("visitedFuncArray"));
  visitedFuncArray = CreateArray("visitedFuncArray");
    
  /* Detect vtables. */
  nextProgressOutput = 0;
  minAddr = SegStart(RealSegByName(".rdata"));
  maxAddr = SegEnd(RealSegByName(".rdata"));
  for(pos = minAddr; pos < maxAddr;) {
    vtSize = computeVTableSize(pos);
    if(vtSize == 0) {
      pos = pos + 4;
    } else {
      SetArrayLong(vtSizeArray, pos, vtSize);
      pos = pos + vtSize * 4;
    }
    if(outputProgress)
      nextProgressOutput = outputProgress("Scanning for vtables... %d%%\n", minAddr, maxAddr, pos, nextProgressOutput);
  }
  
  /* Detect functions which work with vtables. */
  nextProgressOutput = 0;
  minAddr = SegStart(RealSegByName(".text"));
  maxAddr = SegEnd(RealSegByName(".text"));
  for(pos = NextFunction(minAddr - 1); pos != BADADDR; pos = NextFunction(pos)) {
    auto p, maxp;
    maxp = GetFunctionAttr(pos, FUNCATTR_END);
    for(p = pos; p < maxp && p != BADADDR; p = NextHead(p, maxp)) {
      auto dref, leaving;
      leaving = 0;
      for(dref = Dfirst(p); dref != BADADDR; dref = Dnext(p, dref)) {
        if(GetArrayElement(AR_LONG, vtSizeArray, dref) != 0) {
          SetArrayLong(vtAccessFuncArray, pos, 1);
          leaving = 1;
          break;
        }
      }
      if(leaving)
        break;
    }
    if(outputProgress)
      nextProgressOutput = 
        outputProgress("Scanning for vtable accesses... %d%%\n", minAddr, maxAddr, pos, nextProgressOutput);
  }
  
  /* Loop through vtables and dump. */
  nextProgressOutput = 0;
  minAddr = SegStart(RealSegByName(".rdata"));
  maxAddr = SegEnd(RealSegByName(".rdata"));
  for(pos = GetFirstIndex(AR_LONG, vtSizeArray); pos != BADADDR; pos = GetNextIndex(AR_LONG, vtSizeArray, pos)) {
    auto mangledClassName;
    vtSize = GetArrayElement(AR_LONG, vtSizeArray, pos);
    
    /* Check for RTTI and dump. */
    if(isPtrToRtti(pos - 4)) {
      auto rttiPos;
      rttiPos = Dword(pos - 4);
      fprintf(f, "<classname vt=\"%08X\" name=\"%s\" />\n", pos, xmlEncode(getRttiDeMangledClassName(rttiPos)));
      dumpRtti(f, rttiPos);
      mangledClassName = getRttiMangledClassName(rttiPos);
    } else
      mangledClassName = "Class_" + form("%08X", pos);
    
    /* Dump vtable. */
    dumpVTable(f, pos, vtSize, mangledClassName, dumpedMethodArray, vtSizeArray, vtAccessFuncArray, visitedFuncArray);
    
    if(outputProgress)
      nextProgressOutput = outputProgress("Analyzing vtables... %d%%\n", minAddr, maxAddr, pos, nextProgressOutput);
  }
}

static main(void) {
  auto f, pos;

  if(AskYN(1, "Do you wish to scan the executable?")) {
    Message("Scanning...\n");
    
    f = fopen("objtree.txt", "w");
    fprintf(f, "<?xml version=\"1.0\"?>\n<chrdump version=\"0.1\">\n");
    
    dumpChr(f, 1);
    
    fprintf(f, "</chrdump>\n");
    fclose(f);
    
    Exec("objtree.txt");
  }
}











