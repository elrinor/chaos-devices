#ifndef __ARX_VTM_IDC__
#define __ARX_VTM_IDC__

#include <idc.idc>
#include "crec_util.idc"

#define TYPE_CTOR           1
#define TYPE_DTOR           2
#define TYPE_VIRTUAL_DTOR   3
#define TYPE_SCALAR_DTOR    4
#define TYPE_VECTOR_DTOR    5

#define REG_ESI             6
#define REG_EDI             7

static methodTypeToStringShort(type) {
  if(type == TYPE_CTOR)
    return "CTR";
  else if(type == TYPE_DTOR)
    return "DTR";
  else if(type == TYPE_VIRTUAL_DTOR)
    return "VDT";
  else if(type == TYPE_SCALAR_DTOR)
    return "SDD";
  else if(type == TYPE_VECTOR_DTOR)
    return "VDD";
  else 
    return "???";
}

/** @return string containing a mangled representation of a given number. */
static mangleNumber(x) {
  /* - 0 to 9 represents number 1 to 10.
   * - num@ represents hexadecimal number, where num consists of hexadecimal digit A (means 0) to P (means 15). 
   *   For example BCD@ means number 0x123, that is 291.
   * - @ represents number 0. (?)
   * - If allowed, prefix ? represents minus sign. Note that both ?@ and @ represents number 0. */
  auto s, prefix;
 
  if(x < 0)
    prefix = "?";
  else
    prefix = "";
    
  if(x <= 10)
    s = form("%d", x - 1);
  else {
    s = "";
    do {
      s = form("%c%s", 'A' + x % 16, s);
      x = x / 16;
    } while(x > 0);
  }

  return prefix + s + "@";
}

/** @param mangledClassName mangled name of a class.
 * @param methodType type of a special method (TYPE_CTOR, etc...).
 * @param adjustment non-zero for adjustment thunks.
 *
 * @return mangled name of a special method for the given class. */
static mangleMethod(mangledClassName, methodType, adjustment) {
  auto prefix, mid, suffix;
  if(methodType == TYPE_CTOR) {
    prefix = "??0";
    mid = "Q";
    suffix = "AE@XZ";
  } else if(methodType == TYPE_DTOR) {
    prefix = "??1";
    mid = "Q";
    suffix = "AE@XZ";
  } else if(methodType == TYPE_VIRTUAL_DTOR) {
    prefix = "??1";
    mid = "U";
    suffix = "AE@XZ";
  } else if(methodType == TYPE_SCALAR_DTOR) {
    prefix = "??_G";
    mid = "U";
    suffix = "AEPAXI@Z";
  } else if(methodType == TYPE_VECTOR_DTOR) {
    prefix = "??_E";
    mid = "Q";
    suffix = "AEPAXI@Z";
  }
  
  if(adjustment == 0) {
    suffix = mid + suffix;
  } else {
    suffix = mid + "W" + mangleNumber(adjustment) + suffix;
  }
  
  return prefix + mangledClassName + "@@" + suffix;
}

/** @return the 2nd argument of a mov [reg + offset], value instruction at pos, or BADADDR. */
static getMovRegOffsetValue(pos, reg, offset) {
  /* C7 0r xx xx xx xx               mov [reg], xxxxxxxx 
   * C7 4r yy xx xx xx xx            mov [reg+yy], xxxxxxxx 
   * C7 8r yy yy yy yy xx xx xx xx   mov [reg+yyyyyyyy], xxxxxxxx */
  if(Byte(pos) == 0xC7) {
    if(Byte(pos + 1) == reg) {
      if(offset == 0)
        return Dword(pos + 2);
    } else if(Byte(pos + 1) == (0x40 | reg)) {
      if(offset == Byte(pos + 2))
        return Dword(pos + 3);
    } else if(Byte(pos + 1) == (0x80 | reg)) {
      if(offset == Dword(pos + 2))
        return Dword(pos + 6);
    }
  }
  
  return BADADDR;
}

/** @return whether the function starting at pos uses mov [reg + offset], `vtable' instruction. */
static hasMovRegOffsetVtable(pos, reg, offset, recDepth, vtSizeArray, vtAccessFuncArray) {
  auto p, maxp;
  maxp = GetFunctionAttr(pos, FUNCATTR_END);
  if(GetArrayElement(AR_LONG, vtAccessFuncArray, pos) != 0) {
    for(p = pos; p < maxp && p != BADADDR; p = NextHead(p, maxp)) {
      if(GetArrayElement(AR_LONG, vtSizeArray, getMovRegOffsetValue(p, reg, offset)) > 0)
        return 1;
    }
  }
  if(recDepth > 0) {
    for(p = pos; p < maxp && p != BADADDR; p = NextHead(p, maxp)) {
      if(isCall(p)) {
        auto target;
        target = getCallJmpTarget(p);
        if(hasMovRegOffsetVtable(target, REG_ESI, offset, recDepth - 1, vtSizeArray, vtAccessFuncArray))
          return 1;
      }
    }
  }
  return 0;
}

/** Analyzes method at pos for class relations.
 * 
 * @param f file to dump to.
 * @param pos position of the target method.
 * @param vtpos position of the corresponding vtable.
 * @param offset offset of the target subobject in the enclosing object.
 * @param recursive perform recursive analysis? */
static analyzeForRelations(f, pos, vtpos, reg, offset, recDepth, vtSizeArray, vtAccessFuncArray, visitedFuncArray, marker) {
  auto p, maxp;
  
  /* The big TODO: this is actually wrong v_v. */
  
  /* Recursion only through functions that work with vtables. */
  if(pos == BADADDR || GetArrayElement(AR_LONG, vtAccessFuncArray, pos) == 0)
    return;
    
  /* Mark this function. */
  SetArrayLong(visitedFuncArray, pos, marker);
    
  /* Analyze instructions. */
  maxp = GetFunctionAttr(pos, FUNCATTR_END);
  for(p = pos; p < maxp && p != BADADDR; p = NextHead(p, maxp)) {
    auto parentVt;
    parentVt = getMovRegOffsetValue(p, reg, offset);
    if(GetArrayElement(AR_LONG, vtSizeArray, parentVt) > 0)
      fprintf(f, "<relation type=\"lhd\" lvt=\"%08X\" rvt=\"%08X\" />\n", parentVt, vtpos);
      
    if(recDepth > 0 && isCall(p)) {
      auto target;
      target = getCallJmpTarget(p);
      if(GetArrayElement(AR_LONG, visitedFuncArray, target) != marker) 
        analyzeForRelations(f, target, vtpos, REG_ESI /* TODO */, offset, recDepth - 1, vtSizeArray, vtAccessFuncArray, visitedFuncArray, marker);
    }
  }
}

/** Analyzes method at pos.
 *
 * @param f file to dump to.
 * @param pos position of the target method.
 * @param vtpos position of the correcponding vtable.
 * @param vtindex index of the target method in the correcponding vtable.
 * @param offset offset of the target subobject in the enclosing object.
 * @param thunkPos position of the adjustment thunk.
 * @param mangledClassName mangled name of the enclosing class. */
static analyzeMethodRecursive(f, pos, vtpos, vtindex, offset, thunkPos, mangledClassName, dumpedMethodArray, vtSizeArray, vtAccessFuncArray, visitedFuncArray) {
  auto type, funcToAnalyze, dtor, reg;
  type = 0;
  funcToAnalyze = BADADDR;
  dtor = BADADDR;

  /* TODO: recursive esi analysis! */
  if(isJump(pos)) {
    return analyzeMethodRecursive(f, getCallJmpTarget(pos), vtpos, vtindex, offset, thunkPos, mangledClassName, 
      dumpedMethodArray, vtSizeArray, vtAccessFuncArray, visitedFuncArray);
  } else if(matchBytes(pos, "83E9") && isJump(pos + 3)) {
    /* Thunk:
     * 83 E9 xx              sub ecx, xx
     *                       jmp class::`scalar deleting destructor'(uint) */
    if(thunkPos != BADADDR)
      Warning("Double thunk: %a -> %a", thunkPos, pos);
    return analyzeMethodRecursive(f, getCallJmpTarget(pos + 3), vtpos, vtindex, Byte(pos + 2), pos, mangledClassName, 
      dumpedMethodArray, vtSizeArray, vtAccessFuncArray, visitedFuncArray);
  } else if(matchBytes(pos, "81E9") && isJump(pos + 6)) {
    /* Thunk:
     * 81 E9 xx xx xx xx     sub ecx, xxxxxxxx
     *                       jmp class::`scalar deleting destructor'(uint) */
    if(thunkPos != BADADDR)
      Warning("Double thunk: %a -> %a", thunkPos, pos);
    return analyzeMethodRecursive(f, getCallJmpTarget(pos + 6), vtpos, vtindex, Dword(pos + 2), pos, mangledClassName, 
      dumpedMethodArray, vtSizeArray, vtAccessFuncArray, visitedFuncArray);
  } else if(matchBytes(pos, "568BF1E8????????F64424080174") && matchBytes(getCallJmpTarget(pos + 13), "8BC65EC20400")) {
    /* 56                    push esi
     * 8B F1                 mov esi, ecx
     * E8 xx xx xx xx        call near class::~class()
     * F6 44 24 08 01        test [esp+arg_0], 1
     * 74 xx                 jz short @@no_free
     *
     *                       operator delete();
     *
     *   @@no_free:
     * 8B C6                 mov eax, esi
     * 5E                    pop esi
     * C2 04 00              retn 4  */
    type = TYPE_SCALAR_DTOR;
    dtor = getCallJmpTarget(pos + 3);
    funcToAnalyze = dtor;
    reg = REG_ESI;
  } else if(matchBytes(pos, "568BF156E8????????F64424080174") && matchBytes(getCallJmpTarget(pos + 14), "8BC65EC20400")) {
    /* 56                    push esi
     * 8B F1                 mov esi, ecx
     * 56                    push esi
     * E8 xx xx xx xx        call near class::~class()
     * F6 44 24 08 01        test [esp+arg_0], 1
     * 74 xx                 jz short @@no_free
     *
     *                       operator delete();
     *
     *   @@no_free:
     * 8B C6                 mov eax, esi
     * 5E                    pop esi
     * C2 04 00              retn 4  */
    type = TYPE_SCALAR_DTOR;
    dtor = getCallJmpTarget(pos + 4);
    funcToAnalyze = dtor;
    reg = REG_ESI;
  } else if(matchBytes(pos, "568BF156C706????????E8????????F64424080174") && matchBytes(getCallJmpTarget(pos + 20), "8BC65EC20400")) {
    /* 56                    push esi
     * 8B F1                 mov esi, ecx
     * 56                    push esi
     * C7 06 xx xx xx xx     mov [esi], class::`vftable'
     * E8 xx xx xx xx        call near class::~class()
     * F6 44 24 08 01        test [esp+arg_0], 1
     * 74 xx                 jz short @@no_free
     *
     *                       operator delete();
     *
     *   @@no_free:
     * 8B C6                 mov eax, esi
     * 5E                    pop esi
     * C2 04 00              retn 4  */
    type = TYPE_SCALAR_DTOR;
    dtor = getCallJmpTarget(pos + 10);
    funcToAnalyze = dtor;
    reg = REG_ESI;
  } else if(matchBytes(pos, "568BF1FF15????????F64424080174") && matchBytes(getCallJmpTarget(pos + 14), "8BC65EC20400")) {
    /* 56                    push esi
     * 8B F1                 mov esi, ecx
     * FF 15 xx xx xx xx     call far class::~class()
     * F6 44 24 08 01        test [esp+arg_0], 1
     * 74 xx                 jz short @@no_free
     *                               
     *                       operator delete();
     *
     *   @@no_free:
     * 8B C6                 mov eax, esi
     * 5E                    pop esi
     * C2 04 00              retn 4 */
    type = TYPE_SCALAR_DTOR;
    reg = REG_ESI;
    /* Nothing to analyze... */
  } else if(matchBytes(pos, "538A5C2408568BF1F6C302742B8B46FC578D7EFC68????????506A??56E8") /*|| 
            matchBytes(pos, "538A5C2408F6C302568BF1742E8B46FC5768????????8D7EFC5068????????56E8")*/) {
     /* 53                   push ebx
      * 8A 5C 24 08          mov bl, [esp+arg_0]
      * 56                   push esi
      * 8B F1                mov esi, ecx
      * F6 C3 02             test bl, 2
      * 74 2B                jz short loc_100037F8
      * 8B 46 FC             mov eax, [esi-4]
      * 57                   push edi
      * 8D 7E FC             lea edi, [esi-4]
      * 68 xx xx xx xx       push offset class::~class(void)
      * 50                   push eax
      * 6A xx                push xx
      * 56                   push esi
      * E8 xx xx xx xx       call `eh vector destructor iterator'(void *, uint, int, void (*)(void *)) */
    type = TYPE_VECTOR_DTOR;
    dtor = getReferencedAddr(pos + 20);
    funcToAnalyze = dtor;
    reg = REG_ESI;
  } else if(matchInterlaced(pos, "568BF1 8BC6 5E C20400")) {
    /*              ...
     * 56                    push esi
     * 8B F1                 mov esi, ecx 
     * 
     *              ...      mov [esi + offset], class::`vftable'
     *
     * 8B C6                 mov eax, esi
     *              ...
     * 5E                    pop esi
     *              ...
     * C2 04 00              retn 4  */
    if(hasMovRegOffsetVtable(pos, REG_ESI, offset, 1, vtSizeArray, vtAccessFuncArray)) {
      type = TYPE_SCALAR_DTOR;
      funcToAnalyze = pos;
      reg = REG_ESI;
    }
  } else if(matchInterlaced(pos, "578BF9 8BC7 5F C20400")) {
    /*              ...
     * 57                    push edi
     * 8B F9                 mov edi, ecx 
     * 
     *              ...      mov [edi + offset], class::`vftable'
     *
     * 8B C7                 mov eax, edi
     *              ...
     * 5F                    pop edi
     *              ...
     * C2 04 00              retn 4 */
    if(hasMovRegOffsetVtable(pos, REG_EDI, offset, 1, vtSizeArray, vtAccessFuncArray)) {
      type = TYPE_SCALAR_DTOR;
      funcToAnalyze = pos;
      reg = REG_EDI;
    }
  }

  fprintf(f, "<vtentry vt=\"%08X\" index=\"% 3d\" calls=\"%08X\" withOffset=\"% 3d\" />\n", vtpos, vtindex, pos, offset);

  if(GetArrayElement(AR_LONG, dumpedMethodArray, pos) == 0) {
    fprintf(f, "<method addr=\"%08X\" type=\"%s\" argsSize=\"% 3d\" length=\"% 3d\" />\n", pos, methodTypeToStringShort(type),
      GetFrameArgsSize(pos), GetFunctionAttr(pos, FUNCATTR_END) - pos);
    SetArrayLong(dumpedMethodArray, pos, 1);
  }
  
  if(type != 0) {
    auto marker;
    MakeName(pos, mangleMethod(mangledClassName, type, 0));
    MakeName(thunkPos, mangleMethod(mangledClassName, type, offset));
    MakeName(dtor, mangleMethod(mangledClassName, TYPE_VIRTUAL_DTOR, 0));
    
    if(thunkPos != BADADDR)
      marker = thunkPos;
    else
      marker = pos;
    if(funcToAnalyze == pos)
      analyzeForRelations(f, pos, vtpos, reg, offset, 1, vtSizeArray, vtAccessFuncArray, visitedFuncArray, marker);
    else
      analyzeForRelations(f, funcToAnalyze, vtpos, reg, offset, 0, vtSizeArray, vtAccessFuncArray, visitedFuncArray, marker);
  }
}

static analyzeMethod(f, pos, vtpos, vtindex, mangledClassName, dumpedMethodArray, vtSizeArray, vtAccessFuncArray, visitedFuncArray) {
  analyzeMethodRecursive(f, pos, vtpos, vtindex, 0, BADADDR, mangledClassName, dumpedMethodArray, vtSizeArray, vtAccessFuncArray, visitedFuncArray);
}

#endif














