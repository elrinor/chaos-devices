#ifndef __ARX_UTIL_IDC__
#define __ARX_UTIL_IDC__

#include <idc.idc>

/** Decodes a null-terminated string at pos.
 *
 * @param pos position of the beginning of a string.
 * @return value of a null-terminated string. */
static decodeString(pos) {
  auto s, c;
  s = "";
  while((c = Byte(pos)) != 0) {
    s = form("%s%c", s, c);
    pos = pos + 1;
  }
  return s;
}

/** Replaces all occurences of substr in str with replacement. */
static replaceAll(str, substr, replacement) {
  auto substrlen;
  substrlen = strlen(substr);
  while(1) {
    auto pos;
    pos = strstr(str, substr);
    if(pos == -1)
      return str;
    str = form("%s%s%s", substr(str, 0, pos), replacement, substr(str, pos + substrlen, -1));
  }
}

/** Replaces "<", ">", and "&" characters in a given string with "&lt;", "&gt;" and "&amp;" strings respectively. */
static xmlEncode(str) {
  return replaceAll(replaceAll(replaceAll(replaceAll(str, "&", "=^_^="), "=^_^=", "&amp;"), "<", "&lt;"), ">", "&gt;");
}

/** @return whether a pointer is stored at position pos. */
static isPtr(pos) {
  auto flags;
  flags = GetFlags(pos);
  return hasValue(flags) && isData(flags) && isOff0(flags) && (flags & DT_TYPE) == FF_DWRD;
}

/** @return whether a pointer to data is stored at position pos. */
static isPtrToData(pos) {
  auto flags;
  flags = GetFlags(Dword(pos));
  return isPtr(pos) && isData(flags) && hasValue(flags);
}

/** @return whether a pointer to code is stored at position pos. */
static isPtrToCode(pos) {
  auto flags;
  flags = GetFlags(Dword(pos));
  return isPtr(pos) && isCode(flags) && hasValue(flags) && Dword(pos) != 0 && Dword(Dword(pos)) != 0;
}

/** @return target address of jump or call instruction at pos. Doesn't work with dll calls. */
static getCallJmpTarget(pos) {
  return Rfirst0(pos);
}

/** @return address referenced by instruction or data at pos. */
static getReferencedAddr(pos) {
  return Dfirst(pos);
}

/** @return whether byte sequence starting at pos matches the given pattern. */
static matchBytes(pos, match) {
  auto i, len, s;
  len = strlen(match);
  if(len % 2 != 0) {
    Warning("Bad match string in matchBytes: %s", match);
    return 0;
  }
  for(i = 0; i < len; i = i + 2) {
    s = substr(match, i, i + 2);
    if(s != "??" && form("%02X", Byte(pos)) != s)
      return 0;
    pos++;
  }
  return 1;
}

static matchInterlaced(pos, match) {
  auto p, currentMatch, maxpos;
  match = form("%s%s", match, " ");
  maxpos = GetFunctionAttr(pos, FUNCATTR_END);
  currentMatch = "";
  
  for(p = pos; p < maxpos && p != BADADDR;) {
    auto insLen;
    while(currentMatch == "") {
      if(match == "")
        return 1;
      currentMatch = substr(match, 0, strstr(match, " "));
      match = substr(match, strstr(match, " ") + 1, -1);
    }

    //Message("%08X: %s | %s %d\n", p, currentMatch, match, insLen);
    if(matchBytes(p, currentMatch)) {
      currentMatch = "";
      p = p + strlen(currentMatch) / 2;
    } else
      p = NextHead(p, maxpos);
  }
  
  return match == "" && currentMatch == "";
}

/** @return true if instruction at pos is a jump instruction. */
static isJump(pos) {
  /* E9 xx xx xx xx        jmp xxxxxxxx
   * EB xx                 jmp xx */
  auto flags;
  flags = GetFlags(pos);
  return 
    PrevHead(ItemEnd(pos), 0) == pos && /* It must be start of an instruction. */
    isCode(flags) &&                    /* It must be code. */
    (Byte(pos) == 0xE9 || Byte(pos) == 0xEB);
}

/** @return true if instruction at pos is a call instruction. */
static isCall(pos) {
  /* E8 xx xx xx xx        call near f() */
  auto flags;
  flags = GetFlags(pos);
  return 
    PrevHead(ItemEnd(pos), 0) == pos && /* It must be start of an instruction. */
    isCode(flags) &&                    /* It must be code. */
    Byte(pos) == 0xE8;
}

static outputProgress(message, min, max, pos, nextProgressOutput) {
  if(pos >= nextProgressOutput) {
    auto percentDone;
    percentDone = (pos - min) * 100 / (max - min);
    Message(message, percentDone);
    return min + (percentDone + 1) * (max - min) / 100;
  } else
    return nextProgressOutput;
}

/** @return segment with the given name, or BADADDR. */
static RealSegByName(name) {
  auto seg;
  for(seg = FirstSeg(); seg != BADADDR; seg = NextSeg(seg))
    if(SegName(seg) == name)
      return seg;
  return BADADDR;
}













#endif










