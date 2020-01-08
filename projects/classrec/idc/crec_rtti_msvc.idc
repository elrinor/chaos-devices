#ifndef __ARX_RTTI_MSVC_IDC__
#define __ARX_RTTI_MSVC_IDC__

#include <idc.idc>
#include "crec_util.idc"

//----------------------------------------------------------------------------//
// Implementation
//----------------------------------------------------------------------------//

/** Given a pointer to type_info structure, returns demangled name of the corresponding type.
 *
 * @param pos position of type_info.
 * @return demangled name of the corresponding type. */
static decodeClassNameFromTypeInfo(pos) {
  auto s, s2;
  s = "??_7" + decodeString(pos + 12)+"6B@"; /* That's some black magic... */
  s2 = Demangle(s, /*GetLongPrm(INF_SHORT_DN)*/ 8);
  if(s2 != 0)
    return substr(s2, 0, strlen(s2) - 11);
  else
    return s;
}

/** @return whether a pointer to type_info structure is stored at position pos. */
static isPtrToTypeInfo(pos) {
  /* class type_info {
    vfptr_t _vfptr;
    void *_m_data;
    char _m_d_name[1];
  } */

  //Message("isPtrToTypeInfo(%08Xh)\n", pos);
  return 
    isPtrToData(pos) && /* type_info resides in data. */
    isPtrToData(Dword(pos)) && /* vtable for type_info resides in data. */
    isPtrToCode(Dword(Dword(pos))) && /* first element in vtable points into code. */
    Dword(Dword(pos) + 4) == 0 && /* _m_data is zero. */
    (Dword(Dword(pos) + 8) & 0xFFFFFF) == 0x413F2E; /* _m_d_name starts with ".?A". */
}

/** @return whether a pointer to Base Class Array is stored at pos. */
static isPtrToBca(pos) {
  /* struct RTTIBaseClassDescriptor {
    type_info* pTypeInfo;            // type descriptor of the class
    unsigned long numContainedBases; // number of nested classes following in the Base Class Array
    PMD where;                       // pointer-to-member displacement info
    unsigned long attributes;        // flags, usually 0
  }; */

  auto pBca, pBcd, nRecords, i;

  if(!isPtrToData(pos))
    return 0;

  pBca = Dword(pos);
  nRecords = 1;
  for(i = 0; i < nRecords; i = i + 1) {
    if(!isPtrToData(pBca + i * 4))
      return 0;
    
    /* Check BCD. */
    pBcd = Dword(pBca + i * 4);
    if(!isPtrToTypeInfo(pBcd))
      return 0;
    
    if(i == 0)
      nRecords = 1 + Dword(pBcd + 4);
  }
  
  return 1;
}

/** @return whether a pointer to Class Hierarchy Descriptor is stored at pos. */
static isPtrToChd(pos) {
  /* struct _s_RTTIClassHierarchyDescriptor {
    DWORD signature;      // always zero?
    DWORD attributes;     // bit 0 = multiple inheritance, bit 1 = virtual inheritance
    DWORD numBaseClasses; // number of classes in pBaseClassArray
    struct _s_RTTIBaseClassArray* pBaseClassArray;
  }; */
  auto pChd;
  //Message("isPtrToChd(%08Xh)\n", pos);
  pChd = Dword(pos);
  return isPtrToData(pos) && (Dword(pChd + 4) & 0xFFFFFFFC) == 0 && isPtrToBca(pChd + 12);
}

/** Dumps the Base Class Array located at pos into file f. */
static dumpBca(f, pos) {
  /* struct RTTIBaseClassDescriptor {
    type_info* pTypeInfo;            // type descriptor of the class
    unsigned long numContainedBases; // number of nested classes following in the Base Class Array
    PMD where;                       // pointer-to-member displacement info
    unsigned long attributes;        // flags, usually 0
  }; */

  auto i, n;
  n = Dword(Dword(pos) + 4);
  fprintf(f, "<parents ofclass=\"%s\">", xmlEncode(decodeClassNameFromTypeInfo(Dword(Dword(pos)))));
  for(i = 1; i <= n; i = i + dumpBca(f, pos + 4 * i));
  fprintf(f, "</parents>");
  
  return 1 + n;
}


//----------------------------------------------------------------------------//
// Interface
//----------------------------------------------------------------------------//

/** @return whether a pointer to Rtti structure (Complete Object Locator) is stored at pos. */
static isPtrToRtti(pos) {
  /* struct _s_RTTICompleteObjectLocator {
    DWORD signature; // always zero
    DWORD offset;    // offset of this vtable in the class
    DWORD cdOffset;  // constructor call offset
    struct TypeDescriptor* pTypeDescriptor;                   // TypeDescriptor of the class
    struct _s_RTTIClassHierarchyDescriptor* pClassDescriptor; // inheritance hierarchy
  }; */
  
  auto pCol;
  pCol = Dword(pos);
  return isPtrToData(pos) && isPtrToTypeInfo(pCol + 12) && isPtrToChd(pCol + 16);
}

/** @return mangled class name of a class corresponding to RTTI structure (Complete Object Locator) at pos. */
static getRttiMangledClassName(pos) {
  auto s;
  s = decodeString(Dword(pos + 12) + 12);
  return substr(s, 0, strlen(s) - 2);
}

/** @return demangled class name of a class corresponding to RTTI structure (Complete Object Locator) at pos. */
static getRttiDeMangledClassName(pos) {
  return decodeClassNameFromTypeInfo(Dword(pos + 12));
}

/** Dump the Rtti structure (Complete Object Locator) at pos, which corresponds to vtable at vtpos. */
static dumpRtti(f, pos) {
  auto pCol, pChd, pBca;
  
  pCol = pos;
  pChd = Dword(pCol + 16);
  pBca = Dword(pChd + 12);
  
  dumpBca(f, pBca);
  fprintf(f, "\n");
}


#endif
















