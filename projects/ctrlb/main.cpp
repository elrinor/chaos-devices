#include <Windows.h>
#include <tchar.h>
#include <string>

typedef std::basic_string<TCHAR> string;

void DirWalk(string Path, string CrtlBName)
{
  WIN32_FIND_DATA FD;
  HANDLE hSearch;
  string Mask = Path + _T("*");

  if((hSearch = FindFirstFile(Mask.c_str(), &FD)) == INVALID_HANDLE_VALUE)
    return;

  do{
    if((FD.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0)
      MoveFile((Path + FD.cFileName).c_str(), (CrtlBName + FD.cFileName).c_str());
    else if(_tcscmp(FD.cFileName, _T(".")) != 0 && _tcscmp(FD.cFileName, _T("..")) != 0)
      DirWalk(Path + FD.cFileName + string(_T("\\")), CrtlBName + FD.cFileName + string(_T("_")));
  }while(FindNextFile(hSearch, &FD) != 0);
  
  FindClose(hSearch);

  return;
}

int APIENTRY _tWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow)
{
  DirWalk(_T(""), _T(""));
  return 0;
}