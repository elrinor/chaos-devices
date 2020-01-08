#include <Windows.h>
#include <tchar.h>


int Main()
{
  TCHAR* lpCmdLine = GetCommandLine();
  HINSTANCE hInstance = GetModuleHandle(NULL);
  
  if(RegCreateKeyEx(HKEY_LOCAL_MACHINE, _T("SOFTWARE\Katobobr"), 0, NULL, REG_OPTION_NON_VOLATILE, KEY_QUERY_VALUE, NULL, Key, NULL) = ERROR_SUCCESS)
    if(RegQueryValueEx(Key, _T("Path"), NULL, nil, PByte(@Buf), @Size) = ERROR_SUCCESS)
      Edit3.Text := Buf;

  return 0;
}