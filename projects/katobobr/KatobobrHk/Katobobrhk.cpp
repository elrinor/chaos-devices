// Katobobrhk.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"


#ifdef _MANAGED
#pragma managed(push, off)
#endif

HHOOK hMsgHook;
HHOOK hCBTHook;
HMODULE hInstance;

BOOL APIENTRY DllMain(HMODULE hModule, DWORD ul_reason_for_call, LPVOID lpReserved)
{
  hInstance = hModule;
  return TRUE;
}

LRESULT CALLBACK CBTProc(int code, WPARAM wParam, LPARAM lParam)
{
  switch(code) 
  {
  case HCBT_ACTIVATE:
  	return 1; //prevent
  case HCBT_MINMAX:
    switch(lParam & 0xFFFF) 
    {
    case SW_MAXIMIZE:
    case SW_RESTORE:
    case SW_SHOW:
    case SW_SHOWDEFAULT:
    // case SW_SHOWMAXIMIZED: 
    case SW_SHOWMINIMIZED:
    case SW_SHOWMINNOACTIVE:
    case SW_SHOWNA:
    case SW_SHOWNOACTIVATE:
    case SW_SHOWNORMAL:
      return 1; //prevent
    }
  }
  return 0; // allow
}

LRESULT CALLBACK GetMsgProc(int code, WPARAM wParam, LPARAM lParam)
{
  MSG* pMsg = (MSG*)lParam;
  //MessageBox(pMsg->hwnd, "Aaaa", "Aaaa", 0);
  switch(pMsg->message) 
  {
  case WM_SHOWWINDOW:
  case WM_SETFOCUS:
  case WM_ACTIVATE:
  case WM_NCACTIVATE:
    pMsg->message = WM_SHOWWINDOW;
    pMsg->wParam = FALSE;
    pMsg->lParam = 0;
  	break;
  }
  return CallNextHookEx(hMsgHook, code, wParam, lParam);
}

BOOL APIENTRY InstallHook(HWND hWnd)
{
  DWORD dwThreadID;
  if((dwThreadID = GetWindowThreadProcessId(hWnd, NULL)) == NULL)
    return FALSE;
  if((hMsgHook = SetWindowsHookEx(WH_GETMESSAGE, GetMsgProc, hInstance, dwThreadID)) == NULL)
    return FALSE;
  if((hCBTHook = SetWindowsHookEx(WH_CBT, CBTProc, hInstance, dwThreadID)) == NULL)
    return FALSE;
  return TRUE;
}

BOOL APIENTRY RemoveHook()
{
  UnhookWindowsHookEx(hMsgHook);
  UnhookWindowsHookEx(hCBTHook);
  return TRUE;
}

#ifdef _MANAGED
#pragma managed(pop)
#endif

