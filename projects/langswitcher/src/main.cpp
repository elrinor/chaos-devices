#define _WIN32_WINNT 0x0401
#include <Windows.h>
#include <WinUser.h>

//#include <stdio.h>

HHOOK hHook;

//FILE* f = fopen("out.txt", "w");

BOOL Pressed[255];
#define ShiftPressed (Pressed[VK_LSHIFT] || Pressed[VK_RSHIFT])
#define AltPressed   (Pressed[VK_LMENU] || Pressed[VK_RMENU])
#define CtrlPressed  (Pressed[VK_LCONTROL] || Pressed[VK_RCONTROL])

LRESULT CALLBACK LowLevelKeyboardProc(INT nCode, WPARAM wParam, LPARAM lParam)
{
  static DWORD MaxSimultKeys = 0;
  static DWORD SimultKeys = 0;

  BOOL Switchable = FALSE;
  BOOL SwitchEnRu = FALSE;
  BOOL SwitchJp = FALSE;

  if(nCode == HC_ACTION)
  {
    KBDLLHOOKSTRUCT *pkbdllhook = (KBDLLHOOKSTRUCT *)lParam;
/*
    switch (wParam)
    {
    case WM_SYSKEYDOWN:
    case WM_KEYDOWN:
      fprintf(f, "v ");
      break;
    case WM_KEYUP:
    case WM_SYSKEYUP:
      fprintf(f, "^ ");
    }
    fprintf(f, "%X\n", pkbdllhook->vkCode);
    if(pkbdllhook->vkCode == VK_ESCAPE)
    {
      fclose(f);
      ExitProcess(0);
    }
*/
    if(!(pkbdllhook->vkCode >= 0xE9 && pkbdllhook->vkCode <= 0xF5)) // OEM Specific
    {
      switch (wParam)
      {
      case WM_SYSKEYDOWN:
      case WM_KEYDOWN:
        if(!Pressed[pkbdllhook->vkCode])
          SimultKeys++;
        if(MaxSimultKeys < SimultKeys)
          MaxSimultKeys = SimultKeys;
        Pressed[pkbdllhook->vkCode] = TRUE;
        break;
      case WM_KEYUP:
      case WM_SYSKEYUP:
        if(Pressed[pkbdllhook->vkCode])
          SimultKeys--;
        if(SimultKeys == 0)
          MaxSimultKeys = 0;
        if(SimultKeys == 1 && MaxSimultKeys == 2)
          Switchable = TRUE;
        Pressed[pkbdllhook->vkCode] = FALSE;
        switch (pkbdllhook->vkCode)
        {
        case VK_LCONTROL:
        case VK_RCONTROL:
          if(ShiftPressed && !AltPressed)
            SwitchEnRu = TRUE;
          break;
        case VK_LMENU:
        case VK_RMENU:
          if(ShiftPressed && !CtrlPressed)
            SwitchJp = TRUE;
          break;
        case VK_LSHIFT:
        case VK_RSHIFT:
          if(AltPressed && !CtrlPressed)
            SwitchJp = TRUE;
          else if(CtrlPressed && !AltPressed)
            SwitchEnRu = TRUE;
          break;
        }
        break;
      }
    }
  }

  if(Switchable)
  {
    if(SwitchEnRu)
    {
      DWORD dwPID;
      HWND hWnd = GetForegroundWindow();
      DWORD dwTID = GetWindowThreadProcessId(hWnd, &dwPID);
      HKL hKl = GetKeyboardLayout(dwTID);
      HKL hNewKl;
      if(hKl == (HKL)0x04090409) // US English
        hNewKl = (HKL)0x04190419; // Russian
      else
        hNewKl = (HKL)0x04090409; // US English
      PostMessage(hWnd, WM_INPUTLANGCHANGEREQUEST, 0, (LPARAM)hNewKl);
      //PostMessage(hWnd, WM_INPUTLANGCHANGEREQUEST, INPUTLANGCHANGE_FORWARD, 0);
    }
    else if(SwitchJp)
    {
      HWND hWnd = GetForegroundWindow();
      DWORD dwTID = GetWindowThreadProcessId(hWnd, NULL);
      HKL hKl = GetKeyboardLayout(dwTID);
      USHORT LID = (DWORD)hKl & 0x0000FFFF;
      if(LID != 0x0411)
        PostMessage(hWnd, WM_INPUTLANGCHANGEREQUEST, 0, (LPARAM)0x04110411); // Japanese
    }
  }
  
  return CallNextHookEx(hHook, nCode, wParam, lParam);
}

void failed(const char *msg) 
{
  DWORD fm;
  char *msg1, *msg2;
  const char *args[2];

  fm = FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER|FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
                     NULL, GetLastError(), 0, (LPTSTR)&msg1, 0, NULL);
  if(fm == 0)
    ExitProcess(1);
  args[0] = msg;
  args[1] = msg1;
  fm=FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER|FORMAT_MESSAGE_FROM_STRING|FORMAT_MESSAGE_ARGUMENT_ARRAY,
                   "%1: %2", 0, 0, (LPTSTR)&msg2, 0, (va_list*)&args[0]);
  if(fm == 0)
    ExitProcess(1);
  MessageBox(NULL, msg2, "Error", MB_OK|MB_ICONERROR);
  ExitProcess(1);
}

BOOL CALLBACK EnumWindowsProc(HWND hwnd, LPARAM lParam)
{
  return TRUE;
}


int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, INT nCmdShow)
{
  memset(Pressed, 0, 256 * sizeof(BOOL));

  /*
  HWND hWnd = FindWindow("CiceroUIWndFrame", "TF_FloatingLangBar_WndTitle");
  BOOL r;
  while((r = EnumChildWindows(hWnd, EnumWindowsProc, 0)) == 0);
  */
  
  hHook = SetWindowsHookEx(WH_KEYBOARD_LL, LowLevelKeyboardProc, hInstance, 0);

  MSG msg;
  while (GetMessage(&msg, NULL, 0, 0))
  {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }

  UnhookWindowsHookEx(hHook);
  ExitProcess(0);
}