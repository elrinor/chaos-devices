program PuttyWnd;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  Messages;

var
  h: HWND;
begin
  h := FindWindow('PuTTY', 0);
  if(ParamStr(1) <> '') then
  try
    h := StrToInt('$' + ParamStr(1))
  except
  on Exception do
    h := FindWindow(PAnsiChar(ParamStr(1)), 0);
  end;
  //  SendMessage(h, WM_SHOWWINDOW, 0, 0);
  if(ShowWindow(h, SW_SHOWNA)) then
    ShowWindow(h, SW_HIDE);
end.
 