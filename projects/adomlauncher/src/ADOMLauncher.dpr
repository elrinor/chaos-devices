program ADOMLauncher;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  enterbackupname in 'enterbackupname.pas' {PasswordDlg},
  Windows;

{$R *.RES}

var PredWindHandle: HWND;
begin
  Application.Initialize;

  PredWindHandle:=FindWindow('TForm1','ADOM Launcher');
  if (PredWindHandle<>0) then
    begin;
    SetForegroundWindow(PredWindHandle);
    Application.Terminate;
    end;//}
  Application.Title := 'ADOM Launcher';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TPasswordDlg, PasswordDlg);
  Application.Run;
end.
