program Launcher;

uses
  Forms,
  Windows,
  Mainform in 'Mainform.pas' {Form1},
  Prefences in 'Prefences.pas' {Form2},
  About in 'About.pas' {AboutBox};

{$R *.RES}

var PredWindHandle: HWND;
begin
  Application.Initialize;

  PredWindHandle:=FindWindow('TForm1','Система обучения программированию');
  if (PredWindHandle<>0) then
    begin;
    SetForegroundWindow(PredWindHandle);
    Application.Terminate;
    end;//}
  Application.Title:='How to Prog';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
