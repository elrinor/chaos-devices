program MiniTokyoDownloader;

uses
  ArXRunOnce,
  Forms,
  Main in 'Main.pas' {Form1},
  DBase in 'DBase.pas',
  IntegerList in 'IntegerList.pas',
  DBaseList in 'DBaseList.pas',
  FileDownload in 'FileDownload.pas',
  options in 'Options.pas',
  IntegerSet in 'IntegerSet.pas',
  Debug in 'Debug.pas',
  Downloader in 'Downloader.pas',
  DownloadListOutput in 'DownloadListOutput.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
