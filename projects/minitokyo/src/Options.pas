unit options;

interface
var
  MainFolder:String;
  ProxyServer:String;
  ProxyUsername:String;
  ProxyPassword:String;
  BasicAuthentication:Boolean;
  ProxyPort:Integer;
  UseProxy:Boolean;
  DoNotDownload:Boolean;
  DoNotReDownload:Boolean;
  DownloadListType:Integer;

implementation
uses IniFiles,SysUtils,FileCtrl,Forms;

var
  Opt:TIniFile;

procedure ReadOptions;
begin;
Opt:=TIniFile.Create(ExtractFilePath(Application.ExeName)+'Options.ini');
Opt.UpdateFile;
MainFolder:=Opt.ReadString('MiniTokyoDownloader','MainFolder','C:\');
ProxyServer:=Opt.ReadString('MiniTokyoDownloader','ProxyServer','');
ProxyUsername:=Opt.ReadString('MiniTokyoDownloader','ProxyUsername','');
ProxyPassword:=Opt.ReadString('MiniTokyoDownloader','ProxyPassword','');
BasicAuthentication:=Opt.ReadBool('MiniTokyoDownloader','BasicAuthentication',False);
ProxyPort:=Opt.ReadInteger('MiniTokyoDownloader','ProxyPort',8080);
UseProxy:=Opt.ReadBool('MiniTokyoDownloader','UseProxy',False);
DoNotDownload:=Opt.ReadBool('MiniTokyoDownloader','DoNotDownload',False);
DoNotReDownload:=Opt.ReadBool('MiniTokyoDownloader','DoNotReDownload',True);
DownloadListType:=Opt.ReadInteger('MiniTokyoDownloader','DownloadListType',0);
Opt.Free;
end;

procedure WriteOptions;
begin;
Opt:=TIniFile.Create(ExtractFilePath(Application.ExeName)+'Options.ini');
Opt.UpdateFile;
Opt.WriteString('MiniTokyoDownloader','MainFolder',MainFolder);
Opt.WriteString('MiniTokyoDownloader','ProxyServer',ProxyServer);
Opt.WriteString('MiniTokyoDownloader','ProxyUsername',ProxyUsername);
Opt.WriteString('MiniTokyoDownloader','ProxyPassword',ProxyPassword);
Opt.WriteBool('MiniTokyoDownloader','BasicAuthentication',BasicAuthentication);
Opt.WriteInteger('MiniTokyoDownloader','ProxyPort',ProxyPort);
Opt.WriteBool('MiniTokyoDownloader','UseProxy',UseProxy);
Opt.WriteBool('MiniTokyoDownloader','DoNotDownload',DoNotDownload);
Opt.WriteBool('MiniTokyoDownloader','DoNotReDownload',DoNotReDownload);
Opt.WriteInteger('MiniTokyoDownloader','DownloadListType',DownloadListType);
Opt.Free;
end;

initialization
ReadOptions;

finalization
WriteOptions;

end.
