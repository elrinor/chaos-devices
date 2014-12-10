unit Downloader;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FileDownload;

type
  TDownloadCompleteEvent = procedure(Sender:TObject; URL,AdditionalInfo:String; F:TStream) of object;

  TDownloader = class(TCustomControl)
  private
    FOnEndDownload: TDownloadCompleteEvent;
    FProxyParams: TProxyParams;
    FURLs: TStringList;
    FAdditionalInfo: TStringList;
    FBusy: TList;
    FStreams: TList;
    FThreads: TList;
    FDownloadQueue: TStringList;
    FDownloadQueueInfo: TStringList;
    FMaxThreads: Integer;
    FCurrentThreads: Integer;
    FTextMask: String;
  protected
    procedure Paint; override;
    procedure EndHandler(Sender:TObject; Tag:Integer);
    procedure FreeDownload(Tag:Integer);
    procedure SetTextMask(Mask:String);
    procedure SetCurrentThreads(N:Integer);
    property CurrentThreads: Integer read FCurrentThreads write SetCurrentThreads;
  public
    procedure Download(URL:String; Priority:Integer; AdditionalInfo:String); overload;
    procedure Download(URL:String; Priority:Integer); overload;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnEndDownload: TDownloadCompleteEvent read FOnEndDownload write FOnEndDownload;
    property ProxyParams:TProxyParams read FProxyParams write FProxyParams;
    property TextMask:String read FTextMask write SetTextMask;
    property Font;
    property Color;
  end;

procedure Register;
function MakeProxyParams(ProxyServer, ProxyUsername, ProxyPassword:String;
    BasicAuthentication, UseProxy:Boolean;
    ProxyPort:Integer):TProxyParams;

implementation

Uses ExtCtrls, Gauges, HintLabel;

const HintLabelWidth=100;
      FieldWidth=5;
      MaxTryCount=1;
      CMaxThreads=10;


function MakeProxyParams(ProxyServer, ProxyUsername, ProxyPassword:String;
    BasicAuthentication, UseProxy:Boolean;
    ProxyPort:Integer):TProxyParams;
begin;
Result.ProxyServer:=ProxyServer;
Result.ProxyUsername:=ProxyUsername;
Result.ProxyPassword:=ProxyPassword;
Result.BasicAuthentication:=BasicAuthentication;
Result.UseProxy:=UseProxy;
Result.ProxyPort:=ProxyPort;
end;

//============================================================================\\
// TDownloader
//============================================================================\\
procedure TDownloader.SetCurrentThreads(N:Integer);
begin;
FCurrentThreads:=N;
Paint;
end;

procedure TDownloader.SetTextMask(Mask:String);
begin;
FTextMask:=Mask;
Paint;
end;

constructor TDownloader.Create(AOwner: TComponent);
var i:Integer;
begin;
inherited Create(AOwner);
FProxyParams:=MakeProxyParams('','','',False,False,0);
FMaxThreads:=CMaxThreads;
FCurrentThreads:=0;
FTextMask:='%d (%d)';
FBusy:=TList.Create;
FStreams:=TList.Create;
FThreads:=TList.Create;
FURLs:=TStringList.Create;
FAdditionalInfo:=TStringList.Create;
FDownloadQueue:=TStringList.Create;
FDownloadQueueInfo:=TStringList.Create;
for i:=1 to FMaxThreads do FURLs.Add('');
for i:=1 to FMaxThreads do FAdditionalInfo.Add('');
FBusy.Count:=FMaxThreads;
FStreams.Count:=FMaxThreads;
FThreads.Count:=FMaxThreads;
end;

destructor TDownloader.Destroy;
var i:Integer;
begin;
try
FBusy.Free;
for i:=0 to FMaxThreads-1 do
  if Assigned(TStream(FStreams[i])) then TStream(FStreams[i]).Free;
FStreams.Free;
FThreads.Free;
FURLs.Free;
FDownloadQueue.Free;
FDownloadQueueInfo.Free;
except
  on E:Exception do begin;end;
end;
inherited Destroy;
end;

procedure TDownloader.Paint;
begin;
with Self.Canvas do
  begin;
  Brush.Color := Self.Color;
  Brush.Style := bsSolid;
  FillRect(ClientRect);
  Font := Self.Font;
  TextRect(ClientRect,0,0,Format(Self.FTextMask,[Self.CurrentThreads,Self.FDownloadQueue.Count]));
  end;
end;

procedure TDownloader.Download(URL:String; Priority:Integer);
begin;
Download(URL,Priority,'');
end;

procedure TDownloader.Download(URL:String; Priority:Integer; AdditionalInfo:String);
var I:Integer;
begin;
For i:=0 to FMaxThreads-1 do if Boolean(FBusy[i]) then
  if FURLS[i]=URL then
    Exit;
for i:=0 to FDownloadQueue.Count-1 do
  if FDownloadQueue[i]=URL then
    Exit;
if CurrentThreads<FMaxThreads then
  begin;
  For i:=0 to FMaxThreads-1 do if not(Boolean(FBusy[i])) then
    begin;
    CurrentThreads:=CurrentThreads+1;
    FBusy[i]:=Pointer(True);
    FStreams[i]:=Pointer(TMemoryStream.Create);
    FURLs[i]:=URL;
    FAdditionalInfo[i]:=AdditionalInfo;
    FThreads[i]:=Pointer(
    DownloadFile(URL,
                 FStreams[i],
                 i,
                 {BeginHandler}nil,
                 {WorkHandler}nil,
                 EndHandler,
                 nil,
                 FProxyParams.BasicAuthentication,
                 FProxyParams.UseProxy,
                 FProxyParams.ProxyServer,
                 FProxyParams.ProxyUserName,
                 FProxyParams.ProxyPassword,
                 FProxyParams.ProxyPort,
                 MaxTryCount)
                    );
    Break;
    end;
  end
else
  begin;
  FDownloadQueue.Objects[FDownloadQueue.Add(URL)]:=TObject(Priority);
  FDownloadQueueInfo.Add(AdditionalInfo);
  Paint;
  end;
end;

procedure TDownloader.EndHandler(Sender:TObject;Tag:Integer);
var i,MaxPriority,MaxPriorityNumber:Integer;
    URL,AdditionalInfo:String;
begin;
if Assigned(FOnEndDownload) then
  FOnEndDownload(Self, FURLs[Tag], FAdditionalInfo[Tag], TStream(FStreams[Tag]));
FreeDownload(Tag);
if FDownloadQueue.Count<>0 then
  begin;
  MaxPriority:=Low(Integer);
  for i:=0 to FDownloadQueue.Count-1 do
    if Integer(FDownloadQueue.Objects[i])>MaxPriority then
      begin;
      MaxPriority:=Integer(FDownloadQueue.Objects[i]);
      MaxPriorityNumber:=i;
      end;
  URL:=FDownloadQueue[MaxPriorityNumber];
  FDownloadQueue.Delete(MaxPriorityNumber);
  AdditionalInfo:=FDownloadQueueInfo[MaxPriorityNumber];
  FDownloadQueueInfo.Delete(MaxPriorityNumber);
  Paint;
  Download(URL,MaxPriority,AdditionalInfo);
  end;
end;

procedure TDownloader.FreeDownload(Tag:Integer);
begin;
TStream(FStreams[Tag]).Free;
FThreads[Tag]:=nil;
FStreams[Tag]:=nil;
FBusy[Tag]:=Pointer(False);
CurrentThreads:=CurrentThreads-1;
end;



procedure Register;
begin
  RegisterComponents('Samples', [TDownloader]);
end;

end.
 