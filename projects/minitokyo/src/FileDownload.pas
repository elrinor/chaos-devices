unit FileDownload;

interface
uses Classes, IdHTTP, IdComponent;

type
  TProxyParams = record
    ProxyServer, ProxyUsername, ProxyPassword:String;
    BasicAuthentication, UseProxy:Boolean;
    ProxyPort:Integer;
  end;
  TDownloadError = record
    ErrorMessage:String;
    ErrorCode:Integer;
  end;

  TBeginDownloadEvent = procedure (Sender:TObject; BytesTotal,Tag:Integer) of object;
  TWorkDownloadEvent  = procedure (Sender:TObject; BytesDownloaded,Tag:Integer) of object;
  TEndDownloadEvent   = procedure (Sender:TObject; Tag:Integer) of object;
  TErrorDownloadEvent = procedure (Sender:TObject; Error:TDownloadError) of object;

function DownloadFile(
  const URL:String; Dest:TStream; const Tag:Integer;
  OnBegin:TBeginDownloadEvent; OnWork:TWorkDownloadEvent; OnEnd:TEndDownloadEvent; OnError:TErrorDownloadEvent;
  BasicAuthentication,UseProxy:Boolean; const ProxyServer, ProxyUsername, ProxyPassword:String;
  const ProxyPort,MaxTryCount:Integer
                      ):TThread;

implementation
uses SysUtils;

const
  DefaultPriority:TThreadPriority = tpLower;

type
  TDownloadThread = class(TThread)
  private
    FHTTP:TIdHTTP;                // HTTP object (used for download)
    FURL:String;                  // URL to download
    FDest:TStream;                // stream for writing the downloaded file
    FOnBegin:TBeginDownloadEvent;
    FOnWork:TWorkDownloadEvent;
    FOnEnd:TEndDownloadEvent;
    FOnError:TErrorDownloadEvent;
    //FResult:TDownloadResult       //
    //FWorkMax:Integer;
    FTag:Integer;                 // for external use (for handling events with the same handler)
    FMaxTryCount:Integer;       // max Get tries.
  protected
    procedure OnHTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Integer);
    procedure OnHTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Integer);
    procedure Execute; override;
    procedure TerminateEvent(Sender:TObject);
  public
    constructor Create(const URL:String; Dest:TStream; OnBegin:TBeginDownloadEvent; OnWork:TWorkDownloadEvent; OnEnd:TEndDownloadEvent; OnError:TErrorDownloadEvent; Tag,MaxTryCount:Integer; ProxyParams:TProxyParams);
    destructor Destroy; override;
  end;

function DownloadFile(
  const URL:String; Dest:TStream; const Tag:Integer;
  OnBegin:TBeginDownloadEvent; OnWork:TWorkDownloadEvent; OnEnd:TEndDownloadEvent; OnError:TErrorDownloadEvent;
  BasicAuthentication,UseProxy:Boolean; const ProxyServer, ProxyUsername, ProxyPassword:String;
  const ProxyPort,MaxTryCount:Integer
                      ):TThread;
var
  ProxyParams:TProxyParams;
begin;
ProxyParams.UseProxy:=UseProxy;
ProxyParams.ProxyServer:=ProxyServer;
ProxyParams.ProxyPort:=ProxyPort;
ProxyParams.BasicAuthentication:=BasicAuthentication;
ProxyParams.ProxyUsername:=ProxyUsername;
ProxyParams.ProxyPassword:=ProxyPassword;
Result:=TDownloadThread.Create(URL,Dest,OnBegin,OnWork,OnEnd,OnError,Tag,MaxTryCount,ProxyParams);
end;

procedure TDownloadThread.Execute;
var TryCount:Integer;
    Succeded:Boolean;
    Error:TDownloadError;
begin;
TryCount:=0;
repeat
  Inc(TryCount);
  try
    if Assigned(FOnWork) then
      FHTTP.OnWork:=Self.OnHTTPWork;
    FHTTP.OnWorkBegin:=Self.OnHTTPWorkBegin;
    FHTTP.Get(FURL,FDest);
    Succeded:=True;
  except
    on E:Exception do
      begin;
      Error.ErrorMessage:=E.Message;
      Error.ErrorCode:=0;
      if Assigned(FOnError) then
        FOnError(Self,Error);
      end;
  end;
until (TryCount>=FMaxTryCount)or(Succeded);
end;

procedure TDownloadThread.OnHTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Integer);
begin;
FOnWork(Self,AWorkCount,FTag);
end;

procedure TDownloadThread.OnHTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Integer);
begin;
if Assigned(FOnBegin) then FOnBegin(Self,AWorkCountMax,FTag);
end;

constructor TDownloadThread.Create(const URL:String; Dest:TStream; OnBegin:TBeginDownloadEvent; OnWork:TWorkDownloadEvent; OnEnd:TEndDownloadEvent; OnError:TErrorDownloadEvent; Tag,MaxTryCount:Integer; ProxyParams:TProxyParams);
begin;
FreeOnTerminate:=True;
FURL:=URL;
FDest:=Dest;
FOnBegin:=OnBegin;
FOnWork:=OnWork;
FOnEnd:=OnEnd;
FOnError:=OnError;
FTag:=Tag;
Priority:=DefaultPriority;
FHTTP:=TIdHTTP.Create;
FMaxTryCount:=MaxTryCount;
OnTerminate:=TerminateEvent;
If ProxyParams.UseProxy then
  begin;
  FHTTP.ProxyParams.ProxyServer:=ProxyParams.ProxyServer;
  FHTTP.ProxyParams.ProxyPort:=ProxyParams.ProxyPort;
  if ProxyParams.BasicAuthentication then
    begin;
    FHTTP.ProxyParams.ProxyUsername:=ProxyParams.ProxyUsername;
    FHTTP.ProxyParams.ProxyPassword:=ProxyParams.ProxyPassword;
    FHTTP.ProxyParams.BasicAuthentication:=ProxyParams.BasicAuthentication;
    end;
  end;
inherited Create(False);
end;

destructor TDownloadThread.Destroy;
begin;
FHTTP.Free;
{if Assigned(FOnEnd) then FOnEnd(Self,FTag);}
inherited Destroy;
end;

procedure TDownloadThread.TerminateEvent(Sender:TObject);
begin;
if Assigned(FOnEnd) then FOnEnd(Self,FTag);
end;

end.
