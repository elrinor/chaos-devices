unit Log;

interface
Procedure AddLogMessage(const LogMessage:string);

implementation
uses SysUtils;

var f:TextFile;

Procedure AddLogMessage(const LogMessage:string);
begin;
Writeln(f,LogMessage);
end;


initialization
AssignFile(f,ExtractFilePath(ParamStr(0))+'TextLog.txt');
Rewrite(f);

finalization
close(f);

end.
