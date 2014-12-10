unit ArXLog;

interface
procedure LogMessage(S: String);

implementation
uses SysUtils;

var
  F:TextFile;

procedure LogMessage(S: String);
begin;
Writeln(F,DateTimeToStr(Now)+' -> '+S);
end;

initialization
Assign(F,ExtractFilePath(ParamStr(0))+'Log.txt');
ReWrite(F);
finalization
CloseFile(F);
end.
