unit ArXRunOnce;

interface

implementation
uses Windows, Forms, SysUtils;
var
  UniqueMapping: THandle;
  MappingName: PChar;

initialization
if not IsConsole then
  MappingName:=PChar(Application.Title)
else
  MappingName:=PChar(ExtractFileName(ParamStr(0)));
UniqueMapping:=CreateFileMapping($FFFFFFFF, nil, PAGE_READONLY, 0, 32, MappingName);
if UniqueMapping=0 then
  begin;
  // Code if error occurs
  end
else if GetLastError=ERROR_ALREADY_EXISTS then
  begin;
  // Code if the are two copies
  Halt;
  end;

finalization

end.
