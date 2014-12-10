unit Debug;

interface
{$I debug.inc}
uses Classes;

function ST(F: TStream; N:Integer):String;

implementation
uses Math, SysUtils;

// Reads string with length N from F and returns it
function ST(F: TStream; N:Integer):String;
type TCharArray=array[0..1023] of Char;
var LastP:Integer;
    Buf:^TCharArray;
begin;
GetMem(Buf,N+1);
FillChar(Buf^,N+1,' ');
Buf[N]:=#0;
LastP:=F.Position;
F.Read(Buf^, N);
Result:=PChar(Buf);
FreeMem(Buf,N+1);
F.Seek(LastP,soFromBeginning);
end;

end.
