unit ConsoleUtils;

interface
type SetOfChar = Set of Char;

procedure GotoXY(const X, Y : Word);
procedure Clrscr(const Color,BkColor:Word);
procedure ShowCursor(const Show:boolean);
procedure Put(const X,Y:integer; const Color,BkColor:word; const S:String);
procedure PutColored(const X,Y:Integer; const S:String);
Procedure SetColor(const X,Y,Length:Integer; const Color,BkColor:word);
function  ReadStrUsingCharSet(const X,Y:integer; const Color,BkColor:word; MaxLength:integer; const CharSet:SetOfChar):string;
function  ReadKey : Char;
procedure Delay(const n: Integer);
function Ascii(const S: String) : String;
Procedure SetConsTitle(const s:string);

const
  CEmptyString = '                                                                                ';


implementation
uses SysUtils, Windows;

var
 MaxX, MaxY : Word;                   // ������������ ������� ����             
 ConOutHandle, ConInHandle : THandle; // ���������� ����������� ����
 Coord : TCoord;                      // ��� ��������/��������� ������� ������
 NOAW : Cardinal;                     // ��� �������� ����������� ��������� ������� ������ MyCrt


//------------------------------------------------------------------------------
// ��������� ��������� �������
//------------------------------------------------------------------------------
Procedure SetConsTitle(const s:string);
begin;
SetConsoleTitle(PChar(S));
end;

//------------------------------------------------------------------------------
// ��������
//------------------------------------------------------------------------------
procedure Delay(const n: Integer);
var   start: LongInt;
begin
start := GetTickCount;
repeat {     Application.ProcessMessages;}
until (GetTickCount - start) >= n;
end;


//------------------------------------------------------------------------------
// ������� ������ �� ��������� Windows (Koi8-R) � ��������� Dos
//------------------------------------------------------------------------------
function Ascii(const S: String) : String;
begin
SetLength(Result, Length(S));
CharToOem(PChar(S), PChar(Result));
end;

//------------------------------------------------------------------------------
// ��������� ����������� ��� ����������� �����
//------------------------------------------------------------------------------
function GetConInputHandle : THandle;
begin
Result := GetStdHandle(STD_INPUT_HANDLE)
end;

//------------------------------------------------------------------------------
// ��������� ����������� ��� ����������� ������
//------------------------------------------------------------------------------
function GetConOutputHandle : THandle;
begin
Result := GetStdHandle(STD_OUTPUT_HANDLE)
end;

//------------------------------------------------------------------------------
// ��������� ������� � ���������� X, Y
//------------------------------------------------------------------------------
procedure GotoXY(const X,Y:Word);
begin
Coord.X := X - 1; Coord.Y := Y - 1;
SetConsoleCursorPosition(ConOutHandle, Coord);
end;

//------------------------------------------------------------------------------
// ������� ������ - ���������� ��� ���������
//------------------------------------------------------------------------------
procedure Clrscr(const Color,BkColor:Word);
begin
Coord.X := 0; Coord.Y := 0;
FillConsoleOutputCharacter(ConOutHandle, ' ', MaxX*MaxY, Coord, NOAW);
FillConsoleOutputAttribute(ConOutHandle, BkColor*16+Color, MaxX*MaxY, Coord,NOAW);
GotoXY(0, 0);
end;

//------------------------------------------------------------------------------
// ����������/�������� ������
//------------------------------------------------------------------------------
procedure ShowCursor(const Show:boolean);
var CursorInfo : _Console_Cursor_Info;
begin
If (Show = False) then begin; CursorInfo.bVisible := True; CursorInfo.dwSize := 100; end;
if (Show = True)  then begin; CursorInfo.bVisible := True; CursorInfo.dwSize := 1; end;
SetConsoleCursorInfo(ConOutHandle, CursorInfo);
end;

//------------------------------------------------------------------------------
// �������� ������ � �������� ����� ����� ������
//------------------------------------------------------------------------------
procedure Put(const X,Y:integer; const Color,BkColor:word; const S:String);
begin
 Coord.X := X - 1; Coord.Y := Y - 1;
 FillConsoleOutputAttribute (ConOutHandle, BkColor*16+Color, Length(S), Coord, NOAW);
 WriteConsoleOutputCharacter(ConOutHandle, PChar(S), Length(S), Coord, NOAW);
end;

//------------------------------------------------------------------------------
// ����������� ��������� ������� � �������� ����� ����� ������
//------------------------------------------------------------------------------
procedure PutColored(const X,Y:Integer; const S:String);
var i, i1 : Integer;
begin
Coord.X := X - 1; Coord.Y := Y - 1;
i := 0; i1 := 0;
  repeat
  inc(i);
  if (S[i]='�') then
  begin;
    if i1>0 then begin;
      FillConsoleOutputAttribute (ConOutHandle, StrToInt(Copy(S,i1+1,3)), i-i1-4, Coord, NOAW);
      WriteConsoleOutputCharacter(ConOutHandle, PChar(Copy(S,i1+4,i-i1-4)), i-i1-4, Coord, NOAW);
      Coord.X := Coord.X + i - i1 - 4;
    end;
    i1 := i;
  end;
  until (i=Length(S));
end;

//------------------------------------------------------------------------------
// ������ ���� ������ ������
//------------------------------------------------------------------------------
Procedure SetColor(const X,Y,Length:Integer; const Color,BkColor:word);
begin;
 Coord.X := X - 1; Coord.Y := Y - 1;
 FillConsoleOutputAttribute (ConOutHandle, BkColor*16+Color, Length, Coord, NOAW);
end;

//------------------------------------------------------------------------------
// ��������� �������
//------------------------------------------------------------------------------
function ReadKey : Char;
var EventsRead : DWord;
    IBuff : _Input_Record;
begin;

repeat
repeat
ReadConsoleInput(ConInHandle, IBuff, 1, EventsRead);
until (EventsRead > 0) and (IBuff.EventType = KEY_EVENT);
until (IBuff.Event.KeyEvent.bKeyDown = True);

case IBuff.Event.KeyEvent.wVirtualKeyCode of
33: Result := #45;
34: Result := #43;
35: Result := #31;
36: Result := #30;
37: Result := #52;
38: Result := #56;
39: Result := #54;
40: Result := #50;
45: Result := #47;
46: Result := #42;
else Result := IBuff.Event.KeyEvent.AsciiChar;
end;

end;

//------------------------------------------------------------------------------
// ��������� ������ � �������� charset'�� �������� ����� � �������� ����� �������� ������
//------------------------------------------------------------------------------
function ReadStrUsingCharSet(const X,Y:integer; const Color,BkColor:word; MaxLength:integer; const CharSet:setofchar):string;
var C : char;
    i : Integer;
    CursorInfo : _Console_Cursor_Info;
begin;
CursorInfo.bVisible := True; CursorInfo.dwSize := 1; SetConsoleCursorInfo(ConOutHandle, CursorInfo);

Result:='';
Coord.X := X - 1; Coord.Y := Y - 1;
if (MaxLength+X-1)>MaxX then MaxLength:=MaxX-X+1;

FillConsoleOutputAttribute (ConOutHandle, BkColor*16+Color, MaxLength, Coord, NOAW);
FillConsoleOutputCharacter(ConOutHandle, ' ', MaxLength, Coord, NOAW);

i:=1;

repeat
  Coord.X := X-2+i;
  SetConsoleCursorPosition(ConOutHandle, Coord);
  C := ReadKey;
  if (C in CharSet) then if i<=MaxLength then
    begin;
    Result := Result+C;
    WriteConsoleOutputCharacter(ConOutHandle, PChar(Ascii(C)), 1, Coord, NOAW);
    Inc(i);
    end;
  if (C=#8) then if i>1 then
    begin;
    Dec(i);
    Coord.X := Coord.X-1;
    WriteConsoleOutputCharacter(ConOutHandle, ' ', 1, Coord, NOAW);
    Delete(Result, i, 1);
    end;
until (C=#27) or (C=#13);

CursorInfo.bVisible := True; CursorInfo.dwSize := 100; SetConsoleCursorInfo(ConOutHandle, CursorInfo);
end;

//------------------------------------------------------------------------------
// ���������� ���������� �������
//------------------------------------------------------------------------------
function ConProc(CtrlType:DWord) : Bool; stdcall; far;
var
 S : String;
begin
 case CtrlType of
   CTRL_C_EVENT        : S := 'CTRL_C_EVENT';
   CTRL_BREAK_EVENT    : S := 'CTRL_BREAK_EVENT';
   CTRL_CLOSE_EVENT    : S := 'CTRL_CLOSE_EVENT';
   CTRL_LOGOFF_EVENT   : S := 'CTRL_LOGOFF_EVENT';
   CTRL_SHUTDOWN_EVENT : S := 'CTRL_SHUTDOWN_EVENT';
  else
   S := 'UNKNOWN_EVENT';
 end;
 Result := True;
end;

//------------------------------------------------------------------------------
//               �������������
//------------------------------------------------------------------------------
begin
// �������� ���������� ������ (output)
 ConOutHandle := GetConOutputHandle;
 ConInHandle := GetConInputHandle;
// �������� ������������ ������� ����
 MaxX := 80;
 MaxY := 25;


// ������������� ���������� �������
 SetConsoleCtrlHandler(@ConProc, True);

end.





