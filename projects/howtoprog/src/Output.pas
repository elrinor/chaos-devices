unit Output;

interface
var PosX,PosY,ScreenPosX,ScreenPosY:Integer;

Procedure MoveCursor(var X1,Y1:Integer;X2,Y2:Integer);
function FillStringToLength(const s:string; const CharToFill:char; const NeededLength:Integer):string;
function ConditionalAscii(const S:string):string;
Procedure DrawStringOutput(s:string);
Procedure ShowHint(s:string);
Procedure SpecGotoXY(const X,Y:Word);

implementation
uses HowToProgStringList,Options,ConsoleUtils,SysUtils,Math,HTPConstants,ConsoleInterface;
//============================================================================\\
//  Полезные функции и процедуры
//============================================================================\\
function FillStringToLength(const s:string; const CharToFill:char; const NeededLength:Integer):string;
begin;
Result:=S;
while Length(Result)<NeededLength do
  begin;
  Result:=CharToFill+Result;
  end;
end;

//============================================================================\\
//  Процедуры связанные с выводом информации
//============================================================================\\
// Передвижение экрана
procedure MoveScreen(X,Y:integer);
var ReDraw:Boolean;
begin;
ReDraw:=False;
if (X<ScreenPosX) then begin;ScreenPosX:=X;ReDraw:=True;end;
if (Y<ScreenPosY) then begin;ScreenPosY:=Y;ReDraw:=True;end;
if (X>ScreenPosX+SCREEN_WIDTH-1) then begin;ScreenPosX:=X-SCREEN_WIDTH+1;ReDraw:=True;end;
if (Y>ScreenPosY+SCREEN_HEIGHT-1) then begin;ScreenPosY:=Y-SCREEN_HEIGHT+1;ReDraw:=True;end;
if ReDraw then
  begin;
  Strings.ReDrawStrings(1);
  ReDrawScrollBars(ScreenPosX,ScreenPosY);
  end;
end;

// Перекодирование
function ConditionalAscii(const S:string):string;
begin;
if not(DosText) then Result:=Ascii(S)
else Result:=S;
end;

// Перевод курсора в x,y
Procedure SpecGotoXY(const X,Y:Word);
begin;
Put(7,21,Colors[clCodeWindowBorder],Colors[clCodeWindowBack],FillStringToLength(' '+IntToStr(X-1),#205,3)+':'+IntToStr(Y-1)+' '+#205);
MoveScreen(X-1,Y-1);
GotoXY(X-ScreenPosX+1,Y-ScreenPosY+1);
end;

// Передвижение курсора по X
Procedure MoveCursorX(const x1,x2,y:integer);
var i:integer;
begin;
if x1>x2 then for i:=x1-1 downto x2 do begin;Specgotoxy(i,y);Delay(BaseDelayFactor);end
else if x1<x2 then for i:=x1+1 to x2 do begin;Specgotoxy(i,y);Delay(BaseDelayFactor);end;
end;

// Передвижение курсора по Y
Procedure MoveCursorY(const y1,y2,x:integer);
var i:integer;
begin;
if y1>y2 then for i:=y1-1 downto y2 do begin;Specgotoxy(x,i);Delay(BaseDelayFactor);end
else if y1<y2 then for i:=y1+1 to y2 do begin;Specgotoxy(x,i);Delay(BaseDelayFactor);end;
end;

// Перемещение курсора из (X1,Y1) в (X2,Y2)
Procedure MoveCursor(var X1,Y1:Integer;X2,Y2:Integer);
begin;
Inc(X1);
Inc(X2);
Inc(Y1);
Inc(Y2);
if HomeAllowed then
  begin;
  if x1-x2>x2 then
    begin;SpecGotoXY(2,y1);Delay(BaseDelayFactor);MoveCursorX(2,x2,y1);end
  else
    MoveCursorX(x1,x2,y1);
  end
else
  MoveCursorX(x1,x2,y1);
MoveCursorY(y1,y2,x2);
x1:=x2-1;y1:=y2-1;
end;


//============================================================================\\
//  Процедуры связанные с манипулированием строками
//============================================================================\\
// Вывод печатания строки
Procedure DrawStringOutput(s:string);
var i:integer;
begin;
i:=0;
repeat
inc(i);
{if s[i]='~' then
  begin;
  i:=i+1;
  if s[i]='~' then
    begin;
    Strings.InsertCharacter(PosY,PosX,s[i]);
    Put(2,PosY+1,14,1,Strings[PosY]);
    inc(PosX);
    end;
  if s[i]='j' then
    begin;
    PosX:=StrToInt(Copy(s,i+1,2));
    i:=i+3;
    end;
  if s[i]='w' then
    begin;
    Delay(BaseDelayFactor*StrToInt(Copy(s,i+1,3)));
    i:=i+4;
    end;
  if s[i]='W' then
    begin;
    Delay(StrToInt(Copy(s,i+1,3)));
    i:=i+4;
    end;
  if s[i]='f' then begin;MoveCursorX(PosX+1,PosX+1+StrToInt(copy(s,i+1,2)),PosY+1);PosX:=PosX+StrToInt(copy(s,i+1,2));i:=i+3;end;
  if s[i]='b' then begin;MoveCursorX(PosX+1,PosX+1-StrToInt(copy(s,i+1,2)),PosY+1);PosX:=PosX-StrToInt(copy(s,i+1,2));i:=i+3;end;
  if (s[i]='d') then
    if (StrToInt(Copy(S,i+1,2))>0)then
      begin;
      Strings.DeleteCharacters(PosY,PosX-1,1);
      Put(2,PosY+1,14,1,Copy(CEmptyString,1,78));
      Put(2,PosY+1,14,1,Strings[PosY]);
      dec(PosX);
      Insert(FillStringToLength(IntToStr(StrToInt(Copy(s,i+1,2))-1),'0',2),s,i+3);
      Delete(S,i+1,2);
      i:=i-2;
      end
    else
      i:=i+3;
  end
else}
  begin;
  Strings.InsertCharacter(PosY,PosX,s[i]);
  Strings.ReDrawString(PosY);//Put(2,n+1,14,1,Strings[n]);
  inc(PosX);
  end;
Specgotoxy(PosX+1,PosY+1);            
Delay(BaseDelayFactor);
until (i>=Length(s));
end;

// Вывод хинта
Procedure ShowHint(s:string);
var i:integer;
    key:char;
begin;
repeat
if (Length(s)<(MAX_COMMENT_LENGTH-8)) then
  begin;
  Put(2,23,Colors[clCommentColor],Colors[clCommentWindowBack],ConditionalAscii(s)+Ascii(' (дальше)'));
  repeat key:=readkey; until key=' ';
  Put(2,23,Colors[clCommentColor],Colors[clCommentWindowBack],Copy(CEmptyString,1,78));
  s:='';
  end
else
  begin;
  for i:=Min(79,Length(S)) downto 1 do begin;if s[i]=' ' then Break;end;
  Put(2,23,Colors[clCommentColor],Colors[clCommentWindowBack],ConditionalAscii(Copy(s,1,i-1)));
  Delete(S,1,i);
  if Length(s)>=(MAX_COMMENT_LENGTH-8) then
    begin;
    for i:=(MAX_COMMENT_LENGTH-8) downto 1 do begin;if s[i]=' ' then Break;end;
    Put(2,24,Colors[clCommentColor],Colors[clCommentWindowBack],ConditionalAscii(Copy(s,1,i-1))+Ascii(' (дальше)'));
    Delete(S,1,i);
    repeat key:=readkey; until key=' ';
    Put(2,23,Colors[clCommentColor],Colors[clCommentWindowBack],Copy(CEmptyString,1,78));
    Put(2,24,Colors[clCommentColor],Colors[clCommentWindowBack],Copy(CEmptyString,1,78));
    end
  else
    begin;
    Put(2,24,Colors[clCommentColor],Colors[clCommentWindowBack],ConditionalAscii(s)+Ascii(' (дальше)'));
    repeat key:=readkey; until key=' ';
    Put(2,23,Colors[clCommentColor],Colors[clCommentWindowBack],Copy(CEmptyString,1,78));
    Put(2,24,Colors[clCommentColor],Colors[clCommentWindowBack],Copy(CEmptyString,1,78));
    s:='';
    end;
  end;
until s='';
end;

initialization
PosY:=1;
PosX:=1;
ScreenPosY:=1;
ScreenPosX:=1;
end.
