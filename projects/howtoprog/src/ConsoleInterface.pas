unit ConsoleInterface;

interface
Procedure DrawInterface;
Procedure ReDrawScrollBars(const ScreenPosX,ScreenPosY:Integer);
Procedure DrawScrollBars;

implementation
uses ConsoleUtils,Options,HTPConstants;
var LastScreenPosX,LastScreenPosY:Integer;
    ScrollerX:Array[1..MAX_STRING_LENGTH-SCREEN_WIDTH+1]of Integer;
    ScrollerY:Array[1..MAX_STRING_NUMBER-SCREEN_HEIGHT+1]of Integer;

{function RoundNonZero(const X : Real) : Integer;
begin;
Result:=Round(X);
if Result=0 then Result:=1;
end;}


Procedure DrawInterface;
var i:integer;
begin;
clrscr(Colors[clWHITESPACE],Colors[clCodeWindowBack]);gotoxy(2,2);
Put(1,1,Colors[clCodeWindowBorder],Colors[clCodeWindowBack],#201);
Put(80,1,Colors[clCodeWindowBorder],Colors[clCodeWindowBack],#187);
Put(1,21,Colors[clCodeWindowBorder],Colors[clCodeWindowBack],#200);
//Put(80,20,15,1,#188);
for i:=2 to 20 do begin;Put(1,i,Colors[clCodeWindowBorder],Colors[clCodeWindowBack],#186);{Put(80,i,15,1,#186);}end;
for i:=2 to 79 do begin;Put(i,1,Colors[clCodeWindowBorder],Colors[clCodeWindowBack],#205);Put(i,21,Colors[clCodeWindowBorder],Colors[clCodeWindowBack],#205);end;
Put(39-(Length(InFileName) div 2),1,Colors[clCodeWindowHeader],Colors[clCodeWindowBack],' '+Ascii(InFileName)+' ');
Put(3,1,Colors[clCodeWindowBorder],Colors[clCodeWindowBack],'[ ]');
Put(4,1,Colors[clCodeWindowButtons],Colors[clCodeWindowBack],#254);
Put(76,1,Colors[clCodeWindowBorder],Colors[clCodeWindowBack],'[ ]');
Put(77,1,Colors[clCodeWindowButtons],Colors[clCodeWindowBack],#18);
Put(74,1,Colors[clCodeWindowBorder],Colors[clCodeWindowBack],'1');
for i:=4 to 19 do Put(80,i,Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#177);
Put(80,2,Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#30);
Put(80,20,Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#31);
Put(80,3,Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#254);
for i:=21 to 77 do Put(i,21,Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#177);
Put(78,21,Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#16);
Put(19,21,Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#17#254);
Put(79,21,Colors[clCodeWindowSprainer],Colors[clCodeWindowBack],#196#217);
Put(8,21,Colors[clCodeWindowBorder],Colors[clCodeWindowBack],' 1:1 ');

for i:=22 to 24 do Put(1,i,Colors[clCommentColor],Colors[clCommentWindowBack],CEmptyString);
Put(1,22,Colors[clCommentWindowBorder],Colors[clCommentWindowBack],#218);
Put(80,22,Colors[clCommentWindowBorder],Colors[clCommentWindowBack],#191);
Put(1,25,Colors[clCommentWindowBorder],Colors[clCommentWindowBack],#192);
Put(80,25,Colors[clCommentWindowBorder],Colors[clCommentWindowBack],#217);
for i:=23 to 24 do begin;Put(1,i,Colors[clCommentWindowBorder],Colors[clCommentWindowBack],#179);
Put(80,i,Colors[clCommentWindowBorder],Colors[clCommentWindowBack],#179);end;
for i:=2 to 79 do begin;Put(i,22,Colors[clCommentWindowBorder],Colors[clCommentWindowBack],#196);
Put(i,25,Colors[clCommentWindowBorder],Colors[clCommentWindowBack],#196);end;
Put(35,22,Colors[clCommentWindowHeader],Colors[clCommentWindowBack],' COMMENTS ');
end;

Procedure ReDrawScrollBars(const ScreenPosX,ScreenPosY:Integer);
begin;
If (ScrollerX[LastScreenPosX]<>ScrollerX[ScreenPosX]) then {60}
  begin;
  Put(20+ScrollerX[LastScreenPosX],21,Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#177);
  Put(20+ScrollerX[ScreenPosX]    ,21,Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#254);
  LastScreenPosX:=ScreenPosX;
  end;
if (ScrollerY[LastScreenPosY]<>ScrollerY[ScreenPosY]) then {17}
  begin;
  Put(80,3+ScrollerY[LastScreenPosY],Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#177);
  Put(80,3+ScrollerY[ScreenPosY]    ,Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#254);
  LastScreenPosY:=ScreenPosY;
  end;
end;

Procedure DrawScrollBars;
begin;
  Put(20                          ,21,Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#177);
  Put(20+ScrollerX[LastScreenPosX],21,Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#254);
  Put(80,3                          ,Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#177);
  Put(80,3+ScrollerY[LastScreenPosY],Colors[clCodeWindowBack],Colors[clCodeWindowScroller],#254);
end;


var i,max:Integer;

Initialization
DrawInterface;
max:=MAX_STRING_LENGTH-SCREEN_WIDTH+1; for i:=1 to max do ScrollerX[i]:=Round(i/max*57);
max:=MAX_STRING_NUMBER-SCREEN_HEIGHT+1;for i:=1 to max do ScrollerY[i]:=Round(i/max*16);
LastScreenPosX:=1;
LastScreenPosY:=1;
end.
