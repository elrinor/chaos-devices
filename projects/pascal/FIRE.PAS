uses crt;
type
  TScreen=array[1..200,1..320]of byte;
var
  Screen:TScreen absolute $A000:$0000;

const Fire:array[1..30]of Integer=
  (10,40,80,120,160,20,50,70,150,180,
   5,15,75,45,106,115,189,195,35,125,
   210,220,230,240,275,320,310,291,260,265);

var
  b:Byte;
  i,x,y,Col,k,dx,dy:Integer;
  Scr:TScreen;
  d:Extended;
begin;
randomize;
asm
  MOV AH,0
  MOV AL,13H
  INT 10H
end;

while ( ( Port[ $3da ] and $8 ) <> 0 ) do ;
while ( not ( ( Port[ $3da ] and $8 ) <> 0 ) ) do ;
Port[$3c8]:=0;
Port[$3c9]:=0;
Port[$3c9]:=0;
Port[$3c9]:=0;
for b:=1 to 255 do
  begin
  Port[$3c8]:=b;
  Port[$3c9]:=(2+b div 5) div 1;
  Port[$3c9]:=(2+b div 5) div 1*0;
  Port[$3c9]:=(2+b div 5) div 1*0;
  end;

repeat
  FillChar(Scr,sizeof(TScreen),0);
  For x:=1 to 320 do For y:=1 to 200 do If Screen[y,x]<>0 then
    begin;
    k:=1+random(10);
    d:={1.073}1.078;
    Col:=Trunc(Screen[y,x]*d/k);
    If Col<>0 then for i:=1 to k do
      begin;
      if y<190 then
        begin;
      dx:=-2+random(5);
      dy:=random(3);
        end
      else
        begin;
        dx:=-5+random(11);
        dy:=-random(100);
        end;
      if (x+dx>=1)and(x+dx<=320)and(y+dy>=1)and(y+dy<=200) then
        begin;
        if Scr[y+dy,x+dx]+Col<=255 then
          Scr[y+dy,x+dx]:=Scr[y+dy,x+dx]+Col
        else
          Scr[y+dy,x+dx]:=255;
        end;
      end;
    end;
  For i:=1 to 30 do Scr[{200}1,Fire[i]]:={random(256)}255;
  Move(Scr,Screen,sizeof(TScreen));
until keypressed;


asm
  MOV AH,0
  MOV AL,3
  INT 10H
end;
end.
