{gen landscape2}
uses crt;

type TSymb=record
      color:byte;
      char:char;
     end;


const size=64;
var a,a1:array[1..size,1..size]of byte;
    b:array[1..size,1..size]of TSymb;


var dl,x,y,x1,y1,Chance,InitType:integer;
    f:text;
    sx,sy:Integer;
    c:char;
begin;
clrscr;
randomize;
for x:=1 to size div 2 do
for y:=1 to size div 2 do a[x,y]:=1;

for x:=size div 2+1 to size do
for y:=size div 2+1 to size do a[x,y]:=1;
dl:=size div 2;

repeat
dl:=dl div 2;
for x:=1 to size div dl do
  for y:=1 to size div dl do
    begin;
    Chance:=0;
    if (x)*dl+dl<=size then if a[x*dl+dl,y*dl]=0 then Inc(Chance);
    if (x)*dl-dl>=1    then if a[x*dl-dl,y*dl]=0 then Inc(Chance);
    if (y)*dl+dl<=size then if a[x*dl,y*dl+dl]=0 then Inc(Chance);
    if (y)*dl-dl>=1    then if a[x*dl,y*dl-dl]=0 then Inc(Chance);
    if (1+random(4))<=Chance then
      begin;
      for x1:=x*dl-dl+1 to x*dl do
      for y1:=y*dl-dl+1 to y*dl do
        a1[x1,y1]:=0;
      end
    else
      begin;
      for x1:=x*dl-dl+1 to x*dl do
      for y1:=y*dl-dl+1 to y*dl do
        a1[x1,y1]:=1;
      end;
    end;
a:=a1;
until dl=1;

for x:=1 to size do
for y:=1 to size do
  begin;
  if a[x,y]=0 then
    begin;
    b[x,y].char:='#';
    b[x,y].color:=6;
    end;
  if a[x,y]=1 then
    begin;
    b[x,y].char:='=';
    b[x,y].color:=9;
    end;
  end;


sx:=0;
sy:=0;
repeat
c:=readkey;
if c='8' then dec(sy);
if c='2' then inc(sy);
{if c='4' then dec(sx);
if c='6' then inc(sx);}
{if sx<1 then sx:=1;}
if sy<0 then sy:=0;
if sy>size-24 then sy:=size-24;
textcolor(15);gotoxy(1,25);write(sx,' ',sy,'   ');

for x:=1 to size do
  for y:=1 to 24 do
    begin;
    gotoxy(x,y);
    textcolor(b[sx+x, sy+y].color);
    write(b[sx+x, sy+y].char);
    end;
until c=#27;

assign(f,'out.txt');rewrite(f);
for y:=1 to size do
  begin;
  writeln(f);
  for x:=1 to size do
    write(f,b[x,y].char);
  end;
close(f);


end.