uses crt;

function max(a,b:Integer):Integer;
begin;
if a>b then max:=a
else max:=b;
end;

function min(a,b:Integer):Integer;
begin;
if a<b then min:=a
else min:=b;
end;

type TChar=record
       s:char;
       c:byte;
     end;
const Disp:array[0..4]of TChar=((s:'#';c:8),(s:'.';c:8),(s:'O';c:9),(s:'X';c:4),(s:'$';c:14));
      maxx=100;
      maxy=100;
      scrx=80;
      scry=50;
      maxp=2;

var a:array[1..maxx,1..maxy]of byte;
    Screen:array[1..scry,1..scrx]of TChar absolute $B800:0000;
    i:Integer;
    p,sx,sy,px,py:Integer;

procedure Draw;
var x,y:Integer;
begin;
clrscr;
for x:=1 to scrx do
for y:=1 to scry do
  if (x+px-scrx div 2>=1) and (x+px-scrx div 2<=maxx) and
     (y+py-scry div 2>=1) and (y+py-scry div 2<=maxy) then
  Screen[y,x]:=Disp[a[x+px-scrx div 2,y+py-scry div 2]];
gotoxy(scrx div 2,scry div 2);
end;

function check(x,y:Integer):Boolean;
var p:Integer;
    i,i1,q:Integer;
begin;
check:=false;
p:=a[x,y];
for i:=x-4 to x do
  begin;
  q:=0;
  for i1:=i to i+5 do
    if (i1>=1) and (i1<=maxx) then
    if a[i1,y]=p then inc(q);
  if q=5 then check:=true;
  end;

for i:=y-4 to y do
  begin;
  q:=0;
  for i1:=i to i+5 do
    if (i1>=1) and (i1<=maxy) then
    if a[x,i1]=p then inc(q);
  if q=5 then check:=true;
  end;

for i:=0 to 4 do
  begin;
  q:=0;
  for i1:=0 to 4 do
    if (x-i+i1>=1) and (x-i+i1<=maxx) and (y-i+i1>=1) and (y-i+i1<=maxy) then
    if a[x-i+i1,y-i+i1]=p then inc(q);
  if q=5 then check:=true;
  end;

for i:=0 to 4 do
  begin;
  q:=0;
  for i1:=0 to 4 do
    if (x+i-i1>=1) and (x+i-i1<=maxx) and (y-i+i1>=1) and (y-i+i1<=maxy) then
    if a[x+i-i1,y-i+i1]=p then inc(q);
  if q=5 then check:=true;
  end;
end;

function turn(p:Integer):Boolean;
var c:Char;
    done:Boolean;
begin;
repeat
  repeat c:=readkey; until (c in ['8','6','4','2',#27,#13,' ']);
  if c='8' then dec(py);
  if c='2' then inc(py);
  if c='4' then dec(px);
  if c='6' then inc(px);
  if py<2 then py:=2;
  if py>maxy-1 then py:=maxy-1;
  if px<2 then px:=2;
  if px>maxx-1 then px:=maxx-1;

  if px<sx then begin;dec(sx);end;
  if py<sy then begin;dec(sy);end;
  if px>sx+scrx-1 then begin;inc(sx);end;
  if py>sy+scry-1 then begin;inc(sy);end;
  draw;

  if (c in [#13,' '])and(a[px,py]<>1) then c:='_';
until c in [#27,#13,' '];

if (c in [#13,#27]) then
  begin;
  a[px,py]:=p+2;
  draw;
  turn:=check(px,py);
  end;
if c=#27 then turn:=true;
end;

begin;
fillchar(a,sizeof(a),1);
for i:=1 to maxx do begin;a[1,i]:=0;a[maxy,i]:=0;end;
for i:=1 to maxy do begin;a[i,1]:=0;a[i,maxx]:=0;end;
px:=2;py:=2;sx:=1;sy:=1;
Draw;

p:=0;
repeat
  p:=(p+1)mod maxp;
until turn(p);



end.
