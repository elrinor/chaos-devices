uses crt;
type fonarr=array[1..25,1..2] of integer;

const dly=400;

procedure changefon(var fon:fonarr;g:integer);
var f11,f22,f1,f2,i:integer;
begin;

{for i:=2 to 25 do
begin;
fon[i,1]:=fon[i,1]+1;
fon[i,2]:=fon[i,2]+1;

if fon[i,2]>80 then begin;
fon[i,2]:=fon[i,1];
end;

if fon[i,1]>39 then
begin;
if random(10)<fon[i,1]-39 then fon[i,1]:=random(fon[i,2]-39);
end;

end;}
if g=1 then
begin;
f1:=fon[2,1];f2:=fon[2,2];
fon[2,1]:=random(40);
fon[2,2]:=40+random(40);
for i:=3 to 25 do
begin;
f11:=fon[i,1];
fon[i,1]:=f1;
f1:=f11;

f22:=fon[i,2];
fon[i,2]:=f2;
f2:=f22;
end;
end;

end;


procedure makefon(fon:fonarr);
var i:integer;
begin;
clrscr;
textcolor(15);
for i:=2 to 25 do
begin;
gotoxy(fon[i,1],i);
write('.');
gotoxy(fon[i,2],i);
write('.');

end;
end;



procedure boom(x:integer);
var n:integer;
begin;
for n:=1 to 9 do
begin;
if n mod 2=1 then textcolor(13);
if n mod 2=0 then textcolor(14);
gotoxy(x+n,1);
write('†');
gotoxy(x-n,1);
write('†');
delay(dly*10);
end;
end;




var g,i,vistrel,vx,vy,dvy,dlay,dx,dy,x,y:integer;
    c1,c:char;
    fon:fonarr;
begin
randomize;
for i:=2 to 25 do
begin;
fon[i,1]:=random(40);
fon[i,2]:=40+random(40);
end;

g:={0}1;
clrscr;
x:=20;
y:=20;
vistrel:=0;
repeat
g:=g+1;
if g>3 then g:=0;
changefon(fon,g);
makefon(fon);
textcolor(14);
gotoxy(x-1,y);
write('@$@');

if vistrel=1 then
begin;
textcolor(5);
gotoxy(vx,vy);
write('.');
end;

if (c='8') or (c='2') then dlay:=60*dly;
if (c='4') or (c='6') or (c='5') then dlay:=20*dly;
if (c='7') or (c='9') or (c='1') or (c='3') then dlay:=40*dly;
delay(dlay);

if vistrel=0 then c1:=c;
if keypressed then c:=readkey;

if c='8' then begin;dy:=-1;dx:=0;end;
if c='4' then begin;dx:=-1;dy:=0;end;
if c='2' then begin;dy:=1;dx:=0;end;
if c='6' then begin;dx:=1;dy:=0;end;

if c='7' then begin;dy:=-1;dx:=-1;end;
if c='9' then begin;dx:=1;dy:=-1;end;
if c='1' then begin;dy:=1;dx:=-1;end;
if c='3' then begin;dx:=1;dy:=1;end;

if c='5' then begin;dx:=0;dy:=0;end;
if (c=#32) and (vistrel=0) then begin;vx:=x;;vy:=y;dvy:=-1;vistrel:=1;c:=c1;end;
x:=x+dx;
y:=y+dy;
vy:=vy+dvy;

if x<2 then x:=2;
if x>79 then x:=79;
if y<2 then y:=2;
if y>25 then y:=25;

if (vy<1) and (vistrel=1) then begin;boom(vx);vistrel:=0;end;



until c=#27;


end.