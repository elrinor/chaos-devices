unit labgen;

interface
type levarr=array[1..80,1..20] of shortint;
procedure genlab(var lev:levarr;var enx,eny,exx,exy:integer);
procedure gendung(var lev:levarr;var enx,eny,exx,exy:integer;roomqmin,roomqmax:shortint);
procedure generate(var level:levarr;var enx,eny,exx,exy:integer);

implementation
uses crt;
const a=80;
      b=20;
type levrr=array[1..80,1..20] of shortint;

{==================================================}
procedure draw(level:levarr;tx,ty:integer);
var i,i1:integer;
       c:char;
begin;
for i:=1 to a do
begin;
for i1:=1 to b do
begin;
if level[i,i1]=1 then begin;textcolor(8);gotoxy(i,i1);write('#');end;
if level[i,i1]=2 then begin;textcolor(7);gotoxy(i,i1);write('.');end;
if level[i,i1]=3 then begin;textcolor(8);gotoxy(i,i1);write('.');end;
if level[i,i1]=-1 then begin;textcolor(8);gotoxy(i,i1);write('>');end;
if level[i,i1]=-2 then begin;textcolor(14);gotoxy(i,i1);write('<');end;
end;
end;
textcolor(14);gotoxy(tx,ty);write('X');
c:=readkey;if c='q' then halt;
end;
{==================================================}
procedure genlab(var lev:levarr;var enx,eny,exx,exy:integer);
var i,i1,ix,iy,dir,dx,dy,xlab,ylab:shortint;
    usl:boolean;
    tupiks:array[1..2,1..100]of shortint;
    en,ex:shortint;

begin;
for i:=1 to a do for i1:=1 to b do lev[i,i1]:=1;

xlab:=a div 2;
if (a mod 2=0) then xlab:=xlab-1;
ylab:=b div 2;
if (b mod 2=0) then ylab:=ylab-1;

for i:=1 to xlab do begin;
for i1:=1 to ylab do begin;

{draw(lev,i*2,i1*2);}
if (lev[i*2,i1*2]=1) then begin;
lev[i*2,i1*2]:=2;ix:=i;iy:=i1;usl:=false;
if (i+1<xlab)and(usl=false) then if (lev[i*2+2,i1*2]=2) then begin;usl:=true;lev[i*2+1,i1*2]:=2;end;
if (i-1>0)and(usl=false) then if (lev[i*2-2,i1*2]=2) then begin;usl:=true;lev[i*2-1,i1*2]:=2;end;
if (i1+1<ylab)and(usl=false) then if (lev[i*2,i1*2+2]=2) then begin;usl:=true;lev[i*2,i1*2+1]:=2;end;
if (i1-1>0)and(usl=false) then if (lev[i*2,i1*2-2]=2) then begin;usl:=true;lev[i*2,i1*2-1]:=2;end;

repeat

dir:=0;
if (ix+1>xlab) then dir:=dir+1
else if (lev[ix*2+2,iy*2]=2) then dir:=dir+1;
if (ix-1<1) then dir:=dir+1
else if (lev[ix*2-2,iy*2]=2) then dir:=dir+1;
if (iy+1>ylab) then dir:=dir+1
else if (lev[ix*2,iy*2+2]=2) then dir:=dir+1;
if (iy-1<1) then dir:=dir+1
else if (lev[ix*2,iy*2-2]=2) then dir:=dir+1;

if dir<4 then begin;
dir:=random(4);
repeat
usl:=true;
if dir=0 then begin;dx:=2;dy:=0;end;
if dir=1 then begin;dx:=-2;dy:=0;end;
if dir=2 then begin;dx:=0;dy:=2;end;
if dir=3 then begin;dx:=0;dy:=-2;end;
if (ix+dx div 2>xlab)or(ix+dx div 2<1)or(iy+dy div 2>ylab)or(iy+dy div 2<1)then begin;dir:=dir+1;usl:=false;end
else if lev[ix*2+dx,iy*2+dy]=2 then begin;dir:=dir+1;usl:=false;end;
if dir>3 then dir:=0;
until usl=true;
end;

if dir<>4 then
begin;
lev[ix*2+dx,iy*2+dy]:=2;
lev[ix*2+dx div 2,iy*2+dy div 2]:=2;
ix:=ix+dx div 2;
iy:=iy+dy div 2;
{draw(lev,ix*2,iy*2);}
end;

until dir=4;

end;

end;end;


i:=0;usl:=false;
for ix:=1 to xlab do begin;for iy:=1 to ylab do begin;
if ((lev[ix*2-1,iy*2]=1)and(lev[ix*2+1,iy*2]=1)and(lev[ix*2,iy*2+1]=1))or
((lev[ix*2-1,iy*2]=1)and(lev[ix*2+1,iy*2]=1)and(lev[ix*2,iy*2-1]=1))or
((lev[ix*2,iy*2-1]=1)and(lev[ix*2,iy*2+1]=1)and(lev[ix*2+1,iy*2]=1))or
((lev[ix*2,iy*2-1]=1)and(lev[ix*2,iy*2+1]=1)and(lev[ix*2-1,iy*2]=1)) then
begin;inc(i);tupiks[1,i]:=ix*2;tupiks[2,i]:=iy*2;if i=100 then usl:=true;end;
if usl=true then break;end;if usl=true then break;end;

en:=random(i)+1;
repeat ex:=random(i)+1;until ex<>en;

enx:=tupiks[1,en];
eny:=tupiks[2,en];
exx:=tupiks[1,ex];
exy:=tupiks[2,ex];
lev[enx,eny]:=3;
lev[exx,exy]:=4;

end;
{==================================================}
procedure gendung(var lev:levarr;var enx,eny,exx,exy:integer;roomqmin,roomqmax:shortint);

function doorcanbe(x,y:shortint):boolean;
begin;doorcanbe:=false;
if lev[x,y]=2 then
if (x>1)and(x<a)and(y>1)and(y<b) then
if (lev[x-1,y]<>5)and(lev[x+1,y]<>5)and(lev[x,y-1]<>5)and(lev[x,y+1]<>5)then
if ((lev[x-1,y]=1)and(lev[x+1,y]=1))or((lev[x,y-1]=1)and(lev[x,y+1]=1)) then doorcanbe:=true;end;


var ix,iy,i,i1,i2:shortint;
    usl:boolean;
{rooms}
    roomq,roomn,dlx,dly,next:shortint;
    rooms:array[1..8,1..50]of shortint;
    maxdlx,maxdly,mindlx,mindly:shortint;
{corridor}
    x1,y1,x2,y2,x,y,dx,dy:shortint;
    ways:array[1..2,1..b*4]of shortint;
{final correction}
    en,ex:shortint;
{rooms array:}
{1 - x лев. верх. угла}
{2 - y лев. верх. угла}
{3 - x прав. ниж. угла}
{4 - y прав. ниж. угла}
{5 - длина по x       }
{6 - длина по y       }
{7 - x начала кишки   }
{8 - y начала кишки   }
begin;
maxdlx:=5;mindlx:=4;
maxdly:=5;mindly:=4;
if maxdlx=mindlx then inc(maxdlx);
if maxdly=mindly then inc(maxdly);
for i:=1 to a do for i1:=1 to b do lev[i,i1]:=1;
for i:=1 to b*4 do begin;ways[1,i]:=0;ways[2,i]:=0;end;
if (roomqmin>=roomqmax) then roomq:=roomqmin
else roomq:=roomqmin+random(roomqmax-roomqmin+1);

{gen rooms}for roomn:=1 to roomq do
begin;
rooms[5,roomn]:=mindlx+random(maxdlx-mindlx+1);
rooms[6,roomn]:=mindly+random(maxdly-mindly+1);
i1:=0;i2:=0;
{}repeat
if (i1>10)then begin;i1:=0;inc(i2);dec(rooms[5,roomn]);dec(rooms[6,roomn]);
if (rooms[5,roomn]<3) then rooms[5,roomn]:=3;
if (rooms[6,roomn]<3) then rooms[6,roomn]:=3;
end;
rooms[1,roomn]:=random(a-rooms[5,roomn]-1)+1;
rooms[2,roomn]:=random(b-rooms[6,roomn]-1)+1;
rooms[3,roomn]:=rooms[1,roomn]+rooms[5,roomn];
rooms[4,roomn]:=rooms[2,roomn]+rooms[6,roomn];
usl:=true;inc(i1);
for i:=1 to (roomn-1) do begin;
for ix:=rooms[1,roomn] to rooms[3,roomn] do begin;
for iy:=rooms[2,roomn] to rooms[4,roomn] do begin;
if (ix>=rooms[1,i])and(ix<=rooms[3,i])and(iy>=rooms[2,i])and(iy<=rooms[4,i])then usl:=false;
if usl=false then break;end;if usl=false then break;end;if usl=false then break;end;
if i2>10 then begin;usl:=true;roomq:=roomn;end;
{}until usl=true;

for ix:=(rooms[1,roomn]+1) to (rooms[3,roomn]-1) do
for iy:=(rooms[2,roomn]+1) to (rooms[4,roomn]-1) do lev[ix,iy]:=3;

ix:=rooms[1,roomn]+1+random(rooms[5,roomn]-2);
iy:=rooms[2,roomn]+1+random(rooms[6,roomn]-2);
{lev[ix,iy]:=2;}
rooms[7,roomn]:=ix;
rooms[8,roomn]:=iy;
{/gen rooms}end;


lev[rooms[7,1],rooms[8,1]]:=2;


for roomn:=2 to roomq do
begin;
lev[rooms[7,roomn],rooms[8,roomn]]:=2;
x1:=rooms[7,roomn];y1:=rooms[8,roomn];i:=0;i1:=1;

repeat
inc(i);
for ix:=(x1-i) to (x1+i) do begin;
if (ix<a)and(ix>1)and(y1+i<b)and(y1+i>1)then if lev[ix,y1+i]=2 then begin;ways[1,i1]:=ix;ways[2,i1]:=y1+i;inc(i1);end;
if (ix<a)and(ix>1)and(y1-i<b)and(y1-i>1)then if lev[ix,y1-i]=2 then begin;ways[1,i1]:=ix;ways[2,i1]:=y1-i;inc(i1);end;
end;
for iy:=(y1-i+1) to (y1+i-1) do begin;
if (x1+i<a)and(x1+i>1)and(iy<b)and(iy>1)then if lev[x1+i,iy]=2 then begin;ways[1,i1]:=x1+i;ways[2,i1]:=iy;inc(i1);end;
if (x1-i<a)and(x1-i>1)and(iy<b)and(iy>1)then if lev[x1-i,iy]=2 then begin;ways[1,i1]:=x1-i;ways[2,i1]:=iy;inc(i1);end;
end;
until (i1>1);

dec(i1);if i1>1 then i:=random(i1)+1 else i:=1;
x2:=ways[1,i];y2:=ways[2,i];
if random(2)=1 then usl:=true else usl:=false;
x:=x1;y:=y1;

repeat
if x>x2 then dx:=-1
else if x<x2 then dx:=1
else if x=x2 then dx:=0;
if y>y2 then dy:=-1
else if y<y2 then dy:=1
else if y=y2 then dy:=0;
if usl=true then if dx<>0 then dy:=0;
if usl=false then if dy<>0 then dx:=0;

x:=x+dx;y:=y+dy;

if lev[x,y]=1 then lev[x,y]:=2;

{gotoxy(1,21);write(x1,' ',y1,' -> ',x2,' ',y2,'  :  ',x,'+',dx,' ',y,'+',dy,'         ');
gotoxy(1,22);write(i,' ');for next:=1 to 10 do begin;write(ways[1,next],' ',ways[2,next],', ');end;
draw(lev,x,y);}

until (x=x2)and(y=y2);
end;
{draw(lev,1,1);}

for i:=1 to a do for i1:=1 to b do if lev[i,i1]=3 then lev[i,i1]:=2;

for roomn:=1 to roomq do begin;
for ix:=(rooms[1,roomn]+1) to (rooms[3,roomn]-1) do
begin;
if doorcanbe(ix,rooms[2,roomn]) then lev[ix,rooms[2,roomn]]:=5;
if doorcanbe(ix,rooms[4,roomn]) then lev[ix,rooms[4,roomn]]:=5;
end;
for iy:=(rooms[2,roomn]+1) to (rooms[4,roomn]-1) do
begin;
if doorcanbe(rooms[1,roomn],iy) then lev[rooms[1,roomn],iy]:=5;
if doorcanbe(rooms[3,roomn],iy) then lev[rooms[3,roomn],iy]:=5;
end;
end;

for i:=1 to a do for i1:=1 to b do if lev[i,i1]=5 then if (random(4)>0) then lev[i,i1]:=6;


en:=random(roomq)+1;
repeat ex:=random(roomq)+1;until ex<>en;
enx:=rooms[1,en]+rooms[5,en] div 2;
eny:=rooms[2,en]+rooms[6,en] div 2;
exx:=rooms[1,ex]+rooms[5,ex] div 2;
exy:=rooms[2,ex]+rooms[6,ex] div 2;
lev[enx,eny]:=3;
lev[exx,exy]:=4;


end;
{==================================================}
procedure generate(var level:levarr;var enx,eny,exx,exy:integer);
var hlp,tx,ty,dx,dy,oldx,oldy,x,y,nkomnat,i,ix,iy,dlx,dly:integer;
    gox,goy,corx,cory,count,vet:integer;
    hlpbool:boolean;
    arr:array[1..100,1..2]of shortint;
    {c:char;}
begin;

{null all}
for iy:=1 to 20 do begin;for ix:=1 to 80 do begin;level[ix,iy]:=0;end;end;

{gen rooms quantity}
nkomnat:=5+random(10);

{gen start room coords}
i:=0;x:=1+random(74);y:=4+random(16);enx:=x+2;eny:=y+2;

{write room in array}
repeat
i:=i+1;
dlx:=4+random(2);dly:=4+random(2);

  for ix:=x to (x+dlx) do
  begin;
  for iy:=y to (y+dly) do
  begin;
  if ((ix=x) or (ix=x+dlx)) and ((iy>=1) and (iy<=20) and (ix>=1) and (ix<=80)) then if level[ix,iy]<>2 then level[ix,iy]:=1;
  if ((iy=y) or (iy=y+dly)) and ((iy>=1) and (iy<=20) and (ix>=1) and (ix<=80)) then if level[ix,iy]<>2 then level[ix,iy]:=1;

  if (iy>y) and (iy<y+dly) and (ix>x) and (ix<x+dlx) and (iy>=1) and (iy<=20) and (ix>=1) and (ix<=80) then level[ix,iy]:=2;

  if ((ix=1) or (ix=80)) and ((iy>=1) and (iy<=20)) then level[ix,iy]:=1;
  if ((iy=1) or (iy=20)) and ((ix>=1) and (ix<=80)) then level[ix,iy]:=1;
  end;
  end;

{gen next room coord}
oldx:=x;oldy:=y;
repeat
if (random(2)=1) then begin;x:=1+random(74);y:=y-5+random(11);end
else begin;y:=4+random(11);x:=x-5+random(11);end;
if (i>nkomnat-2) then if (abs(enx-x)>20)and(abs(eny-y)>6) then hlpbool:=true
else hlpbool:=false else hlpbool:=true;
until (hlpbool=true) and (abs(oldx-x)>15) and (abs(oldy-y)>6);
if x>74 then x:=74;if x<1 then x:=1;if y>14 then y:=14;if y<1 then y:=4;

{gen corridor}
tx:=x+2;ty:=y+2;hlp:=0;count:=0;hlpbool:=true;
repeat
hlp:=hlp+1;
for ix:=tx-(hlp*2 div 3) to tx+(hlp*2 div 3) do
begin;
for iy:=ty-hlp to ty+hlp do
begin;
if (iy>0)and(iy<20)and(ix>0)and(ix<80)then
if level[ix,iy]=2 then
begin;count:=count+1;arr[count,1]:=ix;arr[count,2]:=iy;hlpbool:=false;end;
end;
end;
until (hlpbool=false);

{draw corridor}
hlp:=random(count)+1;corx:=arr[hlp,1];cory:=arr[hlp,2];
hlp:=random(2);
gox:=corx;goy:=cory;
repeat
if (tx<>gox) and (ty<>goy) then
if hlp=0 then begin;dx:=1;dy:=0;end
else begin;dx:=0;dy:=1;end;

if random(20)=1 then
begin;
if tx>gox then gox:=tx-random(10)-2;
if tx<gox then gox:=tx+random(10)+2;
if tx=gox then gox:=tx{-10+random(21)}+8*random(2)-4;
if ty>goy then goy:=ty-random(10)-2;
if ty<goy then goy:=ty+random(10)+2;
if ty=goy then goy:=ty{-10+random(21)}+8*random(2)-4;
if gox>79 then gox:=79;
if gox<2 then gox:=2;
if goy>19 then goy:=19;
if goy<2 then goy:=2;
end;

if tx=gox then begin;dy:=1;dx:=0;end;
if ty=goy then begin;dx:=1;dy:=0;end;

if tx>gox then dx:=-1*dx;
if tx<gox then dx:=dx;
if ty>goy then dy:=-1*dy;
if ty<goy then dy:=dy;

tx:=tx+dx;
ty:=ty+dy;

if (tx>79) then tx:=79;
if (tx<2) then tx:=2;
if (ty>19) then ty:=19;
if (ty<2) then ty:=2;

level[tx,ty]:=2;

if level[tx+1,ty+1]<>2 then level[tx+1,ty+1]:=1;
if level[tx  ,ty+1]<>2 then level[tx  ,ty+1]:=1;
if level[tx-1,ty+1]<>2 then level[tx-1,ty+1]:=1;
if level[tx-1,  ty]<>2 then level[tx-1,  ty]:=1;
if level[tx-1,ty-1]<>2 then level[tx-1,ty-1]:=1;
if level[tx  ,ty-1]<>2 then level[tx  ,ty-1]:=1;
if level[tx+1,ty-1]<>2 then level[tx+1,ty-1]:=1;
if level[tx+1,  ty]<>2 then level[tx+1,  ty]:=1;
{end gen corridor}
{sysdrawwall(level,tx,ty);delay(50);}
if (tx=gox)and(ty=goy) then begin;gox:=corx;goy:=cory;end;
until (tx=gox) and (ty=goy)

until i=nkomnat;

{gen exit and entrance}
exx:=oldx+2;
exy:=oldy+2;
level[enx,eny]:=3;
level[exx,exy]:=4;

{final correction}
for iy:=2 to 19 do begin;
for ix:=2 to 79 do begin;
if (level[ix,iy]=1)and(level[ix+1,iy+1]=1)and(level[ix+1,iy]=2)and(level[ix,iy+1]=2) then
level[ix+1,iy]:=1;
if (level[ix,iy]=2)and(level[ix+1,iy+1]=2)and(level[ix+1,iy]=1)and(level[ix,iy+1]=1) then
level[ix,iy]:=1;

if level[ix,iy]=2 then
begin;
{sysdrawwall(level,ix,iy);c:=readkey; if c='c' then halt;}
if (level[ix-1,iy]=1)and(level[ix-1,iy-1]=1)and(level[ix,iy-1]=1)and
(level[ix+1,iy-1]=1)and(level[ix+1,iy]=1) then begin;level[ix,iy]:=1;{write(1,ix,iy);readkey};end

else if (level[ix,iy-1]=1)and(level[ix+1,iy-1]=1)and(level[ix+1,iy]=1)and
(level[ix+1,iy+1]=1)and(level[ix,iy+1]=1) then begin;level[ix,iy]:=1;{write(2,ix,iy);readkey};end

else if (level[ix-1,iy]=1)and(level[ix-1,iy+1]=1)and(level[ix,iy+1]=1)and
(level[ix+1,iy+1]=1)and(level[ix+1,iy]=1) then begin;level[ix,iy]:=1;{write(3,ix,iy);readkey};end

else if (level[ix,iy-1]=1)and(level[ix-1,iy-1]=1)and(level[ix-1,iy]=1)and
(level[ix-1,iy+1]=1)and(level[ix,iy+1]=1) then begin;level[ix,iy]:=1;{write(4,ix,iy);readkey};end;
end;
if (level[ix,iy]=1)and(level[ix+1,iy]=2)and(level[ix+1,iy+1]=2)
and(level[ix,iy+1]=2)and(level[ix-1,iy]=2)and(level[ix-1,iy-1]=2)
and(level[ix,iy-1]=2)and(level[ix+1,iy-1]=2)and(level[ix-1,iy+1]=2)then
level[ix,iy]:=2
end;end;
end;
{==================================================}
begin;
end.