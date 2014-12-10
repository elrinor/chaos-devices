{$A+,B-,D+,E+,F-,G+,I-,L+,N+,O-,P-,Q-,R-,S-,T-,V+,X+,Y+}
{$M 16384,0,655360}
uses graph;


function png(const x,y:extended):extended;assembler;
asm
  fld x
  fld y
  fpatan
  fwait
end;

var hhh,ggg:integer;
    f:text;
    c,b:array[0..1000,1..2]of extended;
    a:array[0..1000,1..2]of longint;
    ang1,ang2,n,i:longint;
    lx,ly,x1,y1,x2,y2:extended;
    dl,vx,vy,v1x,v1y,s:extended;
    maxx,maxy,minx,miny,k,rad,kx,ky:extended;
begin;
rad:=0.5;

hhh:=detect;
initgraph(hhh,ggg,'');

assign(f,'input.txt');
reset(f);
n:=0;
repeat
inc(n);
read(f,c[n,1],c[n,2]);
until seekeof(f);
c[0,1]:=c[n,1];
c[0,2]:=c[n,2];
for i:=0 to n do c[i,2]:=c[i,2]*-1;

s:=0;
for i:=1 to n do s:=s+c[i-1,1]*c[i,2]-c[i,1]*c[i-1,2];
if s<0 then begin;
b:=c;
fillchar(c,sizeof(c),0);
for i:=0 to n do c[i]:=b[n-i];
end;

{ end of input & sorting }

maxx:=-99999999;
maxy:=-99999999;
minx:=99999999;
miny:=99999999;
for i:=0 to n do if c[i,1]>maxx then maxx:=c[i,1];
for i:=0 to n do if c[i,2]>maxy then maxy:=c[i,2];
for i:=0 to n do if c[i,1]<minx then minx:=c[i,1];
for i:=0 to n do if c[i,2]<miny then miny:=c[i,2];
maxx:=maxx-minx;
maxy:=maxy-miny;
for i:=0 to n do c[i,1]:=c[i,1]-minx;
for i:=0 to n do c[i,2]:=c[i,2]-miny;
minx:=0;miny:=0;
kx:=640/(maxx+1.5);
ky:=480/(maxy+1.5);
if kx>ky then k:=ky else k:=kx;
rad:=rad*k;
for i:=0 to n do c[i,1]:=c[i,1]*k;
for i:=0 to n do c[i,2]:=c[i,2]*k;
for i:=0 to n do begin;
                 c[i,1]:=c[i,1]+rad;
                 c[i,2]:=c[i,2]+rad;
                 end;

for i:=0 to n do begin;a[i,1]:=round(c[i,1]);a[i,2]:=round(c[i,2]);end;

setcolor(15);
moveto(a[0,1],a[0,2]);
for i:=1 to n do lineto(a[i,1],a[i,2]);


{ fence start }

setcolor(8);

vx:=a[n,1]-a[n-1,1];
vy:=a[n,2]-a[n-1,2];
v1x:=vy;
v1y:=-vx;
dl:=sqrt(sqr(v1x)+sqr(v1y));
v1x:=v1x*rad/dl;
v1y:=v1y*rad/dl;

x2:=a[n,1]+v1x;
y2:=a[n,2]+v1y;
lx:=x2;
ly:=y2;



for i:=0 to n-1 do
begin;
vx:=a[i+1,1]-a[i,1];
vy:=a[i+1,2]-a[i,2];
v1x:=vy;
v1y:=-vx;
dl:=sqrt(sqr(v1x)+sqr(v1y));
v1x:=v1x*rad/dl;
v1y:=v1y*rad/dl;

x1:=a[i,1]+v1x;
y1:=a[i,2]+v1y;
x2:=a[i+1,1]+v1x;
y2:=a[i+1,2]+v1y;

line(round(x1),round(y1),round(x2),round(y2));

ang1:=round(png(lx-a[i,1],ly-a[i,2])/pi*180)-90;
ang2:=round(png(x1-a[i,1],y1-a[i,2])/pi*180)-90;
ang2:=ang2+360;
ang1:=ang1+360;
ang1:=ang1 mod 360;
ang2:=ang2 mod 360;
arc(a[i,1],a[i,2],ang2,ang1,round(rad));

lx:=x2;
ly:=y2;




end;


readln;


end.







