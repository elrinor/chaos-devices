uses crt,graph;
var f:text;
    t,ugol,x,y,u:real;
    a,b,i,i1,xp,yp:integer;
    c:char;
    ar:array[1..20,1..22]of word;
begin;
assign(f,'in.txt');
reset(f);

for i:=1 to 20 do begin;for i1:=1 to 22 do begin;read(f,ar[i,i1]);end;end;

x:=320;y:=240;u:=0;
a:=detect;
initgraph(a,b,'');

repeat
c:=readkey;cleardevice;
if c='4' then u:=u-5;
if c='6' then u:=u+5;
if c='8' then
begin;
x:=x+5*cos(pi/180*u);
y:=y+5*sin(pi/180*u);
end;

if c='5' then
begin;
x:=x-5*cos(pi/180*u);
y:=y-5*sin(pi/180*u);
end;


for i:=1 to 20 do
for i1:=1 to 22 do
begin;
t:=sqrt(sqr(i1-11)+sqr(i-10));
if i-10=0 then begin;if i1-11>0 then ugol:=0 else if i1-11<0 then ugol:=pi;end
else if i-10>0 then begin;if i1-11<>0 then ugol:=arctan((i1-11)/(i-10)) else
ugol:=pi/2;end
else if i-10<0 then begin;if i1-11<>0 then ugol:=arctan((i1-11)/(i-10))+pi else
ugol:=pi*3/2;end;
xp:=round(x+t*cos(pi/180*u+ugol));
yp:=round(y+t*sin(pi/180*u+ugol));
putpixel(xp,yp,ar[i,i1]);
{putpixel(round(x)+i-5,round(y)+i1-5,ar[i,i1]);}

end;



until c=#27;

end.