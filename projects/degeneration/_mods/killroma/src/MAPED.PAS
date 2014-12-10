uses crt;
const a=80;
      b=20;
type levarr=array[1..a,1..b] of shortint;

var i,i1,x,y:integer;
    level:levarr;
    c:char;
    f:file of levarr;

procedure draw;
begin;
for i:=1 to a do
begin;
for i1:=1 to b do
begin;
if level[i,i1]=1 then begin;textcolor(10);gotoxy(i,i1);write('"');end;
if level[i,i1]=2 then begin;textcolor(2);gotoxy(i,i1);write('&');end;
if level[i,i1]=3 then begin;textcolor(6);gotoxy(i,i1);write('.');end;
if level[i,i1]=4 then begin;textcolor(6);gotoxy(i,i1);write('~');end;
if level[i,i1]=5 then begin;textcolor(2);gotoxy(i,i1);write('"');end;
if level[i,i1]=6 then begin;textcolor(1);gotoxy(i,i1);write('=');end;
if level[i,i1]=7 then begin;textcolor(8);gotoxy(i,i1);write('^');end;
if level[i,i1]=8 then begin;textcolor(15);gotoxy(i,i1);write('^');end;
if level[i,i1]=9 then begin;textcolor(8);gotoxy(i,i1);write('*');end;
end;end;
end;

procedure drawone;
begin;
if level[x,y]=1 then begin;textcolor(10);gotoxy(x,y);write('"');end;
if level[x,y]=2 then begin;textcolor(2);gotoxy(x,y);write('&');end;
if level[x,y]=3 then begin;textcolor(6);gotoxy(x,y);write('.');end;
if level[x,y]=4 then begin;textcolor(6);gotoxy(x,y);write('~');end;
if level[x,y]=5 then begin;textcolor(2);gotoxy(x,y);write('"');end;
if level[x,y]=6 then begin;textcolor(1);gotoxy(x,y);write('=');end;
if level[x,y]=7 then begin;textcolor(8);gotoxy(x,y);write('^');end;
if level[x,y]=8 then begin;textcolor(15);gotoxy(x,y);write('^');end;
if level[x,y]=9 then begin;textcolor(8);gotoxy(x,y);write('*');end;
end;


begin;
for i:=1 to a do for i1:=1 to b do level[i,i1]:=1;
x:=1;y:=1;
assign(f,'wild.dat');reset(f);read(f,level);close(f);
for i:=1 to a do for i1:=1 to b do level[i,i1]:=level[i,i1]-6;
clrscr;
draw;

repeat

gotoxy(1,21);textcolor(14);write('X:',x,'  Y:',y,'         ');
gotoxy(x,y);drawone;gotoxy(x,y);
c:=readkey;
if c=#80 then inc(y);
if c=#75 then dec(x);
if c=#77 then inc(x);
if c=#72 then dec(y);
if x>a then x:=a;
if x<1 then x:=1;
if y>b then y:=b;
if y<1 then y:=1;

if c='1' then level[x,y]:=1;
if c='2' then level[x,y]:=2;
if c='3' then level[x,y]:=3;
if c='4' then level[x,y]:=4;
if c='5' then level[x,y]:=5;
if c='6' then level[x,y]:=6;
if c='7' then level[x,y]:=7;
if c='8' then level[x,y]:=8;
if c='9' then level[x,y]:=9;

if c='c' then begin;for i:=1 to a do for i1:=1 to b do level[i,i1]:=1;draw;end;

if c='o' then begin;
for i:=1 to a do begin;level[i,1]:=8;level[i,b]:=8;end;
for i1:=1 to b do begin;level[1,i1]:=8;level[a,i1]:=8;end;
draw;
end;

until c=#27;

for i:=1 to a do for i1:=1 to b do level[i,i1]:=level[i,i1]+6;
assign(f,'wild.dat');rewrite(f);write(f,level);close(f);


end.