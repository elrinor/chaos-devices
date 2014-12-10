uses dos,crt;
procedure g(x,y,l:longint;color:byte;d:word);
var l1,i:longint;
    i1:word;
begin;
for i:=y-l to y+l do begin;
i1:=i;
mem[$A000:(i1-1)*320+x]:=color;
end;
for i:=x-l to x+l do begin;
i1:=i;
mem[$A000:(y-1)*320+i1]:=color;
end;
color:=color+10;
l1:=l div 2;
delay(d);
if l>0 then
begin;
g(x-l,y,l1,color,d);
g(x+l,y,l1,color,d);
g(x,y-l,l1,color,d);
g(x,y+l,l1,color,d);
end;
end;






var
r:registers;
color,d:word;
x,y,l:longint;
begin
 r.ah:=0;
 r.al:=$13;
 intr($10,r);
 l:=50;
 x:=160;y:=100;
 color:=1;
 readln(d);
 g(x,y,l,color,d);



end.
