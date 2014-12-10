const MaxX=100;
      MaxY=100;

var a:array[1..MaxX,1..MaxY]of byte;
    i,count,ix,iy:Integer;
    ang,dx,dy,x,y:Extended;
    f:text;
begin;
randomize;
fillchar(a,sizeof(a),0);
a[MaxX div 2, MaxY div 2]:=1;

count:=0;
while count<(MaxX*MaxY div 2) do
  begin;
  {ang:=Random(360*10)/(180*10)*pi;
  x:=MaxX div 2 + (MaxX div 2)*sin(ang);
  y:=MaxY div 2 + (MaxY div 2)*cos(ang);}
  {repeat
  ix:=1+random(MaxX);
  iy:=1+random(MaxY);
  until a[ix,iy]=0;
  x:=ix;
  y:=iy;}
  x:=1;
  y:=1;
  ang:=Random(360)/180*pi;
  dx:=sin(ang);
  dy:=cos(ang);
  for i:=1 to 1000 do
    begin;
    x:=x+dx;
    y:=y+dy;
    if x>MaxX-1 then x:=2;
    if x<2      then x:=MaxX-1;
    if y>MaxY-1 then y:=2;
    if y<2      then y:=MaxY-1;

    ix:=round(x);
    iy:=round(y);
    if (a[ix,iy]=1)or(a[ix+1,iy]=1)or(a[ix-1,iy]=1)or(a[ix,iy+1]=1)or(a[ix,iy-1]=1) then
      begin;
      a[ix,iy]:=1;
      inc(count);
      break;
      end;
    end;
  end;
assign(f,'out.html');rewrite(f);
write(f,'<body bgcolor=000000><font face=Courier color=999999 size=1pt>');
for ix:=1 to MaxX do
  begin;
  for iy:=1 to MaxY do
    if a[ix,iy]=0 then write(f,'#')
    else write(f,'.');
  write(f,'<br>');
  end;
write(f,'</font>');
close(f);
end.

