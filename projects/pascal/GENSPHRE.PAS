const R=2;
      n=9;
      m=20;

var f:text;
    v:array[1..n,1..m,1..3]of real;
    x,y,z,rs:real;
    i,i1:integer;
begin;


for i:=1 to n do
  begin;
  x:=R*cos(pi*i/(n+1));
  rs:=R*sin(pi*i/(n+1));
  for i1:=1 to m do
    begin;
    y:=rs*sin(2*pi*i1/m);
    z:=rs*cos(2*pi*i1/m);
    v[i,i1,1]:=x;
    v[i,i1,2]:=y;
    v[i,i1,3]:=z;
    end;
  end;
assign(f,'sphere.a3d');
rewrite(f);
writeln(f,'[ArX] Team 3d Model Viewer File');
writeln(f,'Sphere');
writeln(f,n*m+2);
writeln(f,(n+1)*m+n*m);
writeln(f,(n+1)*m);
writeln(f,'0 0 0');

writeln(f,R,' 0 0');
for i:=1 to n do for i1:=1 to m do
  writeln(f,v[i,i1,1]:5:3,' ',v[i,i1,2]:5:3,' ',v[i,i1,3]:5:3);
writeln(f,-R,' 0 0');
writeln(f);

for i:=1 to m do writeln(f,1,' ',i+1);
for i:=1 to (n-1) do for i1:=1 to m do writeln(f,1+m*(i-1)+i1,' ',1+m*(i)+i1);
for i:=1 to m do writeln(f,n*m+2,' ',(n-1)*(m)+1+i);
for i:=1 to n do for i1:=1 to m do writeln(f,1+(i-1)*m+i1,' ',1+(i-1)*m+(i1)mod m+1);


writeln(f);

for i:=1 to (m-1) do writeln(f,'7 8 3 1 ',i+1,' ',i+2);
writeln(f,'7 8 3 1 2 ',m+1);

for i:=1 to (n-1) do
  begin;
  for i1:=1 to (m-1) do
    begin;
    writeln(f,'7 8 4 ',(i-1)*m+i1+1,' ',(i-1)*m+i1+2,' ',(i)*m+i1+2,' ',(i)*m+i1+1);
    end;
  writeln(f,'7 8 4 ',(i-1)*m+1+1,' ',(i-1)*m+m+1,' ',(i)*m+m+1,' ',(i)*m+1+1);
  end;

for i:=1 to (m-1) do writeln(f,'7 8 3 ',m*n+2,' ',m*(n-1)+i+1,' ',m*(n-1)+i+2);
writeln(f,'7 8 3 ',m*n+2,' ',m*n+1,' ',m*(n-1)+2);

close(f);
end.

