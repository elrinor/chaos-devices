{$A+,B-,D+,E+,F-,G+,I-,L+,N+,O-,P-,Q-,R-,S-,T-,V+,X+,Y+}
{$M 16384,0,655360}
uses graph;

procedure drawit(x,y:integer);
begin;
line(x,y+25,x+7,y+7);
line(x+7,y+7,x+25,y);
line(x+25,y,x+43,y+7);
line(x+43,y+7,x+50,y+25);
line(x+50,y+25,x+43,y+43);
line(x+43,y+43,x+25,y+50);
line(x+25,y+50,x+7,y+43);
line(x+7,y+43,x,y+25);

line(x,y+25,x+18,y+18);
line(x+18,y+18,x+25,y);
line(x+25,y,x+32,y+18);
line(x+32,y+18,x+50,y+25);
line(x+50,y+25,x+32,y+32);
line(x+32,y+32,x+25,y+50);
line(x+25,y+50,x+18,y+32);
line(x+18,y+32,x,y+25);
end;


var a,b,i,i1:integer;
begin;
a:=detect;
initgraph(a,b,'');
setcolor(15);
drawit(1,1);
for i:=0 to 11 do
  for i1:=0 to 8 do
    begin;
    drawit(i*50+1,i1*50+1);
    end;
readln;
end.