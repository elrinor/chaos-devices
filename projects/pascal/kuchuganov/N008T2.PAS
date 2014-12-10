{$A+,B-,D+,E+,F-,G+,I-,L+,N+,O-,P-,Q-,R-,S-,T-,V+,X+,Y+}
{$M 16384,0,655360}
uses graph;

procedure grafik(a,b:extended);
const sh=0.1;
var f,p:extended;
    x,y:integer;
begin;
moveto(320,240);

f:=-sh;
repeat
f:=f+sh;
p:=a*cos(f)+b;
x:=round(p*cos(f)*100)+320;
y:=round(p*sin(f)*100)+240;
lineto(x,y);
until f>=8;

end;

var a,b:integer;
begin;
a:=detect;
initgraph(a,b,'');

grafik(1,-1);
setcolor(7);
grafik(0.7,0.1);
setcolor(8);
grafik(-1,-0.5);

setcolor(15);
line(0,240,640,240);
line(320,0,320,480);
moveto(631,229);outtext('X');
moveto(324,1);outtext('Y');
line(320-2,3,320,0);
line(320+2,3,320,0);
line(640-3,240-2,640,240);
line(640-3,240+2,640,240);

readln;
end.