{$A+,B-,D+,E+,F-,G+,I-,L+,N+,O-,P-,Q-,R-,S-,T-,V+,X+,Y+}
{$M 16384,0,655360}

{TO JURY:
1. If the ball moves very slow you can decrease the value of dlay constant
(line 11). This will cause the ball to move faster.
2. My program uses crt library because I've received a comment that
it is allowed.}
uses graph,crt;

const dlay=2000;
      r=10;
{procedure delay(n:integer);
var timer:longint absolute $0:$046C;
    now,need:longint;
begin;
now:=timer;
need:=now+n;
repeat
until timer>=need;
end;}

procedure draw(x,y:integer;f:extended;jjj:integer);
var x1,y1,x2,y2,x3,y3,x4,y4:integer;
    dl,df:extended;
begin;
df:=-300*f/(2*r);
x1:=x+round(r*cos(df));
y1:=y+round(r*sin(df));
x2:=x+round(r*cos(df+pi));
y2:=y+round(r*sin(df+pi));
x3:=x+round(r*cos(df+pi/2));
y3:=y+round(r*sin(df+pi/2));
x4:=x+round(r*cos(df+pi*3/2));
y4:=y+round(r*sin(df+pi*3/2));

setcolor(7);
line(x1,y1,x2,y2);
line(x3,y3,x4,y4);
circle(x,y,r);
delay(jjj);
setcolor(0);
line(x1,y1,x2,y2);
line(x3,y3,x4,y4);
circle(x,y,r);
end;

const a=0.0001;
var hhh,ggg,x,y:integer;
    f,v,pi2:extended;
begin;
pi2:=pi/2;

hhh:=detect;
initgraph(hhh,ggg,'');

setcolor(15);
arc(320,100,180,360,300);
line(0,100,20,100);
line(640,100,620,100);


f:=pi-4*a;
v:=-a*1.5;
repeat
if f>pi2 then v:=v-a;
if f<pi2 then v:=v+a;
f:=f+v;


x:=round((300-r)*cos(f))+320;
y:=round((300-r)*sin(f))+100;

if f>0 then
begin;
draw(x,y,f,dlay);
setcolor(15);
arc(320,100,180,360,300);
end
else
begin;
draw(620-r,100,0,dlay div 6);
setcolor(15);
arc(320,100,180,360,300);
end;

until keypressed;

closegraph;
end.


