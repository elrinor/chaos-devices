uses graph,crt;
var wait,radius2,r1,h2,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15:integer;
    p1,xb,yb,xt,yt,a,b,help1,stepv,radius1,xvbegin1,yvbegin1,deltaxv,deltayv,xvbegin,yvbegin,xv,yv,ygolt,dlhoda,deltav:integer;
    n,m:string;
    c:char;

begin;
clrscr;
randomize;
a:=detect;
initgraph(a,b,'');

wait:=0;
deltav:=0;
ygolt:=3;
xt:=320;
yt:=20;
yv:=0;
xv:=0;
p1:=0;


repeat
if ygolt=4 then
begin;
setcolor(8);
setlinestyle(0,0,1);
rectangle(xt+10,yt-2,xt+25,yt+2);
circle(xt,yt,10);
setlinestyle(0,0,3);
rectangle(xt-15,yt-15,xt+30,yt+15);
setfillstyle(1,8);
floodfill(xt,yt,8);
setfillstyle(1,7);
floodfill(xt-13,yt+13,8);
setlinestyle(0,0,1);
setcolor(12);
circle(xt+48,yt+deltav,3);
xv:=xt+48;
yv:=yt+deltav;
line(xv-4,yv,xv-2,yv);
line(xv+4,yv,xv+2,yv);
line(xv,yv+4,xv,yv+2);
line(xv,yv-4,xv,yv-2);
xvbegin:=xt+35;
yvbegin:=yt;
end;

if ygolt=2 then
begin;
setcolor(8);
setlinestyle(0,0,1);
rectangle(xt-25,yt-2,xt-10,yt+2);
circle(xt,yt,10);
setlinestyle(0,0,3);
rectangle(xt-30,yt-15,xt+15,yt+15);
setfillstyle(1,8);
floodfill(xt,yt,8);
setfillstyle(1,7);
floodfill(xt+13,yt-13,8);
setlinestyle(0,0,1);
setcolor(12);
circle(xt-48,yt-deltav,3);
xv:=xt-48;
yv:=yt-deltav;
line(xv-4,yv,xv-2,yv);
line(xv+4,yv,xv+2,yv);
line(xv,yv+4,xv,yv+2);
line(xv,yv-4,xv,yv-2);
xvbegin:=xt-35;
yvbegin:=yt;
end;

if ygolt=3 then
begin;
setcolor(8);
setlinestyle(0,0,1);
rectangle(xt-2,yt+10,xt+2,yt+25);
circle(xt,yt,10);
setlinestyle(0,0,3);
rectangle(xt-15,yt-15,xt+15,yt+30);
setfillstyle(1,8);
floodfill(xt,yt,8);
setfillstyle(1,7);
floodfill(xt-13,yt-13,8);
setlinestyle(0,0,1);
setcolor(12);
circle(xt-deltav,yt+48,3);
xv:=xt-deltav;
yv:=yt+48;
line(xv-4,yv,xv-2,yv);
line(xv+4,yv,xv+2,yv);
line(xv,yv+4,xv,yv+2);
line(xv,yv-4,xv,yv-2);
xvbegin:=xt;
yvbegin:=yt+35;
end;

if ygolt=1 then
begin;
setcolor(8);
setlinestyle(0,0,3);
rectangle(xt-15,yt-30,xt+15,yt+15);
setlinestyle(0,0,1);
rectangle(xt-2,yt-25,xt+2,yt-10);
circle(xt,yt,10);
setfillstyle(1,7);
floodfill(xt-13,yt+13,8);
setfillstyle(1,8);
floodfill(xt,yt,8);
setlinestyle(0,0,1);
setcolor(12);
circle(xt+deltav,yt-48,3);
xv:=xt+deltav;
yv:=yt-48;
line(xv-4,yv,xv-2,yv);
line(xv+4,yv,xv+2,yv);
line(xv,yv+4,xv,yv+2);
line(xv,yv-4,xv,yv-2);
xvbegin:=xt;
yvbegin:=yt-35;
end;



if c='5' then
begin;
setcolor(0);
setlinestyle(0,0,1);
circle(xv,yv,3);
line(xv-4,yv,xv-2,yv);
line(xv+4,yv,xv+2,yv);
line(xv,yv+4,xv,yv+2);
line(xv,yv-4,xv,yv-2);
xvbegin1:=xvbegin;
yvbegin1:=yvbegin;

if yvbegin=yv then
begin;
setlinestyle(0,0,1);
repeat
setcolor(15);
circle(xvbegin1,yvbegin1,3);
delay(100);
setcolor(0);
circle(xvbegin1,yvbegin1,3);
if ygolt=2 then xvbegin1:=xvbegin1-1;
if ygolt=4 then xvbegin1:=xvbegin1+1;
until (xvbegin1<=71) or (xvbegin1>=569)
end;
if xvbegin=xv then
begin;
setlinestyle(0,0,1);
repeat
setcolor(15);
circle(xvbegin1,yvbegin1,3);
delay(100);
setcolor(0);
circle(xvbegin1,yvbegin1,3);
if ygolt=1 then yvbegin1:=yvbegin1-1;
if ygolt=3 then yvbegin1:=yvbegin1+1;
until (yvbegin1<=1) or (yvbegin1>=480)
end;
deltaxv:=abs(xv-xvbegin);
deltayv:=abs(yv-yvbegin);
if abs(deltaxv)=abs(deltayv) then
begin;
setlinestyle(0,0,1);
repeat
setcolor(15);
circle(xvbegin1,yvbegin1,3);
delay(100);
setcolor(0);
circle(xvbegin1,yvbegin1,3);
if (xvbegin<xv) and (yvbegin<yv) then
begin;
xvbegin1:=xvbegin1+1;
yvbegin1:=yvbegin1+1;
end;
if (xvbegin<xv) and (yvbegin>yv) then
begin;
xvbegin1:=xvbegin1+1;
yvbegin1:=yvbegin1-1;
end;
if (xvbegin>xv) and (yvbegin>yv) then
begin;
xvbegin1:=xvbegin1-1;
yvbegin1:=yvbegin1-1;
end;
if (xvbegin>xv) and (yvbegin<yv) then
begin;
xvbegin1:=xvbegin1-1;
yvbegin1:=yvbegin1+1;
end;
until (xvbegin1<=71) or (xvbegin1>=569) or (yvbegin1<=1) or (yvbegin1>=480)
end;
if deltaxv<>deltayv then
begin;
if (xvbegin<xv) and (yvbegin>yv) and (deltaxv>deltayv) then
begin;
stepv:=deltaxv div deltayv;
repeat
for help1:=1 to stepv do
begin;
setlinestyle(0,0,1);
setcolor(15);
circle(xvbegin1,yvbegin1,3);
delay(100);
setcolor(0);
circle(xvbegin1,yvbegin1,3);
xvbegin1:=xvbegin1+1;
end;
yvbegin1:=yvbegin1-1;
until (xvbegin1<=71) or (xvbegin1>=569) or (yvbegin1<=1) or (yvbegin1>=480)
end;
if (xvbegin<xv) and (yvbegin>yv) and (deltaxv<deltayv) then
begin;
stepv:=deltayv div deltaxv;
repeat
for help1:=1 to stepv do
begin;
setlinestyle(0,0,1);
setcolor(15);
circle(xvbegin1,yvbegin1,3);
delay(100);
setcolor(0);
circle(xvbegin1,yvbegin1,3);
yvbegin1:=yvbegin1-1;
end;
xvbegin1:=xvbegin1+1;
until (xvbegin1<=71) or (xvbegin1>=569) or (yvbegin1<=1) or (yvbegin1>=480)
end;
if (xvbegin>xv) and (yvbegin>yv) and (deltaxv>deltayv) then
begin;
stepv:=deltaxv div deltayv;
repeat
for help1:=1 to stepv do
begin;
setlinestyle(0,0,1);
setcolor(15);
circle(xvbegin1,yvbegin1,3);
delay(100);
setcolor(0);
circle(xvbegin1,yvbegin1,3);
xvbegin1:=xvbegin1-1;
end;
yvbegin1:=yvbegin1-1;
until (xvbegin1<=71) or (xvbegin1>=569) or (yvbegin1<=1) or (yvbegin1>=480)
end;
if (xvbegin>xv) and (yvbegin>yv) and (deltaxv<deltayv) then
begin;
stepv:=deltayv div deltaxv;
repeat
for help1:=1 to stepv do
begin;
setlinestyle(0,0,1);
setcolor(15);
circle(xvbegin1,yvbegin1,3);
delay(100);
setcolor(0);
circle(xvbegin1,yvbegin1,3);
yvbegin1:=yvbegin1-1;
end;
xvbegin1:=xvbegin1-1;
until (xvbegin1<=71) or (xvbegin1>=569) or (yvbegin1<=1) or (yvbegin1>=480)
end;
if (xvbegin>xv) and (yvbegin<yv) and (deltaxv>deltayv) then
begin;
stepv:=deltaxv div deltayv;
repeat
for help1:=1 to stepv do
begin;
setlinestyle(0,0,1);
setcolor(15);
circle(xvbegin1,yvbegin1,3);
delay(100);
setcolor(0);
circle(xvbegin1,yvbegin1,3);
xvbegin1:=xvbegin1-1;
end;
yvbegin1:=yvbegin1+1;
until (xvbegin1<=71) or (xvbegin1>=569) or (yvbegin1<=1) or (yvbegin1>=480)
end;
if (xvbegin>xv) and (yvbegin<yv) and (deltaxv<deltayv) then
begin;
stepv:=deltayv div deltaxv;
repeat
for help1:=1 to stepv do
begin;
setlinestyle(0,0,1);
setcolor(15);
circle(xvbegin1,yvbegin1,3);
delay(100);
setcolor(0);
circle(xvbegin1,yvbegin1,3);
yvbegin1:=yvbegin1+1;
end;
xvbegin1:=xvbegin1-1;
until (xvbegin1<=71) or (xvbegin1>=569) or (yvbegin1<=1) or (yvbegin1>=480)
end;
if (xvbegin<xv) and (yvbegin<yv) and (deltaxv>deltayv) then
begin;
stepv:=deltaxv div deltayv;
repeat
for help1:=1 to stepv do
begin;
setlinestyle(0,0,1);
setcolor(15);
circle(xvbegin1,yvbegin1,3);
delay(100);
setcolor(0);
circle(xvbegin1,yvbegin1,3);
xvbegin1:=xvbegin1+1;
end;
yvbegin1:=yvbegin1+1;
until (xvbegin1<=71) or (xvbegin1>=569) or (yvbegin1<=1) or (yvbegin1>=480)
end;
if (xvbegin<xv) and (yvbegin<yv) and (deltaxv<deltayv) then
begin;
stepv:=deltayv div deltaxv;
repeat
for help1:=1 to stepv do
begin;
setlinestyle(0,0,1);
setcolor(15);
circle(xvbegin1,yvbegin1,3);
delay(100);
setcolor(0);
circle(xvbegin1,yvbegin1,3);
yvbegin1:=yvbegin1+1;
end;
xvbegin1:=xvbegin1+1;
until (xvbegin1<=71) or (xvbegin1>=569) or (yvbegin1<=1) or (yvbegin1>=480)
end;
end;
setlinestyle(0,0,1);
for radius1:=1 to 10 do
begin;
setcolor(12);
circle(xvbegin1,yvbegin1,radius1);
delay(200);
setcolor(0);
circle(xvbegin1,yvbegin1,radius1);
end;
setcolor(12);
circle(xv,yv,3);
line(xv-4,yv,xv-2,yv);
line(xv+4,yv,xv+2,yv);
line(xv,yv+4,xv,yv+2);
line(xv,yv-4,xv,yv-2);
end;

if c='t' then
begin;
setlinestyle(0,0,1);
setcolor(0);
circle(xv,yv,3);
line(xv-4,yv,xv-2,yv);
line(xv+4,yv,xv+2,yv);
line(xv,yv+4,xv,yv+2);
line(xv,yv-4,xv,yv-2);
for radius2:=35 downto 1 do
begin;
setcolor(15);
circle(xt,yt,radius2);
delay(200);
setcolor(0);
circle(xt,yt,radius2);
end;
setfillstyle(1,0);
bar(xt-30,yt-30,xt+30,yt+30);
xt:=random(437);xt:=xt+102;
yt:=random(419);yt:=yt+31;
wait:=1;
end;


setcolor(11);
outtextxy(1,1,'8-вперед');
outtextxy(1,9,'2-назад');
outtextxy(1,17,'4-поворот');
outtextxy(1,25,'  влево');
outtextxy(1,33,'6-поворот');
outtextxy(1,41,'  вправо');
outtextxy(1,49,'5-выстрел');
outtextxy(1,57,'7-двигать');
outtextxy(1,65,'  прицел');
outtextxy(1,73,'  влево');
outtextxy(1,81,'9-двигать');
outtextxy(1,89,'  прицел');
outtextxy(1,97,'  вправо');
outtextxy(572,1,'q-выход');
outtextxy(572,9,'w-ускорение');
outtextxy(572,17,'e-бомба');
outtextxy(572,25,'t-телепорт');

setcolor(6);
setlinestyle(0,0,3);
line(70,0,70,480);
line(570,0,570,480);





if wait=0 then c:=readkey;
if wait=1 then
begin;
c:='1';
wait:=0;
end;





setfillstyle(0,1);
setcolor(0);
setlinestyle(0,0,1);
if ygolt=1 then bar(xt-16,yt-31,xt+16,yt+16);
if ygolt=2 then bar(xt-31,yt-16,xt+16,yt+16);
if ygolt=3 then bar(xt-16,yt-16,xt+16,yt+31);
if ygolt=4 then bar(xt-16,yt-16,xt+31,yt+16);
circle(xv,yv,3);
line(xv-4,yv,xv-2,yv);
line(xv+4,yv,xv+2,yv);
line(xv,yv+4,xv,yv+2);
line(xv,yv-4,xv,yv-2);


if c='8' then dlhoda:=1;
if c='w' then dlhoda:=10;


if (c='8') or (c='w') then
begin;
if ygolt=1 then yt:=yt-dlhoda;
if ygolt=2 then xt:=xt-dlhoda;
if ygolt=3 then yt:=yt+dlhoda;
if ygolt=4 then xt:=xt+dlhoda;
end;



if c='2' then
begin;
if ygolt=1 then yt:=yt+1;
if ygolt=2 then xt:=xt+1;
if ygolt=3 then yt:=yt-1;
if ygolt=4 then xt:=xt-1;
end;



if c='4' then ygolt:=ygolt+1;
if c='6' then ygolt:=ygolt-1;
if ygolt>4 then ygolt:=1;
if ygolt<1 then ygolt:=4;


if c='7' then
begin;
deltav:=deltav-1;
end;


if c='9' then
begin;
deltav:=deltav+1;
end;



if (c='e') and (p1=0) then
begin;
p1:=1;
yb:=yt;
xb:=xt;
c:=readkey;

if ygolt=1 then
begin;
if c='8' then yb:=yb-30;
if c='2' then yb:=yb+15;
if c='4' then xb:=xb-15;
if c='6' then xb:=xb+15;
if c='7' then
begin;
xb:=xb-15;
yb:=yb-30;
end;
if c='9' then
begin;
xb:=xb+15;
yb:=yb-30;
end;
if c='1' then
begin;
xb:=xb-15;
yb:=yb+15;
end;
if c='3' then
begin;
xb:=xb+15;
yb:=yb+15;
end;
end;

if ygolt=3 then
begin;
if c='8' then yb:=yb-15;
if c='2' then yb:=yb+30;
if c='4' then xb:=xb-15;
if c='6' then xb:=xb+15;
if c='7' then
begin;
xb:=xb-15;
yb:=yb-15;
end;
if c='9' then
begin;
xb:=xb+15;
yb:=yb-15;
end;
if c='1' then
begin;
xb:=xb-15;
yb:=yb+30;
end;
if c='3' then
begin;
xb:=xb+15;
yb:=yb+30;
end;
end;

if ygolt=2 then
begin;
if c='8' then yb:=yb-15;
if c='2' then yb:=yb+15;
if c='4' then xb:=xb-30;
if c='6' then xb:=xb+15;
if c='7' then
begin;
xb:=xb-30;
yb:=yb-15;
end;
if c='9' then
begin;
xb:=xb+15;
yb:=yb-15;
end;
if c='1' then
begin;
xb:=xb-30;
yb:=yb+15;
end;
if c='3' then
begin;
xb:=xb+15;
yb:=yb+15;
end;
end;

if ygolt=4 then
begin;
if c='8' then yb:=yb-30;
if c='2' then yb:=yb+15;
if c='4' then xb:=xb-15;
if c='6' then xb:=xb+15;
if c='7' then
begin;
xb:=xb-15;
yb:=yb-15;
end;
if c='9' then
begin;
xb:=xb+30;
yb:=yb-15;
end;
if c='1' then
begin;
xb:=xb-15;
yb:=yb+15;
end;
if c='3' then
begin;
xb:=xb+30;
yb:=yb+15;
end;
end;
setlinestyle(0,0,1);
setcolor(7);
setfillstyle(1,7);
bar(xb-3,yb-2,xb+3,yb+2);
circle(xb+5,yb,2);
end;

if (p1=1) then
begin;
setlinestyle(0,0,1);
setcolor(7);
setfillstyle(1,7);
bar(xb-3,yb-2,xb+3,yb+2);
circle(xb+5,yb,2);

if (c='e') then
begin;
for h2:=1 to 100 do
begin;

if h2>=1 then
begin;
setcolor(c1);
circle(xb,yb,1);
end;

if h2>=2 then
begin;
setcolor(c2);
circle(xb,yb,2);
end;

if h2>=3 then
begin;
setcolor(c3);
circle(xb,yb,3);
end;

if h2>=4 then
begin;
setcolor(c4);
circle(xb,yb,4);
end;

if h2>=5 then
begin;
setcolor(c5);
circle(xb,yb,5);
end;

if h2>=6 then
begin;
setcolor(c6);
circle(xb,yb,6);
end;

if h2>=7 then
begin;
setcolor(c7);
circle(xb,yb,7);
end;

if h2>=8 then
begin;
setcolor(c8);
circle(xb,yb,8);
end;

if h2>=9 then
begin;
setcolor(c9);
circle(xb,yb,9);
end;

if h2>=10 then
begin;
setcolor(c10);
circle(xb,yb,10);
end;

if h2>=11 then
begin;
setcolor(c11);
circle(xb,yb,11);
end;

if h2>=12 then
begin;
setcolor(c12);
circle(xb,yb,12);
end;

if h2>=13 then
begin;
setcolor(c13);
circle(xb,yb,13);
end;

if h2>=14 then
begin;
setcolor(c14);
circle(xb,yb,14);
end;

if h2>=15 then
begin;
setcolor(c15);
circle(xb,yb,15);
end;

c15:=c14;
c14:=c13;
c13:=c12;
c12:=c11;
c11:=c10;
c10:=c9;
c9:=c8;
c8:=c7;
c7:=c6;
c6:=c5;
c5:=c4;
c4:=c3;
c3:=c2;
c2:=c1;
c1:=random(16);
delay(100);
end;
p1:=0;
for r1:=1 to 16 do
begin;
setcolor(0);
circle(xb,yb,r1);
delay(200);
end;
setfillstyle(1,0);
bar(xb-5,yb-5,xb+5,yb+5);
end;

end;














if ygolt=1 then
begin;
if xt<88 then xt:=88;
if xt>552 then xt:=552;
if yt<31 then yt:=31;
if yt>463 then yt:=463;
end;
if ygolt=2 then
begin;
if xt<103 then xt:=103;
if xt>552 then xt:=552;
if yt<16 then yt:=16;
if yt>463 then yt:=463;
end;
if ygolt=3 then
begin;
if xt<88 then xt:=88;
if xt>552 then xt:=552;
if yt<16 then yt:=16;
if yt>448 then yt:=448;
end;
if ygolt=4 then
begin;
if xt<88 then xt:=88;
if xt>537 then xt:=537;
if yt<16 then yt:=16;
if yt>463 then yt:=463;
end;



until c='q'
end.
