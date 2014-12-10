uses graph,crt;
var gd,gm:integer;

procedure vetka(a,x,y,l:longint);
var a1,a2,x1,y1:longint;
begin;

if (a>=0) and (a<90) then
begin;
x1:=x+round(cos(a/(180/pi))*l);
y1:=y-round(sin(a/(180/pi))*l);
end;
if (a>=90) and (a<180) then
begin;
x1:=x-round(cos((180-a)/(180/pi))*l);
y1:=y-round(sin((180-a)/(180/pi))*l);
end;
if (a>=180) and (a<270) then
begin;
x1:=x-round(cos((a-180)/(180/pi))*l);
y1:=y+round(sin((a-180)/(180/pi))*l);
end;
if (a>=270) and (a<360) then
begin;
x1:=x+round(cos((360-a)/(180/pi))*l);
y1:=y+round(sin((360-a)/(180/pi))*l);
end;




if (x>0) and (y>0) and (x<640) and (Y<480) then line(x,y,x1,y1);
delay(500);
a1:=a+25{random(60)};
a2:=a-25{random(60)};
repeat
if a2>=360 then a2:=a2-360;
if a1>=360 then a1:=a1-360;
if a2<0 then a2:=a2+360;
if a1<0 then a1:=a1+360;
until (a2<360) and (a1<360) and (a2>=0) and (a1>=0);
if (l*5) div 6>4 then
begin;
vetka(a1,x1,y1,(l*4) div 5);
vetka(a2,x1,y1,(l*4) div 5);
end;

end;


begin;
randomize;
gd:=detect;
gm:=1;
initgraph(gd,gm,'D:\games\things\tpaskal');
vetka(45,150,330,80);
readln;

end.