uses graph,crt;

const dlay=2000;
      dangle=0.3;

type PChunk=^TChunk;
     TChunk=record
       x,y:extended;
       next:PChunk;
     end;

     TGlista=record
       nach,kon:PChunk;
       fi:extended;
     end;

function IntToStr(i:Longint):String;
var s:String;
begin;
str(i,s);
IntToStr:=s;
end;


procedure InitGlist(var glista:TGlista);
begin;
new(glista.nach);
new(glista.kon);
glista.nach^.next:=glista.kon;
glista.nach^.x:=320;
glista.nach^.y:=240;
glista.kon^.x:=319;
glista.kon^.y:=240;
putpixel(320,240,15);
putpixel(319,240,15);
glista.kon^.next:=nil;
glista.fi:=pi/2;
end;

function KillGlist(x,y:Integer):Boolean;
var count,i,a:Integer;
begin;
KillGlist:=False;
count:=0;
if (GetPixel(x+1,y+1)<>0) then inc(count);
if (GetPixel(x+1,y  )<>0) then inc(count);
if (GetPixel(x+1,y-1)<>0) then inc(count);
if (GetPixel(x  ,y+1)<>0) then inc(count);
if (GetPixel(x  ,y  )<>0) then inc(count);
if (GetPixel(x  ,y-1)<>0) then inc(count);
if (GetPixel(x-1,y+1)<>0) then inc(count);
if (GetPixel(x-1,y  )<>0) then inc(count);
if (GetPixel(x-1,y-1)<>0) then inc(count);
if count>2 then
  begin;
  for i:=1 to 500 do
    begin;
    a:=random(360);
    PutPixel(x+Round(random(10)*cos(a/180*pi)),y+Round(random(10)*sin(a/180*pi)),4);
    Sound(random(10000));
    delay(random(dlay div 2));
    end;
  nosound;
  KillGlist:=True;
  end;
end;

function GoGlist(var glista:TGlista):boolean;
var c:PChunk;
begin;
putpixel(Round(glista.nach^.x),Round(glista.nach^.y),0);
c:=glista.nach;
glista.nach:=glista.nach^.next;
dispose(c);
new(glista.kon^.next);
glista.kon^.next^.x:=glista.kon^.x+cos(glista.fi);
glista.kon^.next^.y:=glista.kon^.y+sin(glista.fi);
glista.kon:=glista.kon^.next;
GoGlist:=KillGlist(Round(glista.kon^.x),Round(glista.kon^.y));
putpixel(Round(glista.kon^.x),Round(glista.kon^.y),15);
end;

function GrowGlist(var glista:TGlista):boolean;
begin;
new(glista.kon^.next);
glista.kon^.next^.x:=glista.kon^.x+cos(glista.fi);
glista.kon^.next^.y:=glista.kon^.y+sin(glista.fi);
glista.kon:=glista.kon^.next;
GrowGlist:=KillGlist(Round(glista.kon^.x),Round(glista.kon^.y));
putpixel(Round(glista.kon^.x),Round(glista.kon^.y),15);
end;

var a,b,h:Integer;
    q:Longint;
    glista:TGlista;
    c:char;
begin;
a:=detect;
InitGraph(a,b,'');
cleardevice;

setcolor(15);
line(1,1,639,1);
line(639,1,639,479);
line(639,479,1,479);
line(1,479,1,1);

InitGlist(glista);

setcolor(0);
q:=0;
repeat
  inc(q);
  bar(1,1,639,10);
  moveto(2,2);
  outtext(IntToStr(q));
  h:=5;
  if keypressed then
    begin;
    c:=readkey;
    if c='4' then glista.fi:=glista.fi-dangle;
    if c='6' then glista.fi:=glista.fi+dangle;
    if c='8' then h:=10;
    end;

  delay(dlay);
  if random(10)<=h then
    begin;
    if GrowGlist(glista) then c:=#27;
    end
  else
    begin;
    if GoGlist(glista) then c:=#27;
    end;
until (c=#27);



end.