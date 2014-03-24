uses graph,crt,dos;
{============================================================================}
{============================================================================}
type TCoord=record
              x:real;
              y:real;
              z:real;
            end;
type TPlaneCoord=record
              x:real;
              y:real;
            end;
type TPolygon=record
              color:byte;
              linecolor:byte;
              n:integer;
              Points:array[1..4]of Integer;
            end;
type TPolyChainItem=record
              no:Integer;
              dl:Real;
            end;
{============================================================================}
{============================================================================}
var Coord:array[1..100] of TCoord;
    Connections:array[1..100,1..2] of Integer;
    ProjectionCoord:array[1..100] of TPlaneCoord;
    PointsToCameraDl:array[1..100] of Real;
    Polygons:array[1..100] of TPolygon;
    PolyChain:array[1..100] of TPolyChainItem;
    CameraCoord:TCoord;
    CameraPlaneZ:Real;
    ScreenMultiplier,dx,dy,dz:Real;
    MaxCoords,MaxConnections,MaxPolygons:integer;
    morph:array[1..6,1..20,1..2]of byte;
    fronts:array[1..6]of integer;
    {fr:set of byte;}
    frontsnow:array[1..6]of integer;
    ftm:array[1..100]of integer;
{============================================================================}
{============================================================================}
function middle(a,b,c,d:real):real;
begin;
middle:=(a+b+c+d)/4;
end;

function imiddle(a,b,c,d:integer):integer;
begin;
imiddle:=(a+b+c+d) div 4;
end;
{============================================================================}
{============================================================================}
procedure Sort(min,max: Integer);
var i,j,h:integer;
    x,y:Real;
begin
 i:=min;j:=max;x:=PolyChain[(min+max) div 2].dl;
 repeat
  while PolyChain[i].dl>x do i:=i+1;
  while x>PolyChain[j].dl do j:=j-1;
  if i<=j then
  begin
   y:=PolyChain[i].dl;PolyChain[i].dl:=PolyChain[j].dl;PolyChain[j].dl:=y;
   h:=PolyChain[i].no;PolyChain[i].no:=PolyChain[j].no;PolyChain[j].no:=h;
   i:=i+1;j:=j-1;
  end;
 until i>j;
 if min<j then Sort(min,j);
 if i<max then Sort(i,max);
end;
{============================================================================}
{============================================================================}
function IntToStr(ch:integer):string;
var st:string;
begin;
str(ch,st);IntToStr:=st;
end;

function StrToInt(s:string):integer;
var o,i:integer;
begin;
val(s,i,o);
if o=0 then StrToInt:=i
else StrToInt:=0;
end;
{============================================================================}
{============================================================================}
function GetLoadingModelName:string;
var i,n:integer;
    ModelsArr:array[1..10] of string;
    DirInfo:SearchRec;
    c:char;
begin;
clrscr;gotoxy(1,1);
Writeln('Choose wisely, mortal:');
n:=0;FindFirst('*.a3d',1,DirInfo);

while DosError = 0 do
begin;
inc(n);
ModelsArr[n]:=DirInfo.name;
writeln('['+chr(ord('a')+n-1)+'] '+ModelsArr[n]);
FindNext(DirInfo);
end;

repeat
c:=readkey;
if not(ord(c)-ord('a')<n) then writeln('I said *WISELY*!');
until (ord(c)-ord('a')<n);

GetLoadingModelName:=ModelsArr[ord(c)-ord('a')+1];
end;
{============================================================================}
{============================================================================}
procedure LoadModel;
var filename,modelname:string;
    f:text;
    i,i1:integer;
    c:char;
begin;
filename:='Rubik.a3d';

assign(f,filename);
reset(f);
readln(f);
readln(f,modelname);
readln(f,MaxCoords);
readln(f,MaxConnections);
readln(f,MaxPolygons);

clrscr;gotoxy(1,1);
writeln('Model Name: '+modelname+' ('+IntToStr(MaxCoords)+' points, '+IntTostr(MaxConnections)+' lines, ',
MaxPolygons,' Polygons)');
writeln('Press Enter to continue');
repeat;c:=readkey;until c=#13;
clrscr;gotoxy(1,1);writeln('Loading... please wait...');

readln(f,dx,dy,dz);
for i:=1 to MaxCoords do readln(f,Coord[i].x,Coord[i].y,Coord[i].z);
for i:=1 to MaxConnections do readln(f,Connections[i,1],Connections[i,2]);
for i:=1 to MaxPolygons do begin;
read(f,Polygons[i].color,Polygons[i].linecolor,Polygons[i].n);
for i1:=1 to (Polygons[i].n-1) do read(f, Polygons[i].Points[i1]);
readln(f,Polygons[i].Points[Polygons[i].n]);
end;
for i:=1 to MaxCoords do with Coord[i] do begin;x:=x+dx;y:=y+dy;z:=z+dz;end;
close(f);
assign(f,'3d_rubic.cfg');
reset(f);
for i1:=1 to 20 do read(f,morph[1,i1,1],morph[1,i1,2]);
for i1:=1 to 20 do read(f,morph[2,i1,2],morph[2,i1,1]);
for i1:=1 to 20 do read(f,morph[3,i1,1],morph[3,i1,2]);
for i1:=1 to 20 do read(f,morph[4,i1,1],morph[4,i1,2]);
for i1:=1 to 20 do read(f,morph[5,i1,2],morph[5,i1,1]);
for i1:=1 to 20 do read(f,morph[6,i1,1],morph[6,i1,2]);

for i:=1 to 6 do read(f,fronts[i]);
read(f,i);
ftm[fronts[1]]:=i;
read(f,i);
ftm[fronts[2]]:=i;
read(f,i);
ftm[fronts[3]]:=i;
read(f,i);
ftm[fronts[4]]:=i;
read(f,i);
ftm[fronts[5]]:=i;
read(f,i);
ftm[fronts[6]]:=i;
{fr:=[];
for i:=1 to 6 do fr:=fr+[fronts[i]];}
frontsnow[1]:=1;
frontsnow[2]:=2;
frontsnow[3]:=3;
frontsnow[4]:=4;
frontsnow[5]:=5;
frontsnow[6]:=6;

close(f);
end;
{============================================================================}
{============================================================================}
Procedure ApplyMorph(n:integer);
var i:integer;
    a:array[1..20]of byte;
begin;
for i:=1 to 20 do
  begin;
  a[i]:=Polygons[morph[n,i,1]].color;
  end;
for i:=1 to 20 do
  begin;
  Polygons[morph[n,i,2]].color:=a[i];
  end;
end;
{============================================================================}
{============================================================================}
var x,y,z,polydl,max,t:Real;
    a,b,i,i1,px,py,pz,maxn:integer;
    c:char;
    Polygon:array[1..4]of PointType;
    page,paged:byte;
begin;
paged:=31;
CameraCoord.x:=0;
CameraCoord.y:=0;
CameraCoord.z:=5.5;
ScreenMultiplier:=100;
CameraPlaneZ:=4.5;

LoadModel;

a:=detect;
InitGraph(a,b,'');
cleardevice;

page:=0;

SetVisualPage(0);
SetActivePage(1);

repeat
 for i:=1 to MaxCoords do begin;
   ProjectionCoord[i].x:=(-1*(CameraCoord.x-Coord[i].x)/(Coord[i].z-CameraCoord.z))*CameraPlaneZ+
   (-1*(Coord[i].x*(CameraCoord.z-Coord[i].z)-Coord[i].z*(CameraCoord.x-Coord[i].x))/
   (Coord[i].z-CameraCoord.z));

   ProjectionCoord[i].y:=(-1*(CameraCoord.y-Coord[i].y)/(Coord[i].z-CameraCoord.z))*CameraPlaneZ+
   (-1*(Coord[i].y*(CameraCoord.z-Coord[i].z)-Coord[i].z*(CameraCoord.y-Coord[i].y))/
   (Coord[i].z-CameraCoord.z));
 end;

 for i:=1 to MaxCoords do begin;
   PointsToCameraDl[i]:=sqrt(sqr(Coord[i].x-CameraCoord.x)+sqr(Coord[i].y-CameraCoord.y)+sqr(Coord[i].z-CameraCoord.z));
 end;

 for i:=1 to MaxPolygons do begin;
   polydl:=0;
   for i1:=1 to Polygons[i].n do polydl:=polydl+PointsToCameraDl[Polygons[i].Points[i1]];
   PolyChain[i].no:=i;
   PolyChain[i].dl:=polydl/Polygons[i].n;
 end;

 Sort(1,MaxPolygons);

 if page=1 then
   begin;
 setfillstyle(1,0);setcolor(0);bar(1,30,640,480);
 for i:=1 to MaxPolygons do begin;
   for i1:=1 to Polygons[PolyChain[i].no].n do begin;
   Polygon[i1].x:=round(ProjectionCoord[Polygons[PolyChain[i].no].Points[i1]].x*ScreenMultiplier+320);
   Polygon[i1].y:=round(ProjectionCoord[Polygons[PolyChain[i].no].Points[i1]].y*ScreenMultiplier+240);
   end;
   setfillstyle(1,Polygons[PolyChain[i].no].color);
   setcolor(Polygons[PolyChain[i].no].linecolor);
   fillpoly(Polygons[PolyChain[i].no].n,Polygon);
   {if (i in fr) then
     begin;
     setcolor(0);
     outtextxy(imiddle(Polygon[1].x,Polygon[2].x,Polygon[3].x,Polygon[4].x),
               imiddle(Polygon[1].y,Polygon[2].y,Polygon[3].y,Polygon[4].y),
               frlabels[i]);
     end;}
 end;
 setcolor(polygons[fronts[frontsnow[1]]].color);outtextxy( 1,1{+paged},'5');
 setcolor(polygons[fronts[frontsnow[2]]].color);outtextxy(11,1{+paged},'4');
 setcolor(polygons[fronts[frontsnow[3]]].color);outtextxy(21,1{+paged},'6');
 setcolor(polygons[fronts[frontsnow[4]]].color);outtextxy(31,1{+paged},'8');
 setcolor(polygons[fronts[frontsnow[5]]].color);outtextxy(41,1{+paged},'2');
 setcolor(polygons[fronts[frontsnow[6]]].color);outtextxy(51,1{+paged},'9');
   end
 else
   begin;
 setfillstyle(1,0);setcolor(0);bar(1,60,640,480);
 for i:=1 to MaxPolygons do begin;
   for i1:=1 to Polygons[PolyChain[i].no].n do begin;
   Polygon[i1].x:=round(ProjectionCoord[Polygons[PolyChain[i].no].Points[i1]].x*ScreenMultiplier+320);
   Polygon[i1].y:=round(ProjectionCoord[Polygons[PolyChain[i].no].Points[i1]].y*ScreenMultiplier+240)+paged;
   end;
   setfillstyle(1,Polygons[PolyChain[i].no].color);
   setcolor(Polygons[PolyChain[i].no].linecolor);
   fillpoly(Polygons[PolyChain[i].no].n,Polygon);
   {if (i in fr) then
     begin;
     setcolor(0);
     outtextxy(imiddle(Polygon[1].x,Polygon[2].x,Polygon[3].x,Polygon[4].x),
               imiddle(Polygon[1].y,Polygon[2].y,Polygon[3].y,Polygon[4].y),
               frlabels[i]);
     end;}
 end;
 setcolor(polygons[fronts[frontsnow[1]]].color);outtextxy( 1,1+paged,'5');
 setcolor(polygons[fronts[frontsnow[2]]].color);outtextxy(11,1+paged,'4');
 setcolor(polygons[fronts[frontsnow[3]]].color);outtextxy(21,1+paged,'6');
 setcolor(polygons[fronts[frontsnow[4]]].color);outtextxy(31,1+paged,'8');
 setcolor(polygons[fronts[frontsnow[5]]].color);outtextxy(41,1+paged,'2');
 setcolor(polygons[fronts[frontsnow[6]]].color);outtextxy(51,1+paged,'9');
   end;
 inc(page);
 page:=page mod 2;
 SetVisualPage(page);
 SetActivePage((page+1) mod 2);
 repeat
 c:=readkey;
 until c in [#80,#75,#77,#72,'+','-','*','/','2','4','5','6','8','9',#27,'q','[',']'];
      if {(c='2')or}(c=#80) then begin;px:= 0;py:=-1;pz:= 0;end
 else if {(c='4')or}(c=#75) then begin;px:= 1;py:= 0;pz:= 0;end
 else if {(c='6')or}(c=#77) then begin;px:=-1;py:= 0;pz:= 0;end
 else if {(c='8')or}(c=#72) then begin;px:= 0;py:= 1;pz:= 0;end
 else if            (c='[') then begin;px:= 0;py:= 0;pz:=-1;end
 else if            (c=']') then begin;px:= 0;py:= 0;pz:= 1;end
 else                            begin;px:= 0;py:= 0;pz:= 0;end;

 if (px<>0) then
 for i:=1 to MaxCoords do
 begin;
 x:=Coord[i].x;
 z:=Coord[i].z;
 Coord[i].x:=x*cos(0.09*px)-z*sin(0.09*px);
 Coord[i].z:=x*sin(0.09*px)+z*cos(0.09*px);
 end;

 if (py<>0) then
 for i:=1 to MaxCoords do
 begin;
 y:=Coord[i].y;
 z:=Coord[i].z;
 Coord[i].y:=y*cos(0.09*py)-z*sin(0.09*py);
 Coord[i].z:=y*sin(0.09*py)+z*cos(0.09*py);
 end;

 if (pz<>0) then
 for i:=1 to MaxCoords do
 begin;
 x:=Coord[i].x;
 y:=Coord[i].y;
 Coord[i].x:=x*cos(0.09*pz)-y*sin(0.09*pz);
 Coord[i].y:=x*sin(0.09*pz)+y*cos(0.09*pz);
 end;


 if (px<>0)or(py<>0)or(pz<>0) then
   begin;
   maxn:=0;
   max:=-9999999999999.0;
   for i:=1 to 6 do
     begin;
     t:=middle(coord[polygons[fronts[i]].points[1]].z,
               coord[polygons[fronts[i]].points[2]].z,
               coord[polygons[fronts[i]].points[3]].z,
               coord[polygons[fronts[i]].points[4]].z);
     if (t>max) then begin;maxn:=i;max:=t;end;
     end;
   frontsnow[1]:=maxn;
   {frlabels[fronts[maxn]]:='F';}

   maxn:=0;
   max:=99999999999999.0;
   for i:=1 to 6 do
     begin;
     t:=middle(coord[polygons[fronts[i]].points[1]].z,
               coord[polygons[fronts[i]].points[2]].z,
               coord[polygons[fronts[i]].points[3]].z,
               coord[polygons[fronts[i]].points[4]].z);
     if (t<max) then begin;maxn:=i;max:=t;end;
     end;
   frontsnow[6]:=maxn;
   {frlabels[fronts[maxn]]:=#0;}

   maxn:=0;
   max:=99999999999999.0;
   for i:=1 to 6 do
     begin;
     t:=middle(coord[polygons[fronts[i]].points[1]].y,
               coord[polygons[fronts[i]].points[2]].y,
               coord[polygons[fronts[i]].points[3]].y,
               coord[polygons[fronts[i]].points[4]].y);
     if (t<max) then begin;maxn:=i;max:=t;end;
     end;
   frontsnow[4]:=maxn;
   {frlabels[fronts[maxn]]:='U';}

   maxn:=0;
   max:=-99999999999999.0;
   for i:=1 to 6 do
     begin;
     t:=middle(coord[polygons[fronts[i]].points[1]].y,
               coord[polygons[fronts[i]].points[2]].y,
               coord[polygons[fronts[i]].points[3]].y,
               coord[polygons[fronts[i]].points[4]].y);
     if (t>max) then begin;maxn:=i;max:=t;end;
     end;
   frontsnow[5]:=maxn;
   {frlabels[fronts[maxn]]:='D';}

   maxn:=0;
   max:=-99999999999999.0;
   for i:=1 to 6 do
     begin;
     t:=middle(coord[polygons[fronts[i]].points[1]].x,
               coord[polygons[fronts[i]].points[2]].x,
               coord[polygons[fronts[i]].points[3]].x,
               coord[polygons[fronts[i]].points[4]].x);
     if (t>max) then begin;maxn:=i;max:=t;end;
     end;
   frontsnow[3]:=maxn;
   {frlabels[fronts[maxn]]:='R';}

   maxn:=0;
   max:=99999999999999.0;
   for i:=1 to 6 do
     begin;
     t:=middle(coord[polygons[fronts[i]].points[1]].x,
               coord[polygons[fronts[i]].points[2]].x,
               coord[polygons[fronts[i]].points[3]].x,
               coord[polygons[fronts[i]].points[4]].x);
     if (t<max) then begin;maxn:=i;max:=t;end;
     end;
   frontsnow[2]:=maxn;
   {frlabels[fronts[maxn]]:='L';}
   end;

 if c='+' then begin;
 CameraCoord.z:=CameraCoord.z-0.3;
 CameraPlaneZ:=CameraPlaneZ-0.3;
 if (CameraPlaneZ<1) then begin;CameraCoord.z:=CameraCoord.z+0.3;CameraPlaneZ:=CameraPlaneZ+0.3;end;
 end;

 if c='-' then begin;
 CameraCoord.z:=CameraCoord.z+0.3;
 CameraPlaneZ:=CameraPlaneZ+0.3;
 end;

 if c='*' then begin;
 ScreenMultiplier:=ScreenMultiplier*1.3;
 end;

 if c='/' then begin;
 ScreenMultiplier:=ScreenMultiplier/1.3;
 end;

 if c in ['2','4','5','6','8','9'] then
   begin;
   if (c='5') then ApplyMorph(ftm[fronts[frontsnow[1]]]);
   if (c='9') then ApplyMorph(ftm[fronts[frontsnow[6]]]);
   if (c='4') then ApplyMorph(ftm[fronts[frontsnow[2]]]);
   if (c='6') then ApplyMorph(ftm[fronts[frontsnow[3]]]);
   if (c='8') then ApplyMorph(ftm[fronts[frontsnow[4]]]);
   if (c='2') then ApplyMorph(ftm[fronts[frontsnow[5]]]);
   end;

until (c=#27)or(c='q');


end.











