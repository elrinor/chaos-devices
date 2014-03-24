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
const HI=300;

var Coord:array[1..HI] of TCoord;
    Connections:array[1..2*HI,1..2] of Integer;
    ProjectionCoord:array[1..HI] of TPlaneCoord;
    PointsToCameraDl:array[1..HI] of Real;
    Polygons:array[1..HI] of TPolygon;
    PolyChain:array[1..HI] of TPolyChainItem;
    CameraCoord:TCoord;
    CameraPlaneZ:Real;
    ScreenMultiplier,dx,dy,dz:Real;
    MaxCoords,MaxConnections,MaxPolygons:integer;
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
filename:=GetLoadingModelName;

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
end;
{============================================================================}
{============================================================================}
var x,y,z,polydl:Real;
    a,b,i,i1,px,py:integer;
    c:char;
    Polygon:array[1..4]of PointType;
    page,paged:byte;
begin;
paged:=31;
CameraCoord.x:=0;
CameraCoord.y:=0;
CameraCoord.z:=5;
ScreenMultiplier:=100;
CameraPlaneZ:=4.0;

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

{ for i:=1 to MaxConnections do begin;
 line (round(ProjectionCoord[Connections[i,1]].x*ScreenMultiplier+320),
       round(ProjectionCoord[Connections[i,1]].y*ScreenMultiplier+240),
       round(ProjectionCoord[Connections[i,2]].x*ScreenMultiplier+320),
       round(ProjectionCoord[Connections[i,2]].y*ScreenMultiplier+240));
 end;}
{ SetVisualPage(0);
 SetActivePage(1);}


 {setfillstyle(1,0);setcolor(0);bar(1,1,640,480);}
 if page=1 then
   begin;
 cleardevice;
 for i:=1 to MaxPolygons do begin;
   for i1:=1 to Polygons[PolyChain[i].no].n do begin;
   Polygon[i1].x:=round(ProjectionCoord[Polygons[PolyChain[i].no].Points[i1]].x*ScreenMultiplier+320);
   Polygon[i1].y:=round(ProjectionCoord[Polygons[PolyChain[i].no].Points[i1]].y*ScreenMultiplier+240);
   end;
   setfillstyle(1,Polygons[PolyChain[i].no].color);
   setcolor(Polygons[PolyChain[i].no].linecolor);
   fillpoly(Polygons[PolyChain[i].no].n,Polygon);
 end;
   end
 else
   begin;
 cleardevice;
 for i:=1 to MaxPolygons do begin;
   for i1:=1 to Polygons[PolyChain[i].no].n do begin;
   Polygon[i1].x:=round(ProjectionCoord[Polygons[PolyChain[i].no].Points[i1]].x*ScreenMultiplier+320);
   Polygon[i1].y:=round(ProjectionCoord[Polygons[PolyChain[i].no].Points[i1]].y*ScreenMultiplier+240)+paged;
   end;
   setfillstyle(1,Polygons[PolyChain[i].no].color);
   setcolor(Polygons[PolyChain[i].no].linecolor);
   fillpoly(Polygons[PolyChain[i].no].n,Polygon);
 end;
   end;
 inc(page);
 page:=page mod 2;
 SetVisualPage(page);
 SetActivePage((page+1) mod 2);

{ SetVisualPage(1);
 SetActivePage(0); }
 c:=readkey;
      if (c='2')or(c=#80) then begin;px:= 0;py:=-1;end
 else if (c='4')or(c=#75) then begin;px:= 1;py:= 0;end
 else if (c='6')or(c=#77) then begin;px:=-1;py:= 0;end
 else if (c='8')or(c=#72) then begin;px:= 0;py:= 1;end
 else                          begin;px:= 0;py:= 0;end;

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

until (c=#27)or(c='q');


end.











