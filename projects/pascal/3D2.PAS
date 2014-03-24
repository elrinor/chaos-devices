uses graph,crt,dos;


function GetLoadingModelName:string;
var i,n:integer;
    ModelsArr:array[1..10] of string;
    DirInfo:SearchRec;
    s:string;
begin;
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
readln(s);
if not((length(s)=1)and(ord(s[1])-ord('a')<n)) then writeln('*WISELY*, I said!');
until (length(s)=1)and(ord(s[1])-ord('a')<n);

GetLoadingModelName:=ModelsArr[ord(s[1])-ord('a')+1];
end;


type TCoord=record
              x:real;
              y:real;
              z:real;
            end;
type TPlaneCoord=record
              x:real;
              y:real;
            end;

const HI=200;
var Coord:array[1..HI] of TCoord;
		Connections:array[1..2*HI,1..2] of Integer;
		ProjectionCoord:array[1..HI] of TPlaneCoord;
		c:Char;
		CameraCoord:TCoord;
		CameraPlaneZ:Real;
		ScreenMultiplier,Los,x,y,z,dx,dy,dz:Real;
		i,a,b,px,py,MaxCoords,MaxConnections:integer;
		filename,s:string;
		f:text;
begin;
CameraCoord.x:=0;
CameraCoord.y:=0;
CameraCoord.z:=5;
ScreenMultiplier:=100;
CameraPlaneZ:=4.0;
Los:=1;

filename:=GetLoadingModelName;
assign(f,filename);
reset(f);
readln(f);
readln(f,s);
for i:=1 to 24 do writeln;
writeln(s);
writeln('Press Enter to continue');
readln;
for i:=1 to 24 do writeln;
writeln('Loading... please wait...');

readln(f,MaxCoords);
readln(f,MaxConnections);
readln(f);
readln(f,dx,dy,dz);
for i:=1 to MaxCoords do readln(f,Coord[i].x,Coord[i].y,Coord[i].z);
for i:=1 to MaxConnections do readln(f,Connections[i,1],Connections[i,2]);
for i:=1 to MaxCoords do with Coord[i] do begin;x:=x+dx;y:=y+dy;z:=z+dz;end;

a:=detect;
InitGraph(a,b,'');
cleardevice;


repeat
 cleardevice;
 for i:=1 to MaxCoords do begin;
   ProjectionCoord[i].x:=(-1*(CameraCoord.x-Coord[i].x)/(Coord[i].z-CameraCoord.z))*CameraPlaneZ+
   (-1*(Coord[i].x*(CameraCoord.z-Coord[i].z)-Coord[i].z*(CameraCoord.x-Coord[i].x))/
   (Coord[i].z-CameraCoord.z));

   ProjectionCoord[i].y:=(-1*(CameraCoord.y-Coord[i].y)/(Coord[i].z-CameraCoord.z))*CameraPlaneZ+
   (-1*(Coord[i].y*(CameraCoord.z-Coord[i].z)-Coord[i].z*(CameraCoord.y-Coord[i].y))/
   (Coord[i].z-CameraCoord.z));
 end;
 for i:=1 to MaxConnections do begin;
 line (round(ProjectionCoord[Connections[i,1]].x*ScreenMultiplier+320),
       round(ProjectionCoord[Connections[i,1]].y*ScreenMultiplier+240),
       round(ProjectionCoord[Connections[i,2]].x*ScreenMultiplier+320),
       round(ProjectionCoord[Connections[i,2]].y*ScreenMultiplier+240));
 end;

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











