uses crt;
type
  TCoord=array[1..3] of extended;
  TScreen=array[1..200,1..320]of byte;
  TSphere=array[1..5]of extended; {x,y,z,R,d(Cam,Sphere)}

const
  {S:TSphere=(0,0,0,10);}
  Cam:TCoord=(0,0,40);
  CamZ=22;
  LMax=3;
  LUsed=3;
  SMax=1;
  SUsed=1;

var
  L:array[1..LMax]of TCoord;
  S:array[1..SMax]of TSphere;
  Screen:TScreen absolute $A000:$0000;
  SOnScr:array[1..320,1..200]of Byte;
  c:Char;


var HMid:Extended;
    HSphere:TSphere;
Procedure SortSpheres(l,r: integer);
var
  i,j: integer;
begin
  i:=l; j:=r; HMid:=S[(l+r) DIV 2,5];
  repeat
    while S[i,5]<HMid do i:=i+1;
    while HMid<S[j,5] do j:=j-1;
    if i<=j then
    begin
      HSphere:=S[i]; S[i]:=S[j]; S[j]:=HSphere;
      Inc(i);
      Dec(j);
    end;
  until i>j;
  if l<j then SortSpheres(l,j);
  if i<r then SortSpheres(i,r);
end;



Procedure DrawSphere;
var Scr:TScreen;
    ya,xa,x,y,Col,i:Integer;
    H:TCoord;
    ExCol,ExColAdd,MinZ:Extended;
begin;
Fillchar(Scr,sizeof(Scr),0);

For i:=1 to LUsed do
  begin;
  If (L[i,3]<=CamZ) then
    begin;
    X:=160+Round(10*(Cam[1]+(L[i,1]-Cam[1])*(CamZ-Cam[3])/(L[i,3]-Cam[3])));
    Y:=100+Round(5/6*10*(Cam[2]+(L[i,2]-Cam[2])*(CamZ-Cam[3])/(L[i,3]-Cam[3])));
    If (X>=1)and(X<=320)and(Y>=1)and(Y<=200) then
      Scr[y,x]:=255;
    end;
  end;


MinZ:=Cam[3]-(sqr(Cam[3]-S[1,3])-sqr(S[1,4]))/(Cam[3]-S[1,3]);
for ya:=1 to 180 do
  begin;
for xa:=1 to 180 do
  begin;

  H[3]:=S[1,3]+S[1,4]*sin(ya/180*pi)*sin(xa/180*pi);
  if H[3]<MinZ then continue;
  H[1]:=S[1,1]+S[1,4]*sin(ya/180*pi)*cos(xa/180*pi);
  H[2]:=S[1,2]+S[1,4]*cos(ya/180*pi);

  ExCol:=0;
  For i:=1 to LUsed do
    begin;
    ExColAdd:=(-H[1]*(H[1]-L[i,1])-H[2]*(H[2]-L[i,2])-H[3]*(H[3]-L[i,3]))/
      Sqrt( (sqr(H[1]-L[i,1])+sqr(H[2]-L[i,2])+sqr(H[3]-L[i,3]))*(sqr(H[1])+sqr(H[2])+sqr(H[3])) );
    If ExColAdd<0 then ExColAdd:=0;
    ExCol:=ExCol+(1-ExCol)*ExColAdd;
    end;
  Col:=Round(255*ExCol);

  If Col<1 then Col:=1;
  X:=160+Round(10*(Cam[1]+(H[1]-Cam[1])*(CamZ-Cam[3])/(H[3]-Cam[3])));
  Y:=100+Round(5/6*10*(Cam[2]+(H[2]-Cam[2])*(CamZ-Cam[3])/(H[3]-Cam[3])));
  If (X>=1)and(X<=320)and(Y>=1)and(Y<=200) then
    Scr[y,x]:=Col;
  end;

  {if ya mod 10=0 then
    begin;
    Move(Scr,Screen,Sizeof(TScreen));
    c:=readkey;
    if c=#27 then halt;
    end;}
  end;

For i:=1 to LUsed do
  begin;
  If (L[i,3]>=S[1,4])and(L[i,3]<=CamZ) then
    begin;
    X:=160+Round(10*(Cam[1]+(L[i,1]-Cam[1])*(CamZ-Cam[3])/(L[i,3]-Cam[3])));
    Y:=100+Round(5/6*10*(Cam[2]+(L[i,2]-Cam[2])*(CamZ-Cam[3])/(L[i,3]-Cam[3])));
    If (X>=1)and(X<=320)and(Y>=1)and(Y<=200) then
      Scr[y,x]:=255;
    end;
  end;


Move(Scr,Screen,Sizeof(TScreen));
end;


var b:Byte;
    i:Integer;
    H:TCoord;
begin;
asm
  MOV AH,0
  MOV AL,13H
  INT 10H
end;

while ( ( Port[ $3da ] and $8 ) <> 0 ) do ;
while ( not ( ( Port[ $3da ] and $8 ) <> 0 ) ) do ;

Port[$3c8]:=0;
Port[$3c9]:=0;
Port[$3c9]:=0;
Port[$3c9]:=0;
for b:=1 to 255 do
  begin
  Port[$3c8]:=b;
  Port[$3c9]:=2+b div 5;
  Port[$3c9]:=2+b div 5;
  Port[$3c9]:=2+b div 5;
  end;

{For i:=1 to 255 do Screen[10,i]:=i;

readkey;}

S[1,1]:=0;
S[1,2]:=0;
S[1,3]:=0;
S[1,4]:=10;

L[1,1]:=15;
L[1,2]:=10;
L[1,3]:=0;

L[2,1]:=-5;
L[2,2]:=0;
L[2,3]:=15;

L[3,1]:=10;
L[3,2]:=0;
L[3,3]:=30;

repeat
  For i:=1 to SUsed do
    S[i,5]:=sqr(S[i,1]-Cam[1])+sqr(S[i,2]-Cam[2])+sqr(S[i,3]-Cam[3]);
  SortSpheres(1,SUsed);

  FillChar(SOnScr,sizeof(SOnScr),0);
  For i:=1 to SUsed do
    begin;



    end;

  DrawSphere;
  H[1]:=L[1,1]*cos(4*pi/180)-L[1,3]*sin(4*pi/180);
  H[3]:=L[1,1]*sin(4*pi/180)+L[1,3]*cos(4*pi/180);
  H[2]:=L[1,2];
  L[1]:=H;
  H[2]:=L[2,2]*cos(-8*pi/180)-L[2,3]*sin(-8*pi/180);
  H[3]:=L[2,2]*sin(-8*pi/180)+L[2,3]*cos(-8*pi/180);
  H[1]:=L[2,1];
  L[2]:=H;
  H[2]:=L[3,2]*cos(7*pi/180)-L[3,3]*sin(7*pi/180);
  H[3]:=L[3,2]*sin(7*pi/180)+L[3,3]*cos(7*pi/180);
  H[1]:=L[3,1];
  L[3]:=H;
  {Delay();}
until keypressed;


asm
  MOV AH,0
  MOV AL,3
  INT 10H
end;
end.