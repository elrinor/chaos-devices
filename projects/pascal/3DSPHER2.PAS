uses crt;
{DEFINE DEBUG}

type
  TCoord=array[1..3] of extended;
  TScreen=array[1..200,1..320]of byte;
  TEntity=record
    tip:Byte; {0-light,1-sphere}
    Coord:TCoord;
    R:Extended;
    D:Extended;
  end;

const
  {Camera}
  CamDX=160;
  CamDY=100;
  CamZoom=10;
  Cam:TCoord=(0,0,50);
  CamZ=30;
  CamYMlp=5/6;

  {Lights & Spheres}
  EMax=3;

  eps=0.0000001;

var
  E:array[1..EMax]of TEntity;
  Screen:TScreen absolute $A000:$0000;
  SOnScr:array[1..320,1..200]of Byte;
  c:Char;

Function Max(const a,b:Integer):Integer; assembler;
asm
  MOV AX,b
  CMP AX,a
  JG @@Nafig
  MOV AX,a
@@Nafig:
end;

Function Min(const a,b:Integer):Integer; assembler;
asm
  MOV AX,b
  CMP AX,a
  JL @@Nafig
  MOV AX,a
@@Nafig:
end;

Function RoundUp(const X:Extended):Integer;
begin;
If Frac(X)<eps then RoundUp:=Trunc(X)
else RoundUp:=Trunc(X)+1;
end;


Procedure GetScreenCoord(const V1,V2,V3:Extended;var x,y:Integer);
var K:Extended;
begin;
K:=(CamZ-Cam[3])/(V3-Cam[3]);
X:=CamDX+Round(        CamZoom*(Cam[1]+(V1-Cam[1])*K));
Y:=CamDY+Round(CamYMlp*CamZoom*(Cam[2]+(V2-Cam[2])*K));
end;

Procedure Get3DCoord(var V1,V2,V3:Extended;const x,y:Integer);
var K:Extended;
begin;
V3:=CamZ;
V1:=(x-CamDX)/CamZoom;
V2:=(y-CamDY)/(CamZoom*CamYMlp);
end;


Procedure GenerateSOnScr;
var i,sx,sy:Integer;
    R2,AC2,k,x,y,dx,dy,dy2,dx2,NewDy:Extended;
begin;
FillChar(SOnScr,sizeof(SOnScr),0);
For i:=1 to EMax do
  if E[i].tip=1 then
    begin;
    k:=(CamZ-Cam[3])/(E[i].Coord[3]-Cam[3]);
    x:=CamDX+        CamZoom*(Cam[1]+(E[i].Coord[1]-Cam[1])*K);
    y:=CamDY+CamYMlp*CamZoom*(Cam[2]+(E[i].Coord[2]-Cam[2])*K);
    AC2:=(sqr(E[i].Coord[1]-Cam[1])+sqr(E[i].Coord[2]-Cam[2])+sqr(E[i].Coord[3]-Cam[3]));
    R2:=sqr(E[i].R);
    dx:=CamZoom*abs((Cam[3]-CamZ)*Sqrt((R2*AC2-sqr(R2))/sqr(AC2-R2)));
    dy:=dx*CamYMlp;
    dx2:=sqr(dx);
    dy2:=sqr(dy);
    for sx:=max(RoundUp(x-dx),1) to min(Trunc(x+dx),320) do
      begin;
      NewDy:=sqrt(abs(dy2-dy2/dx2*sqr(sx-x)));
      for sy:=max(RoundUp(y-NewDy),1) to min(Trunc(y+NewDy),200) do
        SOnScr[sx,sy]:=i;
      end;
    end
  else
    begin;
    GetScreenCoord(E[i].Coord[1],E[i].Coord[2],E[i].Coord[3],sx,sy);
    if (sx>1)and(sx<320)and(sy>1)and(sy<200) then
      SOnScr[sx,sy]:=i;
    end;
end;


var HMid:Extended;
    HSphere:TEntity;
Procedure SortSpheres(l,r: integer);
var
  i,j: integer;
begin
  i:=l; j:=r; HMid:=E[(l+r) DIV 2].D;
  repeat
    while E[i].D>HMid do i:=i+1;
    while HMid>E[j].D do j:=j-1;
    if i<=j then
    begin
      HSphere:=E[i]; E[i]:=E[j]; E[j]:=HSphere;
      Inc(i);
      Dec(j);
    end;
  until i>j;
  if l<j then SortSpheres(l,j);
  if i<r then SortSpheres(i,r);
end;

Procedure DrawMasks;
var Scr:TScreen;
    sx,sy:Integer;
begin;
Fillchar(Scr,sizeof(Scr),0);

For sx:=1 to 320 do for sy:=1 to 200 do
  Scr[sy,sx]:=SOnScr[sx,sy]*(255 div EMax);
Move(Scr,Screen,Sizeof(TScreen));
{Readkey;}
end;

Procedure DrawSpheres;
var Scr:TScreen;
    sx,sy,i,j,Col:Integer;
    H,H2,H3:TCoord;
    a,b,c,t,ExCol,ExColAdd,DlH3,DlH2:Extended;
label Nafig;
begin;
Fillchar(Scr,sizeof(Scr),0);

For sx:=1 to 320 do for sy:=1 to 200 do
  begin;
  if SOnScr[sx,sy]<>0 then
    if E[SOnScr[sx,sy]].tip=0 then
      begin;
      Scr[sy,sx]:=255;
      end
    else
      begin;
      Get3DCoord(H[1],H[2],H[3],sx,sy);
      H[1]:=H[1]-Cam[1];
      H[2]:=H[2]-Cam[2];
      H[3]:=H[3]-Cam[3];
      H2[1]:=Cam[1]-E[SOnScr[sx,sy]].Coord[1];
      H2[2]:=Cam[2]-E[SOnScr[sx,sy]].Coord[2];
      H2[3]:=Cam[3]-E[SOnScr[sx,sy]].Coord[3];
      a:=sqr(H[1])+sqr(H[2])+sqr(H[3]);
      b:=H2[1]*H[1]+H2[2]*H[2]+H2[3]*H[3];
      c:=sqr(H2[1])+sqr(H2[2])+sqr(H2[3])-sqr(E[SOnScr[sx,sy]].R);
      t:=(-b-sqrt(abs(b*b-a*c)))/a;
      H[1]:=t*H[1]+Cam[1];
      H[2]:=t*H[2]+Cam[2];
      H[3]:=t*H[3]+Cam[3];
      H2[1]:=-H[1]+E[SOnScr[sx,sy]].Coord[1];
      H2[2]:=-H[2]+E[SOnScr[sx,sy]].Coord[2];
      H2[3]:=-H[3]+E[SOnScr[sx,sy]].Coord[3];

      DlH2:=Sqrt(sqr(H2[1])+sqr(H2[2])+sqr(H2[3]));
      ExCol:=0;
      For i:=1 to EMax do if E[i].tip=0 then
        begin;
        For j:=1 to EMax do if E[j].tip=1 then
          begin;
          a:=sqr(E[i].Coord[1]-E[j].Coord[1])+
             sqr(E[i].Coord[2]-E[j].Coord[2])+
             sqr(E[i].Coord[3]-E[j].Coord[3])-sqr(E[j].R);
          b:=sqr(H[1]-E[j].Coord[1])+
             sqr(H[2]-E[j].Coord[2])+
             sqr(H[3]-E[j].Coord[3])-sqr(E[j].R);
          if (a>0)and(b>0) then
            if sqrt(a)+sqrt(b)<
               sqrt(sqr(H[1]-E[i].Coord[1])+
                    sqr(H[2]-E[i].Coord[2])+
                    sqr(H[3]-E[i].Coord[3])) then
              begin;
              Col:=1;
              goto Nafig;
              end;
          end;

        H3[1]:=H[1]-E[i].Coord[1];
        H3[2]:=H[2]-E[i].Coord[2];
        H3[3]:=H[3]-E[i].Coord[3];
        DlH3:=sqrt(sqr(H3[1])+sqr(H3[2])+sqr(H3[3]));
        ExColAdd:=(H3[1]*H2[1]+H3[2]*H2[2]+H3[3]*H2[3])/
          (DlH3*DlH2);
        If ExColAdd<0 then ExColAdd:=0;
        ExCol:=ExCol+(1-ExCol)*ExColAdd/(DlH3/4+3/4);
        end;
      Col:=Round(255*ExCol);
      If Col<1 then Col:=1;

      Nafig:
      Scr[sy,sx]:=Col;
      end;
  end;
Move(Scr,Screen,Sizeof(TScreen));
end;



var b:Byte;
    i,sx,sy,dx,dy:Integer;
    k,x,y,z:Extended;
    H:TCoord;
begin;

{$IFNDEF DEBUG}
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
{$ENDIF}

E[1].tip:=1;
E[1].Coord[1]:=7;
E[1].Coord[2]:=0;
E[1].Coord[3]:=-7;
E[1].R:=5;

E[2].tip:=1;
E[2].Coord[1]:=-7;
E[2].Coord[2]:=0;
E[2].Coord[3]:=7;
E[2].R:=5;

E[3].tip:=0;
E[3].Coord[1]:=0;
E[3].Coord[2]:=25;
E[3].Coord[3]:=0;
E[3].R:=0;

repeat
  For i:=1 to EMax do
    E[i].D:=sqr(E[i].Coord[1]-Cam[1])+sqr(E[i].Coord[2]-Cam[2])+sqr(E[i].Coord[3]-Cam[3]);
  SortSpheres(1,EMax);

  GenerateSOnScr;

  {DrawMasks;}
  DrawSpheres;

  For i:=1 to EMax do if E[i].tip=0 then
    begin;
    H[1]:=E[i].Coord[1]*cos(8*pi/180)-E[i].Coord[2]*sin(8*pi/180);
    H[2]:=E[i].Coord[1]*sin(8*pi/180)+E[i].Coord[2]*cos(8*pi/180);
    H[3]:=E[i].Coord[2];
    E[i].Coord:=H;
    end
  else
    begin;
    H[1]:=E[i].Coord[1]*cos(4*pi/180)-E[i].Coord[3]*sin(4*pi/180);
    H[3]:=E[i].Coord[1]*sin(4*pi/180)+E[i].Coord[3]*cos(4*pi/180);
    H[2]:=E[i].Coord[2];
    E[i].Coord:=H;
    end;
  {H[2]:=L[2,2]*cos(-8*pi/180)-L[2,3]*sin(-8*pi/180);
  H[3]:=L[2,2]*sin(-8*pi/180)+L[2,3]*cos(-8*pi/180);
  H[1]:=L[2,1];
  L[2]:=H;
  H[2]:=L[3,2]*cos(7*pi/180)-L[3,3]*sin(7*pi/180);
  H[3]:=L[3,2]*sin(7*pi/180)+L[3,3]*cos(7*pi/180);
  H[1]:=L[3,1];
  L[3]:=H;}
  {Delay();}
until keypressed;


asm
  MOV AH,0
  MOV AL,3
  INT 10H
end;
end.