{$A+,B-,D+,E+,F-,G+,I+,L+,N+,O-,P-,Q+,R+,S+,T-,V+,X+,Y+}
{$M 65520,0,655360}
uses crt;
{DEFINE DEBUG}
{$IFDEF DEBUG}
var HHH:Integer;
{$ENDIF}

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
  CamDX:integer=160;
  CamDY:integer=100;
  CamZoom:extended=10;
  Cam:TCoord=(0,0,50);
  Cam1:extended=0;
  Cam2:extended=0;
  Cam3:extended=50;
  CamZ:extended=30;
  CamYMlp:extended=5/6;

  {Lights & Spheres}
  EMax=9;

  eps=0.0000001;

var
  E:array[1..EMax]of TEntity;
  Screen:TScreen absolute $A000:$0000;
  SOnScr:array[1..320,1..200]of Byte;
  c:Char;

{Function ExtMax(const a,b:Extended):Extended;
begin;
if a>b then ExtMax:=a
else ExtMax:=b;
end;}
function ExtMax(const a,b:Extended):Extended; assembler;
asm
  FLD A
  FLD B
  FCOM ST(1)
  FSTSW AX
  SAHF
  JA @@Nafig
  FSTP ST(0)
@@Nafig:
end;

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

{Function RoundUp(const X:Extended):Integer;
begin;
If Frac(X)<eps then RoundUp:=Trunc(X)
else RoundUp:=Trunc(X)+1;
end;}
Function RoundUP(const a:Extended):Integer; assembler;
var b,c:Integer;
asm
  MOV B, 7026
  FSTCW C
  FLDCW B
  FLD A
  FRNDINT
  FISTP B
  MOV AX, B
  FLDCW C
end;


{Procedure GetScreenCoord(const V1,V2,V3:Extended;var x,y:Integer);
var K:Extended;
begin;
K:=(CamZ-Cam[3])/(V3-Cam[3]);
X:=CamDX+Round(        CamZoom*(Cam[1]+(V1-Cam[1])*K));
Y:=CamDY+Round(CamYMlp*CamZoom*(Cam[2]+(V2-Cam[2])*K));
end;}
Procedure GetScreenCoord(const V1,V2,V3:Extended;var x1,y1:Integer);
var x,y:Integer;
begin;
asm
  FLD CamZ
  FLD Cam3
  FSUBP
  FLD V3
  FLD Cam3
  FSUBP
  FDIVP

  FLD V1
  FLD Cam1
  FSUBP

  FMUL ST(0), ST(1)
  FLD Cam1
  FADDP
  FLD CamZoom
  FMULP
  FRNDINT
  FIADD CamDX
  FISTP x

  FLD V2
  FLD Cam2
  FSUBP

  FMULP
  FLD Cam2
  FADDP
  FLD CamZoom
  FMULP
  FLD CamYMlp
  FMULP
  FRNDINT
  FIADD CamDY
  FISTP y
end;
x1:=x;y1:=y;
end;


Procedure Get3DCoord(var V1,V2,V3:Extended;const x,y:Integer);
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
              goto Nafig;
          end;

        H3[1]:=H[1]-E[i].Coord[1];
        H3[2]:=H[2]-E[i].Coord[2];
        H3[3]:=H[3]-E[i].Coord[3];
        DlH3:=sqrt(sqr(H3[1])+sqr(H3[2])+sqr(H3[3]));
        ExColAdd:=(H3[1]*H2[1]+H3[2]*H2[2]+H3[3]*H2[3])/
          (DlH3*DlH2);
        If ExColAdd<0 then ExColAdd:=0;
        ExCol:=ExCol+(1-ExCol)*ExColAdd/(ExtMax(DlH3/4+3/4,1));

        Nafig:
        end;
      Col:=Round(255*ExCol);
      If Col<1 then Col:=1;

      Scr[sy,sx]:=Col;
      end;
  end;
Move(Scr,Screen,Sizeof(TScreen));
end;

Procedure RenderCycle;
var i:Integer;
    H:TCoord;
begin;
  For i:=1 to EMax do
    E[i].D:=sqr(E[i].Coord[1]-Cam[1])+sqr(E[i].Coord[2]-Cam[2])+sqr(E[i].Coord[3]-Cam[3]);
  SortSpheres(1,EMax);

  GenerateSOnScr;

  {DrawMasks;}
  DrawSpheres;

  For i:=1 to EMax do if E[i].tip=0 then
    begin;
    H[3]:=E[i].Coord[3]*cos(2*pi/180)-E[i].Coord[2]*sin(2*pi/180);
    H[2]:=E[i].Coord[3]*sin(2*pi/180)+E[i].Coord[2]*cos(2*pi/180);
    H[1]:=E[i].Coord[1];
    E[i].Coord:=H;
    end
  else
    begin;
    H[1]:=E[i].Coord[1]*cos(2*pi/180)-E[i].Coord[3]*sin(2*pi/180);
    H[3]:=E[i].Coord[1]*sin(2*pi/180)+E[i].Coord[3]*cos(2*pi/180);
    H[2]:=E[i].Coord[2];
    E[i].Coord:=H;
    end;
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
E[1].Coord[1]:=0;
E[1].Coord[2]:=0;
E[1].Coord[3]:=0;
E[1].R:=7;

E[2].tip:=1;
E[2].Coord[1]:=3;
E[2].Coord[2]:=0;
E[2].Coord[3]:=10;
E[2].R:=2;

E[3].tip:=0;
E[3].Coord[1]:=0;
E[3].Coord[2]:=0;
E[3].Coord[3]:=15;
E[3].R:=0;

E[4].tip:=0;
E[4].Coord[1]:=20;
E[4].Coord[2]:=0;
E[4].Coord[3]:=-15;
E[4].R:=0;

E[5].tip:=0;
E[5].Coord[1]:=20;
E[5].Coord[2]:=15;
E[5].Coord[3]:=0;
E[5].R:=0;

E[6].tip:=0;
E[6].Coord[1]:=-20;
E[6].Coord[2]:=-15;
E[6].Coord[3]:=0;
E[6].R:=0;

E[7].tip:=0;
E[7].Coord[1]:=-3;
E[7].Coord[2]:=0;
E[7].Coord[3]:=15;
E[7].R:=0;

E[8].tip:=0;
E[8].Coord[1]:=3;
E[8].Coord[2]:=0;
E[8].Coord[3]:=15;
E[8].R:=0;

E[9].tip:=1;
E[9].Coord[1]:=-3;
E[9].Coord[2]:=0;
E[9].Coord[3]:=10;
E[9].R:=2;

{$IFDEF DEBUG}
For HHH:=1 to 58 do
  RenderCycle;
{$ENDIF}

repeat
  RenderCycle;
until keypressed;


asm
  MOV AH,0
  MOV AL,3
  INT 10H
end;
end.