unit REngine;

interface
uses RTypes, Classes, Graphics, IntStack;

const PartialRefractionBorder=0.0;
const Exp05:array[1..12]of Float=(1,0.5,0.25,0.125,0.0625,0.03125,0.015625,0.0078125,0.00390625,0.001953125,0.0009765625,0.00048828125);

type
  TREngine = class(TObject)
  private
    FEntities: array[-1..300] of TEntity;
    FECount: Integer;
    FCam: TCamera;
    FAmbientColor: T3Float;
    FTraceCount: Integer;
    FMaxTraceCount: Integer;
    FEntityStack: TIntStack;
    FC: array[0..11] of T3Float;
  protected
    procedure Trace(V:TVector; out Color:T3Float; out Point:T3Float);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddEntity(Entity: TEntity);
    function Render(Time:Integer):TBitmap;
    property AmbientColor: T3Float read FAmbientColor write FAmbientColor;
    property Camera: TCamera read FCam write FCam;
    property MaxTraceCount: Integer read FMaxTraceCount write FMaxTraceCount;
  end;

implementation
uses SysUtils, RUtils;

constructor TREngine.Create;
begin;
inherited Create;
FEntityStack:=TIntStack.Create;
FECount:=0;
FEntities[-1]:=TEntity.Create;
FEntities[-1].KRefraction:=1;
//FEntities[-1].KTransparent:=Make3Float(0,0,0);
FEntities[-1].Index:=-1;
end;

destructor TREngine.Destroy;
begin;
FEntityStack.Free;
inherited Destroy;
end;

procedure TREngine.AddEntity(Entity: TEntity);
begin;
Inc(FECount);
FEntities[FECount-1]:=Entity;
Entity.Index:=FECount-1;
end;

// Render
function TREngine.Render(Time:Integer):TBitmap;
var h,w,x,y,i:Integer;
    r,g,b:Float;
    p:PByteArray;
    Color, Point: T3Float;
    Ray: TVector;
    Progress, MaxProgress, PercentDone:Integer;
begin;
Result:=TBitmap.Create;
Result.Height:=FCam.ResY;
Result.Width:=FCam.ResX;
Result.PixelFormat:=pf24bit;
w:=Result.Width*3; w:=w+(4-w mod 4)mod 4;
h:=Result.Height-1;
P:=Result.ScanLine[Result.Height-1];

{FEntityStack.Clear;
FEntityStack.Push(-1);
Ray.Coord:=Make3Float(0,-3,0.4);
Ray.Direction:=Normalize(Make3Float(0,1,0));
Trace(Ray,Color,Point);  }


{x:=231;y:=207;
FTraceCount:=0;
FEntityStack.Clear;
FEntityStack.Push(-1);
FC[1]:=Make3Float(1,1,1);
Trace(FCam.GetRay(x,y),Color,Point);} 

MaxProgress:=FCam.ResX*FCam.ResY;
Progress:=0;
PercentDone:=-1;

for x:=0 to FCam.ResX-1 do for y:=0 to FCam.ResY-1 do
  begin;
  Inc(Progress);
  if(Round(100*Progress/MaxProgress)>PercentDone) then
    begin;
    PercentDone:=Round(100*Progress/MaxProgress);
    Write('Rendering ',PercentDone,'%  '#13);
    end;

  if (FCam is TDoFCamera) then
    begin;
    r:=0;
    g:=0;
    b:=0;
    for i:=1 to (FCam as TDoFCamera).RayCount do
      begin;
      FTraceCount:=0;
      FEntityStack.Clear;
      FEntityStack.Push(-1);
      FC[1]:=Make3Float(1,1,1);
      Trace((FCam as TDoFCamera).GetRay(x,y),Color, Point);
      r:=r+Color[0];
      g:=g+Color[1];
      b:=b+Color[2];
      end;
    r:=r / (FCam as TDoFCamera).RayCount;
    g:=g / (FCam as TDoFCamera).RayCount;
    b:=b / (FCam as TDoFCamera).RayCount;
    P^[(h-y)*w+x*3+0]:=Trunc(b*255);
    P^[(h-y)*w+x*3+1]:=Trunc(g*255);
    P^[(h-y)*w+x*3+2]:=Trunc(r*255);
    end
  else
    begin;
    FTraceCount:=0;
    FEntityStack.Clear;
    FEntityStack.Push(-1);
    FC[1]:=Make3Float(1,1,1);
    Trace(FCam.GetRay(x,y),Color,Point);
    P^[(h-y)*w+x*3+0]:=Trunc(Color[2]*255);
    P^[(h-y)*w+x*3+1]:=Trunc(Color[1]*255);
    P^[(h-y)*w+x*3+2]:=Trunc(Color[0]*255);
    end;
  end;

writeln;  
end;


// Trace
procedure TREngine.Trace(V:TVector; out Color:T3Float; out Point:T3Float);
var i,j:Integer;
    Intersection, NearestIntersection: TIntersection;
    MinDistance, Distance: Float;
    NewColor, NewPoint: T3Float;
    NewV: TVector;
    CosNV, D, K: Float;
    Leaves: Integer;
begin;
Inc(FTraceCount);
if (FTraceCount>=MaxTraceCount+1) or ((FC[FTraceCount][0]+FC[FTraceCount][1]+FC[FTraceCount][2])/(3)<0.01) then
  begin;
  Point:=Make3Float(0,0,0);
  Color:=AmbientColor;
  Exit;
  end;
NearestIntersection.Entity:=-1;
MinDistance:=1000000000000000000.0;
V.Coord:=VAdd(V.Coord,VMlp(V.Direction,0.0001)); //to avoid reintersections
for i:=0 to FECount-1 do
  begin;
  if FEntities[i].Intersects(V, MinDistance) then
    begin;
    FEntities[i].Intersect(V, Intersection);
    Distance:=VAbs2(VSub(V.Coord,Intersection.Normal.Coord));
    if Distance<MinDistance then
      begin;
      NearestIntersection:=Intersection;
      MinDistance:=Distance;
      end;
    end;
  end;

if (NearestIntersection.Entity=-1) then
  begin;
  Point:=Make3Float(0,0,0);
  Color:=AmbientColor;
  end
else
  begin;
  Intersection:=NearestIntersection;
  Color:=Intersection.Color;

  NewV.Coord:=Intersection.Normal.Coord;
  if (Intersection.CRefraction[0]>0)or(Intersection.CRefraction[1]>0)or(Intersection.CRefraction[2]>0) then
    begin;
    if FEntityStack.Last=Intersection.Entity then {entity stack - add}
      Leaves:=FEntityStack.Pop
    else
      begin;
      Leaves:=FEntityStack.Last;
      FEntityStack.Push(Intersection.Entity);
      end;
    CosNV:=-VSMlp(V.Direction,Intersection.Normal.Direction); {refraction}
    K:=FEntities[Leaves].KRefraction/FEntities[FEntityStack.Last].KRefraction;
    D:=sqr(K)*(sqr(CosNV)-1)+1;
    if D>0 then
      begin;
      NewV.Direction:=Normalize(VAdd(VMlp(V.Direction,K),VMlp(Intersection.Normal.Direction, -sqrt(D)+K*CosNV)));
      FC[FTraceCount+1]:=VMlp(FC[FTraceCount],Intersection.CRefraction);
      Trace(NewV, NewColor, NewPoint);
      Color:=VAdd(Color, VMlp(NewColor,Intersection.CRefraction));
      end;
    if FEntityStack.Last=Intersection.Entity then {entity stack - remove}
      FEntityStack.Pop
    else
      FEntityStack.Push(Leaves);
    if (Intersection.CReflection[0]>0)or(Intersection.CReflection[1]>0)or(Intersection.CReflection[2]>0)or(D<0) then {reflection}
      begin;
      NewV.Direction:=VAdd(V.Direction,VMlp(Intersection.Normal.Direction,2*CosNV));
      if (D<0) then
        FC[FTraceCount+1]:=VMlp(FC[FTraceCount],VAdd(Intersection.CReflection, Intersection.CRefraction))
      else
        FC[FTraceCount+1]:=VMlp(FC[FTraceCount],Intersection.CReflection);
      Trace(NewV,NewColor,NewPoint);
      if (D<0) then
        Color:=VAdd(Color, VMlp(NewColor,VAdd(Intersection.CReflection, Intersection.CRefraction)))
      else
        Color:=VAdd(Color, VMlp(NewColor,Intersection.CReflection))
      end;
    end
  else if (Intersection.CReflection[0]>0)or(Intersection.CReflection[1]>0)or(Intersection.CReflection[2]>0) then {reflection only}
    begin;
    NewV.Direction:=VAdd(V.Direction,VMlp(Intersection.Normal.Direction,-2*VSMlp(V.Direction,Intersection.Normal.Direction)));
    FC[FTraceCount+1]:=VMlp(FC[FTraceCount],Intersection.CReflection);
    Trace(NewV,NewColor,NewPoint);
    Color:=VAdd(Color, VMlp(NewColor,Intersection.CReflection));
    end;
  if (Intersection.CDiffusion[0]>0)or(Intersection.CDiffusion[1]>0)or(Intersection.CDiffusion[2]>0) then {diffusion}
    begin;
    NewPoint:=CVector111;
    for i:=0 to FECount-1 do if (FEntities[i] is TLight) then
      begin;
      NewV.Direction:=VSub((FEntities[i] as TLight).Coord, Intersection.Normal.Coord);
      if VSMlp(NewV.Direction,Intersection.Normal.Direction)>0 then
        begin;
        MinDistance:=VAbs2(NewV.Direction);
        NewV.Direction:=VMlp(NewV.Direction, 1/sqrt(MinDistance));
        NewV.Coord:=VAdd(Intersection.Normal.Coord,VMlp(NewV.Direction,0.0001));
        NewColor:=CVector111;
        for j:=0 to FECount-1 do {if (j<>Intersection.Entity) then}
          begin;
          if FEntities[j].Intersects(NewV, MinDistance) then
            begin;
            FEntities[j].Intersect(NewV, NearestIntersection);
            Distance:=VAbs2(VSub(NewV.Coord,NearestIntersection.Normal.Coord));
            if Distance<MinDistance then
              begin;
              if (NearestIntersection.CRefraction[0]=0)and(NearestIntersection.CRefraction[1]=0)and(NearestIntersection.CRefraction[2]=0) then
                begin;
                NewColor:=Make3Float(0,0,0);
                Break;
                end
              else
                NewColor:=VMlp(NewColor, NearestIntersection.CRefraction);
              end;
            end;
          end;
        NewPoint:=VMlp(NewPoint, VSub(CVector111, VMlp(VMlp(NewColor, VSMlp(Intersection.Normal.Direction,NewV.Direction)), (FEntities[i] as TLight).GetColor(VMlp(NewV.Direction,-1)))));
        end;
      end;
    Color:=VAdd(Color, VMlp(VSub(CVector111, NewPoint), Intersection.CDiffusion));
    end;
  end;
Dec(FTraceCount);
end;

initialization

finalization

end.

