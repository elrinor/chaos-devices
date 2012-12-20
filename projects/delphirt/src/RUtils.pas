unit RUtils;

interface
uses RTypes;

const
  CVector111: T3Float = (1.0, 1.0, 1.0);
  CVector000: T3Float = (0.0, 0.0, 0.0);
  CVector100: T3Float = (1.0, 0.0, 0.0);
  CVector010: T3Float = (0.0, 1.0, 0.0);
  CVector001: T3Float = (0.0, 0.0, 1.0);

function VMlp(v1,v2: T3Float):T3Float; overload;
function VMlp(v1: T3Float; k:Float): T3Float; overload;
function VAbs(v: T3Float):Float;
function VAbs2(v: T3Float):Float;
function VAdd(v1,v2: T3Float):T3Float;
function VSub(v1,v2: T3Float):T3Float;
function VCos(v1,v2: T3Float):Float;
function Normalize(v: T3Float): T3Float;
function VVMlp(v1,v2: T3Float): T3Float;
function VSMlp(v1,v2: T3Float): Float;
procedure GetLinearCoord(V, VX, VY: T3Float; out x,y: Float);
procedure GetLinearCoordP(V, VX, VY, VZ: T3Float; out Coord: T3Float);

implementation
uses SysUtils;

function VMlp(v1,v2: T3Float):T3Float; overload;
begin;
Result[0]:=v1[0]*v2[0];
Result[1]:=v1[1]*v2[1];
Result[2]:=v1[2]*v2[2];
end;

function VMlp(v1: T3Float; k:Float): T3Float;
begin;
Result[0]:=v1[0]*k;
Result[1]:=v1[1]*k;
Result[2]:=v1[2]*k;
end;

function VAbs(v: T3Float):Float;
begin;
Result:=Sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]);
end;

function VAbs2(v: T3Float):Float;
begin;
Result:=(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]);
end;

function Normalize(v: T3Float): T3Float;
var L:Float;
begin;
try
L:=VAbs(v);
Result[0]:=v[0]/L;
Result[1]:=v[1]/L;
Result[2]:=v[2]/L;
except on e:exception do;
end;

end;

function VAdd(v1,v2: T3Float):T3Float;
begin;
Result[0]:=v1[0]+v2[0];
Result[1]:=v1[1]+v2[1];
Result[2]:=v1[2]+v2[2];
end;

function VSub(v1,v2: T3Float):T3Float;
begin;
Result[0]:=v1[0]-v2[0];
Result[1]:=v1[1]-v2[1];
Result[2]:=v1[2]-v2[2];
end;

function VCos(v1,v2: T3Float):Float;
begin;
Result:=(v1[0]*v2[0]+v1[1]*v2[1]+v1[2]*v2[2])/sqrt((v1[0]*v1[0]+v1[1]*v1[1]+v1[2]*v1[2])*(v2[0]*v2[0]+v2[1]*v2[1]+v2[2]*v2[2]));
end;

function VVMlp(v1,v2: T3Float): T3Float;
begin;
Result[0]:= v1[1]*v2[2]-v2[1]*v1[2];
Result[1]:=-v1[0]*v2[2]+v2[0]*v1[2];
Result[2]:= v1[0]*v2[1]-v2[0]*v1[1];
end;

function VSMlp(v1,v2: T3Float): Float;
begin;
Result:=(v1[0]*v2[0]+v1[1]*v2[1]+v1[2]*v2[2]);
end;

procedure GetLinearCoord(V, VX, VY: T3Float; out x,y: Float);
var y2,xy,yv:Float;
begin;
y2:=VAbs2(VY);
xy:=VSMlp(VX, VY);
yv:=VSMlp(V, VY);
x:=(VSMlp(V, VX)*y2-yv*xy)/(VAbs2(VX)*y2-sqr(xy));
y:=(yv-x*xy)/y2;
end;

procedure GetLinearCoordP(V, VX, VY, VZ: T3Float; out Coord: T3Float);
begin;
Coord[0]:=VSMlp(V,VX)/VAbs2(VX);
Coord[1]:=VSMlp(V,VY)/VAbs2(VY);
Coord[2]:=VSMlp(V,VZ)/VAbs2(VZ);
end;


end.
