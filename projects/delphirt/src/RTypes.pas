unit RTypes;

interface
uses Graphics;

type
  Float = Single;

const
  Rnd:array[1..16,1..2]of Float=((-0.167, 0.265), (-0.755, 0.192), (-0.58, -0.178), (0.595, 0.0537), (0.762, -0.934), (-0.639, -0.0551), (-0.852, 0.807), (0.881, 0.474), (0.739, 0.925), (-0.528, -0.68), (-0.132, 0.276), (0.835, -0.387), (0.813, 0.908), (0.0261, 0.17), (0.87, 0.819), (0.0948, -0.928));

type
  T3Float = array[0..2] of Float;    {rgb or xyz}
  T3FloatArray = array[0..1000000] of T3Float;
  P3FloatArray = ^T3FloatArray;

  TVector = record
    Coord: T3Float;
    Direction: T3Float;
  end;

  TBasis = array[0..3] of T3Float;

  TIntersection = record
    Normal: TVector;
    Color: T3Float;
    CReflection, CRefraction, CDiffusion: T3Float;
    Entity: Integer;
  end;

  TEntity = class(TObject)
  private
    FIndex: Integer;
    FKRefraction: Float;
    //FKTransparent: T3Float;
  protected
  public
    function Intersects(const Ray:TVector; MaxDistance: Float):Boolean; virtual; abstract;
    procedure Intersect(const Ray:TVector; out Intersection:TIntersection); virtual; abstract;
    //procedure SetTime(Time:Integer); virtual; abstract;
    property KRefraction: Float read FKRefraction write FKRefraction;
    //property KTransparent: T3Float read FKTransparent write FKTransparent;
    property Index: Integer read FIndex write FIndex;
  end;

  TCamera = class(TObject)
  private
    FCoord: T3Float;
    FResX: Integer;
    FResY: Integer;
  protected
  public
    //procedure SetTime(Time:Integer); virtual; abstract;
    function GetRay(X,Y:Integer): TVector; virtual; abstract;
    property Coord: T3Float read FCoord write FCoord;
    property ResX: Integer read FResX write FResX;
    property ResY: Integer read FResY write FResY;
  end;

  TLight = class(TEntity)
  private
    FCoord: T3Float;
  public
    function GetColor(Direction: T3Float):T3Float; virtual; abstract;
    property Coord: T3Float read FCoord write FCoord;
  end;

  TSpotLight = class(TLight)
  private
    FColor: T3Float;
  public
    constructor Create(Coord, Color: T3Float);
    function Intersects(const Ray:TVector; MaxDistance: Float):Boolean; override;
    function GetColor(Direction: T3Float):T3Float; override;
    //procedure SetTime(Time:Integer); override;
  end;

  TStillCamera = class(TCamera)
  private
    FTop, FRight, FFront: T3Float;
  public
    constructor Create(Coord: T3Float; Right,Top,Front: T3Float);
    function GetRay(X,Y:Integer): TVector; override;
    //procedure SetTime(Time:Integer); override;
    property Top:T3Float read FTop write FTop;
    property Right:T3Float read FRight write FRight;
    property Front:T3Float read FFront write FFront;
  end;

  TDoFCamera = class(TStillCamera)
  private
    FFocalFront: T3Float;
    FJitterSize: Float;
    FRayCount: Integer;
    FNmb: Integer;
    FC: array[0..100,1..2] of Float;
  public
    constructor Create(Coord,Right,Top,Front,FocalFront: T3Float; JitterSize: Float; RayCount: Integer);
    function GetRay(X,Y:Integer):TVector; override;
    property RayCount:Integer read FRayCount;
    property FocalFront: T3Float read FFocalFront write FFocalFront;
  end;

  TStillSphere = class(TEntity)
  private
    FCoord: T3Float;
    FRadius: Float;
    FColor: T3Float;
    FCReflection: T3Float;
    FCRefraction: T3Float;
    FCDiffusion: T3Float;
  public
    constructor Create(Coord, CReflection, CRefraction, CDiffusion, Color: T3Float; KRefraction, Radius: Float);
    function Intersects(const Ray:TVector; MaxDistance: Float):Boolean; override;
    procedure Intersect(const Ray:TVector; out Intersection:TIntersection); override;
    //procedure SetTime(Time:Integer); override;
    property Coord: T3Float read FCoord write FCoord;
    property Radius: Float read FRadius write FRadius;
  end;

  TTexturedSphere = class(TStillSphere)
  private
    FTexture: P3FloatArray;
    FSizeX, FSizeY: Integer;
    FMlpX, FMlpY:Float;
  protected
    function GetTextureColor(const Point:T3Float): T3Float; virtual;
    procedure GetTextureCoord(const Point:T3Float; out x,y: Integer); virtual;
  public
    constructor Create(Texture: TBitmap; MlpX, MlpY:Float; Coord, CReflection, CRefraction, CDiffusion, Color: T3Float; KRefraction, Radius: Float);
    destructor Destroy; override;
    procedure Intersect(const Ray:TVector; out Intersection:TIntersection); override;
  end;

  TCustomPlane = class(TEntity)
  private
    FNormal: TVector;
    FColor: T3Float;
    FCReflection: T3Float;
    FCRefraction: T3Float;
    FCDiffusion: T3Float;
  protected
    function DoIntersects(const Ray:TVector):Boolean;
    procedure DoIntersect(const Ray:TVector; out Point: T3Float);
  public
    constructor Create(Normal:TVector; Color, CReflection, CDiffusion:T3Float);
  end;

  TStillPlane = class(TCustomPlane)
  private
  protected
  public
    function Intersects(const Ray:TVector; MaxDistance: Float):Boolean; override;
    procedure Intersect(const Ray:TVector; out Intersection:TIntersection); override;
    //procedure SetTime(Time:Integer); override;
  end;

  TStillParallelogram = class(TStillPlane)
  private
    FV1,FV2: T3Float;
    FLastIntersection: TIntersection;
    FX, FY: Float;
  protected
  public
    function Intersects(const Ray:TVector; MaxDistance: Float):Boolean; override;
    procedure Intersect(const Ray:TVector; out Intersection:TIntersection); override;
    constructor Create(O,V1,V2: T3Float; Color, CReflection, CDiffusion: T3Float);
  end;

  TTexturedPlane = class(TStillPlane)
  private
    FTexture: P3FloatArray;
    FSizeX, FSizeY: Integer;
    FMlpX, FMlpY: Float;
    FV1, FV2: T3Float;
  protected
    function GetTextureColor(const Point:T3Float): T3Float; virtual;
    procedure GetTextureCoord(const Point:T3Float; out x,y: Integer); virtual;
  public
    procedure Intersect(const Ray:TVector; out Intersection:TIntersection); override;
    constructor Create(Texture:TBitmap; MlpX, MlpY:Float; O,V1,V2: T3Float; Color, CReflection, CDiffusion:T3Float);
    destructor Destroy; override;
  end;

  TTexturedParallelogram = class(TTexturedPlane)
  private
    FLastIntersection: TIntersection;
    FX, FY: Float;
  protected
  public
    function Intersects(const Ray:TVector; MaxDistance: Float):Boolean; override;
    procedure Intersect(const Ray:TVector; out Intersection:TIntersection); override;
  end;

  TStillEllipse = class(TStillParallelogram)
  private
  protected
  public
    function Intersects(const Ray:TVector; MaxDistance: Float):Boolean; override;
    constructor Create(O,V1,V2: T3Float; Color, CReflection, CDiffusion: T3Float);
  end;

  TTexturedEllipse = class(TStillEllipse)
  private
    FTexture: P3FloatArray;
    FSizeX, FSizeY: Integer;
    FMlpX, FMlpY: Float;
  protected
  public
    procedure Intersect(const Ray:TVector; out Intersection:TIntersection); override;
    constructor Create(Texture:TBitmap; MlpX, MlpY:Float;  O,V1,V2: T3Float; Color, CReflection, CDiffusion: T3Float);
  end;

  TStillTube = class(TStillSphere)
  private
    FZ, FX, FY, FR: T3Float;
    FLastIntersection: TIntersection;
  protected
  public
    constructor Create(Coord, X, Y, Z, CReflection, CDiffusion, Color: T3Float);
    function Intersects(const Ray:TVector; MaxDistance: Float):Boolean; override;
    procedure Intersect(const Ray:TVector; out Intersection:TIntersection); override;
  end;

  TStillCylinder = class(TStillSphere)
  private
    FTop, FBottom: TStillEllipse;
    FTube: TStillTube;
    FLastIntersection: TIntersection;
  protected
    property Radius;
    property Coord;
  public
    constructor Create(Coord, X, Y, Z, CReflection, CRefraction, CDiffusion, Color: T3Float; KRefraction: Float);
    destructor Destroy; override;
    function Intersects(const Ray:TVector; MaxDistance: Float):Boolean; override;
    procedure Intersect(const Ray:TVector; out Intersection:TIntersection); override;
  end;

  TStillFlatRing = class(TStillEllipse)
  private
    FEllipse2: TStillEllipse;
  protected
  public
    constructor Create(Coord, X1, Y1, X2, Y2, CReflection, CDiffusion, Color: T3Float);
    destructor Destroy; override;
    function Intersects(const Ray:TVector; MaxDistance: Float):Boolean; override;
  end;

  TStillRing = class(TStillCylinder)
  private
    FTube2: TStillTube;
  protected
  public
    constructor Create(Coord, X1, Y1, X2, Y2, Z, CReflection, CRefraction, CDiffusion, Color: T3Float; KRefraction: Float);
    destructor Destroy; override;
    function Intersects(const Ray:TVector; MaxDistance: Float):Boolean; override;
  end;

  TMovingRing = class(TStillRing)
  private
    FBasis:TBasis;
    FBasisChanged:Boolean;
  protected
    procedure SetBasis(Basis:TBasis);
  public
    constructor Create(Coord, X1, Y1, X2, Y2, Z, CReflection, CRefraction, CDiffusion, Color: T3Float; KRefraction: Float);
    function Intersects(const Ray:TVector; MaxDistance: Float):Boolean; override;
    procedure Intersect(const Ray:TVector; out Intersection:TIntersection); override;
    property Basis:TBasis read FBasis write SetBasis;
  end;

  TTexturedTube = class(TStillTube)
  private
    FTexture: P3FloatArray;
    FSizeX, FSizeY: Integer;
    FMlpX, FMlpY: Float;
  protected
  public
    procedure Intersect(const Ray:TVector; out Intersection:TIntersection); override;
    constructor Create(Texture:TBitmap; MlpX, MlpY: Float; Coord, X, Y, Z, CReflection, CDiffusion, Color: T3Float);
  end;

  TTexturedFlatRing = class(TTexturedEllipse)
  private
    FEllipse2: TStillEllipse;
  protected
  public
    constructor Create(Texture:TBitmap; MlpX, MlpY:Float; Coord, X1, Y1, X2, Y2, CReflection, CDiffusion, Color: T3Float);
    destructor Destroy; override;
    function Intersects(const Ray:TVector; MaxDistance: Float):Boolean; override;
  end;

  TTexturedRing = class(TStillRing)
  private
    FTopBottomTexture,FTubeTexture: P3FloatArray;
    FTSizeX,FTSizeY,FUSizeX,FUSizeY:Integer;
  protected
  public
    constructor Create(UpDownTexture:TBitmap; UMlpX, UMlpY:Float; TubeTexture:TBitmap; TMlpX,TMlpY:Float; Coord, X1, Y1, X2, Y2, Z, CReflection, CRefraction, CDiffusion, Color: T3Float; KRefraction: Float);
  end;

  TMetaBalls3 = class(TEntity)
  private
    FSpheres: array[1..3] of TStillSphere;
    FCoord: array[1..3] of T3Float;
    FRadius: array[1..3] of Float;
    FColor: T3Float;
    FCReflection: T3Float;
    FCRefraction: T3Float;
    FCDiffusion: T3Float;
    FThreshold: Float;
    FLastIntersection:T3Float;
    FLastWeights:T3Float;
    FStep:Float;
  public
    constructor Create(Coord1, Coord2, Coord3, CReflection, CRefraction, CDiffusion, Color: T3Float; KRefraction, Radius1, Radius2, Radius3, Threshold: Float);
    function Intersects(const Ray:TVector; MaxDistance: Float):Boolean; override;
    procedure Intersect(const Ray:TVector; out Intersection:TIntersection); override;
    //procedure SetTime(Time:Integer); override;
    //property Coord: T3Float read FCoord write FCoord;
    //property Radius: Float read FRadius write FRadius;
  end;

function Make3Float(const a,b,c:Float):T3Float;
function MakeVector(const Coord,Direction:T3Float):TVector;
function MakeBasis(const o,x,y,z:T3Float):TBasis;

implementation
uses RUtils, SysUtils, Math;

procedure LoadTexture(Bitmap: TBitmap; out Texture: P3FloatArray; out SizeX, SizeY: Integer);
var h,w,x,y:Integer;
    p:PByteArray;
begin;
if Bitmap<>nil then
  begin;
  SizeY:=Bitmap.Height;
  SizeX:=Bitmap.Width;
  Bitmap.PixelFormat:=pf24bit;
  w:=Bitmap.Width*3; w:=w+(4-w mod 4)mod 4;
  h:=Bitmap.Height-1;
  P:=Bitmap.ScanLine[Bitmap.Height-1];
  GetMem(Texture, (SizeX)*(SizeY)*SizeOf(T3Float));
  for x:=0 to Bitmap.Width-1 do for y:=0 to Bitmap.Height-1 do
    begin;
    Texture[y*SizeX+x][0]:=P^[(h-y)*w+x*3+2]/256;
    Texture[y*SizeX+x][1]:=P^[(h-y)*w+x*3+1]/256;
    Texture[y*SizeX+x][2]:=P^[(h-y)*w+x*3+0]/256;
    end;
  end;  
end;

//============================================================================\\
// Functions & Procedures
//============================================================================\\
function Make3Float(const a,b,c:Float):T3Float;
begin;
Result[0]:=a;
Result[1]:=b;
Result[2]:=c;
end;

function MakeVector(const Coord,Direction:T3Float):TVector;
begin;
Result.Coord:=Coord;
Result.Direction:=Direction;
end;

function MakeBasis(const o,x,y,z:T3Float):TBasis;
begin;
Result[0]:=o;
Result[1]:=x;
Result[2]:=y;
Result[3]:=z;
end;

//============================================================================\\
// TStillCamera
//============================================================================\\
constructor TStillCamera.Create(Coord: T3Float; Right,Top,Front: T3Float);
begin;
inherited Create;
FCoord:=Coord;
FTop:=Top;
FRight:=Right;
FFront:=Front;
end;

function TStillCamera.GetRay(X,Y:Integer): TVector;
begin;
Result.Coord:=Self.Coord;
Result.Direction:=Normalize(VAdd(FFront,VAdd(VMlp(FRight, 2*X/(FResX-1)-1), VMlp(FTop, 2*Y/(FResY-1)-1))));
end;

{procedure TStillCamera.SetTime(Time:Integer);
begin;
end;}

//============================================================================\\
// TStillSphere
//============================================================================\\
constructor TStillSphere.Create(Coord, CReflection, CRefraction, CDiffusion, Color: T3Float; KRefraction, Radius: Float);
begin;
FCoord:=Coord;
FCReflection:=CReflection;
FCRefraction:=CRefraction;
FCDiffusion:=CDiffusion;
FColor:=Color;
FKRefraction:=KRefraction;
FRadius:=Radius;
//FKTransparent:=Make3Float(0,0,0);
end;

function TStillSphere.Intersects(const Ray:TVector; MaxDistance: Float):Boolean;
var V:T3Float;
    CosVR, VAbs2_:Float;
begin;
V:=VSub(FCoord,Ray.Coord);
VAbs2_:=VAbs2(V);
if VAbs2_<sqr(FRadius) then
  Result:=True
else
  begin;
  CosVR:=VSMlp(V,Ray.Direction)/sqrt(VAbs2_);
  Result:=(CosVR>0) and (sqr(CosVR)>1-sqr(FRadius)/VAbs2(V));
  end;
{V:=VSub(FCoord,Ray.Coord);
CosVR:=VCos(V,Ray.Direction);
Result:=(CosVR>0) and (sqr(CosVR)>1-sqr(FRadius)/VAbs2(V));}
end;

procedure TStillSphere.Intersect(const Ray:TVector; out Intersection:TIntersection);
var ScH,D,K1,K2:Float;
    N:T3Float;
begin;
N:=(VSub(FCoord,Ray.Coord));
ScH:=VSMlp(N,Ray.Direction);
D:=Sqrt(Abs( Sqr(ScH)-VAbs2(N)+Sqr(FRadius) ));
K1:=(ScH-D);
K2:=(ScH+D);
if K1<0 then K1:=K2;
Intersection.Normal.Coord:=VAdd(Ray.Coord,VMlp(Ray.Direction,K1));
Intersection.Normal.Direction:=Normalize(VSub(Intersection.Normal.Coord,FCoord));
if VSMlp(Intersection.Normal.Direction, Ray.Direction)>0 then
  Intersection.Normal.Direction:=VMlp(Intersection.Normal.Direction,-1);
Intersection.Color:=FColor;
Intersection.CReflection:=FCReflection;
Intersection.CRefraction:=FCRefraction;
Intersection.CDiffusion:=FCDiffusion;
Intersection.Entity:=Self.Index;
end;

{procedure TStillSphere.SetTime(Time:Integer);
begin;
end;}

//============================================================================\\
// TTexturedSphere
//============================================================================\\
constructor TTexturedSphere.Create(Texture: TBitmap; MlpX, MlpY:Float; Coord, CReflection, CRefraction, CDiffusion, Color: T3Float; KRefraction, Radius: Float);
begin;
inherited Create(Coord, CReflection, CRefraction, CDiffusion, Color, KRefraction, Radius);
LoadTexture(Texture, FTexture, FSizeX, FSizeY);
FMlpX:=MlpX;
FMlpY:=MlpY;
end;

destructor TTexturedSphere.Destroy;
begin;
FreeMem(FTexture, (FSizeX+1)*(FSizeY+1)*SizeOf(T3Float));
inherited Destroy;
end;

procedure TTexturedSphere.Intersect(const Ray:TVector; out Intersection:TIntersection); 
begin;
inherited Intersect(Ray,Intersection);
Intersection.Color:=VMlp(Intersection.Color, GetTextureColor(Intersection.Normal.Coord));
end;

procedure TTexturedSphere.GetTextureCoord(const Point:T3Float; out x,y: Integer);
var R: T3Float;
begin;
R:=Normalize(VSub(Point,FCoord));
y:=Round((ArcSin(R[2])/pi+0.5)*(FSizeY-1)/FMlpY) mod FSizeY;
x:=Round((ArcTan2(R[1],R[0])/pi+1)*(FSizeX-1)/FMlpX) mod FSizeX;
if x<0 then x:=x+FSizeX;
if y<0 then y:=y+FSizeY;
end;

function TTexturedSphere.GetTextureColor(const Point:T3Float): T3Float;
var x,y: Integer;
begin;
GetTextureCoord(Point,x,y);
Result:=FTexture[y*FSizeX+x];
end;

//============================================================================\\
// TCustomPlane
//============================================================================\\
function TCustomPlane.DoIntersects(const Ray:TVector):Boolean;
begin;
Result:=VSMlp(VSub(Ray.Coord,FNormal.Coord),FNormal.Direction)*VSMlp(FNormal.Direction,Ray.Direction)<0;
end;

procedure TCustomPlane.DoIntersect(const Ray:TVector; out Point: T3Float);
begin;
Point:=VSub(Ray.Coord,VMlp(Ray.Direction,-VSMlp(FNormal.Direction,VSub(FNormal.Coord,Ray.Coord))/VSMlp(FNormal.Direction,Ray.Direction)));
end;

constructor TCustomPlane.Create(Normal:TVector; Color, CReflection, CDiffusion:T3Float);
begin;
inherited Create;
FNormal:=Normal;
FNormal.Direction:=Normalize(FNormal.Direction);
FColor:=Color;
FCReflection:=CReflection;
FCRefraction:=Make3Float(0,0,0);
FCDiffusion:=CDiffusion;
FKRefraction:=1;
//FKTransparent:=Make3Float(0,0,0);
end;

//============================================================================\\
// TStillPlane
//============================================================================\\
function TStillPlane.Intersects(const Ray:TVector; MaxDistance: Float):Boolean;
begin;
Result:=DoIntersects(Ray);
end;

procedure TStillPlane.Intersect(const Ray:TVector; out Intersection:TIntersection);
begin;
DoIntersect(Ray, Intersection.Normal.Coord);
if VSMlp(Ray.Direction,FNormal.Direction)>0 then
  Intersection.Normal.Direction:=VMlp(FNormal.Direction,-1)
else
  Intersection.Normal.Direction:=FNormal.Direction;
Intersection.Color:=FColor;
Intersection.CReflection:=FCReflection;
Intersection.CRefraction:=FCRefraction;
Intersection.CDiffusion:=FCDiffusion;
Intersection.Entity:=FIndex;
end;

{procedure TStillPlane.SetTime(Time:Integer);
begin;
end;}

//============================================================================\\
// TStillParallelogram
//============================================================================\\
function TStillParallelogram.Intersects(const Ray:TVector; MaxDistance: Float):Boolean;
begin;
if (inherited Intersects(Ray, MaxDistance)) then
  begin;
  inherited Intersect(Ray, FLastIntersection);
  GetLinearCoord(VSub(FLastIntersection.Normal.Coord, FNormal.Coord), FV1, FV2, FX, FY);
  if (FX>=0)and(FX<1)and(FY>=0)and(FY<1) then
    Result:=True
  else
    Result:=False;
  end
else
  Result:=False;
end;

procedure TStillParallelogram.Intersect(const Ray:TVector; out Intersection:TIntersection);
begin;
Intersection:=FLastIntersection;
end;

constructor TStillParallelogram.Create(O,V1,V2: T3Float; Color, CReflection, CDiffusion: T3Float);
begin;
inherited Create(MakeVector(O,VVMlp(V1,V2)),Color,CReflection, CDiffusion);
FV1:=V1;
FV2:=V2;
end;

//============================================================================\\
// TTexturedPlane
//============================================================================\\
procedure TTexturedPlane.Intersect(const Ray:TVector; out Intersection:TIntersection);
begin;
inherited Intersect(Ray, Intersection);
Intersection.Color:=VMlp(Intersection.Color, GetTextureColor(Intersection.Normal.Coord));
end;

constructor TTexturedPlane.Create(Texture:TBitmap; MlpX, MlpY:Float; O,V1,V2: T3Float; Color, CReflection, CDiffusion:T3Float);
begin;
inherited Create(MakeVector(O,VVMlp(V1,V2)),Color,CReflection, CDiffusion);
FV1:=V1;
FV2:=V2;
FMlpX:=MlpX;
FMlpY:=MlpY;
LoadTexture(Texture, FTexture, FSizeX, FSizeY);
end;

destructor TTexturedPlane.Destroy;
begin;
FreeMem(FTexture, (FSizeX)*(FSizeY)*SizeOf(T3Float));
inherited Destroy;
end;

procedure TTexturedPlane.GetTextureCoord(const Point:T3Float; out x,y: Integer);
var xf, yf: Float;
begin;
GetLinearCoord(VSub(Point, FNormal.Coord), FV1, FV2, xf, yf);
x:=Round(xf / FMlpX * FSizeX) mod FSizeX;
if x<0 then x:=x+FSizeX;
y:=Round(yf / FMlpY * FSizeY) mod FSizeY;
if y<0 then y:=y+FSizeY;
end;

function TTexturedPlane.GetTextureColor(const Point:T3Float): T3Float;
var x,y: Integer;
begin;
GetTextureCoord(Point,x,y);
Result:=FTexture[y*FSizeX+x];
end;


//============================================================================\\
// TTexturedParallelogram
//============================================================================\\
function TTexturedParallelogram.Intersects(const Ray:TVector; MaxDistance: Float):Boolean;
begin;
if (inherited Intersects(Ray, MaxDistance)) then
  begin;
  inherited Intersect(Ray, FLastIntersection);
  GetLinearCoord(VSub(FLastIntersection.Normal.Coord, FNormal.Coord), FV1, FV2, FX, FY);
  if (FX>=0)and(FX<1)and(FY>=0)and(FY<1) then
    Result:=True
  else
    Result:=False;
  end
else
  Result:=False;
end;

procedure TTexturedParallelogram.Intersect(const Ray:TVector; out Intersection:TIntersection);
begin;
Intersection:=FLastIntersection;
end;

//============================================================================\\
// TSpotLight
//============================================================================\\
constructor TSpotLight.Create(Coord, Color: T3Float);
begin;
FCoord:=Coord;
FColor:=Color;
end;

function TSpotLight.Intersects(const Ray:TVector; MaxDistance: Float):Boolean;
begin;
Result:=False;
end;

function TSpotLight.GetColor(Direction: T3Float):T3Float;
begin;
Result:=FColor;
end;

{procedure TSpotLight.SetTime(Time:Integer);
begin;
end;}

//============================================================================\\
// TStillEllipse
//============================================================================\\
function TStillEllipse.Intersects(const Ray:TVector; MaxDistance: Float):Boolean;
begin;
Result:=(inherited Intersects(Ray,MaxDistance))and(sqr(FX-0.5)+sqr(FY-0.5)<0.25);
end;

constructor TStillEllipse.Create(O,V1,V2: T3Float; Color, CReflection, CDiffusion: T3Float);
begin;
inherited Create(VSub(O, VAdd(V1,V2)),VMlp(V1,2),VMlp(V2,2),Color,CReflection,CDiffusion);
end;


//============================================================================\\
// TTexturedEllipse
//============================================================================\\
procedure TTexturedEllipse.Intersect(const Ray:TVector; out Intersection:TIntersection);
var X,Y:Integer;
begin;
Inherited Intersect(Ray,Intersection);
Y:=Round(FY/FMlpY*FSizeY) mod FSizeY;
X:=Round(FX/FMlpX*FSizeX) mod FSizeX;
if Y<0 then Y:=Y+FSizeY;
if X<0 then X:=X+FSizeX;
Intersection.Color:=VMlp(Intersection.Color, FTexture[Y*FSizeX+X]);
end;

constructor TTexturedEllipse.Create(Texture:TBitmap; MlpX, MlpY:Float;  O,V1,V2: T3Float; Color, CReflection, CDiffusion: T3Float);
begin;
inherited Create(O,V1,V2,Color,CReflection,CDiffusion);
LoadTexture(Texture, FTexture, FSizeX, FSizeY);
FMlpX:=MlpX;
FMlpY:=MlpY;
end;

//============================================================================\\
// TStillTube
//============================================================================\\
constructor TStillTube.Create(Coord, X, Y, Z, CReflection, CDiffusion, Color: T3Float);
begin;
if VAbs2(X)>VAbs2(Y) then
  inherited Create(Coord, CReflection, Make3Float(0,0,0), CDiffusion, Color, 1, VAbs(VAdd(X, Z)))
else
  inherited Create(Coord, CReflection, Make3Float(0,0,0), CDiffusion, Color, 1, VAbs(VAdd(Y, Z)));
FX:=X;
FY:=Y;
FZ:=Z;
end;

function TStillTube.Intersects(const Ray:TVector; MaxDistance: Float):Boolean;
var va,ra,vb,rb,a4,b4,va2b4_b2a4,d,dd,k1,k2:Float;
    r:T3Float;
begin;
if not inherited Intersects(Ray, 1000000000000000000.0) then
  begin;
  Result:=False;
  exit;
  end;
Result:=False;
r:=VSub(Ray.Coord, FCoord);
va:=VSMlp(Ray.Direction, FX);
vb:=VSMlp(Ray.Direction, FY);
ra:=VSMlp(R, FX);
rb:=VSMlp(R, FY);
a4:=sqr(VAbs2(FX));
b4:=sqr(VAbs2(FY));
va2b4_b2a4:=sqr(va)*b4+sqr(vb)*a4;
d:=va2b4_b2a4-sqr(va*rb-ra*vb);
if d>=0 then
  begin;
  d:=sqrt(a4*b4*d);
  dd:=-(b4*va*ra+a4*rb*vb);
  k1:=dd+d;
  k2:=dd-d;
  if k2>0 then
    begin;
    k2:=k2/va2b4_b2a4;
    FLastIntersection.Normal.Coord:=VAdd(Ray.Coord,VMlp(Ray.Direction,k2));
    FR:=VSub(FLastIntersection.Normal.Coord, FCoord);
    if Abs(VSMlp(FR,FZ)/VAbs2(FZ))<=1 then
      begin;
      if VAbs2(VSub(Ray.Coord,FLastIntersection.Normal.Coord))<MaxDistance then
        begin;
        Result:=True;
        exit;
        end
      else
        begin;
        Result:=False;
        exit;
        end;
      end;
    end;
  if k1>0 then
    begin;
    k1:=k1/va2b4_b2a4;
    FLastIntersection.Normal.Coord:=VAdd(Ray.Coord,VMlp(Ray.Direction,k1));
    FR:=VSub(FLastIntersection.Normal.Coord, FCoord);
    if Abs(VSMlp(FR,FZ)/VAbs2(FZ))<=1 then
      begin;
      if VAbs2(VSub(Ray.Coord,FLastIntersection.Normal.Coord))<MaxDistance then
        begin;
        Result:=True;
        exit;
        end
      else
        begin;
        Result:=False;
        exit;
        end;
      end;
    end;
  end;
end;

procedure TStillTube.Intersect(const Ray:TVector; out Intersection:TIntersection);
begin;
Intersection:=FLastIntersection;
Intersection.Normal.Direction:=Normalize(VSub(VMlp(FR,VAbs2(FZ)),VMlp(FZ,VSMlp(FZ,FR))));
if VSMlp(Intersection.Normal.Direction, Ray.Direction)>0 then
  Intersection.Normal.Direction:=VMlp(Intersection.Normal.Direction,-1);
Intersection.Color:=FColor;
Intersection.CReflection:=FCReflection;
Intersection.CRefraction:=FCRefraction;
Intersection.CDiffusion:=FCDiffusion;
Intersection.Entity:=FIndex;
end;

//============================================================================\\
// TStillCylinder
//============================================================================\\
constructor TStillCylinder.Create(Coord, X, Y, Z, CReflection, CRefraction, CDiffusion, Color: T3Float; KRefraction: Float);
begin;
if VAbs2(X)>VAbs2(Y) then
  inherited Create(Coord, CReflection, CRefraction, CDiffusion, Color, KRefraction, VAbs(VAdd(X, Z)))
else
  inherited Create(Coord, CReflection, CRefraction, CDiffusion, Color, KRefraction, VAbs(VAdd(Y, Z)));
if Self is TStillCylinder then
  begin;
  FTop:=TStillEllipse.Create(VAdd(Coord,Z),X,Y,Color,CReflection,CDiffusion);
  FBottom:=TStillEllipse.Create(VSub(Coord,Z),X,Y,Color,CReflection,CDiffusion);
  FTube:=TStillTube.Create(Coord,X,Y,Z,CReflection,CDiffusion,Color);
  end;
end;

destructor TStillCylinder.Destroy;
begin;
FTop.Free;
FBottom.Free;
inherited Destroy;
end;

function TStillCylinder.Intersects(const Ray:TVector; MaxDistance: Float):Boolean;
var Intersections:array[0..1]of TIntersection;
    i,IntCount: Integer;
    Distance,MinDistance:Float;
begin;
if inherited Intersects(Ray, 1000000000000000000.0) then
  begin;
  IntCount:=0;
  if (FTop.Intersects(Ray,MaxDistance)) then
    begin;
    FTop.Intersect(Ray,Intersections[IntCount]);
    Inc(IntCount);
    end;
  if (FBottom.Intersects(Ray,MaxDistance)) then
    begin;
    FBottom.Intersect(Ray,Intersections[IntCount]);
    Inc(IntCount);
    end;
  if IntCount<2 then if FTube.Intersects(Ray,MaxDistance) then
    begin;
    FTube.Intersect(Ray,Intersections[IntCount]);
    Inc(IntCount);
    end;
  if IntCount>0 then
    begin;
    Result:=True;
    MinDistance:=1000000000000000000.0;
    for i:=0 to IntCount-1 do
      begin;
      Distance:=VAbs2(VSub(Ray.Coord,Intersections[i].Normal.Coord));
      if Distance<MinDistance then
        begin;
        MinDistance:=Distance;
        FLastIntersection:=Intersections[i];
        end;
      end;
    end
  else
    Result:=False;
  end
else
  Result:=False;
end;

procedure TStillCylinder.Intersect(const Ray:TVector; out Intersection:TIntersection);
begin;
Intersection:=FLastIntersection;
Intersection.Entity:=FIndex;
Intersection.CRefraction:=FCRefraction;
end;

//============================================================================\\
// TStillFlatRing
//============================================================================\\
constructor TStillFlatRing.Create(Coord, X1, Y1, X2, Y2, CReflection, CDiffusion, Color: T3Float);
begin;
inherited Create(Coord, X1, Y1, Color, CReflection, CDiffusion);
FEllipse2:=TStillEllipse.Create(Coord, X2, Y2, Make3Float(0,0,0), Make3Float(0,0,0), Make3Float(0,0,0));
end;

destructor TStillFlatRing.Destroy;
begin;
FEllipse2.Free;
end;

function TStillFlatRing.Intersects(const Ray:TVector; MaxDistance: Float):Boolean;
begin;
Result:=(inherited Intersects(Ray, MaxDistance))and(not(FEllipse2.Intersects(Ray, MaxDistance)));
end;

//============================================================================\\
// TStillRing
//============================================================================\\
constructor TStillRing.Create(Coord, X1, Y1, X2, Y2, Z, CReflection, CRefraction, CDiffusion, Color: T3Float; KRefraction: Float);
begin;
inherited Create(Coord, X1, Y1, Z, CReflection, CRefraction, CDiffusion, Color, KRefraction);
if (Self is TStillRing) then
  begin;
  FTop:=TStillFlatRing.Create(VAdd(Coord,Z),X1,Y1,X2,Y2,CReflection,CDiffusion,Color);
  FBottom:=TStillFlatRing.Create(VSub(Coord,Z),X1,Y1,X2,Y2,CReflection,CDiffusion,Color);
  FTube:=TStillTube.Create(Coord,X1,Y1,Z,CReflection,CDiffusion,Color);
  FTube2:=TStillTube.Create(Coord,X2,Y2,Z,CReflection,CDiffusion,Color);
  end;
end;

destructor TStillRing.Destroy;
begin;
FTube2.Free;
inherited Destroy;
end;

function TStillRing.Intersects(const Ray:TVector; MaxDistance: Float):Boolean;
begin;
Result:=inherited Intersects(Ray, MaxDistance);
if FTube2.Intersects(Ray,MaxDistance) then
  begin;
  if Result then
    begin;
    if VAbs2(VSub(FTube2.FLastIntersection.Normal.Coord,Ray.Coord))<VAbs2(VSub(FLastIntersection.Normal.Coord,Ray.Coord)) then
      FTube2.Intersect(Ray,FLastIntersection);
    end
  else
    begin;
    FTube2.Intersect(Ray,FLastIntersection);
    Result:=True;
    end;
  end;
end;


//============================================================================\\
// TMovingRing
//============================================================================\\
constructor TMovingRing.Create(Coord, X1, Y1, X2, Y2, Z, CReflection, CRefraction, CDiffusion, Color: T3Float; KRefraction: Float);
begin;
inherited Create(Coord, X1, Y1, X2, Y2, Z, CReflection, CRefraction, CDiffusion, Color, KRefraction);
FBasis[0]:=Make3Float(0,0,0);
FBasis[1]:=Make3Float(1,0,0);
FBasis[2]:=Make3Float(0,1,0);
FBasis[3]:=Make3Float(0,0,1);
FBasisChanged:=False;
end;

function TMovingRing.Intersects(const Ray:TVector; MaxDistance: Float):Boolean;
var Ray2:TVector;
begin;
if not(FBasisChanged) then
  Result:=Intersects(Ray,MaxDistance)
else
  begin;
  try
  GetLinearCoordP(VSub(Ray.Coord,FBasis[0]),FBasis[1],FBasis[2],FBasis[3],Ray2.Coord);
  GetLinearCoordP(Ray.Direction,FBasis[1],FBasis[2],FBasis[3],Ray2.Direction);
  Result:=inherited Intersects(Ray2,1000000000000000000.0)
  except
    on E:Exception do ;
    end;
  end;
end;

procedure TMovingRing.SetBasis(Basis:TBasis);
begin;
FBasis:=Basis;
FBasisChanged:=True;
end;

procedure TMovingRing.Intersect(const Ray:TVector; out Intersection:TIntersection);
var Int2:TVector;
begin;
inherited Intersect(Ray, Intersection);
if (FBasisChanged) then
  begin;
  Int2.Coord:=VAdd(VAdd(FBasis[0],VMlp(FBasis[1],Intersection.Normal.Coord[0])),VAdd(VMlp(FBasis[2],Intersection.Normal.Coord[1]),VMlp(FBasis[3],Intersection.Normal.Coord[2])));
  Int2.Direction:=VAdd(VMlp(FBasis[1],Intersection.Normal.Direction[0]),VAdd(VMlp(FBasis[2],Intersection.Normal.Direction[1]),VMlp(FBasis[3],Intersection.Normal.Direction[2])));
  Intersection.Normal:=Int2;
  end;
end;

//============================================================================\\
// TTexturedTube
//============================================================================\\
procedure TTexturedTube.Intersect(const Ray:TVector; out Intersection:TIntersection);
var R: T3Float;
    x,y:Integer;
begin;
inherited Intersect(Ray, Intersection);
GetLinearCoordP(VSub(Intersection.Normal.Coord,FCoord),FX,FY,FZ,R);
y:=Round( (R[2]+1)/(2*VAbs(FZ))*(FSizeY-1)/FMlpY ) mod FSizeY;
x:=Round((ArcTan2(R[1],R[0])/pi+1)*(FSizeX-1)/FMlpX) mod FSizeX;
if x<0 then x:=x+FSizeX;
if y<0 then y:=y+FSizeY;
{
y:=Round((ArcSin(R[2])/pi+0.5)*(FSizeY-1)/FMlpY) mod FSizeY;
x:=Round((ArcTan2(R[1],R[0])/pi+1)*(FSizeX-1)/FMlpX) mod FSizeX;
if x<0 then x:=x+FSizeX;
if y<0 then y:=y+FSizeY;
}
Intersection.Color:=VMlp(Intersection.Color,FTexture[y*FSizeX+x]);
end;

constructor TTexturedTube.Create(Texture:TBitmap; MlpX, MlpY: Float; Coord, X, Y, Z, CReflection, CDiffusion, Color: T3Float);
begin;
inherited Create(Coord, X, Y, Z, CReflection, CDiffusion, Color);
LoadTexture(Texture,FTexture,FSizeX,FSizeY);
FMlpX:=MlpX;
FMlpY:=MlpY;
end;


//============================================================================\\
// TTexturedFlatRing
//============================================================================\\
constructor TTexturedFlatRing.Create(Texture:TBitmap; MlpX, MlpY:Float; Coord, X1, Y1, X2, Y2, CReflection, CDiffusion, Color: T3Float);
begin;
inherited Create(Texture, MlpX, MlpY, Coord, X1, Y1, Color, CReflection, CDiffusion);
FEllipse2:=TStillEllipse.Create(Coord, X2, Y2, Make3Float(0,0,0), Make3Float(0,0,0), Make3Float(0,0,0));
end;

destructor TTexturedFlatRing.Destroy;
begin;
FEllipse2.Free;
end;

function TTexturedFlatRing.Intersects(const Ray:TVector; MaxDistance: Float):Boolean;
begin;
Result:=(inherited Intersects(Ray, MaxDistance))and(not(FEllipse2.Intersects(Ray, MaxDistance)));
end;


//============================================================================\\
// TTexturedRing
//============================================================================\\
constructor TTexturedRing.Create(UpDownTexture:TBitmap; UMlpX, UMlpY:Float; TubeTexture:TBitmap; TMlpX,TMlpY:Float; Coord, X1, Y1, X2, Y2, Z, CReflection, CRefraction, CDiffusion, Color: T3Float; KRefraction: Float);
begin;
Inherited Create(Coord, X1, Y1, X2, Y2, Z, CReflection, CRefraction, CDiffusion, Color, KRefraction);
if (Self is TTexturedRing) then
  begin;
  LoadTexture(UpDownTexture,FTopBottomTexture,FUSizeX,FUSizeY);
  LoadTexture(TubeTexture,FTubeTexture,FTSizeX,FTSizeY);
  FTop:=TTexturedFlatRing.Create(nil,UMlpX,UMlpY,VAdd(Coord,Z),X1,Y1,X2,Y2,CReflection,CDiffusion,Color);
  FBottom:=TTexturedFlatRing.Create(nil,UMlpX,UMlpY,VSub(Coord,Z),X1,Y1,X2,Y2,CReflection,CDiffusion,Color);
  FTube:=TTexturedTube.Create(nil,TMlpX,TMlpY,Coord,X1,Y1,Z,CReflection,CDiffusion,Color);
  FTube2:=TTexturedTube.Create(nil,TMlpX,TMlpY,Coord,X2,Y2,Z,CReflection,CDiffusion,Color);
  (FTop as TTexturedFlatRing).FTexture:=FTopBottomTexture;
  (FTop as TTexturedFlatRing).FSizeX:=FUSizeX;
  (FTop as TTexturedFlatRing).FSizeY:=FUSizeY;
  (FBottom as TTexturedFlatRing).FTexture:=FTopBottomTexture;
  (FBottom as TTexturedFlatRing).FSizeX:=FUSizeX;
  (FBottom as TTexturedFlatRing).FSizeY:=FUSizeY;
  (FTube as TTexturedTube).FTexture:=FTubeTexture;
  (FTube as TTexturedTube).FSizeX:=FTSizeX;
  (FTube as TTexturedTube).FSizeY:=FTSizeY;
  (FTube2 as TTexturedTube).FTexture:=FTubeTexture;
  (FTube2 as TTexturedTube).FSizeX:=FTSizeX;
  (FTube2 as TTexturedTube).FSizeY:=FTSizeY;
  end;
end;

//============================================================================\\
// TMetaBalls3
//============================================================================\\
constructor TMetaBalls3.Create(Coord1, Coord2, Coord3, CReflection, CRefraction, CDiffusion, Color: T3Float; KRefraction, Radius1, Radius2, Radius3, Threshold: Float);
begin;
Inherited Create;
FCoord[1]:=Coord1;
FCoord[2]:=Coord2;
FCoord[3]:=Coord3;
FCReflection:=CReflection;
FCRefraction:=CRefraction;
FCDiffusion:=CDiffusion;
FColor:=Color;
FKRefraction:=KRefraction;
FRadius[1]:=Radius1;
FRadius[2]:=Radius2;
FRadius[3]:=Radius3;
FThreshold:=Threshold;
FSpheres[1]:=TStillSphere.Create(FCoord[1],Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(0,0,0),1,FRadius[1]);
FSpheres[2]:=TStillSphere.Create(FCoord[2],Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(0,0,0),1,FRadius[2]);
FSpheres[3]:=TStillSphere.Create(FCoord[3],Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(0,0,0),1,FRadius[3]);
FStep:=min(FRadius[1],min(FRadius[2],FRadius[3]))/40;
end;

function TMetaBalls3.Intersects(const Ray:TVector; MaxDistance: Float):Boolean;
var SI:array[1..3] of Boolean;
    NeedCalculate, ISet:Boolean;
    Intersection, Intersection2:TIntersection;
    C,V:T3Float;
    T,T11,T22,T33,T1,T2,T3:Float;
begin;
SI[1]:=FSpheres[1].Intersects(Ray,MaxDistance);
SI[2]:=FSpheres[2].Intersects(Ray,MaxDistance);
SI[3]:=FSpheres[3].Intersects(Ray,MaxDistance);
if SI[1] or SI[2] or SI[3] then
  begin;
  ISet:=False;
  if SI[1] then
    begin;
    FSpheres[1].Intersect(Ray,Intersection);
    ISet:=True;
    end;
  if SI[2] then
    begin;
    if ISet then
      begin;
      FSpheres[2].Intersect(Ray,Intersection2);
      if (abs(Intersection2.Normal.Coord[1]-Ray.Coord[1])<abs(Intersection.Normal.Coord[1]-Ray.Coord[1])) or
         (abs(Intersection2.Normal.Coord[2]-Ray.Coord[2])<abs(Intersection.Normal.Coord[2]-Ray.Coord[2])) then
        Intersection:=Intersection2;
      end
    else
      begin;
      FSpheres[2].Intersect(Ray,Intersection);
      ISet:=True;
      end;
    end;
  if SI[3] then
    begin;
    if ISet then
      begin;
      FSpheres[3].Intersect(Ray,Intersection2);
      if (abs(Intersection2.Normal.Coord[1]-Ray.Coord[1])<abs(Intersection.Normal.Coord[1]-Ray.Coord[1])) or
         (abs(Intersection2.Normal.Coord[2]-Ray.Coord[2])<abs(Intersection.Normal.Coord[2]-Ray.Coord[2])) then
        Intersection:=Intersection2;
      end
    else
      begin;
      FSpheres[3].Intersect(Ray,Intersection);
      ISet:=True;
      end;
    end;

  C:=Intersection.Normal.Coord;

  C:=VAdd(C,VMlp(Ray.Direction,5*FStep));
  repeat
    C:=VAdd(C,VMlp(Ray.Direction,FStep));

    T11:=0.5*VAbs2(VSub(C,FCoord[1]))/sqr(FRadius[1]);
    if T11>0.5 then
      T1:=0
    else
      T1:=sqr(T11)-T11+0.25;

    T22:=0.5*VAbs2(VSub(C,FCoord[2]))/sqr(FRadius[2]);
    if T22>0.5 then
      T2:=0
    else
      T2:=sqr(T22)-T22+0.25;

    T33:=0.5*VAbs2(VSub(C,FCoord[3]))/sqr(FRadius[3]);
    if T33>0.5 then
      T3:=0
    else
      T3:=sqr(T33)-T33+0.25;

    T:=T1+T2+T3;

    {V:=VSub(C, FCoord[1]);
    T1:=1/(1+abs(V[0]*V[1]*V[2]));
    V:=VSub(C, FCoord[2]);
    T2:=1/(1+abs(V[0]*V[1]*V[2]));
    V:=VSub(C, FCoord[3]);
    T3:=1/(1+abs(V[0]*V[1]*V[2]));
    T:=T1+T2+T3;}

    {if (T11>0.5) and (T22>0.5) and (T33>0.5) then
      begin;
      Result:=False;
      exit;
      end;}
    // Damn, it's a cheat!
    if (Abs(C[0])>1.5) or (Abs(C[1])>1.5) then
      begin;
      Result:=False;
      exit;
      end;
  until (T>FThreshold);

  Result:=True;
  FLastIntersection:=C;
  FLastWeights[0]:=T1;
  FLastWeights[1]:=T2;
  FLastWeights[2]:=T3;
  end
else
  Result:=False;
end;

procedure TMetaBalls3.Intersect(const Ray:TVector; out Intersection:TIntersection);
var W:Float;
begin;
Intersection.Normal.Coord:=FLastIntersection;
W:=FLastWeights[0]+FLastWeights[1]+FLastWeights[2];
Intersection.Normal.Direction:=
Normalize(VAdd(     VMlp(Normalize(VSub(FLastIntersection,FCoord[1])),FLastWeights[0]),
               VAdd(VMlp(Normalize(VSub(FLastIntersection,FCoord[2])),FLastWeights[1]),
                    VMlp(Normalize(VSub(FLastIntersection,FCoord[3])),FLastWeights[2]))));
Intersection.Color:=FColor;
Intersection.CReflection:=FCReflection;
Intersection.CRefraction:=FCRefraction;
Intersection.CDiffusion:=FCDiffusion;
Intersection.Entity:=Self.Index;
end;

//============================================================================\\
// TDoFCamera
//============================================================================\\
constructor TDoFCamera.Create(Coord,Right,Top,Front,FocalFront: T3Float; JitterSize: Float; RayCount: Integer);
var i:Integer;
begin;
Randomize;
for i:=0 to RayCount-1 do
  begin;
  FC[i,1]:=Rnd[i,1]*JitterSize;
  FC[i,2]:=Rnd[i,2]*JitterSize;
  end;
FNmb:=0;
FFocalFront:=FocalFront;
FJitterSize:=JitterSize;
FRayCount:=RayCount;
end;

function TDoFCamera.GetRay(X,Y:Integer):TVector;
var FocalPoint: T3Float;
    ImagePoint: T3Float;
    k: Float;
begin;
//Result.Coord:=Self.Coord;
k:=VAbs(FFocalFront)/VAbs(FFront);
ImagePoint:=VAdd(Coord,VAdd(FFront,VAdd(VMlp(FRight, 2*X/(FResX-1)-1), VMlp(FTop, 2*Y/(FResY-1)-1))));
FocalPoint:=VAdd(Coord,VAdd(FFocalFront,VAdd(VMlp(VMlp(FRight,k), 2*X/(FResX-1)-1), VMlp(VMlp(FTop,k), 2*Y/(FResY-1)-1))));
//ImagePoint:=VAdd(ImagePoint, VAdd(VMlp(Normalize(FRight),(Random*2-1)*FJitterSize),VMlp(Normalize(FTop),(Random*2-1)*FJitterSize)));
//ImagePoint:=VAdd(ImagePoint, VAdd(VMlp(Normalize(FRight),Sin(FNmb/FRayCount*2*pi)*FJitterSize),VMlp(Normalize(FTop),Cos(FNmb/FRayCount*2*pi)*FJitterSize)));
ImagePoint:=VAdd(ImagePoint, VAdd(VMlp(Normalize(FRight),FC[FNmb,1]),VMlp(Normalize(FTop),FC[FNmb,2])));
Result.Coord:=ImagePoint;
Result.Direction:=Normalize(VSub(FocalPoint,ImagePoint));
FNmb:=FNmb+1;
FNmb:=FNmb mod FRayCount;
end;

end.
