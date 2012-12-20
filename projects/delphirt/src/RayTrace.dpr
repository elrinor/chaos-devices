program RayTrace;
{$APPTYPE CONSOLE}
{
ArXRay - current
implemented:
точечные источники света
зеркала (с переотражениями)
преломление
}

uses
  Windows,
  SysUtils,
  RTypes in 'RTypes.pas',
  REngine in 'REngine.pas',
  RUtils in 'RUtils.pas',
  Graphics,
  IntStack in 'IntStack.pas';

var
  Engine: TREngine;

var
  Rings:array[1..3]of TStillRing;

procedure CreateSphere;
begin;
//Engine.AddEntity(TStillSphere.Create(Make3Float(0,0,0),Make3Float(0.1,0.1,0.1),Make3Float(0.8,0.8,0.8),Make3Float(0,0,0),Make3Float(0,0,0.05),1.5,1));
Rings[1]:=TStillRing.Create(Make3Float(0,0,0),Make3Float(1.2,0,0),Make3Float(0,1.2,0),Make3Float(1.1,0,0),Make3Float(0,1.1,0),
Make3Float(0,0,0.1),Make3Float(0.3,0.3,0.3),Make3Float(0,0,0),Make3Float(0.2,0.2,0.2),Make3Float(0.5,0.5,0.1),1);
Rings[2]:=TStillRing.Create(Make3Float(0,0,0),Make3Float(1.2,0,0),Make3Float(0,0,1.2),Make3Float(1.1,0,0),Make3Float(0,0,1.1),
Make3Float(0,0.1,0),Make3Float(0.3,0.3,0.3),Make3Float(0,0,0),Make3Float(0.2,0.2,0.2),Make3Float(0.5,0.5,0.1),1);
Rings[3]:=TStillRing.Create(Make3Float(0,0,0),Make3Float(0,1.2,0),Make3Float(0,0,1.2),Make3Float(0,1.1,0),Make3Float(0,0,1.1),
Make3Float(0.1,0,0),Make3Float(0.3,0.3,0.3),Make3Float(0,0,0),Make3Float(0.2,0.2,0.2),Make3Float(0.5,0.5,0.1),1);
Engine.AddEntity(Rings[1]);
Engine.AddEntity(Rings[2]);
Engine.AddEntity(Rings[3]);
end;


procedure Rotate(var r1,r2,r3:T3Float; a1,a2,a3:Float);
var a:array[1..10]of T3Float;
begin;
  a[1]:=VAdd(VMlp(r1,cos(a1*2*pi)),       VMlp(r2,sin(a1*2*pi)));
  a[2]:=VAdd(VMlp(r1,cos(a1*2*pi+pi/2)),  VMlp(r2,sin(a1*2*pi+pi/2)));
  a[3]:=r3;
  a[4]:=VAdd(VMlp(a[2],cos(a2*2*pi)),     VMlp(a[3],sin(a2*2*pi)));
  a[5]:=VAdd(VMlp(a[2],cos(a2*2*pi+pi/2)),VMlp(a[3],sin(a2*2*pi+pi/2)));
  a[2]:=a[4];
  a[3]:=a[5];
  a[4]:=VAdd(VMlp(a[1],cos(a3*2*pi)),     VMlp(a[3],sin(a3*2*pi)));
  a[5]:=VAdd(VMlp(a[1],cos(a3*2*pi+pi/2)),VMlp(a[3],sin(a3*2*pi+pi/2)));
  a[1]:=a[4];
  a[3]:=a[5];
  r1:=a[1];
  r2:=a[2];
  r3:=a[3];
end;

procedure RotateSphere(a1,a2,a3:Float);
var a:array[1..3]of T3Float;
begin;
  a[1]:=Make3Float(1,0,0);
  a[2]:=Make3Float(0,1,0);
  a[3]:=Make3Float(0,0,1);
  Rotate(a[1],a[2],a[3],a1,a2,a3);
  Rings[1].Create(Make3Float(0,0,0),VMlp(a[1],1.2),VMlp(a[2],1.2),VMlp(a[1],1.1),VMlp(a[2],1.1),VMlp(a[3],0.1),
  Make3Float(0.4,0.4,0.4),Make3Float(0.0,0.0,0.0),Make3Float(0.3,0.3,0.1),Make3Float(0.3,0.3,0.05),1.2);
  a[1]:=Make3Float(1,0,0);
  a[2]:=Make3Float(0,0,1);
  a[3]:=Make3Float(0,1,0);
  Rotate(a[1],a[2],a[3],a1,-a2,a3);
  Rings[2].Create(Make3Float(0,0,0),VMlp(a[1],1.3),VMlp(a[2],1.3),VMlp(a[1],1.2),VMlp(a[2],1.2),VMlp(a[3],0.1),
  Make3Float(0.4,0.4,0.4),Make3Float(0.0,0.0,0.0),Make3Float(0.3,0.3,0.1),Make3Float(0.3,0.3,0.05),1.2);
  a[1]:=Make3Float(0,1,0);
  a[2]:=Make3Float(0,0,1);
  a[3]:=Make3Float(1,0,0);
  Rotate(a[1],a[2],a[3],-a1,a2,a3);
  Rings[3].Create(Make3Float(0,0,0),VMlp(a[1],1.4),VMlp(a[2],1.4),VMlp(a[1],1.3),VMlp(a[2],1.3),VMlp(a[3],0.1),
  Make3Float(0.4,0.4,0.4),Make3Float(0.0,0.0,0.0),Make3Float(0.3,0.3,0.1),Make3Float(0.3,0.3,0.05),1.2);
end;

procedure CreateEye(Coord, X, Y, Z, Color, CReflection, CRefraction, CDiffusion: T3Float; KRefraction:Float);
begin;
Engine.AddEntity(TStillRing.Create(Coord,X,Y,VMlp(X,0.3),VMlp(Y,0.85),Z,CReflection,CRefraction,CDiffusion,Color,KRefraction));
Engine.AddEntity(TStillCylinder.Create(Coord,VMlp(X,0.25),VMlp(Y,0.25),Z,CReflection,CRefraction,CDiffusion,Color,KRefraction));
end;

procedure CreateColumn(Coord: T3Float);
var Texture: TBitmap;
begin;
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\marble02.bmp');
Engine.AddEntity(TTexturedTube.Create(Texture,1,1/2.25,VAdd(Coord,Make3Float(0,0, 2.2)),Make3Float(0.5,0,0),Make3Float(0,0.5,0),Make3Float(0,0,-1.5),Make3Float(0.3,0.3,0.3),Make3Float(0.3,0.3,0.3),Make3Float(0.4,0.4,0.4)));
Engine.AddEntity(TTexturedTube.Create(Texture,1,1/2.25,VAdd(Coord,Make3Float(0,0,-2.2)),Make3Float(0.5,0,0),Make3Float(0,0.5,0),Make3Float(0,0,+1.5),Make3Float(0.3,0.3,0.3),Make3Float(0.3,0.3,0.3),Make3Float(0.4,0.4,0.4)));
Engine.AddEntity(TTexturedSphere.Create(Texture,1,-1,Coord,Make3Float(0.3,0.3,0.3),Make3Float(0,0,0),Make3Float(0.0,0.0,0.0),Make3Float(0.4,0.4,0.4),1,0.5));
Engine.AddEntity(TTexturedSphere.Create(Texture,1,1,VAdd(Coord,Make3Float(0,0, 0.7)),Make3Float(0.3,0.3,0.3),Make3Float(0,0,0),Make3Float(0.3,0.3,0.3),Make3Float(0.4,0.4,0.4),1,0.5));
Engine.AddEntity(TTexturedSphere.Create(Texture,1,1,VAdd(Coord,Make3Float(0,0,-0.7)),Make3Float(0.3,0.3,0.3),Make3Float(0,0,0),Make3Float(0.3,0.3,0.3),Make3Float(0.4,0.4,0.4),1,0.5));
Texture.Free;
end;

procedure CreateScene;
var Texture, Texture1:TBitmap;
begin;
//Camera
//Engine.Camera.Free;
//Engine.Camera:=TStillCamera.Create(Make3Float(0,6,1.5-3),Make3Float(1,0,0),VMlp(Make3Float(0,0,1),-1),Make3Float(0,1,0));
//Engine.Camera:=TStillCamera.Create(Make3Float(0,8,0),Make3Float(1.5,0,0),VMlp(Make3Float(0,0,1.5),-1),Make3Float(0,-2,0));
//Engine.Camera:=TStillCamera.Create(Make3Float(0,0,0),Make3Float(0,1,0),VMlp(Make3Float(1,0,0),-1),Make3Float(0,0,-2));
//Engine.Camera.ResX:=400;
//Engine.Camera.ResY:=400;

// Light
Engine.AddEntity(TSpotLight.Create(Make3Float(0,-9,-1),Make3Float(0.4,0.4,0.4)));


//Floor
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\pattern01.bmp');
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/10,1/3,Make3Float(-10,-10,-3),Make3Float(0,20,0),Make3Float(6,0,0),
Make3Float(0.4,0.4,0.4),Make3Float(0.3,0.3,0.3),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/10,1/3,Make3Float(4,-10,-3),Make3Float(0,20,0),Make3Float(6,0,0),
Make3Float(0.4,0.4,0.4),Make3Float(0.3,0.3,0.3),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/3,1/4,Make3Float(-4,-10,-3),Make3Float(0,6,0),Make3Float(8,0,0),
Make3Float(0.4,0.4,0.4),Make3Float(0.3,0.3,0.3),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/3,1/4,Make3Float(-4,4,-3),Make3Float(0,6,0),Make3Float(8,0,0),
Make3Float(0.4,0.4,0.4),Make3Float(0.3,0.3,0.3),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4,1/4,Make3Float(-4,-4,-3.5),Make3Float(0,8,0),Make3Float(8,0,0),
Make3Float(0.4,0.4,0.4),Make3Float(0.3,0.3,0.3),Make3Float(0.3,0.3,0.3)));
Texture.Free;
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\marble04.bmp');
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4,4,Make3Float(-4,-4,-3),Make3Float(0,8,0),Make3Float(0,0,-0.5),
Make3Float(0.5,0.5,0.5),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4,4,Make3Float(-4,-4,-3),Make3Float(8,0,0),Make3Float(0,0,-0.5),
Make3Float(0.5,0.5,0.5),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4,4,Make3Float(4,4,-3),Make3Float(-8,0,0),Make3Float(0,0,-0.5),
Make3Float(0.5,0.5,0.5),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4,4,Make3Float(4,4,-3),Make3Float(0,-8,0),Make3Float(0,0,-0.5),
Make3Float(0.5,0.5,0.5),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2)));
Texture.Free;



//Altar
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\marble01.bmp');
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/2,1/4,Make3Float(-2,9,1-3),Make3Float(0,-2,0),Make3Float(4,0,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/2,1/0.2,Make3Float(-2,7,1-3),Make3Float(0,2,0),Make3Float(0,0,-0.2),
Make3Float(0.5,0.5,0.5),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/2,1/0.2,Make3Float(2,7,1-3),Make3Float(0,2,0),Make3Float(0,0,-0.2),
Make3Float(0.5,0.5,0.5),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4,1/0.2,Make3Float(-2,7,1-3),Make3Float(4,0,0),Make3Float(0,0,-0.2),
Make3Float(0.5,0.5,0.5),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/1.6,1,Make3Float(-1.8,7.2,0.9-3),Make3Float(0,1.6,0),Make3Float(0,0,-1),
Make3Float(0.5,0.5,0.5),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/1.6,1,Make3Float(1.8,7.2,0.9-3),Make3Float(0,1.6,0),Make3Float(0,0,-1),
Make3Float(0.5,0.5,0.5),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/3.6,1,Make3Float(-1.8,7.2,0.9-3),Make3Float(3.6,0,0),Make3Float(0,0,-1),
Make3Float(0.5,0.5,0.5),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2)));
Texture.Free;



//Walls
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\wall01.bmp');
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/14,1/6,Make3Float(8,10,  -3),Make3Float(0,-20,0),Make3Float(0,0,6),
Make3Float(0.6,0.6,0.6),Make3Float(0.1,0.1,0.1),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/5,1/6,Make3Float(8,-10, -3),Make3Float(-6,0,0),Make3Float(0,0,6),
Make3Float(0.6,0.6,0.6),Make3Float(0.1,0.1,0.1),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/5,1/6,Make3Float(-2,-10, -3),Make3Float(-6,0,0),Make3Float(0,0,6),
Make3Float(0.6,0.6,0.6),Make3Float(0.1,0.1,0.1),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/14,1/6,Make3Float(-8,-10,-3),Make3Float(0,20,0),Make3Float(0,0,6),
Make3Float(0.6,0.6,0.6),Make3Float(0.1,0.1,0.1),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/14,1/6,Make3Float(-8,10, -3),Make3Float(16,0,0),Make3Float(0,0,6),
Make3Float(0.6,0.6,0.6),Make3Float(0.1,0.1,0.1),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4,1/2,Make3Float(-2,-10, 1),Make3Float(4,0,0),Make3Float(0,0,2),
Make3Float(0.6,0.6,0.6),Make3Float(0.1,0.1,0.1),Make3Float(0.3,0.3,0.3)));
Texture.Free;



//Walls2
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\marble02.bmp');
Engine.AddEntity(TTexturedFlatRing.Create(Texture,1/3,1/100,Make3Float(-7,6,-3),Make3Float(0,2.51,0),Make3Float(0,0,100),Make3Float(0,2,0),
Make3Float(0,0,5),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedFlatRing.Create(Texture,1/3,1/100,Make3Float(-7,1,-3),Make3Float(0,2.51,0),Make3Float(0,0,100),Make3Float(0,2,0),
Make3Float(0,0,5),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedFlatRing.Create(Texture,1/3,1/100,Make3Float(-7,-4,-3),Make3Float(0,2.51,0),Make3Float(0,0,100),Make3Float(0,2,0),
Make3Float(0,0,5),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4*2,1/6*2,Make3Float(-7,-10,-3),Make3Float(0,3.51,0),Make3Float(0,0,6),
Make3Float(0.5,0.5,0.5),Make3Float(0.0,0.0,0.0),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/2*2,1/6*2,Make3Float(-7,8.49,-3),Make3Float(0,2.51,0),Make3Float(0,0,6),
Make3Float(0.5,0.5,0.5),Make3Float(0.0,0.0,0.0),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedTube.Create(Texture,1/7,2,Make3Float(-7.5,6,-3),Make3Float(0,2,0),
Make3Float(0,0,5),Make3Float(0.5,0,0),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedTube.Create(Texture,1/7,2,Make3Float(-7.5,1,-3),Make3Float(0,2,0),
Make3Float(0,0,5),Make3Float(0.5,0,0),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedTube.Create(Texture,1/7,2,Make3Float(-7.5,-4,-3),Make3Float(0,2,0),
Make3Float(0,0,5),Make3Float(0.5,0,0),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5)));

Engine.AddEntity(TTexturedFlatRing.Create(Texture,1/3,1/100,Make3Float(7,6,-3),Make3Float(0,2.51,0),Make3Float(0,0,100),Make3Float(0,2,0),
Make3Float(0,0,5),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedFlatRing.Create(Texture,1/3,1/100,Make3Float(7,1,-3),Make3Float(0,2.51,0),Make3Float(0,0,100),Make3Float(0,2,0),
Make3Float(0,0,5),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedFlatRing.Create(Texture,1/3,1/100,Make3Float(7,-4,-3),Make3Float(0,2.51,0),Make3Float(0,0,100),Make3Float(0,2,0),
Make3Float(0,0,5),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4*2,1/6*2,Make3Float(7,-10,-3),Make3Float(0,3.51,0),Make3Float(0,0,6),
Make3Float(0.5,0.5,0.5),Make3Float(0.0,0.0,0.0),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/2*2,1/6*2,Make3Float(7,8.49,-3),Make3Float(0,2.51,0),Make3Float(0,0,6),
Make3Float(0.5,0.5,0.5),Make3Float(0.0,0.0,0.0),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedTube.Create(Texture,1/20,1,Make3Float(7.5,6,-3),Make3Float(0,2,0),
Make3Float(0,0,5),Make3Float(0.5,0,0),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedTube.Create(Texture,1/20,1,Make3Float(7.5,1,-3),Make3Float(0,2,0),
Make3Float(0,0,5),Make3Float(0.5,0,0),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedTube.Create(Texture,1/20,1,Make3Float(7.5,-4,-3),Make3Float(0,2,0),
Make3Float(0,0,5),Make3Float(0.5,0,0),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5)));
//Texture.Free;

//Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\marble01.bmp');
Engine.AddEntity(TTexturedTube.Create(Texture,10,1/6,Make3Float(-7,8.5,0),Make3Float(0,0.1,0),
Make3Float(0.1,0,0),Make3Float(0,0,3),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.3,0.2)));
Engine.AddEntity(TTexturedTube.Create(Texture,10,1/6,Make3Float(-7,3.5,0),Make3Float(0,0.1,0),
Make3Float(0.1,0,0),Make3Float(0,0,3),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.3,0.2)));
Engine.AddEntity(TTexturedTube.Create(Texture,10,1/6,Make3Float(-7,-1.5,0),Make3Float(0,0.1,0),
Make3Float(0.1,0,0),Make3Float(0,0,3),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.3,0.2)));
Engine.AddEntity(TTexturedTube.Create(Texture,10,1/6,Make3Float(-7,-6.5,0),Make3Float(0,0.1,0),
Make3Float(0.1,0,0),Make3Float(0,0,3),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.3,0.2)));

Engine.AddEntity(TTexturedTube.Create(Texture,10,1/6,Make3Float(7,8.5,0),Make3Float(0,0.1,0),
Make3Float(0.1,0,0),Make3Float(0,0,3),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.3,0.2)));
Engine.AddEntity(TTexturedTube.Create(Texture,10,1/6,Make3Float(7,3.5,0),Make3Float(0,0.1,0),
Make3Float(0.1,0,0),Make3Float(0,0,3),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.3,0.2)));
Engine.AddEntity(TTexturedTube.Create(Texture,10,1/6,Make3Float(7,-1.5,0),Make3Float(0,0.1,0),
Make3Float(0.1,0,0),Make3Float(0,0,3),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.3,0.2)));
Engine.AddEntity(TTexturedTube.Create(Texture,10,1/6,Make3Float(7,-6.5,0),Make3Float(0,0.1,0),
Make3Float(0.1,0,0),Make3Float(0,0,3),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.3,0.2)));
Texture.Free;



//Central Altar
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\marble04.bmp');
Engine.AddEntity(TTexturedTube.Create(Texture,3,3,Make3Float(0,0,0-3),Make3Float(0.7,0,0),
Make3Float(0,0.7,0),Make3Float(0,0,0.5),Make3Float(0.2,0.2,0.2),Make3Float(0.3,0.3,0.3),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedEllipse.Create(Texture,3,3,Make3Float(0,0,0.5-3),Make3Float(0.7,0,0),Make3Float(0,0.7,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.2,0.2),Make3Float(0.3,0.3,0.3)));

Engine.AddEntity(TTexturedTube.Create(Texture,1,20,Make3Float(0,0,-3.25),Make3Float(2,0,0),
Make3Float(0,1,0),Make3Float(0,0,0.25),Make3Float(0.2,0.2,0.2),Make3Float(0.3,0.3,0.3),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedFlatRing.Create(Texture,1,2,Make3Float(0,0,-3),
Make3Float(2,0,0),Make3Float(0,1,0),Make3Float(1,0,0),Make3Float(0,1,0),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2),Make3Float(0.5,0.5,0.5)));

Engine.AddEntity(TTexturedTube.Create(Texture,1,20,Make3Float(0,0,-3.25),Make3Float(1,0,0),
Make3Float(0,2,0),Make3Float(0,0,0.25),Make3Float(0.2,0.2,0.2),Make3Float(0.3,0.3,0.3),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedFlatRing.Create(Texture,1,2,Make3Float(0,0,-3),
Make3Float(1,0,0),Make3Float(0,2,0),Make3Float(1,0,0),Make3Float(0,1,0),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2),Make3Float(0.5,0.5,0.5)));

Engine.AddEntity(TTexturedTube.Create(Texture,1,20,Make3Float(0,0,-3.25),Make3Float(2/sqrt(2),2/sqrt(2),0),
Make3Float(-1/sqrt(2),1/sqrt(2),0),Make3Float(0,0,0.25),Make3Float(0.2,0.2,0.2),Make3Float(0.3,0.3,0.3),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedFlatRing.Create(Texture,1,2,Make3Float(0,0,-3),
Make3Float(2/sqrt(2),2/sqrt(2),0),Make3Float(-1/sqrt(2),1/sqrt(2),0),Make3Float(1,0,0),Make3Float(0,1,0),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2),Make3Float(0.5,0.5,0.5)));

Engine.AddEntity(TTexturedTube.Create(Texture,1,20,Make3Float(0,0,-3.25),Make3Float(-2/sqrt(2),2/sqrt(2),0),
Make3Float(1/sqrt(2),1/sqrt(2),0),Make3Float(0,0,0.25),Make3Float(0.2,0.2,0.2),Make3Float(0.3,0.3,0.3),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedFlatRing.Create(Texture,1,2,Make3Float(0,0,-3),
Make3Float(-2/sqrt(2),2/sqrt(2),0),Make3Float(1/sqrt(2),1/sqrt(2),0),Make3Float(1,0,0),Make3Float(0,1,0),Make3Float(0.3,0.3,0.3),Make3Float(0.2,0.2,0.2),Make3Float(0.5,0.5,0.5)));

Engine.AddEntity(TTexturedTube.Create(Texture,1,20,Make3Float(0,0,-3.25),Make3Float(1,0,0),
Make3Float(0,1,0),Make3Float(0,0,0.25),Make3Float(0.2,0.2,0.2),Make3Float(0.3,0.3,0.3),Make3Float(0.5,0.5,0.5)));
Texture.Free;



//Sky
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\sky.bmp');
Engine.AddEntity(TTexturedSphere.Create(Texture,2,-1,Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(1,1,1),1,100000));
Texture.Free;



//Columns
CreateColumn(Make3Float(5.5, 6,0));
CreateColumn(Make3Float(5.5, 3,0));
CreateColumn(Make3Float(5.5, 0,0));
CreateColumn(Make3Float(5.5,-3,0));
CreateColumn(Make3Float(5.5,-6,0));
CreateColumn(Make3Float(-5.5, 6,0));
CreateColumn(Make3Float(-5.5, 3,0));
CreateColumn(Make3Float(-5.5, 0,0));
CreateColumn(Make3Float(-5.5,-3,0));
CreateColumn(Make3Float(-5.5,-6,0));

// Ancient tablet
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\ancient01.bmp');
Engine.AddEntity(TTexturedEllipse.Create(Texture,1,1,Make3Float(0,9.5,0.5),Make3Float(2.2,0,0),Make3Float(0,0,-2.3),Make3Float(0.5,0.5,0.5),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5)));
Texture.Free;
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\ancient02.bmp');
Engine.AddEntity(TTexturedTube.Create(Texture,0.05,1,Make3Float(0,9.75,0.5),Make3Float(2.2,0,0),Make3Float(0,0,-2.3),Make3Float(0,0.25,0),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5)));
Texture.Free;

// Coins
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\ancient03.bmp');
Engine.AddEntity(TTexturedEllipse.Create(Texture,1,1,Make3Float(0.5,7.9,1.1-3),Make3Float(0.3,0,0),Make3Float(0,0.3,0),Make3Float(0.7,0.7,0.7),Make3Float(0.0,0.0,0.0),Make3Float(0.3,0.3,0.3)));
Texture.Free;
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\ancient04.bmp');
Engine.AddEntity(TTexturedTube.Create(Texture,0.05,1,Make3Float(0.5,7.9,1.05-3),Make3Float(0.3,0,0),Make3Float(0,0.3,0),Make3Float(0,0,0.05),Make3Float(0.0,0.0,0.0),Make3Float(0.3,0.3,0.3),Make3Float(0.7,0.7,0.7)));
Texture.Free;
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\ancient05.bmp');
Engine.AddEntity(TTexturedFlatRing.Create(Texture,1,1,Make3Float(1.1,8.3,1.1-3),Make3Float(0.3,0,0),Make3Float(0,0.3,0),
Make3Float(0.069,0,0),Make3Float(0,0.069,0),Make3Float(0.2,0.2,0.2),Make3Float(0.3,0.3,0.3),Make3Float(0.5,0.5,0.5)));
Texture.Free;
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\ancient06.bmp');
Engine.AddEntity(TTexturedTube.Create(Texture,0.05,1,Make3Float(1.1,8.3,1.05-3),Make3Float(0.069,0,0),Make3Float(0,0.069,0),
Make3Float(0,0,0.05),Make3Float(0.2,0.2,0.2),Make3Float(0.3,0.3,0.3),Make3Float(0.5,0.5,0.5)));
Engine.AddEntity(TTexturedTube.Create(Texture,0.05,1,Make3Float(1.1,8.3,1.05-3),Make3Float(0.3,0,0),Make3Float(0,0.3,0),
Make3Float(0,0,0.05),Make3Float(0.2,0.2,0.2),Make3Float(0.3,0.3,0.3),Make3Float(0.5,0.5,0.5)));
Texture.Free;
Engine.AddEntity(TStillCylinder.Create(Make3Float(1.5,7.5,1.3-3),Make3Float(0.3,0,0),Make3Float(0,0.3,0),Make3Float(0,0,0.3),Make3Float(0.1,0.1,0.1),
Make3Float(0.8,0.8,0.75),Make3Float(0,0,0),Make3Float(0,0,0.05),1.2));

//Planets
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\planet01.bmp');
Engine.AddEntity(TTexturedSphere.Create(Texture,2,-1,Make3Float(-0.5,8,1.5-3),Make3Float(0.1,0.1,0.1),Make3Float(0,0,0),Make3Float(0.3,0.3,0.3),Make3Float(0.6,0.6,0.6),1,0.5));
Texture.Free;
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\planet02.bmp');
Engine.AddEntity(TTexturedSphere.Create(Texture,2,1,Make3Float(-1.3,7.8,1.1-3),Make3Float(0.1,0.1,0.1),Make3Float(0,0,0),Make3Float(0.3,0.3,0.3),Make3Float(0.6,0.6,0.6),1,0.1));
Texture.Free;

//Glass Sphere
Engine.AddEntity(TStillSphere.Create(Make3Float(0.2,7.2,1.3-3),Make3Float(0.0,0.0,0.0),Make3Float(0.2,0.9,0.2),Make3Float(0,0,0),Make3Float(0,0,0),1.2,0.3));

// stone spheres
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\stone01.bmp');
Engine.AddEntity(TTexturedSphere.Create(Texture,1,1,Make3Float(-4,9,-2),Make3Float(0,0,0),Make3Float(0,0,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5),1,1));
Engine.AddEntity(TTexturedSphere.Create(Texture,1,1,Make3Float(4,9,-2),Make3Float(0,0,0),Make3Float(0,0,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5),1,1));
Engine.AddEntity(TTexturedSphere.Create(Texture,1,1,Make3Float(-4,9,2),Make3Float(0,0,0),Make3Float(0,0,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5),1,1));
Engine.AddEntity(TTexturedSphere.Create(Texture,1,1,Make3Float(4,9,2),Make3Float(0,0,0),Make3Float(0,0,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5),1,1));
Engine.AddEntity(TTexturedSphere.Create(Texture,1,1,Make3Float(-4,-9,-2),Make3Float(0,0,0),Make3Float(0,0,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5),1,1));
Engine.AddEntity(TTexturedSphere.Create(Texture,1,1,Make3Float(4,-9,-2),Make3Float(0,0,0),Make3Float(0,0,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5),1,1));
Engine.AddEntity(TTexturedSphere.Create(Texture,1,1,Make3Float(-4,-9,2),Make3Float(0,0,0),Make3Float(0,0,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5),1,1));
Engine.AddEntity(TTexturedSphere.Create(Texture,1,1,Make3Float(4,-9,2),Make3Float(0,0,0),Make3Float(0,0,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5),1,1));
Texture.Free;
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\granite01.bmp');
Engine.AddEntity(TTexturedSphere.Create(Texture,2*4/3,2,Make3Float(-4,9,0),Make3Float(0,0,0),Make3Float(0,0,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5),1,0.7));
Engine.AddEntity(TTexturedSphere.Create(Texture,2*4/3,2,Make3Float(4,9,0),Make3Float(0,0,0),Make3Float(0,0,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5),1,0.7));
Engine.AddEntity(TTexturedSphere.Create(Texture,2*4/3,2,Make3Float(-4,-9,0),Make3Float(0,0,0),Make3Float(0,0,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5),1,0.7));
Engine.AddEntity(TTexturedSphere.Create(Texture,2*4/3,2,Make3Float(4,-9,0),Make3Float(0,0,0),Make3Float(0,0,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.5,0.5,0.5),1,0.7));
Texture.Free;
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\marble01.bmp');
Engine.AddEntity(TTexturedTube.Create(Texture,10,1/6,Make3Float(-4,9,0),Make3Float(0,0.1,0),
Make3Float(0.1,0,0),Make3Float(0,0,3),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.3,0.2)));
Engine.AddEntity(TTexturedTube.Create(Texture,10,1/6,Make3Float(4,9,0),Make3Float(0,0.1,0),
Make3Float(0.1,0,0),Make3Float(0,0,3),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.3,0.2)));
Engine.AddEntity(TTexturedTube.Create(Texture,10,1/6,Make3Float(-4,-9,0),Make3Float(0,0.1,0),
Make3Float(0.1,0,0),Make3Float(0,0,3),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.3,0.2)));
Engine.AddEntity(TTexturedTube.Create(Texture,10,1/6,Make3Float(4,-9,0),Make3Float(0,0.1,0),
Make3Float(0.1,0,0),Make3Float(0,0,3),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.3,0.2)));
Engine.AddEntity(TTexturedTube.Create(Texture,10,1/6,Make3Float(0,-9,2),Make3Float(0,0.1,0),
Make3Float(0,0,0.1),Make3Float(3,0,0),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.3,0.2)));
Engine.AddEntity(TTexturedTube.Create(Texture,10,1/6,Make3Float(0,-9,2),Make3Float(0,0.1,0),
Make3Float(0,0,0.1),Make3Float(3,0,0),Make3Float(0,0,0),Make3Float(0.5,0.5,0.5),Make3Float(0.2,0.3,0.2)));
Texture.Free;

//Ceiling
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\marble04.bmp');
Engine.AddEntity(TTexturedParallelogram.Create(Texture,4*1/20,4*1/8,Make3Float(-10,-10,3),Make3Float(0,20,0),Make3Float(8,0,0),
Make3Float(0.6,0.6,0.6),Make3Float(0.1,0.1,0.1),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,4*1/20,4*1/8,Make3Float(2,-10,3),Make3Float(0,20,0),Make3Float(8,0,0),
Make3Float(0.6,0.6,0.6),Make3Float(0.1,0.1,0.1),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,4*1/8,4*1/4,Make3Float(-2,-10,3),Make3Float(0,8,0),Make3Float(4,0,0),
Make3Float(0.6,0.6,0.6),Make3Float(0.1,0.1,0.1),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,4*1/8,4*1/4,Make3Float(-2,2,3),Make3Float(0,8,0),Make3Float(4,0,0),
Make3Float(0.6,0.6,0.6),Make3Float(0.1,0.1,0.1),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4,1/2,Make3Float(2,2,3),Make3Float(0,-4,0),Make3Float(0,0,1),
Make3Float(0.6,0.6,0.6),Make3Float(0.1,0.1,0.1),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,4*1/4,4*1/2,Make3Float(2,-2,3),Make3Float(-4,0,0),Make3Float(0,0,1),
Make3Float(0.6,0.6,0.6),Make3Float(0.1,0.1,0.1),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,4*1/4,4*1/2,Make3Float(-2,-2,3),Make3Float(0,4,0),Make3Float(0,0,1),
Make3Float(0.6,0.6,0.6),Make3Float(0.1,0.1,0.1),Make3Float(0.3,0.3,0.3)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,4*1/4,4*1/2,Make3Float(-2,2,3),Make3Float(4,0,0),Make3Float(0,0,1),
Make3Float(0.6,0.6,0.6),Make3Float(0.1,0.1,0.1),Make3Float(0.3,0.3,0.3)));
Texture.Free;

//Portal
Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\marble01.bmp');
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1,1/4,Make3Float(-2,-9.5,-3),Make3Float(1,0,0),Make3Float(0,0,4),
Make3Float(0.5,0.5,0.5),Make3Float(0.1,0.1,0.1),Make3Float(0.4,0.4,0.4)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1,1/4,Make3Float(1,-9.5,-3),Make3Float(1,0,0),Make3Float(0,0,4),
Make3Float(0.5,0.5,0.5),Make3Float(0.1,0.1,0.1),Make3Float(0.4,0.4,0.4)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/2,1,Make3Float(-1,-9.5,0),Make3Float(2,0,0),Make3Float(0,0,1),
Make3Float(0.5,0.5,0.5),Make3Float(0.1,0.1,0.1),Make3Float(0.4,0.4,0.4)));

Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4,1,Make3Float(-2,-10.5,0),Make3Float(4,0,0),Make3Float(0,1,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.1,0.1,0.1),Make3Float(0.4,0.4,0.4)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4,1,Make3Float(-1,-10.5,-3),Make3Float(0,0,4),Make3Float(0,1,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.1,0.1,0.1),Make3Float(0.4,0.4,0.4)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4,1,Make3Float(1,-10.5,-3),Make3Float(0,0,4),Make3Float(0,1,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.1,0.1,0.1),Make3Float(0.4,0.4,0.4)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4,1,Make3Float(-2,-10.5,-3),Make3Float(0,0,4),Make3Float(0,1,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.1,0.1,0.1),Make3Float(0.4,0.4,0.4)));
Engine.AddEntity(TTexturedParallelogram.Create(Texture,1/4,1,Make3Float(2,-10.5,-3),Make3Float(0,0,4),Make3Float(0,1,0),
Make3Float(0.5,0.5,0.5),Make3Float(0.1,0.1,0.1),Make3Float(0.4,0.4,0.4)));
Texture.Free;

//Rotator
CreateSphere;
end;


var
  Frame, Texture: TBitmap;
  i:Integer;
  a:array[0..10]of T3Float;
  MetaBall:TMetaBalls3;
  t: Float;
begin
SetPriorityClass(GetCurrentProcess,IDLE_PRIORITY_CLASS);
Engine:=TREngine.Create;
Engine.AmbientColor:=Make3Float(0.2,0.5,0.2);
Engine.MaxTraceCount:=9;
//Engine.Camera:=TStillCamera.Create(Make3Float(0,-5,0),Make3Float(-0.5,0,0),VMlp(Make3Float(0,0,0.5),-1),Make3Float(0,1,0));
Engine.Camera:=TDofCamera.Create(Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(0,0,0),0.07,8);
Engine.Camera.ResX:=400;
Engine.Camera.ResY:=400;

CreateScene;
MetaBall:=TMetaBalls3.Create(Make3Float(0,0,1),
                             Make3Float(0,0,-1),
                             Make3Float(0,0,1),
                             Make3Float(1,1,1),
                             Make3Float(0,0,0),
                             Make3Float(0,0,0),
                             Make3Float(0,0,0),
                             1,
                             0.5,
                             0.5,
                             0.5,
                             0.15);
Engine.AddEntity(MetaBall);


//Sky
{Texture:=TBitmap.Create;Texture.LoadFromFile('Textures\sky.bmp');
Engine.AddEntity(TTexturedSphere.Create(Texture,2,-1,Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(0,0,0),Make3Float(1,1,1),1,100000));
Texture.Free;}



for i:=0 to 359 do//359 downto 300 do
  begin;
  {if(i mod 3=2) then
    continue;}
  t:=i/360;
  Writeln(i,' of 360');
  MetaBall.Create           (Make3Float(0.6*sin(t*2*pi),0.6*cos(t*2*pi),0.6*cos(t*2*pi)),
                             Make3Float(0,0.6*sin(t*2*pi+pi/2),0.6*cos(t*2*pi+pi/2)),
                             Make3Float(0.6*cos(t*2*pi+pi),0.6*sin(t*2*pi+pi),0),
                             Make3Float(0.8,0.8,0.8),
                             Make3Float(0.0,0.0,0.0),
                             Make3Float(0.2,0.2,0.2),
                             Make3Float(0,0,0),
                             1.0,
                             0.9,
                             0.8,
                             0.7,
                             0.13);
  Engine.Camera.Coord:=Make3Float(3.9*sin(t*2*pi),-6.0*cos(t*2*pi),-1);
  a[1]:=VSub(Make3Float(0,0,0),Engine.Camera.Coord);
  a[2]:=VSub(Make3Float(0,8,-2),Engine.Camera.Coord);
  a[0][1]:=VAbs2(a[1]);
  a[0][2]:=VAbs2(a[2]);
  a[0][0]:=sqrt(1/(1/a[0][1]+1/a[0][2]));
  a[3]:=Normalize(VAdd(VMlp(Normalize(a[1]),1/a[0][1]),VMlp(Normalize(a[2]),1/a[0][2])));
  (Engine.Camera as TDofCamera).Front:=VMlp(Normalize(a[3]),0.1);
  (Engine.Camera as TDofCamera).FocalFront:=VMlp((Engine.Camera as TDofCamera).Front,10*a[0][0]);
  (Engine.Camera as TDofCamera).Top:=Make3Float(0,0,-0.05);
  (Engine.Camera as TDofCamera).Right:=VMlp(Normalize(VVMlp((Engine.Camera as TStillCamera).Front,(Engine.Camera as TStillCamera).Top)),0.05);
  {(Engine.Camera as TStillCamera).Front:=VMlp(Normalize(a[3]),0.1);
  (Engine.Camera as TStillCamera).Top:=Make3Float(0,0,-0.05);
  (Engine.Camera as TStillCamera).Right:=VMlp(Normalize(VVMlp((Engine.Camera as TStillCamera).Front,(Engine.Camera as TStillCamera).Top)),0.05);}

  {Engine.Camera.Coord:=Make3Float(3.0*sin(i/80*pi),3.0*cos(i/80*pi),0);
  (Engine.Camera as TDofCamera).Front:=Make3Float(-0.5*sin(i/80*pi),-0.5*cos(i/80*pi),0);
  (Engine.Camera as TDofCamera).FocalFront:=Make3Float(-3*sin(i/80*pi),-3*cos(i/80*pi),0);
  (Engine.Camera as TDofCamera).Top:=Make3Float(0,0,-0.5);
  (Engine.Camera as TDofCamera).Right:=VMlp(Normalize(VVMlp((Engine.Camera as TStillCamera).Front,(Engine.Camera as TStillCamera).Top)),0.5);}

  RotateSphere(t,t*2,t*4);

  Frame:=Engine.Render(0);
  if i<10 then
    Frame.SaveToFile('00'+IntToStr(i)+'.bmp')
  else if i<100 then
    Frame.SaveToFile('0'+IntToStr(i)+'.bmp')
  else
    Frame.SaveToFile(IntToStr(i)+'.bmp')
  end;
Engine.Free;
end.
