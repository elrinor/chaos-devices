unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ExtCtrls;

type
  TForm1 = class(TForm)
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox1: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    Label5: TLabel;
    SpinEdit5: TSpinEdit;
    Label6: TLabel;
    SpinEdit6: TSpinEdit;
    SpinEdit7: TSpinEdit;
    Label7: TLabel;
    Label8: TLabel;
    Button1: TButton;
    ComboBox2: TComboBox;
    Label9: TLabel;
    SpinEdit8: TSpinEdit;
    Label10: TLabel;
    Image1: TImage;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

Function Mix(c1,c2,p1,p2:Integer):Integer;
begin;
Result:=Round((((c1 and $00FF0000) shr 16)*p1+((c2 and $00FF0000) shr 16)*p2)/100) shl 16+
        Round((((c1 and $0000FF00) shr  8)*p1+((c2 and $0000FF00) shr  8)*p2)/100) shl  8+
        Round((((c1 and $000000FF))*p1+((c2 and $000000FF))*p2)/100);
end;


procedure TForm1.Button1Click(Sender: TObject);
const max=256;
var x,y:Integer;
    V,rx,ry,fi1,fi2,alphav,azimut,obl,react,hh,mm,r1,r2,Time:integer;
    atmc,r1e,r2e,vangle,x1,y1,x2,y2,reactc:Extended;
    bmp,bmp2:TBitMap;
    atm:string;
begin
bmp:=TBitMap.Create;
bmp2:=TBitMap.Create;
bmp.Width:=400;
bmp.Height:=400;
bmp.Canvas.Pen.Width:=3;

obl:=Form1.ComboBox2.ItemIndex+1;
react:=Form1.ComboBox1.ItemIndex+1;
rx:=Round(Form1.SpinEdit1.Value*12/15);
ry:=Round(Form1.SpinEdit2.Value*12/15);
alphav:=Form1.SpinEdit4.Value;
azimut:=Form1.SpinEdit3.Value;
V:=Form1.SpinEdit5.Value;
hh:=Form1.SpinEdit6.Value;
mm:=Form1.SpinEdit7.Value;
Time:=Form1.SpinEdit8.Value;
Vangle:=(360-(270-azimut))/180*pi;
if V=0 then V:=1;

case react of
  1: reactc:=1;
  2: reactc:=0.663;
end;


case alphav of
  0..44:   begin;fi1:=180;fi2:= 45;end;
  45..90:  begin;fi1:=180;fi2:= 90;end;
  91..135:  begin;fi1:=180;fi2:=135;end;
  136..180:begin;fi1:=360;fi2:=180;end;
  181..359:begin;fi1:=360;fi2:=360;end;
end;

if Time<24 then
  begin;
  if (hh>22)or(hh<6) then
    begin;
    if (((obl=1) or (obl=2))and(V<2))or((obl=1)and(V>=2)and(V<=4)) then begin;atm:='инверсия';atmc:=0.45;end
    else begin;atm:='изотермия';atmc:=1;end;
    end
  else
    begin;
    if (((obl=1) or (obl=2))and(V<2))or((obl=1)and(V>=2)and(V<=4)) then begin;atm:='конвекция';atmc:=1.15;end
    else begin;atm:='изотермия';atmc:=1;end;
    end;
  end
else
  begin;
  atmc:=1;
  atm:='';
  end;


{f ~ atmc(konv-2, iso-1, inv-0.5),react,V,3.3/(Doza),sqrt(Time)}
r2e:=((reactc)*(atmc)*(V+0.3)*(sqrt(5)*3.3/sqrt(5))  ) *(8*sqrt(Time)+14*ln(Time)/ln(10)-0.03*Time+4*(pi/2+arctan((Time-12)*10))+4*(pi/2+arctan((Time-24)*10))-10*(pi/2+arctan((Time-720)*10))-12*(pi/2+arctan((Time-1440)*10)) )*0.06*15/12;
r1e:=((reactc)*(atmc)*(V+0.3)*(sqrt(5)*3.3/sqrt(50)) ) *(8*sqrt(Time)+14*ln(Time)/ln(10)-0.03*Time+4*(pi/2+arctan((Time-12)*10))+4*(pi/2+arctan((Time-24)*10))-10*(pi/2+arctan((Time-720)*10))-12*(pi/2+arctan((Time-1440)*10)) )*0.06*15/12;
r1:=round(r1e*12/15);
r2:=round(r2e*12/15);

for x:=1 to 400 do for y:=1 to 400 do
  bmp.Canvas.Pixels[x,y]:=$FFFFFF;

{bmp.Canvas.Brush.Color:=$FFFFFF;}
bmp.Canvas.Brush.Color:=$AAAAFF;
bmp.Canvas.Ellipse(rx-r2,ry-r2,rx+r2,ry+r2);
bmp.Canvas.Brush.Color:=$0000FF;
bmp.Canvas.Ellipse(rx-r1,ry-r1,rx+r1,ry+r1);
bmp.Canvas.Ellipse(rx-1,ry-1,rx+1,ry+1);

if fi1=180 then
  begin;
  bmp.Canvas.MoveTo(Round(rx-cos(vangle-pi/2)*(r1)),Round(ry+sin(vangle-pi/2)*(r1)));
  bmp.Canvas.LineTo(Round(rx-cos(vangle+pi/2)*(r1)),Round(ry+sin(vangle+pi/2)*(r1)));
  bmp.Canvas.Brush.Color:=$8888FF;
  bmp.Canvas.FloodFill(Round(rx-cos(vangle-pi)*r1/2),Round(ry+sin(vangle-pi)*r1/2),$000000,fsBorder);
  end;

if fi2<360 then
  begin;
  x2:=-cos(vangle+(fi2/2)/180*pi);
  y2:=sin(vangle+(fi2/2)/180*pi);
  x1:=Round(rx-cos(vangle+(fi2/2)/180*pi+pi/2)*r1);
  y1:=Round(ry+sin(vangle+(fi2/2)/180*pi+pi/2)*r1);
  while (x1>0)and(y1>0)and(x1<400)and(y1<400) do
    begin;
    bmp.Canvas.Pixels[Round(x1),Round(y1)]:=$000000;
    try bmp.Canvas.Pixels[Round(x1),Round(y1)-1]:=$000000; except on E:Exception do begin;end;end;
    try bmp.Canvas.Pixels[Round(x1),Round(y1)+1]:=$000000; except on E:Exception do begin;end;end;
    try bmp.Canvas.Pixels[Round(x1)-1,Round(y1)]:=$000000; except on E:Exception do begin;end;end;
    try bmp.Canvas.Pixels[Round(x1)+1,Round(y1)]:=$000000; except on E:Exception do begin;end;end;
    x1:=x1+x2;
    y1:=y1+y2;
    end;

  x2:=-cos(vangle-(fi2/2)/180*pi);
  y2:=sin(vangle-(fi2/2)/180*pi);
  x1:=Round(rx-cos(vangle-(fi2/2)/180*pi-pi/2)*r1);
  y1:=Round(ry+sin(vangle-(fi2/2)/180*pi-pi/2)*r1);
  while (x1>0)and(y1>0)and(x1<400)and(y1<400) do
    begin;
    bmp.Canvas.Pixels[Round(x1),Round(y1)]:=$000000;
    try bmp.Canvas.Pixels[Round(x1),Round(y1)-1]:=$000000; except on E:Exception do begin;end;end;
    try bmp.Canvas.Pixels[Round(x1),Round(y1)+1]:=$000000; except on E:Exception do begin;end;end;
    try bmp.Canvas.Pixels[Round(x1)-1,Round(y1)]:=$000000; except on E:Exception do begin;end;end;
    try bmp.Canvas.Pixels[Round(x1)+1,Round(y1)]:=$000000; except on E:Exception do begin;end;end;
    x1:=x1+x2;
    y1:=y1+y2;
    end;

if r2-r1>2 then
  begin;
  bmp.Canvas.Brush.Color:=$4444FF;
  bmp.Canvas.FloodFill(Round(rx+cos(vangle-pi)*(r1+2)),Round(ry-sin(vangle-pi)*(r1+2)),$000000,fsBorder);
  end;

  x2:=-cos(vangle+(fi2/2)/180*pi);
  y2:=sin(vangle+(fi2/2)/180*pi);
  x1:=Round(rx);
  y1:=Round(ry);
  while (x1>0)and(y1>0)and(x1<400)and(y1<400) do
    begin;
    bmp.Canvas.Pixels[Round(x1),Round(y1)]:=$000000;
    try bmp.Canvas.Pixels[Round(x1),Round(y1)-1]:=$000000; except on E:Exception do begin;end;end;
    try bmp.Canvas.Pixels[Round(x1),Round(y1)+1]:=$000000; except on E:Exception do begin;end;end;
    try bmp.Canvas.Pixels[Round(x1)-1,Round(y1)]:=$000000; except on E:Exception do begin;end;end;
    try bmp.Canvas.Pixels[Round(x1)+1,Round(y1)]:=$000000; except on E:Exception do begin;end;end;
    x1:=x1+x2;
    y1:=y1+y2;
    end;

  x2:=-cos(vangle-(fi2/2)/180*pi);
  y2:=sin(vangle-(fi2/2)/180*pi);
  x1:=Round(rx);
  y1:=Round(ry);
  while (x1>0)and(y1>0)and(x1<400)and(y1<400) do
    begin;
    bmp.Canvas.Pixels[Round(x1),Round(y1)]:=$000000;
    try bmp.Canvas.Pixels[Round(x1),Round(y1)-1]:=$000000; except on E:Exception do begin;end;end;
    try bmp.Canvas.Pixels[Round(x1),Round(y1)+1]:=$000000; except on E:Exception do begin;end;end;
    try bmp.Canvas.Pixels[Round(x1)-1,Round(y1)]:=$000000; except on E:Exception do begin;end;end;
    try bmp.Canvas.Pixels[Round(x1)+1,Round(y1)]:=$000000; except on E:Exception do begin;end;end;
    x1:=x1+x2;
    y1:=y1+y2;
    end;
  end;

bmp2.LoadFromFile('map.bmp');
for x:=0 to 399 do for y:=0 to 399 do
  bmp2.Canvas.Pixels[x,y]:=Mix(bmp2.Canvas.Pixels[x,y],bmp.Canvas.Pixels[x,y],50,50);

Form1.Image1.Canvas.Draw(1,1,bmp2);

bmp.Free;
bmp2.Free;

Form1.Label11.Caption:='Радиус эвакуации: '+IntToStr(Round(r1e))+' км';
Form1.Label12.Caption:='Радиус укрытия: '+IntToStr(Round(r2e))+' км';

end;

procedure TForm1.FormCreate(Sender: TObject);
var bmp:TBitMap;
begin
Form1.ComboBox1.ItemIndex:=0;
Form1.ComboBox2.ItemIndex:=0;
Form1.Image1.Picture.LoadFromFile('map.bmp');
end;

end.

