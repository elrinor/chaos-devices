unit mainform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
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
const MaxX=1024;                // Размеры. Может быть только 2^X
      MaxY=1024;                //
      Mount=200;                // гора
      Water=140;                // уровень воды
      Deep=20;                  // глубокая вода
      StartStep=128;            // \
      StartAmplitude=128;       // - НЕ МЕНЯТЬ
      StepMlp=0.5;              // /
      AmplitudeMlp=0.52;        // умножитель амплитуды - можно играть
      Add=128;                  // добавление высоты
      Shadow=0.9;               // теневой коэффициент (1=нет тени 0=черная тень)
      HeightMlp=0.5;            // Коэф-т умножения высоты при выводе в 3D


procedure TForm1.FormCreate(Sender: TObject);
type arr=array[0..MaxX,0..MaxY]of real;
     Parr=^arr;
var a,b:Parr;
    step:Integer;
    Amplitude:Integer;
    x,y:Integer;
    color:Integer;
    Bmp:TBitMap;
    f:real;
    x1,y1,height:integer;
begin
New(a);
New(b);
randomize;
Fillchar(a^,sizeof(a^),0);
step:=Round(StartStep/StepMlp);
Amplitude:=Round(StartAmplitude/AmplitudeMlp);
repeat
  step:=Trunc(step * StepMlp);
  amplitude:=Round(amplitude * AmplitudeMlp);
  fillchar(b^,sizeof(b^),0);
  x:=0;
  while (x<=MaxX) do
    begin;
    y:=0;
    while (y<=MaxY) do
      begin;
      b^[x,y]:=(Amplitude div 2)-Random(Amplitude + 1);
      if (x=0)or(y=0)or(x=MaxX)or(y=MaxY) then b^[x,y]:=-(Amplitude div 2);
      y:=y+step;
      end;
    x:=x+step;
    end;

  y:=-step;
  repeat
    y:=y+step;
    for x:=0 to MaxX do
      if (x mod step<>0) then
        begin;
        f:=(1-cos(pi*(x mod step)/step))*0.5;
        b^[x,y]:=b^[x-x mod step,y]*(1-f)+b^[x-x mod step+step,y]*f;
        end;
  until (y>=MaxY);

  if step<>1 then
  for x:=0 to MaxX do
    for y:=0 to MaxY do
      if (y mod step<>0) then
        begin;
        f:=(1-cos(pi*(y mod step)/step))*0.5;
        b^[x,y]:=b^[x,y-y mod step]*(1-f)+b^[x,y-y mod step+step]*f;
        end;

  for x:=0 to MaxX do
    for y:=0 to MaxY do
      a^[x,y]:=a^[x,y]+b^[x,y];

until (step=1);

// Считаем цвета
Bmp:=TBitMap.Create;
Bmp.Height:=MaxY+1;
Bmp.Width:=MaxX+1;
for x:=0 to MaxX do
  for y:=0 to MaxY do
    begin;
    height:=Add+Round(a^[x,y]);
    if height>Mount then color:=$000054A8
    else if height>Water then color:=Round(($00000054*(height-Water)+$00000099*(Mount-height))/(Mount-Water))*256+Round($000000A8*(height-Water)/(Mount-Water))
    else if height=Water then color:=$004C6619
    else if height>Deep then color:=Round(($00000044*(Water-height)+$00000099*(height-Deep))/(Water-Deep))*256*256+Round($00000033*(height-Deep)/(Water-Deep))*256+Round($00000033*(height-Deep)/(Water-Deep))
    else color:=$00440000;
    Bmp.Canvas.Pixels[x,y]:=color;
    end;

// Накладывает тени
fillchar(b^,sizeof(b^),0);
for x:=0 to MaxX do
  for y:=0 to MaxY do
    begin;
    height:=Round(a^[x,y]-1);
    x1:=x;
    y1:=y;
    repeat
      inc(x1);
      inc(y1);
      dec(height);
      if (x1>MaxX)or(y1>MaxY) then break;
      if (height<a^[x1,y1]) then break;
      if b^[x1,y1]=1 then break;
      Bmp.Canvas.Pixels[x1,y1]:=Round(((Bmp.Canvas.Pixels[x1,y1] and $00FF0000) div 65536)*Shadow)*65536+Round(((Bmp.Canvas.Pixels[x1,y1] and $0000FF00)div 256)*Shadow)*256+Round((Bmp.Canvas.Pixels[x1,y1] and $000000FF)*Shadow);
      b^[x1,y1]:=1;
    until (1=2)
    end;
Bmp.SaveToFile('1.bmp');
Bmp.Free;

// Вывод в 3D
Bmp:=TBitMap.Create;
Bmp.Height:=MaxY+1;
Bmp.Width:=MaxX+1;
for x:=0 to MaxX do
  for y:=0 to MaxY do
    Bmp.Canvas.Pixels[x,y]:=$00000000;

for y:=0 to MaxY do
  for x:=0 to MaxX do
    begin;
    for height:=Add+Round(a^[x,y]) downto Add+Round(a^[x,y])-8 do
      begin;
      if height>Mount then color:=$000054A8
      else if height>Water then color:=Round(($00000054*(height-Water)+$00000099*(Mount-height))/(Mount-Water))*256+Round($000000A8*(height-Water)/(Mount-Water))
      else if height=Water then color:=$004C6619
      else if height>Deep then color:=Round(($00000044*(Water-height)+$00000099*(height-Deep))/(Water-Deep))*256*256+Round($00000033*(height-Deep)/(Water-Deep))*256+Round($00000033*(height-Deep)/(Water-Deep))
      else color:=$00440000;
      if b^[x,y]=1 then color:=Round(((color and $00FF0000) div 65536)*Shadow)*65536+Round(((color and $0000FF00)div 256)*Shadow)*256+Round((color and $000000FF)*Shadow);
      try
        Bmp.Canvas.Pixels[Round((x-MaxX div 2)*(y+MaxY)/(2*MaxY)+MaxX div 2),Round(-(height*HeightMlp)*(y+MaxY)/(2*MaxY)+MaxY div 10*9-MaxY*(MaxY-y)/(3*MaxY))]:=color;
      except
        on E:Exception do begin;end;
      end;
      end;
    end;

Bmp.SaveToFile('2.bmp');
Bmp.Free;

Dispose(a);
Dispose(b);
end;



end.

