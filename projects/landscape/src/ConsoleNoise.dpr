program ConsoleNoise;
{$APPTYPE CONSOLE}
uses SysUtils,Graphics,Math;

const MaxX=1024;                 // Размеры. Может быть только 2^X
      MaxY=1024;                 //
      Mount=100;                // гора
      Water=0;                  // уровень воды
      Deep=-100;                // глубокая вода
      StartStep=128;            // \
      StartAmplitude=128;       // - НЕ МЕНЯТЬ
      StepMlp=0.5;              // /
      AmplitudeMlp=0.5;         // умножитель амплитуды - можно играть
      ShadowU=9;ShadowD=10;     // теневой коэффициент ShadowU/ShadowD(1=нет тени 0=черная тень)
      HeightMlpU=1;HeightMlpD=2;// Коэф-т умножения высоты при выводе в 3D
      TurnCount=120;            //

const cosinus:array[0..180] of Extended=(1.00000, 0.99985, 0.99939, 0.99863, 0.99756, 0.99619, 0.99452, 0.99255, 0.99027, 0.98769, 0.98481, 0.98163, 0.97815, 0.97437, 0.97030, 0.96593, 0.96126, 0.95630, 0.95106, 0.94552, 0.93969, 0.93358, 0.92718, 0.92050, 0.91355, 0.90631, 0.89879, 0.89101, 0.88295, 0.87462, 0.86603, 0.85717, 0.84805, 0.83867, 0.82904, 0.81915, 0.80902, 0.79864, 0.78801, 0.77715, 0.76604, 0.75471, 0.74314, 0.73135, 0.71934, 0.70711, 0.69466, 0.68200, 0.66913, 0.65606, 0.64279, 0.62932, 0.61566, 0.60182, 0.58779, 0.57358, 0.55919, 0.54464, 0.52992, 0.51504, 0.50000, 0.48481, 0.46947, 0.45399, 0.43837, 0.42262, 0.40674, 0.39073, 0.37461, 0.35837, 0.34202, 0.32557, 0.30902, 0.29237, 0.27564, 0.25882, 0.24192, 0.22495, 0.20791, 0.19081, 0.17365, 0.15643, 0.13917, 0.12187, 0.10453, 0.08716, 0.06976, 0.05234, 0.03490, 0.01745,
0.00000, -0.01745, -0.03490, -0.05234, -0.06976, -0.08716, -0.10453, -0.12187, -0.13917, -0.15643, -0.17365, -0.19081, -0.20791, -0.22495, -0.24192, -0.25882, -0.27564, -0.29237, -0.30902, -0.32557, -0.34202, -0.35837, -0.37461, -0.39073, -0.40674, -0.42262, -0.43837, -0.45399, -0.46947, -0.48481, -0.50000, -0.51504, -0.52992, -0.54464, -0.55919, -0.57358, -0.58779, -0.60182, -0.61566, -0.62932, -0.64279, -0.65606, -0.66913, -0.68200, -0.69466, -0.70711, -0.71934, -0.73135, -0.74314, -0.75471, -0.76604, -0.77715, -0.78801, -0.79864, -0.80902, -0.81915, -0.82904, -0.83867, -0.84805, -0.85717, -0.86603, -0.87462, -0.88295, -0.89101, -0.89879, -0.90631, -0.91355, -0.92050, -0.92718, -0.93358, -0.93969, -0.94552, -0.95106, -0.95630, -0.96126, -0.96593, -0.97030, -0.97437, -0.97815, -0.98163, -0.98481, -0.98769, -0.99027, -0.99255, -0.99452, -0.99619, -0.99756, -0.99863, -0.99939, -0.99985, -1.00000);

type arr=array[0..MaxX,0..MaxY]of integer;
var a,b,c,d:arr;
    step:Integer;
    Amplitude:Integer;
    x,y:Integer;
    color:Integer;
    Bmp:TBitMap;
    x1,y1,height:integer;
    t:text;
    colors:array[-500..500] of integer;
    scolors:array[-500..500] of integer;
    fy:array[0..StartStep] of Extended;
    i:integer;
    angle:Integer;
    nsin,ncos:Extended;
begin
Writeln('Landscape Generator by Alexandr ''[ArX]Elric!'' Fokin.');
Writeln('Loading... Please wait');
Writeln('Landscape generation could take up to '+FloatToStr(Round(100*10*(MaxX*MaxY/(1024*1024)))/100)+' seconds on Athlon 2500XP');
randomize;

for i:=-500 to 500 do
  begin;
  if i>Mount then colors[i]:=$000054A8
  else if i>Water then colors[i]:=(($00000054*(i-Water)+$00000099*(Mount-i))div(Mount-Water)) shl 8+(($000000A8*(i-Water))div(Mount-Water))
  else if i=Water then colors[i]:=$004C6619
  else if i>Deep then colors[i]:=(($00000044*(Water-i)+$00000099*(i-Deep))div(Water-Deep)) shl 16+(($00000033*(i-Deep))div(Water-Deep)) shl 8+(($00000033*(i-Deep))div(Water-Deep))
  else colors[i]:=$00440000;
  scolors[i]:=(((colors[i] and $00FF0000) shr 16)*ShadowU div ShadowD) shl 16+(((colors[i] and $0000FF00)shr 8)*ShadowU div ShadowD)shl 8+((colors[i] and $000000FF)*ShadowU div ShadowD);
  end;
Fillchar(a,sizeof(a),0);
step:=Round(StartStep/StepMlp);
Amplitude:=Round(StartAmplitude/AmplitudeMlp);
repeat
  step:=Trunc(step * StepMlp);
  amplitude:=Round(amplitude * AmplitudeMlp);
  fillchar(b,sizeof(b),0);

  x:=0;
  while (x<=MaxX) do
    begin;
    b[x,0]:=-(Amplitude shr 1);
    b[x,MaxY]:=-(Amplitude shr 1);
    x:=x+step;
    end;
  y:=0;
  while (y<=MaxY) do
    begin;
    b[0,y]:=-(Amplitude shr 1);
    b[MaxX,y]:=-(Amplitude shr 1);
    y:=y+step;
    end;
  x:=step;
  while (x<=MaxX-step) do
    begin;
    y:=step;
    while (y<=MaxY-step) do
      begin;
      if {sqrt(sqr(x-MaxX/2)+sqr(y-MaxY/2))>MaxX/2-5}(x=0)or(y=0)or(x=MaxX)or(y=MaxY) then b[x,y]:=-(Amplitude shr 1)
      else b[x,y]:=(Amplitude shr 1)-Random(Amplitude + 1);
      y:=y+step;
      end;
    x:=x+step;
    end;

  for i:=0 to step-1 do fy[i]:=(1-cosinus[(180*i) div step])*0.5;
  y:=-step;
  repeat
    y:=y+step;
    for x:=0 to MaxX do
      if (x mod step<>0) then
        b[x,y]:=Round(b[x-x mod step,y]*(1-fy[x mod step])+b[x-x mod step+step,y]*fy[x mod step]);
  until (y>=MaxY);

  if step<>1 then
  for x:=0 to MaxX do
    for y:=0 to MaxY do
      if (y mod step<>0) then
        b[x,y]:=Round(b[x,y-y mod step]*(1-fy[y mod step])+b[x,y-y mod step+step]*fy[y mod step]);

  for x:=0 to MaxX do
    for y:=0 to MaxY do
      a[x,y]:=a[x,y]+b[x,y];

until (step=1);

// Считаем цвета
Bmp:=TBitMap.Create;
Bmp.Height:=MaxY+1;
Bmp.Width:=MaxX+1;
fillchar(b,sizeof(b),0);
for x1:=0 to MaxX do
  begin;
  x:=x1-1;
  y:=-1;
  height:=a[x1,0]+1;
    repeat
      inc(x);
      inc(y);
      dec(height);
      if (x>MaxX)or(y>MaxY) then break;
      if (height>a[x,y]) then
        begin;
        Bmp.Canvas.Pixels[x,y]:=scolors[a[x,y]];
        b[x,y]:=scolors[a[x,y]]
        end
      else
        begin;
        Bmp.Canvas.Pixels[x,y]:=colors[a[x,y]];
        b[x,y]:=colors[a[x,y]];
        height:=a[x,y];
        end;
    until (1=2);
  end;
for y1:=1 to MaxX do
  begin;
  x:=-1;
  y:=y1-1;
  height:=a[0,y1]+1;
    repeat
      inc(x);
      inc(y);
      dec(height);
      if (x>MaxX)or(y>MaxY) then break;
      if (height>a[x,y]) then
        begin;
        Bmp.Canvas.Pixels[x,y]:=scolors[a[x,y]];
        b[x,y]:=scolors[a[x,y]]
        end
      else
        begin;
        Bmp.Canvas.Pixels[x,y]:=colors[a[x,y]];
        b[x,y]:=colors[a[x,y]];
        height:=a[x,y];
        end;
    until (1=2);
  end;
Bmp.SaveToFile('001.bmp');
Bmp.Free;

// Вывод в 3D
for i:=1 to TurnCount do
  begin;
  for x:=0 to MaxX do
    for y:=0 to MaxY do
      begin;
      c[x,y]:=a[0,0];
      d[x,y]:=$00440000;
      end;
  angle:=(i-1)*3;
  if (angle>=0) and (angle<90) then
    begin;
    ncos:=cosinus[angle];
    nsin:=cosinus[90-angle];
    end;
  if (angle>=90) and (angle<180) then
    begin;
    ncos:=cosinus[angle];
    nsin:=cosinus[-90+angle];
    end;
  if (angle>=180) and (angle<270) then
    begin;
    ncos:=cosinus[360-angle];
    nsin:=cosinus[-90+angle];
    end;
  if (angle>=270) and (angle<360) then
    begin;
    ncos:=cosinus[360-angle];
    nsin:=cosinus[450-angle];
    end;

  if angle=0 then
    begin;
    c:=a;
    d:=b;
    end
  else
  for x:=0 to MaxX*2 do
    for y:=0 to MaxY*2 do
      begin;
      x1:=Round(MaxX/2+(x/2-MaxX/2)*ncos-(y/2-MaxY/2)*nsin);
      y1:=Round(MaxY/2+(x/2-MaxX/2)*nsin+(y/2-MaxY/2)*ncos);
      if (x1>=0)and(y1>=0)and(x1<=MaxX)and(y1<=MaxY) then
        begin;
        c[x div 2,y div 2]:=a[x1,y1];
        d[x div 2,y div 2]:=b[x1,y1];
        end;
      end;
  Bmp:=TBitMap.Create;
  Bmp.Height:=MaxY+1;
  Bmp.Width:=MaxX+1;
  for x:=0 to MaxX do
    for y:=0 to MaxY do
      Bmp.Canvas.Pixels[x,y]:=$00440000;

  for y:=1 to MaxY-1 do
    for x:=1 to MaxX-1 do
      begin;
      for height:=c[x,y] downto min(min(c[x-1,y],c[x+1,y]),min(c[x,y-1],c[x,y+1]))-2 do
        begin;
        try
          Bmp.Canvas.Pixels[(x-MaxX shr 1)*(y+MaxY) div (2*MaxY)+MaxX shr 1,-(height*(y+MaxY)*HeightMlpU div HeightMlpD) div (2*MaxY)+(MaxY*9) div 10-MaxY*(MaxY-y) div (3*MaxY)]:=d[x,y];
        except
          on E:Exception do begin;end;
        end;
        end;
      end;

{  for y:=1 to MaxY-1 do
    for x:=1 to MaxX-1 do
      begin;
      for height:=a[x,y] downto min(min(a[x-1,y],a[x+1,y]),min(a[x,y-1],a[x,y+1]))-2 do
        begin;
        if b[x,y]=1 then color:=scolors[a[x,y]]
        else color:=colors[a[x,y]];
        try
          Bmp.Canvas.Pixels[(x-MaxX shr 1)*(y+MaxY) div (2*MaxY)+MaxX shr 1,-(height*(y+MaxY)*HeightMlpU div HeightMlpD) div (2*MaxY)+(MaxY*9) div 10-MaxY*(MaxY-y) div (3*MaxY)]:=color;
        except
          on E:Exception do begin;end;
        end;
        end;
      end;}
  if (i+1)<10 then
    Bmp.SaveToFile('00'+IntToStr(i+1)+'.bmp')
  else if (i+1)<100 then
    Bmp.SaveToFile('0'+IntToStr(i+1)+'.bmp')
  else
    Bmp.SaveToFile(IntToStr(i+1)+'.bmp');
  Bmp.Free;
  Writeln('Turn ',i,'...');
  end;

Writeln('');
writeln('Done!');
Writeln('See 1.bmp and 2.bmp for results.');
Writeln('Press [Enter] to exit.');
Readln;
end.

