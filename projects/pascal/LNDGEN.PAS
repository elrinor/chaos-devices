{gen landscape}
uses crt;
const colors:array[0..15]of string[6]=
 ('000000','0000BB','00C000','','','','A05000','C0C0C0','C0C0C0','1111FF','00C000','','','','FFFF00','FFFFFF');

type TSymb=record
      color:byte;
      char:char;
     end;

var a:array[0..99,0..99]of byte;
    b:array[0..99,0..99]of TSymb;

procedure CosmeticalCorrect(x,y:Integer);
var q:Integer;
begin;
x:=(x+100) mod 100;
y:=(y+100) mod 100;
if b[x,y].char='=' then
  begin;
  q:=0;
  if (b[(x+99) mod 100,y].char<>'=') then inc(q);
  if (b[(x+1) mod 100,y].char<>'=') then inc(q);
  if (b[x,(y+99) mod 100].char<>'=') then inc(q);
  if (b[x,(y+1) mod 100].char<>'=') then inc(q);
  if q>=3 then
    begin;
    case random(4) of
      0:b[x,y]:=b[(x+99) mod 100,y];
      1:b[x,y]:=b[(x+1) mod 100,y];
      2:b[x,y]:=b[x,(y+99) mod 100];
      3:b[x,y]:=b[x,(y+1) mod 100];
    end;
    end;
  end;
if b[x,y].char<>'=' then
  begin;
  q:=0;
  if (b[(x+99) mod 100,y].char='=') then inc(q);
  if (b[(x+1) mod 100,y].char='=') then inc(q);
  if (b[x,(y+99) mod 100].char='=') then inc(q);
  if (b[x,(y+1) mod 100].char='=') then inc(q);
  if q>=3 then
    begin;
    case random(4) of
      0:b[x,y]:=b[(x+99) mod 100,y];
      1:b[x,y]:=b[(x+1) mod 100,y];
      2:b[x,y]:=b[x,(y+99) mod 100];
      3:b[x,y]:=b[x,(y+1) mod 100];
    end;
    end;
  end;
end;


var incr,i,sx,sy,dlx,dly,x,y:Integer;
    waterlevel:Integer;
    c:Char;
    f:text;
begin;
clrscr;
randomize;
fillchar(a,sizeof(a),0);

{elevation}
for i:=1 to 2000 do
  begin;
  sx:=random(100);
  sy:=random(100);
  dlx:=16{+random(30)};
  dly:=16{+random(30)};
  for x:=sx to sx+dlx-1 do
    for y:=sy to sy+dly-1 do
      begin;
      if (sqr(sx+7-x)+sqr(sy+7-y)<=50) then
        begin;
        incr:=1+random(5);
        if a[x mod 100,y mod 100]<250 then
          inc(a[x mod 100,y mod 100],incr);
        end;
      end;
  end;


waterlevel:=100;

for x:=0 to 99 do
  for y:=0 to 99 do
    begin;
    if (a[x,y]<waterlevel div 2) then
      begin;
      b[x,y].char:='=';
      b[x,y].color:=1;
      end
    else if (a[x,y]<=waterlevel) then
      begin;
      b[x,y].char:='=';
      b[x,y].color:=9;
      end
    else if (a[x,y]<waterlevel +10) then
      begin;
      b[x,y].char:='"';
      b[x,y].color:=10;
      end
    else if (a[x,y]<waterlevel +20) then
      begin;
      b[x,y].char:='~';
      b[x,y].color:=6;
      end
    else if (a[x,y]<waterlevel +30) then
      begin;
      b[x,y].char:='^';
      b[x,y].color:=8;
      end
    else if (a[x,y]>=waterlevel +30) then
      begin;
      b[x,y].char:='^';
      b[x,y].color:=15;
      end;
    end;

for x:=0 to 99 do
  for y:=0 to 99 do
    begin;
    CosmeticalCorrect(x,y);
    end;

for x:=99 downto 0 do
  for y:=99 downto 0 do
    begin;
    CosmeticalCorrect(x,y);
    end;

{for x:=0 to 99 do
  for y:=0 to 99 do
    begin;
    CosmeticalCorrect(x,y);
    end;

for x:=99 downto 0 do
  for y:=99 downto 0 do
    begin;
    CosmeticalCorrect(x,y);
    end;}


for x:=0 to 99 do
  for y:=0 to 99 do
    begin;
    if (b[x,y].char='~')or(b[x,y].char='^') then
      begin;
      if (b[(x+99) mod 100,y].char='=')
      or (b[(x+1) mod 100,y].char='=')
      or (b[x,(y+99) mod 100].char='=')
      or (b[x,(y+1) mod 100].char='=')
      or (b[(x+99) mod 100,(y+1) mod 100].char='=')
      or (b[(x+1) mod 100,(y+99) mod 100].char='=')
      or (b[(x+99) mod 100,(y+99) mod 100].char='=')
      or (b[(x+1) mod 100,(y+1) mod 100].char='=') then
        begin;
        b[x,y].char:='"';
        b[x,y].color:=10;
        end;
      end;
    end;

textcolor(15);
gotoxy(70,25);
write(waterlevel);

sx:=0;
sy:=0;
repeat
c:=readkey;
if c='8' then dec(sy);
if c='2' then inc(sy);
if c='4' then dec(sx);
if c='6' then inc(sx);
if sx<0 then sx:=sx+100;
if sy<0 then sy:=sy+100;
textcolor(15);gotoxy(1,25);write(sx,' ',sy,'   ');

for x:=1 to 80 do
  for y:=1 to 24 do
    begin;
    gotoxy(x,y);
    textcolor(b[(sx+x) mod 100, (sy+y) mod 100].color);
    write(b[(sx+x) mod 100, (sy+y) mod 100].char);
    end;
until c=#27;

assign(f,'out.html');rewrite(f);
writeln(f,'<body bgcolor=000000><font face="Courier New">');
for y:=0 to 99 do
  begin;
  writeln(f,'<br>');
  for x:=0 to 99 do
    begin;
    write(f,'<font color=',colors[b[x,y].color],'>',b[x,y].char,'</font>');
    end;
  end;
writeln(f,'</font></body>');
close(f);
end.
