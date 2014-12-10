uses crt;
type levarr=array[1..80,4..23] of integer;
type allevarr=array[1..80,4..23,1..3] of integer;
type alltextarr=array[0..20] of string;
type allechoarr=array[1..25] of string;

var allevel:allevarr;
    txts:alltextarr;
    echos:allechoarr;

const dlay=10;

{==================================================}
procedure sysdrawwall(level:levarr;tx,ty:integer);
var i,i1:integer;
begin;
for i:=1 to 80 do
begin;
for i1:=4 to 23 do
begin;
if level[i,i1]=1 then begin;textcolor(8);gotoxy(i,i1);write('#');end;
if level[i,i1]=2 then begin;textcolor(7);gotoxy(i,i1);write('.');end;
if level[i,i1]=-1 then begin;textcolor(8);gotoxy(i,i1);write('>');end;
if level[i,i1]=-2 then begin;textcolor(14);gotoxy(i,i1);write('<');end;
end;
end;
textcolor(14);gotoxy(tx,ty);write('X');
end;
{==================================================}
{<сделано на глобальную переменную>}
function innear(sloi,n,x,y:integer):integer;
var i:integer;
begin;
i:=0;
if allevel[x+1,y+1,sloi]=n then i:=i+1;
if allevel[x  ,y+1,sloi]=n then i:=i+1;
if allevel[x-1,y+1,sloi]=n then i:=i+1;
if allevel[x-1,y  ,sloi]=n then i:=i+1;
if allevel[x-1,y-1,sloi]=n then i:=i+1;
if allevel[x  ,y-1,sloi]=n then i:=i+1;
if allevel[x+1,y-1,sloi]=n then i:=i+1;
if allevel[x+1,y  ,sloi]=n then i:=i+1;
innear:=i;
end;
{==================================================}
{<на глобальную переменную не делать!!!>}
procedure generate(var level:levarr;var enx,eny:integer);
var count,hlp,rnd,exx,exy,dx,dy,tx,ty,oldx,oldy,x,y,nkomnat,i,i1,ix,iy,dlx,dly:integer;
begin;

{null all}
for i:=4 to 23 do
begin;
for i1:=1 to 80 do
begin;
level[i1,i]:=0;
end;
end;

{gen rooms quantity}
nkomnat:=4+random(6);

{gen start room coords}
i:=0;
x:=1+random(74);
y:=4+random(16);
enx:=x+2;
eny:=y+2;

{write room in array}
repeat
i:=i+1;
dlx:=4+random(3);
dly:=4+random(3);

  for ix:=x to (x+dlx) do
  begin;
  for iy:=y to (y+dly) do
  begin;
  if ((ix=x) or (ix=x+dlx)) and ((iy>=4) and (iy<=23) and (ix>=1) and (ix<=80)) then if level[ix,iy]<>2 then level[ix,iy]:=1;
  if ((iy=y) or (iy=y+dly)) and ((iy>=4) and (iy<=23) and (ix>=1) and (ix<=80)) then if level[ix,iy]<>2 then level[ix,iy]:=1;

  if (iy>y) and (iy<y+dly) and (ix>x) and (ix<x+dlx) and (iy>=4) and (iy<=23) and (ix>=1) and (ix<=80) then level[ix,iy]:=2;

  if ((ix=1) or (ix=80)) and ((iy>=4) and (iy<=23)) then level[ix,iy]:=1;
  if ((iy=4) or (iy=23)) and ((ix>=1) and (ix<=80)) then level[ix,iy]:=1;
  end;
  end;

{gen next room coord}
oldx:=x;
oldy:=y;
repeat
if (random(2)=1) then begin;x:=1+random(74);y:=y-5+random(11);end
else begin;y:=4+random(11);x:=x-5+random(11);end;
until (abs(oldx-x)>15) and (abs(oldy-y)>6);
if x>74 then x:=74;
if x<1 then x:=1;
if y>14 then y:=14;
if y<4 then y:=4;

{gen corridor}
tx:=oldx+2;
ty:=oldy+2;
rnd:=random(2);
count:=0;
repeat

if (tx<>x+2) and (ty<>y+2) then
if rnd=0 then begin;dx:=1;dy:=0;end
else begin;dx:=0;dy:=1;end;
if tx=x+2 then begin;dy:=1;dx:=0;end;
if ty=y+2 then begin;dx:=1;dy:=0;end;

if tx>x+2 then dx:=-1*dx;
if tx<x+2 then dx:=dx;
if ty>y+2 then dy:=-1*dy;
if ty<y+2 then dy:=dy;

if count<121 then
{1}if dx=0 then
begin;
if (level[tx,ty+dy]=1) and (level[tx+1,ty]=2) then
begin;dx:=1;dy:=0;count:=count+1;end
else if (level[tx,ty+dy]=1) and (level[tx-1,ty]=2) then
begin;dx:=-1;dy:=0;count:=count+1;end;end
{2}else if dy=0 then
begin;
if (level[tx+dx,ty]=1) and (level[tx,ty+1]=2) then
begin;dy:=1;dx:=0;count:=count+1;end
else if (level[tx+dx,ty]=1) and (level[tx,ty-1]=2) then
begin;dy:=-1;dx:=0;count:=count+1;end;end;


tx:=tx+dx;
ty:=ty+dy;


if (tx>79) then tx:=79;
if (tx<2) then tx:=2;
if (ty>22) then ty:=22;
if (ty<5) then ty:=5;

level[tx,ty]:=2;

if level[tx+1,ty+1]<>2 then level[tx+1,ty+1]:=1;
if level[tx  ,ty+1]<>2 then level[tx  ,ty+1]:=1;
if level[tx-1,ty+1]<>2 then level[tx-1,ty+1]:=1;
if level[tx-1,  ty]<>2 then level[tx-1,  ty]:=1;
if level[tx-1,ty-1]<>2 then level[tx-1,ty-1]:=1;
if level[tx  ,ty-1]<>2 then level[tx  ,ty-1]:=1;
if level[tx+1,ty-1]<>2 then level[tx+1,ty-1]:=1;
if level[tx+1,  ty]<>2 then level[tx+1,  ty]:=1;
{end gen corridor}

{sysdrawwall(level,tx,ty);}
until (tx=x+2) and (ty=y+2)

until i=nkomnat;

{gen exit and entrance}
exx:=oldx+2;
exy:=oldy+2;
level[enx,eny]:=-1;
level[exx,exy]:=-2;
end;
{==================================================}
{<сделано на глобальную переменную>}
procedure genmoney;
var sten,chan,i,ix,iy:integer;
begin;
for iy:=5 to 22 do
begin;
for ix:=2 to 79 do
begin;

sten:=-1;
if allevel[ix,iy,1]=2 then sten:=innear(1,1,ix,iy);
if (ix=1) or (ix=80) or (iy=4) or (iy=23) or (innear(2,0,ix,iy)<4) then sten:=-1;

if sten>-1 then
begin;
chan:=0;
for i:=1 to (sten div 2+1) do chan:=chan-1+random(5)+random(sten div 6+1);
if (1+random(100)<chan) then allevel[ix,iy,2]:=random(chan*2+1)+random(sten*5)+random(15)+1;
end;
end;
end;
end;
{==================================================}
procedure endall(gold:longint);
begin;
clrscr;
repeat
textcolor(random(16));
delay(1*dlay);
gotoxy(1,1);
write('Your score: ',gold);
until keypressed;
end;
{==================================================}
procedure skazatbred;
begin;
txts[11]:='Вдалеке слышны странные звуки, будто Тима нажрался горохового супа и пердит';
txts[12]:='Вы слышите чью-то речь: "У-у-у... Кеша!" "Да ты на себя посмотри, гнездо!"';
txts[13]:='Вы слышите чью-то речь: "Тима-Зубан" "Ща как дам в глаз, сам зубаном станешь!"';
txts[14]:='Вы слышите чей-то храп. Похоже Таха опять нажрался кошек и дрыхнет';
txts[15]:='Издалека слышны голова и дикое щелканье. Похоже Zloy и Loky играют дуэль';
txts[16]:='Вы спотыкаетесь на камне и вляпываетесь лицом в кучу дерьма. Здесь явно был Таха';
txts[17]:='Вы слышите чью-то речь: "Ну че, Гнилст, мозги-то совсем прогнили?"';
txts[18]:='Внезапно вам в лицо пахнуло говном. Нет, Таха однозначно был здесь!';
txts[19]:='Вдалеке слышны странные звуки, будто дрист опять вляпался в дерьмо и дико матерится';
txts[20]:='"Считаю до трех! Раз-два-три! Ага...ты, играющий в поиски золота, давай быстро стирай с доски!"';
end;
{==================================================}
procedure welcome;
var s:string;
    i:integer;
begin;
clrscr;
delay(10*dlay);
textcolor(8);
s:='Поиски';
for i:=1 to (length(s) div 2+length(s) mod 2) do
begin;
gotoxy(37+i,5);
write(s[i]);
gotoxy(37+length(s)-i+1,5);
write(s[length(s)-i+1]);
delay(13*dlay);
end;

textcolor(14);
for i:=1 to 40 do
begin;
gotoxy(i,6);
write('$');
gotoxy(80-i+1,6);
write('$');
gotoxy(i-1,6);
write(' ');
gotoxy(80-i+2,6);
write(' ');
delay(5*dlay);
end;

s:='Золота';
for i:=(length(s) div 2+length(s) mod 2) downto 1 do
begin;
gotoxy(37+i,6);
write(s[i]);
gotoxy(37+length(s)-i+1,6);
write(s[length(s)-i+1]);
delay(5*dlay);
end;

textcolor(7);
s:='В подвале коттеджа дяди Вани';
for i:=1 to (length(s) div 2+length(s) mod 2) do
begin;
gotoxy(26+i,7);
write(s[i]);
gotoxy(26+length(s)-i+1,7);
write(s[length(s)-i+1]);
delay(5*dlay);
end;
gotoxy(1,25);
textcolor(15);
write('Press ANY key');
readkey;
end;
{==================================================}
{<в целях неизвестно чего на глобальную переменную не переделано>}
procedure drawlev(level:allevarr);
var i,i1:integer;
begin;
for i:=1 to 80 do
begin;
for i1:=4 to 23 do
begin;
if level[i,i1,3]=2 then
begin;
if level[i,i1,1]=1 then begin;textcolor(8);gotoxy(i,i1);write('#');end;
if level[i,i1,1]=2 then begin;textcolor(7);gotoxy(i,i1);write('.');end;
if level[i,i1,1]=-1 then begin;textcolor(8);gotoxy(i,i1);write('>');end;
if level[i,i1,1]=-2 then begin;textcolor(14);gotoxy(i,i1);write('<');end;
if level[i,i1,2]>0 then begin;textcolor(14);gotoxy(i,i1);write('$');end;
end;
end;
end;
end;
{==================================================}
{<сделано на глобальную переменную>}
procedure syscopy(lev:levarr;n:integer);
var i,i1:integer;
begin;
for i:=1 to 80 do
begin;
for i1:=4 to 23 do
begin;
allevel[i,i1,n]:=lev[i,i1];
end;
end;
end;
{==================================================}
{<сделано на глобальную переменную>}
procedure letmeseeit(x,y:integer);
var ix,iy,cx,cy,gox,goy,sten,sten1:integer;
begin;
for ix:=1 to 80 do
begin;
for iy:=4 to 23 do
begin;
if allevel[ix,iy,3]=2 then allevel[ix,iy,3]:=1;
end;
end;

for ix:=x-5 to x+5 do
begin;
for iy:=y-4 to y+4 do
begin;
if not((abs(ix-x)+abs(iy-y)>6))and(ix>0)and(ix<81)and(iy>3)and(iy<24)then allevel[ix,iy,3]:=2;
end;
end;

for ix:=x-5 to x+5 do
begin;
for iy:=y-4 to y+4 do
begin;
if (ix>0)and(ix<81)and(iy>3)and(iy<24)then
begin;

{путь 1}
gox:=ix;goy:=iy;cx:=x;cy:=y;sten:=0;
repeat
if cx<gox then cx:=cx+1;if cx>gox then cx:=cx-1;
if cy<goy then cy:=cy+1;if cy>goy then cy:=cy-1;
if (allevel[cx,cy,1]=1)and(not((cx=gox)and(cy=goy)))then sten:=1;
until (sten=1)or((cx=gox)and(cy=goy));

{путь 2}
gox:=ix;goy:=iy;cx:=x;cy:=y;sten1:=0;
repeat
if (abs(cx-gox)-abs(cy-goy))=0 then
begin;
if cx<gox then cx:=cx+1;if cx>gox then cx:=cx-1;
if cy<goy then cy:=cy+1;if cy>goy then cy:=cy-1;
end
else
begin;
if (abs(cx-gox)-abs(cy-goy))<0 then
begin;if cy<goy then cy:=cy+1;if cy>goy then cy:=cy-1;end;
if (abs(cx-gox)-abs(cy-goy))>0 then
begin;if cx<gox then cx:=cx+1;if cx>gox then cx:=cx-1;end;
end;
if (allevel[cx,cy,1]=1)and(not((cx=gox)and(cy=goy)))then sten1:=1;
until (sten1=1)or((cx=gox)and(cy=goy));

if (sten+sten1)>1 then allevel[ix,iy,3]:=0;
end;
end;
end;
end;
{==================================================}
{<В известных одному ведущему программеру целях>}
{<на глобальную переменную не переделано>}
procedure nullall(var lev:allevarr;var echos:allechoarr);
var ix,iy:integer;
begin;
for ix:=1 to 80 do
begin;
for iy:=4 to 23 do
begin;
lev[ix,iy,1]:=0;
lev[ix,iy,2]:=0;
lev[ix,iy,3]:=0;
end;
end;
for ix:=1 to 25 do
begin;
echos[ix]:=txts[0];
end;
end;
{==================================================}
procedure inittext;
var f:text;
    i:integer;
begin;
i:=-1;
assign(f,'alltexts.txt');
reset(f);
repeat
i:=i+1;
readln(f,txts[i]);
until eof(f);
close(f);
end;
{==================================================}
procedure addecho(st:string;var nofecho:integer);
var n,i:integer;
begin;
nofecho:=25;

repeat
for i:=2 to 25 do
begin;
echos[i-1]:=echos[i];
end;

if length(st)>80 then begin;n:=80;
repeat
n:=n-1;until (n=0)or(st[n]=' ');
if n>0 then begin;echos[25]:=copy(st,1,n);delete(st,1,n);end
else
begin;echos[25]:=copy(st,1,80);delete(st,1,80);end;end
else begin;echos[25]:=copy(st,1,length(st));st:='';end;

until length(st)=0;
end;
{==================================================}
procedure writeecho(var n:integer);
begin;textcolor(7);if n<3 then n:=3;if n>25 then n:=25;
gotoxy(1,1);write(txts[0]);gotoxy(1,1);write(echos[n-2]);
gotoxy(1,2);write(txts[0]);gotoxy(1,2);write(echos[n-1]);
gotoxy(1,3);write(txts[0]);gotoxy(1,3);write(echos[n]);end;
{==================================================}
var   steni:levarr;
      nofecho,i,i1,px,py:integer;
      gold:longint;
      c:char;
      hlpstrok:string;
begin
randomize;
welcome;
clrscr;
gold:=0;
nofecho:=25;
inittext;

nullall(allevel,echos);
generate(steni,px,py);
syscopy(steni,1);
genmoney;
skazatbred;

repeat
if random(10)=1 then addecho(txts[11+random(10)],nofecho);
letmeseeit(px,py);
drawlev(allevel);
gotoxy(px,py);textcolor(7);write('@');
writeecho(nofecho);
c:=readkey;

if (allevel[px-1,py-1,1]=1) and (c='7') then addecho(txts[1],nofecho);
if (allevel[px  ,py-1,1]=1) and (c='8') then addecho(txts[1],nofecho);
if (allevel[px+1,py-1,1]=1) and (c='9') then addecho(txts[1],nofecho);
if (allevel[px+1,py  ,1]=1) and (c='6') then addecho(txts[1],nofecho);
if (allevel[px+1,py+1,1]=1) and (c='3') then addecho(txts[1],nofecho);
if (allevel[px  ,py+1,1]=1) and (c='2') then addecho(txts[1],nofecho);
if (allevel[px-1,py+1,1]=1) and (c='1') then addecho(txts[1],nofecho);
if (allevel[px-1,py  ,1]=1) and (c='4') then addecho(txts[1],nofecho);

if ((allevel[px-1,py-1,1]=2) or (allevel[px-1,py-1,1]<0)) and (c='7') then begin;px:=px-1;py:=py-1;end;
if ((allevel[px  ,py-1,1]=2) or (allevel[px  ,py-1,1]<0)) and (c='8') then begin;py:=py-1;         end;
if ((allevel[px+1,py-1,1]=2) or (allevel[px+1,py-1,1]<0)) and (c='9') then begin;px:=px+1;py:=py-1;end;
if ((allevel[px+1,py  ,1]=2) or (allevel[px+1,py  ,1]<0)) and (c='6') then begin;px:=px+1;         end;
if ((allevel[px+1,py+1,1]=2) or (allevel[px+1,py+1,1]<0)) and (c='3') then begin;px:=px+1;py:=py+1;end;
if ((allevel[px  ,py+1,1]=2) or (allevel[px  ,py+1,1]<0)) and (c='2') then begin;py:=py+1;         end;
if ((allevel[px-1,py+1,1]=2) or (allevel[px-1,py+1,1]<0)) and (c='1') then begin;px:=px-1;py:=py+1;end;
if ((allevel[px-1,py  ,1]=2) or (allevel[px-1,py  ,1]<0)) and (c='4') then begin;px:=px-1;         end;

if c='5' then
begin;
if allevel[px,py,2]>0 then
begin;
str(allevel[px,py,2],hlpstrok);addecho(txts[2]+' '+hlpstrok+' '+txts[4],nofecho);
gold:=gold+allevel[px,py,2];allevel[px,py,2]:=0;
end;
if allevel[px,py,1]=-2 then begin;endall(gold);c:=#27;end;
end;

if c=']' then nofecho:=nofecho+1;
if c='[' then nofecho:=nofecho-1;
until c=#27

end.








