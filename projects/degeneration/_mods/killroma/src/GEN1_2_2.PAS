{$A+,B-,D+,E+,F-,G-,I+,L+,N-,O-,P-,Q+,R+,S+,T-,V+,X+,Y+}
{$M 64000,0,655360}
uses crt,strtmenu,useful,labgen,geninfo;
type allevarr=array[1..80,1..20,1..3] of shortint;
type alltextarr=array[0..20] of string;
type allechoarr=array[0..100] of string;
var allevel:allevarr;
    monster:array[1..50,1..2]of integer;
    txts:alltextarr;
    echos:allechoarr;
    progdir:string;
    h:character;

const dlay=10;
const filname='dat';
const a=80;
      b=20;
{==================================================}
{<не сделано на глобальную переменную (и не надо!)>}
function innear(sloi,n,x,y,rad:integer;lev:allevarr):integer;
var kol,ix,iy:integer;
begin;kol:=0;
for ix:=(x-rad) to (x+rad) do begin;
for iy:=(y-rad) to (y+rad) do begin;
if (iy>0)and(iy<b+1)and(ix>0)and(ix<a+1)and(lev[ix,iy,sloi]=n) then kol:=kol+1;
end;end;innear:=kol;end;
{==================================================}
procedure GenMonster(var level:allevarr);
var i,ix,iy:integer;
begin;
for i:=1 to 50 do
  begin;
  repeat
    ix:=1+random(80);
    iy:=1+random(20);
  until (level[ix,iy,1]=2);
  level[ix,iy,2]:=i;
  monster[i,1]:=ix;
  monster[i,2]:=iy;
  end;
end;

{<не сделано на глобальную переменную>}
{0-нет денег}
{>0-кол-во денег, равное x*10+random(10)}
{procedure genmoney(var level:allevarr);
var sten,chan,i,ix,iy,money:integer;
begin;
for iy:=2 to (b-1) do begin;
for ix:=2 to (a-1) do begin;
sten:=-1;
if level[ix,iy,1]=2 then sten:=innear(1,1,ix,iy,1,level);
if (innear(2,0,ix,iy,3,level)<49) then sten:=-1;
if sten>-1 then
begin;chan:=0;
for i:=1 to (sten div 2+1) do chan:=chan-1+random(5)+random(sten div 6+1);
if (1+random(100)<chan) then begin;money:=random(chan*2+1)+random(sten*5)+random(15)+1;
money:=money div 10;if money>127 then money:=127;level[ix,iy,2]:=money;end;
end;end;end;end;}
{==================================================}
{<в целях неизвестно чего на глобальную переменную не переделано>}
procedure drawlev;
var i,i1:integer;
begin;
for i:=1 to a do
begin;
for i1:=1 to b do
begin;
if allevel[i,i1,3]=2 then
begin;
if allevel[i,i1,1]=1 then begin;textcolor(8);gotoxy(i,i1+3);write('#');end;
if allevel[i,i1,1]=2 then begin;textcolor(7);gotoxy(i,i1+3);write('.');end;
if allevel[i,i1,1]=3 then begin;textcolor(8);gotoxy(i,i1+3);write('>');end;
if allevel[i,i1,1]=4 then begin;textcolor(8);gotoxy(i,i1+3);write('<');end;
if allevel[i,i1,1]=5 then begin;textcolor(6);gotoxy(i,i1+3);write('/');end;
if allevel[i,i1,1]=6 then begin;textcolor(6);gotoxy(i,i1+3);write('+');end;
if allevel[i,i1,1]=7 then begin;textcolor(10);gotoxy(i,i1+3);write('"');end;
if allevel[i,i1,1]=8 then begin;textcolor(2);gotoxy(i,i1+3);write('&');end;
if allevel[i,i1,1]=9 then begin;textcolor(6);gotoxy(i,i1+3);write('.');end;
if allevel[i,i1,1]=10 then begin;textcolor(6);gotoxy(i,i1+3);write('~');end;
if allevel[i,i1,1]=11 then begin;textcolor(2);gotoxy(i,i1+3);write('"');end;
if allevel[i,i1,1]=12 then begin;textcolor(1);gotoxy(i,i1+3);write('=');end;
if allevel[i,i1,1]=13 then begin;textcolor(8);gotoxy(i,i1+3);write('^');end;
if allevel[i,i1,1]=14 then begin;textcolor(15);gotoxy(i,i1+3);write('^');end;
if allevel[i,i1,1]=15 then begin;textcolor(8);gotoxy(i,i1+3);write('*');end;
if allevel[i,i1,2]>0 then begin;textcolor(4);gotoxy(i,i1+3);write('R');end;
end;
end;
end;
end;
{==================================================}
{<сделано на глобальную переменную>}
procedure syscopy(lev:levarr;n:integer);
var i,i1:integer;
begin;for i:=1 to a do begin;for i1:=1 to b do begin;
allevel[i,i1,n]:=lev[i,i1];end;end;end;
{==================================================}
{<сделано на глобальную переменную>}
procedure letmeseeit(x,y:integer);
var ix,iy,cx,cy,gox,goy,sten,sten1,sten2:integer;
begin;
for ix:=1 to a do
begin;
for iy:=1 to b do
begin;
if allevel[ix,iy,3]=2 then allevel[ix,iy,3]:=1;
end;
end;

for ix:=x-5 to x+5 do
begin;
for iy:=y-4 to y+4 do
begin;
if not((abs(ix-x)+abs(iy-y)>6))and(ix>0)and(ix<a+1)and(iy>0)and(iy<b+1)then allevel[ix,iy,3]:=2;
end;
end;

for ix:=x-5 to x+5 do
begin;
for iy:=y-4 to y+4 do
begin;
if (ix>0)and(ix<a+1)and(iy>0)and(iy<b+1)then
begin;

{путь 1}
gox:=ix;goy:=iy;cx:=x;cy:=y;sten:=0;
repeat
if cx<gox then cx:=cx+1;if cx>gox then cx:=cx-1;
if cy<goy then cy:=cy+1;if cy>goy then cy:=cy-1;
if ((allevel[cx,cy,1]=1)or(allevel[cx,cy,1]=6))and(not((cx=gox)and(cy=goy)))then sten:=1;
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
if ((allevel[cx,cy,1]=1)or(allevel[cx,cy,1]=6))and(not((cx=gox)and(cy=goy)))then sten1:=1;
until (sten1=1)or((cx=gox)and(cy=goy));

if (sten+sten1)>1 then allevel[ix,iy,3]:=0;
end;
end;
end;
end;
{==================================================}
procedure nullallechos(var echos:allechoarr);
var ix,iy:integer;
begin;for ix:=1 to 100 do begin;echos[ix]:=txts[0];end;end;
{==================================================}
procedure inittext;
var f:text;
    i:integer;
begin;
getdir(0,progdir);progdir:=progdir+'/';
i:=-1;assign(f,progdir+'gamedata\alltexts.'+filname);reset(f);
repeat i:=i+1;readln(f,txts[i]);until eof(f);close(f);end;
{==================================================}
{typeecho=0-это просто сообщение,оно не остается в массиве и держится}
{ровно один ход, чаще всего не несет важной информации}
{typeecho=1-сообщение остается в массиве, оно важно}
procedure addecho(st:string;var nofecho:integer;typeecho:integer);
var n,i:integer;
begin;
nofecho:=100;
if typeecho=1 then
begin;
echos[0]:='';
repeat
for i:=2 to 100 do
begin;
echos[i-1]:=echos[i];
end;
if length(st)>80 then begin;n:=80;
repeat
n:=n-1;until (n=0)or(st[n]=' ');
if n>0 then begin;echos[100]:=copy(st,1,n);delete(st,1,n);end
else
begin;echos[100]:=copy(st,1,80);delete(st,1,80);end;end
else begin;echos[100]:=copy(st,1,length(st));st:='';end;
until length(st)=0;
end;
if typeecho=0 then echos[0]:=st;
end;
{==================================================}
procedure writeecho(var n:integer);
begin;textcolor(7);if n<3 then n:=3;if n>100 then n:=100;
if echos[0]='' then begin;
gotoxy(1,1);write(txts[0]);gotoxy(1,1);write(echos[n-2]);
gotoxy(1,2);write(txts[0]);gotoxy(1,2);write(echos[n-1]);
gotoxy(1,3);write(txts[0]);gotoxy(1,3);write(echos[n]);end
else if echos[0]='[]' then begin;
gotoxy(1,3);write(txts[0]);
gotoxy(1,1);write(txts[0]);gotoxy(1,1);write(echos[n-1]);
gotoxy(1,2);write(txts[0]);gotoxy(1,2);write(echos[n]);end
else begin;
gotoxy(1,3);write(txts[0]);gotoxy(1,3);write(echos[0]);echos[0]:='[]';
gotoxy(1,1);write(txts[0]);gotoxy(1,1);write(echos[n-1]);
gotoxy(1,2);write(txts[0]);gotoxy(1,2);write(echos[n]);end
end;
{==================================================}
function yesnoquest(quest:string;var nofecho:integer):boolean;
var c:char;
begin;
addecho(quest,nofecho,0);writeecho(nofecho);
repeat
c:=readkey;
until (c='y')or(c='Y')or(c='n')or(c='N');
if (c='y')or(c='Y') then yesnoquest:=true;
if (c='n')or(c='N') then yesnoquest:=false;
end;
{==================================================}
procedure directionsel(var dirx,diry,nofecho:integer);
var c:char;
begin;
addecho('Выберите направление [1234567890]',nofecho,0);writeecho(nofecho);
repeat c:=readkey;if c=#0 then c:=readkey;
until (c='1')or(c='2')or(c=#80)or(c='3')or(c='4')or(c=#75)or(c='6')or(c=#77)or(c='7')or(c='8')or(c=#72)or(c='9');
if (c='1')          then begin;dirx:=-1;diry:= 1;end;
if (c='2')or(c=#80) then begin;dirx:= 0;diry:= 1;end;
if (c='3')          then begin;dirx:= 1;diry:= 1;end;
if (c='4')or(c=#75) then begin;dirx:=-1;diry:= 0;end;
if (c='6')or(c=#77) then begin;dirx:= 1;diry:= 0;end;
if (c='7')          then begin;dirx:=-1;diry:=-1;end;
if (c='8')or(c=#72) then begin;dirx:= 0;diry:=-1;end;
if (c='9')          then begin;dirx:= 1;diry:=-1;end;
end;
{==================================================}
procedure drawinterface(h:character;time:timrec;gold:longint);
begin;
textcolor(7);
gotoxy(1,24);write('Зд:',h.hp.now,'/',h.hp.max,'  ');
gotoxy(1,25);write('Ма:',h.mp.now,'/',h.mp.max,'  ');
gotoxy(15,24);write('Защита:(',h.uvert,'/',h.defen,')[',h.pu,']');
gotoxy(15,25);write(' Атака:+',h.att,'/',h.dmg.a,'d',h.dmg.b,'+',h.dmg.c);
gotoxy(35,24);write('Ромычей убито:',gold,'  ');
gotoxy(35,25);write('Время:        ');
gotoxy(35,25);write('Время:',time.h,':',time.m,':',time.s,'  ');
end;
{==================================================}
procedure whatisthetime(hodi:longint;var time:timrec);
begin;
time.s:=(hodi mod 10)*6;hodi:=hodi div 10;
time.m:=hodi mod 60;hodi:=hodi div 60;
time.h:=12+hodi mod 24;hodi:=hodi div 24;
time.d:=16+hodi mod 28;hodi:=hodi div 28;
time.g:=7+hodi mod 12;hodi:=hodi div 12;
time.y:=1117+hodi;
if time.s>59 then begin;time.s:=time.s-60;time.m:=time.m+1;end;
if time.m>59 then begin;time.m:=time.m-60;time.h:=time.h+1;end;
if time.h>23 then begin;time.h:=time.h-24;time.d:=time.d+1;end;
if time.d>27 then begin;time.d:=time.d-28;time.g:=time.g+1;end;
if time.g>12 then begin;time.g:=time.g-12;time.y:=time.y+1;end;
end;
{==================================================}
procedure showtime(time:timrec;var nofecho:integer);
var mess:string;
begin;
addecho('Сегодня '+makestr(time.d)+'-й день месяца '+months[time.g]+' '+makestr(time.y)+'-го года',nofecho,0);
end;
{==================================================}
var dx,dy,dirx,diry,nofecho,px,py,dpx,dpy:integer;
{st}hodi,gold:longint;
{an}c:char;
{da}name:string;
{rt}i,i1,glub:integer;
{  }enx,eny,exx,exy:integer;
    hod:boolean;
    time:timrec;
    levarrf:file of levarr;

var   steni:levarr;
{help}hlpint:integer;
begin
loadingdata(dlay);
randomize;
inittext;
getdirname(progdir,filname);
initoptions;
splashscreen(dlay);
repeat
gold:=0;dpx:=0;dpy:=0;nofecho:=25;hodi:=0;
time.s:=0;time.m:=0;time.h:=12;time.d:=16;time.g:=7;time.y:=1117;
glub:=1;
fillchar(allevel,sizeof(allevel),0);
nullallechos(echos);
gendung(steni,enx,eny,exx,exy,10,15);
px:=enx;py:=eny;
syscopy(steni,1);
genmonster(allevel);


startmenu(h,gold,hodi);
whatisthetime(hodi,time);
clrscr;

repeat
{if random(100)=0 then
  begin;
  if allevel[px-1,py-1,1]in [2,3,4,5] then allevel[px-1,py-1,1]
  end;}

letmeseeit(px,py);drawlev;
drawinterface(h,time,gold);
gotoxy(px,py+3);textcolor(7);write('@');
writeecho(nofecho);gotoxy(px,py+3);
repeat
c:=readkey;if c=#0 then c:=readkey;
until c in ['1','2','3','4','6','7','8','9',#27,#80,#75,#77,#72,'c','<','>','[',']','t'];
hod:=false;dpx:=0;dpy:=0;


if (c='1')          then begin;dpx:=-1;dpy:= 1;end;
if (c='2')or(c=#80) then begin;dpx:= 0;dpy:= 1;end;
if (c='3')          then begin;dpx:= 1;dpy:= 1;end;
if (c='4')or(c=#75) then begin;dpx:=-1;dpy:= 0;end;
if (c='6')or(c=#77) then begin;dpx:= 1;dpy:= 0;end;
if (c='7')          then begin;dpx:=-1;dpy:=-1;end;
if (c='8')or(c=#72) then begin;dpx:= 0;dpy:=-1;end;
if (c='9')          then begin;dpx:= 1;dpy:=-1;end;

if pos(c,'0123456789')<>0 then
  begin;
  if allevel[px+dpx,py+dpy,1]=1 then
    begin;
    addecho(txts[1],nofecho,0);
    dpx:=0;
    dpy:=0;
    end
  else if (allevel[px+dpx,py+dpy,1] in [2,3,4,5]) then
    begin;
    if allevel[px+dpx,py+dpy,2]>0 then
      begin;
      monster[allevel[px+dpx,py+dpy,2],1]:=0;
      monster[allevel[px+dpx,py+dpy,2],2]:=0;
      allevel[px+dpx,py+dpy,2]:=0;
      case random(3) of
        0:addecho('Вы зверски расчленили одного из Ромычей.',nofecho,1);
        1:addecho('Быстрый удар в нос вырубает Ромыча мгновенно.',nofecho,1);
        2:addecho('Несколькими ударами меча вы отрубаете Ромычу все конечности.',nofecho,1);
      end;
      inc(gold);
      hod:=true;
      end
    else
      begin;
      px:=px+dpx;
      py:=py+dpy;
      dpx:=0;
      dpy:=0;
      hod:=true;
      end;
    end;
  end;

if allevel[px,py,1]=3 then addecho(txts[5],nofecho,0);
if allevel[px,py,1]=4 then addecho(txts[6],nofecho,0);
{if allevel[px,py,2]>0 then addecho(txts[3]+'останки '+txts[4],nofecho,0);}

if allevel[px+dpx,py+dpy,1]=6 then
begin;
if yesnoquest(txts[9],nofecho) then begin;allevel[px+dpx,py+dpy,1]:=5;addecho(txts[10],nofecho,0);dpx:=0;dpy:=0;hod:=true;end;
end;

if c='c' then
begin;
directionsel(dirx,diry,nofecho);
if allevel[px+dirx,py+diry,1]=5 then begin;allevel[px+dirx,py+diry,1]:=6;addecho(txts[11],nofecho,0);dpx:=0;dpy:=0;
hod:=true;end
else addecho(txts[12],nofecho,0);
end;

{if c=',' then
begin;
if allevel[px,py,2]>0 then
begin;
hlpint:=allevel[px,py,2]*10+random(10);
addecho(txts[2]+' '+makestr(hlpint)+' '+txts[4],nofecho,1);
gold:=gold+hlpint;allevel[px,py,2]:=0;
hod:=true;
end
else addecho(txts[13],nofecho,0);
end;}

if c='<' then
begin;
if allevel[px,py,1]=4 then begin;
dec(glub);
if glub>0 then begin;
fillchar(allevel,sizeof(allevel),0);
gendung(steni,enx,eny,exx,exy,10,15);
px:=enx;py:=eny;addecho(txts[0],nofecho,0);
syscopy(steni,1);
genmonster(allevel);
clrscr;
hod:=true;
end
else begin;fillchar(allevel,sizeof(allevel),0);
assign(levarrf,progdir+'gamedata/wild.'+filname);reset(levarrf);read(levarrf,steni);close(levarrf);
syscopy(steni,1);px:=3;py:=3;clrscr;hod:=true;end;
end
else addecho(txts[8],nofecho,0);
end;

if c='>' then
begin;
if (allevel[px,py,1]=3)or(allevel[px,py,1]=15) then begin;
inc(glub);
fillchar(allevel,sizeof(allevel),0);
genlab(steni,enx,eny,exx,exy);
px:=exx;py:=exy;addecho(txts[0],nofecho,0);
syscopy(steni,1);
genmonster(allevel);
clrscr;
hod:=true;
end
else addecho(txts[7],nofecho,0);
end;

if c=']' then nofecho:=nofecho+1;
if c='[' then nofecho:=nofecho-1;

if c='t' then showtime(time,nofecho);

if hod=true then
  begin;
  hodi:=hodi+1;
  time.s:=time.s+6;
  if time.s>59 then begin;time.s:=time.s-60;time.m:=time.m+1;end;
  if time.m>59 then begin;time.m:=time.m-60;time.h:=time.h+1;end;
  if time.h>23 then begin;time.h:=time.h-24;time.d:=time.d+1;end;
  if time.d>27 then begin;time.d:=time.d-28;time.g:=time.g+1;end;
  if time.g>12 then begin;time.g:=time.g-12;time.y:=time.y+1;end;

  for i:=1 to 50 do
    begin;
    if (allevel[monster[i,1],monster[i,2],2]=0) then
      begin;
      monster[i,1]:=0;
      monster[i,2]:=0;
      end;
    if (monster[i,1]<>0)and(monster[i,2]<>0) then
    begin;
    dx:=0;dy:=0;
    if px<monster[i,1] then dx:=-1;
    if px>monster[i,1] then dx:= 1;
    if py<monster[i,2] then dy:=-1;
    if py>monster[i,2] then dy:= 1;
    if (monster[i,1]+dx=px)and(monster[i,2]+dy=py) then
      begin;

      addecho(txts[14],nofecho,1);
      dec(h.hp.now);
      end;
    if (allevel[monster[i,1]+dx,monster[i,2]+dy,1]in [2,3,4,5]) and (allevel[monster[i,1]+dx,monster[i,2]+dy,2]=0) and
       (not ((monster[i,1]+dx=px)and(monster[i,2]+dy=py))) then
      begin;
      allevel[monster[i,1]+dx,monster[i,2]+dy,2]:=allevel[monster[i,1],monster[i,2],2];
      allevel[monster[i,1],monster[i,2],2]:=0;
      monster[i,1]:=monster[i,1]+dx;
      monster[i,2]:=monster[i,2]+dy;
      end
    end;
    end;
  end;


until (c=#27)or(hodi=5000)or(h.hp.now<=0);

if c=#27 then endall(h,gold,hodi);
if hodi>5000 then begin;clrscr;textcolor(14);gotoxy(1,1);write('Игра закончена');
textcolor(6);gotoxy(1,2);write('Вы исчерпали временной лимит (8 часов 20 минут)');readok(1,3);
endall(h,gold,hodi);
hodi:=0;gold:=0;end;
if h.hp.now<=0 then begin;clrscr;textcolor(14);gotoxy(1,1);write('Игра закончена');
textcolor(6);gotoxy(1,2);write('Вы были зверски растерзаны толпой очумелых Ромычей.');readok(1,3);
hodi:=5000;endall(h,gold,hodi);
hodi:=0;gold:=0;end;

until 1=2;

end.








