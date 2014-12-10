unit useful;

interface
{==================================================}
type cubic=record
       a:shortint;
       b:shortint;
       c:shortint;
     end;
{==================================================}
type harka=record
       unm:shortint;
       max:shortint;
       mmd:shortint;
       nmd:shortint;
       now:shortint;
     end;
type baseharka=record
       now:integer;
       max:integer;
     end;
type character=record
       st:harka;
       dex:harka;
       con:harka;
       per:harka;
       int:harka;
       wis:harka;
       cha:harka;
       lck:harka;
       hp:baseharka;
       mp:baseharka;
       att:integer;
       uvert:integer;
       defen:integer;
       pu:integer;
       dmg:cubic;
       kdmg:cubic;
       race:shortint;
       prof:shortint;
       sex:shortint;
       month:shortint;
       day:shortint;
       name:string;
     end;
{==================================================}
type timrec=record
       s:shortint;
       m:shortint;
       h:shortint;
       d:shortint;
       g:shortint;
       y:integer;
     end;
{==================================================}

procedure changeint(var ch1,ch2:integer);
procedure changestr(var ch1,ch2:string);
function  makestr(ch:integer):string;
function  makeint(st:string):integer;
procedure curoff(b:boolean);
procedure curon(b:boolean);
procedure wrt(x,y,c:shortint;s:string);

implementation
uses dos,crt;
{==================================================}
procedure wrt(x,y,c:shortint;s:string);
begin;
if (c>=0)and(c<=15)then textcolor(c);
gotoxy(x,y);write(s);end;
{==================================================}
procedure curoff(b:boolean);
var r:registers;
begin;
if b=false then
begin;
r.ax:=$0100;
r.cx:=$2607;
intr($10,r);
end;
end;
{==================================================}
procedure curon(b:boolean);
var r:registers;
begin;
r.ax:=$0100;
r.cx:=$0506;
intr($10,r);
end;
{==================================================}
procedure changeint(var ch1,ch2:integer);
var hlp:integer;
begin;
hlp:=ch1;ch1:=ch2;ch2:=hlp;
end;
{==================================================}
procedure changestr(var ch1,ch2:string);
var hlp:string;
begin;
hlp:=ch1;ch1:=ch2;ch2:=hlp;
end;
{==================================================}
function makestr(ch:integer):string;
var st:string;
begin;
str(ch,st);makestr:=st;
end;
{==================================================}
function makeint(st:string):integer;
var ch,o:integer;
begin;
val(st,ch,o);
if o=0 then makeint:=ch
else makeint:=0;
end;
{==================================================}
begin;
end.

