unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Edit3: TEdit;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Label3: TLabel;
    RadioButton3: TRadioButton;
    Label4: TLabel;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    CheckBox1: TCheckBox;
    RadioButton6: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  a:array[1..300000] of longint;
  smallpr:set of byte;

implementation

{$R *.DFM}

//============================================================================\\
// Initializing
//============================================================================\\
procedure TForm1.FormCreate(Sender: TObject);
var i,q:longint;
    f:File of Longint;
begin
Form1.Edit1.Text:='Bitmap';
Form1.Edit2.Text:='10';
Form1.Edit3.Text:='7';
Form1.RadioButton1.Checked:=True;
q:=0;

AssignFile(f,'pr.txt');ReSet(f);
Read(f,q);
for i:=1 to q do Read(f,a[i]);
closefile(f);

smallpr:=[];
i:=1;
repeat
smallpr:=smallpr+[a[i]];
inc(i);
until a[i]>=255;

{Assignfile(f,'1.txt');rewrite(f);
for i:=2 to 2000000 do
  begin;
  pr:=True;
  for i1:=1 to q do
    if (i mod a[i1]=0) then
      begin;
      pr:=false;
      break;
      end;
  if pr then
    begin;
    inc(q);
    a[q]:=i;
    Writeln(f,a[q]);
    end;
  end;
closefile(f);}
end;


//============================================================================\\
// Generating Image
//============================================================================\\
procedure TForm1.Button1Click(Sender: TObject);
var mods:longint;
    q:Longint;
    cBLACK,cWHITE:TColor;

function IsPalindrom(k:Longint):Boolean;
var s:string;
    i:integer;
begin;
Result:=True;
s:=IntToStr(k);
for i:=1 to Length(s) div 2 do if s[i]<>s[Length(s)-i+1] then begin;Result:=False;exit;end;
end;

function sum(k:Longint):Byte;
const change:array['0'..'9'] of byte=(0,1,2,3,4,5,6,7,8,9);
var s:string;
    i:integer;
begin;
Result:=0;
s:=IntToStr(k);
for i:=1 to Length(s) do
  Result:=Result+change[s[i]];

end;

function Udovl(k:Longint):Boolean;
begin;
Result:=True;
if Form1.RadioButton1.Checked then
  begin;
  if k>a[q] then inc(q);
  if k=a[q] then Result:=True
  else Result:=False;
  exit;
  end;
if Form1.RadioButton2.Checked then
  begin;
  if k mod mods=0 then Result:=True
  else Result:=False;
  exit;
  end;
if Form1.RadioButton3.Checked then
  begin;
  if (Sum(k) in smallpr) then Result:=True
  else Result:=False;
  exit;
  end;
if Form1.RadioButton4.Checked then
  begin;
  if (Sum(k) mod mods=0) then Result:=True
  else Result:=False;
  exit;
  end;
if Form1.RadioButton5.Checked then
  begin;
  if (Sum(k) >= mods) then Result:=True
  else Result:=False;
  exit;
  end;
if Form1.RadioButton6.Checked then
  begin;
  if IsPalindrom(k) then Result:=True
  else Result:=False;
  exit;
  end;
end;


var n:longint;
    Pict:TBitmap;
    i,k,x,y:integer;
begin
q:=1;

try
  n:=StrToInt(Form1.Edit2.Text);
except
  on E:Exception do
    begin;
    Application.MessageBox(PChar('Количество витков спирали - Это число, а не та фигня, которую вы написали!'),PChar('!'),MB_OK);
    exit;
    end;
end;

if (Form1.RadioButton2.Checked)or(Form1.RadioButton4.Checked)or(Form1.RadioButton5.Checked) then
  begin;
  try
    mods:=StrToInt(Form1.Edit3.Text);
  except
    on E:Exception do
      begin;
      Application.MessageBox(PChar('N - Это число, а не та фигня, которую вы написали!'),PChar('!'),MB_OK);
      exit;
      end;
  end;
  end;

if Form1.CheckBox1.Checked then
  begin;
  cBLACK:=clBLACK;
  cWHITE:=CLWHITE;
  end
else
  begin;
  cBLACK:=CLWHITE;
  cWHITE:=clBLACK;
  end;
Form1.ProgressBar1.Max:=n;
Pict:=TBitmap.Create;
Pict.Width:=1+2*n;
Pict.Height:=1+2*n;

for x:=0 to Pict.Width-1 do
  for y:=0 to Pict.Height-1 do
    Pict.Canvas.Pixels[x,y]:=cWHITE;

k:=0;
Pict.Canvas.Pixels[n,n]:=clRED;
for i:=1 to n do
  begin;
  Form1.ProgressBar1.Position:=i;

  y:=n-i;
  for x:=(n+1-i) to (n+i-1) do
    begin;
    inc(k);
    if Udovl(k) then Pict.Canvas.Pixels[x,y]:=cWHITE
                else Pict.Canvas.Pixels[x,y]:=cBLACK;
    end;

  x:=n+i;
  for y:=(n-i) to (n+i) do
    begin;
    inc(k);
    if Udovl(k) then Pict.Canvas.Pixels[x,y]:=cWHITE
                else Pict.Canvas.Pixels[x,y]:=cBLACK;
    end;

  y:=n+i;
  for x:=(n+i-1) downto (n+1-i) do
    begin;
    inc(k);
    if Udovl(k) then Pict.Canvas.Pixels[x,y]:=cWHITE
                else Pict.Canvas.Pixels[x,y]:=cBLACK;
    end;

  x:=n-i;
  for y:=(n+i) downto (n-i) do
    begin;
    inc(k);
    if Udovl(k) then Pict.Canvas.Pixels[x,y]:=cWHITE
                else Pict.Canvas.Pixels[x,y]:=cBLACK;
    end;
  end;

try
  Pict.SaveToFile(ExtractFilePath(Application.ExeName)+Form1.Edit1.Text+'.bmp');
except
  on E:Exception do
    begin;
    Application.MessageBox(PChar('У тебя руки кривые??? Ты какое имя файла набрал? Сохраняю в Bitmap.bmp!'),PChar('!'),MB_OK);
    Pict.SaveToFile(ExtractFilePath(Application.ExeName)+'Bitmap.bmp');
    end;
end;

Form1.ProgressBar1.Position:=1;
Application.MessageBox(PChar('Сделано!'),PChar('!'),MB_OK);
end;


end.
