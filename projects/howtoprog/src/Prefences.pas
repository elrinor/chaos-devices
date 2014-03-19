unit Prefences;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Mask, MainForm, ExtCtrls, MyColorGrid, ColorGrd, inifiles;

type IntArray0to16=array[0..16] of Integer;

const Colors:array[0..15] of String=('Черный','Темно-синий','Темно-зеленый','Темно-голубой','Кроваво-красный','Темно-малиновый','Коричневый','Серый','Темно-серый','Синий','Зеленый','Голубой','Ярко-красный','Малиновый','Желтый','Белый');
const ColorNames:array[0..16] of String=('Цвет пробела','Цвет комментария','Цвет строки','Цвет идентификатора','Цвет числа','Цвет зарезервированного слова','Цвет символа','Цвет окна редактирования кода','Цвет границы окна редактирования кода','Цвет заголовка окна редактирования кода','Цвет полос прокрутки окна редактирования кода','Цвет полосы изменения размеров окна редактирования кода','Цвет кнопок окна редактирования кода','Цвет окна комментариев','Цвет границы окна комментариев','Цвет заголовка окна комментариев','Цвет текста в окне комментариев');
const IniFileColorNames:array[0..16] of String=('Whitespace_Color','Comment_Color','String_Color','Identifier_Color','Number_Color','Reserved_Word_Color','Symbol_Color','CodeWindow_Back_Color','CodeWindow_Border_Color','CodeWindow_Header_Color','CodeWindow_Scroller_Color','CodeWindow_Sprainer_Color','CodeWindow_Buttons_Color','CommentWindow_Back_Color','CommentWindow_Border_Color','CommentWindow_Header_Color','CommentWindow_Text_Color');
const Defaults:IntArray0to16=(14,7,10,14,10,15,15,1,15,15,3,10,10,0,7,7,7);
const ColorWrap:array[0..15]of integer=(0,4,2,6,1,5,3,7,8,12,10,14,9,13,11,15);
const DefaultDefaultColor=14;

var ColorsAssigned:IntArray0to16;
    DefaultColor:Integer;

type
  TForm2 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    MaskEdit1: TMaskEdit;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    GroupBox1: TGroupBox;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    BitBtn3: TBitBtn;
    ComboBox2: TComboBox;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    MyColorGrid1: TMyColorGrid;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
    procedure ComboBox2Click(Sender: TObject);
    procedure MyColorGrid1Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
//    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

procedure TForm2.FormCreate(Sender: TObject);
var i:Integer;
begin
SaveDialog1.Filter:='Файлы цветовых настроек (*.cls)|*.CLS';
SaveDialog1.DefaultExt:='CLS';
SaveDialog1.FilterIndex:=1;
SaveDialog1.InitialDir:=ExtractFilePath(Application.ExeName);
SaveDialog1.Options:=OpenDialog1.Options+[ofNoChangeDir]-[ofAllowMultiSelect]+[ofOverwritePrompt];

OpenDialog1.Filter:='Файлы цветовых настроек (*.cls)|*.CLS';
OpenDialog1.DefaultExt:='CLS';
OpenDialog1.FilterIndex:=1;
OpenDialog1.InitialDir:=ExtractFilePath(Application.ExeName);
OpenDialog1.Options:=OpenDialog1.Options+[ofNoChangeDir]-[ofAllowMultiSelect]+[ofFileMustExist];
for i:=0 to 16 do Form2.ComboBox1.Items.Insert(i,ColorNames[i]);
for i:=0 to 15 do Form2.ComboBox2.Items.Insert(i,Colors[i]);
for i:=0 to 16 do ColorsAssigned[i]:=OptionsFile.ReadInteger('HowToProg',IniFileColorNames[i],Defaults[i]);
DefaultColor:=OptionsFile.ReadInteger('HowToProg','Default_Text_Color',DefaultDefaultColor);
end;

procedure TForm2.CheckBox2Click(Sender: TObject);
var i:integer;
begin
if Form2.CheckBox2.Checked then
  begin;
  for i:=0 to Form2.ComboBox1.Items.Count do Form2.ComboBox1.Items.Delete(0);
  for i:=0 to 16 do Form2.ComboBox1.Items.Insert(i,ColorNames[i]);
  Form2.ComboBox1.ItemIndex:=0;
  Form2.ComboBox2.ItemIndex:=ColorsAssigned[Form2.ComboBox1.ItemIndex];
  Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[ColorsAssigned[Form2.ComboBox1.ItemIndex]];
  end
else
  begin;
  for i:=0 to Form2.ComboBox1.Items.Count do Form2.ComboBox1.Items.Delete(0);
  Form2.ComboBox1.Items.Insert(0,'Стандартный цвет текста');
  for i:=7 to 16 do Form2.ComboBox1.Items.Insert(i-6,ColorNames[i]);
  Form2.ComboBox1.ItemIndex:=0;
  if Form2.ComboBox1.ItemIndex=0 then
    begin;
    Form2.ComboBox2.ItemIndex:=DefaultColor;
    Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[DefaultColor];
    end
  else
    begin;
    Form2.ComboBox2.ItemIndex:=ColorsAssigned[Form2.ComboBox1.ItemIndex+6];
    Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[ColorsAssigned[Form2.ComboBox1.ItemIndex+6]];
    end;
  end;
end;

procedure TForm2.BitBtn1Click(Sender: TObject);
var i:Integer;
begin
  OptionsFile.WriteInteger('HowToProg','BaseDelayFactor',StrToInt(Form2.MaskEdit1.Text));
  OptionsFile.WriteBool('HowToProg','HomeAllowed'           ,Form2.CheckBox1.Checked);
  OptionsFile.WriteBool('HowToProg','SyntaxHighlight'       ,Form2.CheckBox2.Checked);
  OptionsFile.WriteBool('HowToProg','DosText'               ,Form2.RadioButton2.Checked);
  For i:=0 to 16 do OptionsFile.WriteInteger('HowToProg',IniFileColorNames[i],ColorsAssigned[i]);
  OptionsFile.WriteInteger('HowToProg','Default_Text_Color',DefaultColor);
end;

procedure TForm2.BitBtn3Click(Sender: TObject);
var i:integer;
begin
  Form2.MaskEdit1.Text:='0100';
  Form2.CheckBox1.Checked:=True;
  Form2.CheckBox2.Checked:=True;
  Form2.RadioButton2.Checked:=False;
  Form2.RadioButton1.Checked:=True;
  for i:=0 to Form2.ComboBox1.Items.Count do Form2.ComboBox1.Items.Delete(0);
  for i:=0 to 16 do Form2.ComboBox1.Items.Insert(i,ColorNames[i]);
  DefaultColor:=DefaultDefaultColor;
  ColorsAssigned:=Defaults;
  Form2.ComboBox1.ItemIndex:=0;
  Form2.ComboBox2.ItemIndex:=ColorsAssigned[Form2.ComboBox1.ItemIndex];
  Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[ColorsAssigned[Form2.ComboBox1.ItemIndex]];
end;

procedure TForm2.ComboBox1Click(Sender: TObject);
begin
if (Form2.CheckBox2.Checked) then
  begin;
  Form2.ComboBox2.ItemIndex:=ColorsAssigned[Form2.ComboBox1.ItemIndex];
  Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[ColorsAssigned[Form2.ComboBox1.ItemIndex]];
  end
else if Form2.ComboBox1.ItemIndex=0 then
    begin;
    Form2.ComboBox2.ItemIndex:=DefaultColor;
    Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[DefaultColor];
    end
  else
    begin;
    Form2.ComboBox2.ItemIndex:=ColorsAssigned[Form2.ComboBox1.ItemIndex+6];
    Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[ColorsAssigned[Form2.ComboBox1.ItemIndex+6]];
    end;
end;

procedure TForm2.ComboBox2Click(Sender: TObject);
begin
if (Form2.CheckBox2.Checked) then
  begin;
  ColorsAssigned[Form2.ComboBox1.ItemIndex]:=Form2.ComboBox2.ItemIndex;
  Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[Form2.ComboBox2.ItemIndex];
  end
else if Form2.ComboBox1.ItemIndex=0 then
    begin;
    DefaultColor:=Form2.ComboBox2.ItemIndex;
    Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[DefaultColor];
    end
  else
    begin;
    ColorsAssigned[Form2.ComboBox1.ItemIndex+6]:=Form2.ComboBox2.ItemIndex;
    Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[ColorsAssigned[Form2.ComboBox1.ItemIndex+6]];
    end;
end;

procedure TForm2.MyColorGrid1Click(Sender: TObject);
begin
Form2.ComboBox2.ItemIndex:=ColorWrap[Form2.MyColorGrid1.ForegroundIndex];
if (Form2.CheckBox2.Checked) then
  begin;
  ColorsAssigned[Form2.ComboBox1.ItemIndex]:=Form2.ComboBox2.ItemIndex;
  Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[Form2.ComboBox2.ItemIndex];
  end
else if Form2.ComboBox1.ItemIndex=0 then
    begin;
    DefaultColor:=Form2.ComboBox2.ItemIndex;
    Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[DefaultColor];
    end
  else
    begin;
    ColorsAssigned[Form2.ComboBox1.ItemIndex+6]:=Form2.ComboBox2.ItemIndex;
    Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[ColorsAssigned[Form2.ComboBox1.ItemIndex+6]];
    end;
end;

procedure TForm2.BitBtn4Click(Sender: TObject);
var OptionsFile:TIniFile;
    f:TextFile;
begin
if SaveDialog1.Execute then
  begin;
  AssignFile(f,SaveDialog1.Filename);
  ReWrite(f);
  CloseFile(f);

  OptionsFile:=TIniFile.Create(SaveDialog1.Filename);
  OptionsFile.UpdateFile;

  OptionsFile.WriteInteger('HowToProg','Whitespace_Color',          ColorsAssigned[0]);
  OptionsFile.WriteInteger('HowToProg','Comment_Color',             ColorsAssigned[1]);
  OptionsFile.WriteInteger('HowToProg','String_Color',              ColorsAssigned[2]);
  OptionsFile.WriteInteger('HowToProg','Identifier_Color',          ColorsAssigned[3]);
  OptionsFile.WriteInteger('HowToProg','Number_Color',              ColorsAssigned[4]);
  OptionsFile.WriteInteger('HowToProg','Reserved_Word_Color',       ColorsAssigned[5]);
  OptionsFile.WriteInteger('HowToProg','Symbol_Color',              ColorsAssigned[6]);
  OptionsFile.WriteInteger('HowToProg','CODEWINDOW_BACK_COLOR',     ColorsAssigned[7]);
  OptionsFile.WriteInteger('HowToProg','CODEWINDOW_BORDER_COLOR',   ColorsAssigned[8]);
  OptionsFile.WriteInteger('HowToProg','CODEWINDOW_HEADER_COLOR',   ColorsAssigned[9]);
  OptionsFile.WriteInteger('HowToProg','CODEWINDOW_SCROLLER_COLOR', ColorsAssigned[10]);
  OptionsFile.WriteInteger('HowToProg','CODEWINDOW_SPRAINER_COLOR', ColorsAssigned[11]);
  OptionsFile.WriteInteger('HowToProg','CODEWINDOW_BUTTONS_COLOR',  ColorsAssigned[12]);
  OptionsFile.WriteInteger('HowToProg','COMMENTWINDOW_BACK_COLOR',  ColorsAssigned[13]);
  OptionsFile.WriteInteger('HowToProg','COMMENTWINDOW_BORDER_COLOR',ColorsAssigned[14]);
  OptionsFile.WriteInteger('HowToProg','COMMENTWINDOW_HEADER_COLOR',ColorsAssigned[15]);
  OptionsFile.WriteInteger('HowToProg','COMMENTWINDOW_TEXT_COLOR',  ColorsAssigned[16]);
  OptionsFile.WriteInteger('HowToProg','DEFAULT_TEXT_COLOR',        DefaultColor);

  OptionsFile.Free;
  end;
end;

procedure TForm2.BitBtn5Click(Sender: TObject);
var OptionsFile:TIniFile;
begin
if OpenDialog1.Execute then
  begin;
  OptionsFile:=TIniFile.Create(OpenDialog1.Filename);
  OptionsFile.UpdateFile;

  ColorsAssigned[ 0]:= OptionsFile.ReadInteger('HowToProg','Whitespace_Color',          ColorsAssigned[0]);
  ColorsAssigned[ 1]:= OptionsFile.ReadInteger('HowToProg','Comment_Color',             ColorsAssigned[1]);
  ColorsAssigned[ 2]:= OptionsFile.ReadInteger('HowToProg','String_Color',              ColorsAssigned[2]);
  ColorsAssigned[ 3]:= OptionsFile.ReadInteger('HowToProg','Identifier_Color',          ColorsAssigned[3]);
  ColorsAssigned[ 4]:= OptionsFile.ReadInteger('HowToProg','Number_Color',              ColorsAssigned[4]);
  ColorsAssigned[ 5]:= OptionsFile.ReadInteger('HowToProg','Reserved_Word_Color',       ColorsAssigned[5]);
  ColorsAssigned[ 6]:= OptionsFile.ReadInteger('HowToProg','Symbol_Color',              ColorsAssigned[6]);
  ColorsAssigned[ 7]:= OptionsFile.ReadInteger('HowToProg','CODEWINDOW_BACK_COLOR',     ColorsAssigned[7]);
  ColorsAssigned[ 8]:= OptionsFile.ReadInteger('HowToProg','CODEWINDOW_BORDER_COLOR',   ColorsAssigned[8]);
  ColorsAssigned[ 9]:= OptionsFile.ReadInteger('HowToProg','CODEWINDOW_HEADER_COLOR',   ColorsAssigned[9]);
  ColorsAssigned[10]:= OptionsFile.ReadInteger('HowToProg','CODEWINDOW_SCROLLER_COLOR', ColorsAssigned[10]);
  ColorsAssigned[11]:= OptionsFile.ReadInteger('HowToProg','CODEWINDOW_SPRAINER_COLOR', ColorsAssigned[11]);
  ColorsAssigned[12]:= OptionsFile.ReadInteger('HowToProg','CODEWINDOW_BUTTONS_COLOR',  ColorsAssigned[12]);
  ColorsAssigned[13]:= OptionsFile.ReadInteger('HowToProg','COMMENTWINDOW_BACK_COLOR',  ColorsAssigned[13]);
  ColorsAssigned[14]:= OptionsFile.ReadInteger('HowToProg','COMMENTWINDOW_BORDER_COLOR',ColorsAssigned[14]);
  ColorsAssigned[15]:= OptionsFile.ReadInteger('HowToProg','COMMENTWINDOW_HEADER_COLOR',ColorsAssigned[15]);
  ColorsAssigned[16]:= OptionsFile.ReadInteger('HowToProg','COMMENTWINDOW_TEXT_COLOR',  ColorsAssigned[16]);
  DefaultColor:=       OptionsFile.ReadInteger('HowToProg','DEFAULT_TEXT_COLOR',        DefaultColor);

  OptionsFile.Free;
  if (Form2.CheckBox2.Checked) then
    begin;
    Form2.ComboBox2.ItemIndex:=ColorsAssigned[Form2.ComboBox1.ItemIndex];
    Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[Form2.ComboBox2.ItemIndex];
    end
  else if Form2.ComboBox1.ItemIndex=0 then
      begin;
      Form2.ComboBox2.ItemIndex:=DefaultColor;
      Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[DefaultColor];
      end
    else
      begin;
      Form2.ComboBox2.ItemIndex:=ColorsAssigned[Form2.ComboBox1.ItemIndex+6];
      Form2.MyColorGrid1.ForegroundIndex:=ColorWrap[ColorsAssigned[Form2.ComboBox1.ItemIndex+6]];
      end;
  end;
end;

end.
