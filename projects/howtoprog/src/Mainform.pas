unit MainForm;

interface

uses
  SysUtils, Windows, Forms, IniFiles, ShellAPI, Menus, FileCtrl, Controls, StdCtrls, Classes, Buttons, About,
  ImgList,extctrls;

type TScene=(scLessons,scProgas);

type
  TForm1 = class(TForm)
    Label5: TLabel;
    MainMenu1: TMainMenu;
    Play: TMenuItem;
    Exit: TMenuItem;
    File1: TMenuItem;
    Help1: TMenuItem;
    About: TMenuItem;
    Help: TMenuItem;
    GroupBox1: TGroupBox;
    FileListBox1: TFileListBox;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    Memo1: TMemo;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure PlayButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PlayClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure HelpClick(Sender: TObject);
    procedure ShowOptionsClick(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure FileListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FileListBox1Click(Sender: TObject);
    procedure ChangeButtonClick(Sender: TObject);
    Procedure SetScene(NewScene:TScene);
    procedure FileListBox1DblClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
  private
    FScene:TScene;
    FLesson:String;
    FLessonIndex:Integer;
  public
    property Scene:TScene read FScene write SetScene;
  end;



var
  Form1: TForm1;
  OptionsFile:TIniFile;


function FillStringToLength(const s:string; const NeededLength:Integer; const CharToFill:char):string;

implementation

uses Prefences;

{$R *.DFM}

//============================================================================\\
// Некоторые важные функции и процедуры
//============================================================================\\
function ExecuteFile(const FileName, Params, DefaultDir: string; ShowCmd: Integer): THandle;
var zFileName, zParams, zDir: array[0..79] of Char;
begin
Result := ShellExecute(Application.MainForm.Handle, nil, StrPCopy(zFileName, FileName), StrPCopy(zParams, Params), StrPCopy(zDir, DefaultDir), ShowCmd);
end;

function FillStringToLength(const s:string; const NeededLength:Integer; const CharToFill:char):string;
begin;
Result:=S;
while Length(Result)<NeededLength do
  begin;
  Result:=CharToFill+Result;
  end;
end;

//============================================================================\\
// Actionlist
//============================================================================\\
Procedure DoPlay;
begin;
if (Form1.FileListBox1.FileName<>'') then
  begin;
  OptionsFile.WriteString('HowToProg','InFileName',Form1.FileListBox1.FileName);
  ExecuteFile('HowToProg.exe',ExtractFilePath(Application.ExeName),ExtractFilePath(Application.ExeName),SW_MAXIMIZE);
  end;
end;

Procedure DoShowHelp;
begin;
if FileExists(ExtractFilePath(Application.ExeName)+'Help.hlp') then
  ExecuteFile('Help.hlp',ExtractFilePath(Application.ExeName),ExtractFilePath(Application.ExeName),SW_MAXIMIZE)
else
  Application.MessageBox(PChar('Файл помощи не найден.'),PChar('Файл не найден'),MB_OK);
end;

Procedure DoWriteDescription;
var s:String;
begin;
try
  s:=ExtractFileName(Form1.FileListBox1.FileName);
  if (ExtractFileExt(s)<>'.lsn') then
    begin;
    Delete(s,Length(s)-Length(ExtractFileExt(s))+1,Length(ExtractFileExt(s)));
    s:=s+'.nfo';
    Form1.Memo1.Lines.LoadFromFile(s);
    end
  else
    begin;
    Form1.Memo1.Lines.LoadFromFile(s);
    end;
except
  Form1.Memo1.Lines.Clear;
  Form1.Memo1.Lines[0]:='Без комментариев';
end;
end;

Procedure DoChangeToProgas;
begin;
  Form1.GroupBox1.Caption:='Выберите файл программы:';
  Form1.SpeedButton1.Hint:='Назад, к списку доступных уроков';
  Form1.FLessonIndex:=Form1.FileListBox1.ItemIndex;
  Form1.FLesson:=ExtractFilename(Form1.FileListBox1.FileName);
  Form1.FLesson:=Copy(Form1.FLesson,1,Length(Form1.FLesson)-Length(ExtractFileExt(Form1.FLesson)));
  Form1.FileListBox1.Mask:='*.proga';
  Form1.FileListBox1.Directory:=ExtractFilePath(Application.ExeName)+'Lessons\'+Form1.FLesson;
  Form1.FileListBox1.ItemIndex:=0;
  Form1.Memo1.Clear;
  DoWriteDescription;
end;

Procedure DoChangeToLessons;
begin;
  Form1.GroupBox1.Caption:='Выберите файл урока:';
  Form1.SpeedButton1.Hint:='Вперед, к списку программ в уроке';
  Form1.FileListBox1.Mask:='*.lsn';
  Form1.FileListBox1.Directory:=ExtractFilePath(Application.ExeName)+'Lessons';
  Form1.FileListBox1.ItemIndex:=Form1.FLessonIndex;
  Form1.Memo1.Clear;
  DoWriteDescription;
end;

Procedure DoShowOptions;
var DosText:Boolean;
begin;
Form2.MaskEdit1.Text:=FillStringToLength(OptionsFile.ReadString('HowToProg','BaseDelayFactor','0000'),4,'0');
Form2.CheckBox1.Checked:=OptionsFile.ReadBool('HowToProg','HomeAllowed',TRUE);
DosText:=OptionsFile.ReadBool('HowToProg','DosText',FALSE);
if (DosText=False) then Form2.RadioButton1.Checked:=True
else Form2.RadioButton2.Checked:=True;
Form2.CheckBox2.Checked:=OptionsFile.ReadBool('HowToProg','SyntaxHighlight',TRUE);
Form2.ComboBox1.ItemIndex:=0;
Form2.ComboBox2.ItemIndex:=ColorsAssigned[Form2.ComboBox1.ItemIndex];
Form2.ShowModal;
end;

//============================================================================\\
// Мелкие События обьектов
//============================================================================\\
procedure TForm1.PlayButtonClick(Sender: TObject);
begin;
case FScene of
  scProgas:
    begin;
    DoPlay;
    end;
  scLessons:
    begin;
    DoChangeToProgas;
    end;
end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OptionsFile.Free;
end;

procedure TForm1.PlayClick(Sender: TObject);
begin
  if FScene=scProgas then DoPlay;
end;

procedure TForm1.ExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.AboutClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TForm1.HelpClick(Sender: TObject);
begin
  DoShowHelp;
end;

procedure TForm1.N1Click(Sender: TObject);
begin
  if FScene=scProgas then DoPlay;
end;

procedure TForm1.N2Click(Sender: TObject);
begin
  ExecuteFile(Form1.FileListBox1.Filename,ExtractFilePath(Application.ExeName),ExtractFilePath(Application.ExeName),SW_MAXIMIZE);
end;

procedure TForm1.ShowOptionsClick(Sender: TObject);
begin
  DoShowOptions;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  if FScene=scProgas then DoPlay;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
DoShowOptions;
end;

//============================================================================\\
// События обьектов
//============================================================================\\
procedure TForm1.FormCreate(Sender: TObject);
begin
Application.HelpFile:=ExtractFilePath(Application.ExeName)+'Help.hlp';
OptionsFile:=TIniFile.Create(ExtractFilePath(Application.ExeName)+'options.ini');
OptionsFile.UpdateFile;
Form1.Scene:=scLessons;

Form1.ComboBox1.Items:=Form1.FileListBox1.Items;
Form1.ComboBox1.ItemIndex:=Form1.FileListBox1.ItemIndex;
end;

// Клик правой кнопкой на списке
procedure TForm1.FileListBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var PressPoint:TPoint;
    SelectedItem:Integer;
begin
if FScene=scProgas then
if (Button=mbRight) then
  begin;
  PressPoint.X:=X;
  PressPoint.Y:=Y;
  SelectedItem:=Form1.FileListBox1.ItemAtPos(PressPoint,True);
  if (SelectedItem<>-1) then
    begin;
    Form1.FileListBox1.ItemIndex:=SelectedItem;
    PressPoint:=Form1.FileListBox1.ClientToScreen(PressPoint);
    Form1.FileListBox1.PopupMenu.Popup(PressPoint.X, PressPoint.Y);
    end;
  end;
end;

// Вывод коммента в Memo1 для *.proga
procedure TForm1.FileListBox1Click(Sender: TObject);
var s:string;
begin
DoWriteDescription;
if FScene=scLessons then
  begin;
  FLesson:=Form1.FileListBox1.Filename;
  FLessonIndex:=Form1.FileListBox1.ItemIndex;
  Form1.ComboBox1.ItemIndex:=Form1.FileListBox1.ItemIndex;
  end;
end;

procedure TForm1.ChangeButtonClick(Sender: TObject);
begin
if FScene=scProgas then Scene:=scLessons
else Scene:=scProgas;
end;


Procedure TForm1.SetScene(NewScene:TScene);
begin;
case NewScene of
  scProgas:
    begin;
    DoChangeToProgas;
    end;
  scLessons:
    begin;
    DoChangeToLessons;
    end;
end;
Form1.FScene:=NewScene;
end;


procedure TForm1.FileListBox1DblClick(Sender: TObject);
begin
case Form1.FScene of
  scLessons: Form1.Scene:=scProgas;
  scProgas: DoPlay;
end;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
if FScene=scProgas then Scene:=scLessons
else Scene:=scProgas;
end;

procedure TForm1.ComboBox1Click(Sender: TObject);
begin
if Form1.ComboBox1.ItemIndex<>FLessonIndex then
  begin;
  FLessonIndex:=Form1.ComboBox1.ItemIndex;
  Form1.FLesson:=ExtractFilename(Form1.ComboBox1.Text);
  Form1.FLesson:=Copy(Form1.FLesson,1,Length(Form1.FLesson)-Length(ExtractFileExt(Form1.FLesson)));
  case FScene of
    scLessons:
      begin;
      Form1.FileListBox1.ItemIndex:=Form1.ComboBox1.ItemIndex;
      DoWriteDescription;
      end;
    scProgas:
      begin;
      Form1.FileListBox1.Directory:=ExtractFilePath(Application.ExeName)+'Lessons\'+Form1.FLesson;
      Form1.FileListBox1.ItemIndex:=0;
      Form1.Memo1.Clear;
      DoWriteDescription;
      end;
  end;
  end;
end;

end.

