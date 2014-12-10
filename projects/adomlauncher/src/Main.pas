unit Main;

interface

uses
  Windows, SysUtils, Forms, IniFiles, ShellAPI, Classes, Controls, StdCtrls,
  FileCtrl, Consts, enterbackupname;

type
  TForm1 = class(TForm)
    DirectoryListBox1: TDirectoryListBox;
    DriveComboBox1: TDriveComboBox;
    Label1: TLabel;
    FileListBox1: TFileListBox;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Label3: TLabel;
    procedure DriveComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure FileListBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  OptionsFile: TIniFile;
  ExePath: String;


implementation

{$R *.DFM}
//============================================================================\\
// Полезные функции и процедуры
//============================================================================\\
function ExecuteFile(const FileName, Params, DefaultDir: string; ShowCmd: Integer): THandle;
var zFileName, zParams, zDir: array[0..79] of Char;
begin
Result := ShellExecute(Application.MainForm.Handle, nil, StrPCopy(zFileName, FileName), StrPCopy(zParams, Params), StrPCopy(zDir, DefaultDir), ShowCmd);
end;

function HasAttr(const FileName: string; Attr: Word): Boolean;
var
 FileAttr: Integer;
begin
  FileAttr := FileGetAttr(FileName);
  if FileAttr = -1 then FileAttr := 0;
  Result := (FileAttr and Attr) = Attr;
end;

procedure CopyFile(const FileName, DestName: string);
var
  CopyBuffer: Pointer; { buffer for copying }
  BytesCopied: Longint;
  Source, Dest: Integer; { handles }
  Len: Integer;
  Destination: TFileName; { holder for expanded destination name }
const
  ChunkSize: Longint = 8192; { copy in 8K chunks }
begin
  Destination := ExpandFileName(DestName); { expand the destination path }
  if HasAttr(Destination, faDirectory) then { if destination is a directory... }
  begin
    Len :=  Length(Destination);
    if Destination[Len] = '\' then
      Destination := Destination + ExtractFileName(FileName) { ...clone file name }
    else
      Destination := Destination + '\' + ExtractFileName(FileName); { ...clone file name }
  end;
GetMem(CopyBuffer, ChunkSize); { allocate the buffer }
  try
    Source := FileOpen(FileName, fmShareDenyWrite); { open source file }
    if Source < 0 then raise EFOpenError.CreateFmt(SFOpenError, [FileName]);
    try
      Dest := FileCreate(Destination); { create output file; overwrite existing }
      if Dest < 0 then raise EFCreateError.CreateFmt(SFCreateError, [Destination]);
      try
        repeat
          BytesCopied := FileRead(Source, CopyBuffer^, ChunkSize); { read chunk }
          if BytesCopied > 0 then { if we read anything... }
            FileWrite(Dest, CopyBuffer^, BytesCopied); { ...write chunk }
        until BytesCopied < ChunkSize; { until we run out of chunks }
      finally
        FileClose(Dest); { close the destination file }
      end;
    finally
      FileClose(Source); { close the source file }
    end;
  finally
    FreeMem(CopyBuffer, ChunkSize); { free the buffer }
  end;
end;


//============================================================================\\
// Код обработчиков событий
//============================================================================\\
procedure TForm1.DriveComboBox1Change(Sender: TObject);
begin
Form1.DirectoryListBox1.Drive:=Form1.DriveComboBox1.Drive;
end;

procedure TForm1.FormCreate(Sender: TObject);
var f:TextFile;
    s:string;
    i:integer;
begin
ExePath:=ExtractFilePath(Application.Exename);
try
  OptionsFile:=TIniFile.Create(ExePath+'ADOMLauncher.ini');
except
  AssignFile(f,ExePath+'ADOMLauncher.ini');
  ReWrite(f);
  CloseFile(f);
  OptionsFile:=TIniFile.Create(ExePath+'ADOMLauncher.ini');
end;
OptionsFile.UpdateFile;
Form1.DirectoryListBox1.Directory:=OptionsFile.ReadString('ADOMLauncher','ADOMPath','C:\');
Form1.DriveComboBox1.Drive:=ExtractFileDrive(Form1.DirectoryListBox1.Directory)[1];
s:=OptionsFile.ReadString('ADOMLauncher','Character',':-)');
if pos('.svg',s)<>0 then
  begin;
  for i:=0 to (Form1.FileListBox1.Items.Count-1) do if Pos(S,Form1.FileListBox1.Items[i])<>0 then Break;
  Form1.FileListBox1.ItemIndex:=i;
  Form1.Label3.Caption:='Ваш персонаж: '+ExtractFileName(Form1.FileListBox1.FileName);
  end;
end;


procedure TForm1.DirectoryListBox1Change(Sender: TObject);
begin
if FileExists(Form1.DirectoryListBox1.Directory+'\Adom.exe') then
  begin;
  if not DirectoryExists(Form1.DirectoryListBox1.Directory+'\Backup') then
    if not CreateDir(Form1.DirectoryListBox1.Directory+'\Backup') then
      begin;
      raise Exception.Create('Не могу создать директорию '+Form1.DirectoryListBox1.Directory+'\Backup');
      Application.Terminate;
      end;
  Form1.FileListBox1.Directory:=Form1.DirectoryListBox1.Directory+'\Adom_Dat\SaveDg';
  Form1.FileListBox1.Update;
  OptionsFile.WriteString('ADOMLauncher','ADOMPath',Form1.DirectoryListBox1.Directory+'\');
  end
else
  begin;
  Form1.FileListBox1.Clear;
  Form1.Label3.Caption:='Ваш персонаж: ';
  end;
end;

procedure TForm1.FileListBox1Click(Sender: TObject);
begin
Form1.Label3.Caption:='Ваш персонаж: '+ExtractFileName(Form1.FileListBox1.FileName);
OptionsFile.WriteString('ADOMLauncher','Character',ExtractFileName(Form1.FileListBox1.FileName));
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
if (FileExists(Form1.DirectoryListBox1.Directory+'\Adom.exe'))and(Form1.FileListBox1.Items.Count<>0)and(Form1.FileListBox1.ItemIndex<>-1) then
  begin;
  if FileExists(Form1.FileListBox1.FileName) then
    CopyFile(Form1.FileListBox1.FileName,Form1.DirectoryListBox1.Directory+'\Backup\'+ExtractFileName(Form1.FileListBox1.FileName))
  else
    CopyFile(Form1.DirectoryListBox1.Directory+'\Backup\'+ExtractFileName(Form1.FileListBox1.FileName),Form1.FileListBox1.FileName);
  ExecuteFile('Adom.exe','-l '+ExtractFileName(Form1.FileListBox1.FileName),Form1.DirectoryListBox1.Directory,SW_MAXIMIZE);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
if (FileExists(Form1.DirectoryListBox1.Directory+'\Adom.exe'))and(Form1.FileListBox1.Items.Count<>0)and(Form1.FileListBox1.ItemIndex<>-1) then
  begin;
  if FileExists(Form1.FileListBox1.FileName) then
    CopyFile(Form1.FileListBox1.FileName,Form1.DirectoryListBox1.Directory+'\Backup\'+ExtractFileName(Form1.FileListBox1.FileName))
  else
    CopyFile(Form1.DirectoryListBox1.Directory+'\Backup\'+ExtractFileName(Form1.FileListBox1.FileName),Form1.FileListBox1.FileName);
  PassWordDlg.Password.Text:='';
  if (PassWordDlg.ShowModal=mrOk) then
    if DirectoryExists(Form1.DirectoryListBox1.Directory+'\Backup\'+PassWordDlg.Password.Text) then Application.MessageBox(PChar('Эта директория уже существует!'),PChar('Ошибка'),mb_Ok)
  else
    begin;
    try
      MkDir(Form1.DirectoryListBox1.Directory+'\Backup\'+PassWordDlg.Password.Text);
      CopyFile(Form1.DirectoryListBox1.Directory+'\Backup\'+ExtractFileName(Form1.FileListBox1.FileName),Form1.DirectoryListBox1.Directory+'\Backup\'+PassWordDlg.Password.Text+'\'+ExtractFileName(Form1.FileListBox1.FileName));
    except
      Application.MessageBox(PChar('Невозможно создать директорию!'),PChar('Ошибка'),mb_Ok)
    end;
    end;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
if (FileExists(Form1.DirectoryListBox1.Directory+'\Adom.exe'))and(Form1.FileListBox1.Items.Count<>0)and(Form1.FileListBox1.ItemIndex<>-1) then
  begin;
  if FileExists(Form1.FileListBox1.FileName) then
    CopyFile(Form1.FileListBox1.FileName,Form1.DirectoryListBox1.Directory+'\Backup\'+ExtractFileName(Form1.FileListBox1.FileName))
  else
    CopyFile(Form1.DirectoryListBox1.Directory+'\Backup\'+ExtractFileName(Form1.FileListBox1.FileName),Form1.FileListBox1.FileName);
  end;
OptionsFile.Destroy;
end;

end.
