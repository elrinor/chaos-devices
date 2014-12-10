unit Main;

interface
{TODO
- качать странички по одной при апдейте + вынести это в настройки
+ при генерации списка скачки учитывать уже скачанные файлы.
? при сортировке по папкам скачанного сделать ProgressBar.
- выбор какие типы в какие подпапки сохранять (wallpapers, art, etc.)
- красивый download с progressbar'ом
}


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, FileUtil,
  Menus, ImgList, Dialogs, ExtCtrls, Downloader, StdCtrls, EventedListBox,
  Buttons, HintLabel, NumberEdit, OptionsEdit, OptionsCheckBox, ComCtrls,
  Spin, NoBugSpinEdit, DBase, FileDownload, Options, IdHTTP, FileCtrl, jpeg,
  OptionsComboBox;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    BitBtn1: TBitBtn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    TabSheet3: TTabSheet;
    GroupBox2: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    ComboBox1: TComboBox;
    Label6: TLabel;
    OptionsCheckBox1: TOptionsCheckBox;
    OptionsCheckBox2: TOptionsCheckBox;
    OptionsEdit1: TOptionsEdit;
    OptionsEdit2: TOptionsEdit;
    OptionsEdit3: TOptionsEdit;
    NumberEdit2: TNumberEdit;
    EventedListBox1: TEventedListBox;
    GroupBox1: TGroupBox;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    HintLabel6: THintLabel;
    HintLabel5: THintLabel;
    HintLabel3: THintLabel;
    HintLabel2: THintLabel;
    HintLabel1: THintLabel;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    GroupBox3: TGroupBox;
    Label8: TLabel;
    BitBtn4: TBitBtn;
    ImageList1: TImageList;
    Label18: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Bevel1: TBevel;
    ComboBox2: TComboBox;
    HintLabel4: THintLabel;
    HintLabel8: THintLabel;
    OptionsCheckBox3: TOptionsCheckBox;
    BitBtn6: TBitBtn;
    Image1: TImage;
    Label3: TLabel;
    OpenDialog3: TOpenDialog;
    OptionsCheckBox4: TOptionsCheckBox;
    GroupBox4: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    BitBtn7: TBitBtn;
    GroupBox5: TGroupBox;
    Label2: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Label14: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    BitBtn5: TBitBtn;
    OpenDialog2: TOpenDialog;
    Label26: TLabel;
    BitBtn8: TBitBtn;
    Label27: TLabel;
    Label28: TLabel;
    OptionsComboBox1: TOptionsComboBox;
    OptionsEdit4: TOptionsEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EventedListBox1Change(Sender: TObject);
    procedure Downloader1EndDownload(Sender: TObject; URL, AdditionalInfo: String;
      F: TStream);
    procedure EventedListBox1Call(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
  private
    procedure BeginDownload(ID:Integer);
    procedure ContinueDownload(ID:Integer; AType:Integer; Page:Integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses IntegerSet, DownloadListOutput;

const
  DefaultURL='http://browse.minitokyo.net/gallery/';
  ExtCount=1;
  Exts:array[1..ExtCount]of String=('.jpg');

var
  CurrentDownloads: TMap_Int_Int;

{$R *.DFM}
function FillStringToLength(const s:string; const NeededLength:Integer; const CharToFill:char):string;
begin;
Result:=S;
while Length(Result)<NeededLength do
  begin;
  Result:=CharToFill+Result;
  end;
end;

function MyIntToStr(Value:Integer):String;
begin;
if Value<>-1 then
  Result:=IntToStr(Value)
else
  Result:='?';
end;

function GetSubDir(AType:Integer):String;
begin;
case AType of
  1,2: Result:='Wallpapers\';
  3,4: Result:='Art\HandDrawn\';
  5..9: Result:='Art\';
  else Result:='';
end;
end;

function MakeFileName(const AnimeName:String; ImageNumber:Integer; ImageType:Integer; Ext:String):String; overload;
begin;
if Ext[1]<>'.' then Ext:='.'+Ext;
Result:=MainFolder+AnimeName+'\'+GetSubDir(ImageType)+'['+FillStringToLength(IntToStr(ImageNumber),7,'0')+']'+Ext;
end;

function MakeFileName(const AnimeName:String; ImageNumber:Integer; ImageType:Integer):String; overload;
begin;
Result:=MakeFileName(AnimeName,ImageNumber,ImageType,'.jpg');
end;

function MakeFileName(ImageNumber:Integer; Ext:String):String; overload;
var ID:Integer;
begin;
ID:=MiniTokyo.Items.ItemsByImage[ImageNumber].ID;
Result:=MakeFileName(MiniTokyo.Items[ID].Name,ImageNumber,MiniTokyo.Items[ID].Images.Types[ImageNumber],Ext);
end;

function MakeFileName(ImageNumber:Integer):String; overload;
begin;
Result:=MakeFileName(ImageNumber,'.jpg');
end;


//============================================================================\\
// TForm1
//============================================================================\\
procedure TForm1.BeginDownload(ID:Integer);
var i:Integer;
begin;
if not DoNotDownload then
if not(CurrentDownloads[ID]<>0) then
if (MiniTokyo.Items[ID].ImagesCount[0]>MiniTokyo.Items[ID].Images.Count[0])or(MiniTokyo.Items[ID].ImagesCount[0]=-1) then
  begin;
  for i:=0 to MiniTokyo.TypesCount-1 do
    begin;
    Downloader1.Download('http://browse.minitokyo.net/gallery/?cid='+MiniTokyo.Types[i]+'&aid='+IntToStr(ID)+'&order=dos&flip=desc&page=1',10,'BEGIN;'+IntToStr(ID)+';'+IntToStr(i)+';');
    CurrentDownloads.Items[ID]:=CurrentDownloads.Items[ID]+1;
    end;
  end
else if (Now-MiniTokyo.Items[ID].LastUpdate>1) then
  begin;
  Downloader1.Download('http://browse.minitokyo.net/gallery/?cid='+MiniTokyo.Types[0]+'&aid='+IntToStr(ID)+'&order=dos&flip=desc&page=1',10,'CHECK;'+IntToStr(ID)+';');
  CurrentDownloads.Items[ID]:=CurrentDownloads.Items[ID]+1;
  end;
end;

procedure TForm1.ContinueDownload(ID:Integer; AType:Integer; Page:Integer);
var i:Integer;
begin;
if not DoNotDownload then
if AType<>0 then
  if MiniTokyo.Items[ID].ImagesCount[AType]>MiniTokyo.Items[ID].Images.Count[AType] then
    begin;
    if MiniTokyo.Items[ID].ImagesCount[AType]/MiniTokyo.Items[ID].Images.Count[AType]>=2 then
      begin;
      for i:=Page to (MiniTokyo.Items[ID].ImagesCount[AType]-1) div 6+1 do
        begin;
        Downloader1.Download('http://browse.minitokyo.net/gallery/?cid='+MiniTokyo.Types[AType]+'&aid='+IntToStr(ID)+'&order=dos&flip=desc&page='+IntToStr(i),1,'CONTINUE;'+IntToStr(ID)+';'+IntToStr(AType)+';');
        CurrentDownloads.Items[ID]:=CurrentDownloads.Items[ID]+1;
        end;
      end
    else
      begin;
      Downloader1.Download('http://browse.minitokyo.net/gallery/?cid='+MiniTokyo.Types[AType]+'&aid='+IntToStr(ID)+'&order=dos&flip=desc&page='+IntToStr(Page),1,'SLOWCONTINUE;'+IntToStr(ID)+';'+IntToStr(AType)+';'+IntToStr(Page+1)+';');
      CurrentDownloads.Items[ID]:=CurrentDownloads.Items[ID]+1;
      end;
    end;
end;

// Eat File
procedure TForm1.BitBtn1Click(Sender: TObject);
var F:TFileStream;
    i:Integer;
begin
if OpenDialog1.Execute then
  for i:=0 to OpenDialog1.Files.Count-1 do
    begin;
    F:=TFileStream.Create(OpenDialog1.Files[i], fmOpenRead or fmShareDenyWrite);
    MiniTokyo.UpdateDataBase(F,False);
    F.Free;
    end;
end;

// Download File
procedure TForm1.BitBtn2Click(Sender: TObject);
begin
Downloader1.ProxyParams:=MakeProxyParams(ProxyServer,ProxyUsername,ProxyPassword,BasicAuthentication,UseProxy,ProxyPort);
Downloader1.Download(DefaultURL,100,'UPDATE;');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
CurrentDownloads:=TMap_Int_Int.Create;
CurrentDownloads.ErrorValue:=0;
PageControl1.ActivePageIndex:=0;
OptionsCheckBox1.ConnectedVariable:=@UseProxy;
OptionsCheckBox2.ConnectedVariable:=@BasicAuthentication;
OptionsEdit1.ConnectedVariable:=@ProxyServer;
OptionsEdit2.ConnectedVariable:=@ProxyUsername;
OptionsEdit3.ConnectedVariable:=@ProxyPassword;
NumberEdit2.ConnectedVariable:=@ProxyPort;
OptionsEdit4.ConnectedVariable:=@MainFolder;
OptionsCheckBox3.ConnectedVariable:=@DoNotDownload;
OptionsCheckBox4.ConnectedVariable:=@DoNotReDownload;
OptionsComboBox1.ConnectedVariable:=@DownloadListType;
MiniTokyo:=TMiniTokyo.Create(ExtractFilePath(Application.ExeName)+'MiniTokyo.db',EventedListBox1,ComboBox1,ComboBox2);
if DirectoryExists(ExtractFilePath(Application.ExeName)+'Merge') then
  if FileExists(ExtractFilePath(Application.ExeName)+'Merge\MiniTokyo.db') then
    MiniTokyo.MergeWithFile(ExtractFilePath(Application.ExeName)+'Merge\MiniTokyo.db');
Label7.Caption:=DBaseVersion;
Label14.Caption:=DBaseHeader;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
MiniTokyo.Free;
CurrentDownloads.Free;
end;

procedure TForm1.EventedListBox1Change(Sender: TObject);
var Name:String;
    ID:Integer;
begin
EventedListBox1.Call;
if EventedListBox1.ItemIndex<>-1 then
  begin;
  Name:=EventedListBox1.Items[EventedListBox1.ItemIndex];
  ID:=MiniTokyo.Items.ItemsByName[Name].ID;
  Downloader1.ProxyParams:=MakeProxyParams(ProxyServer,ProxyUsername,ProxyPassword,BasicAuthentication,UseProxy,ProxyPort);
  BeginDownload(ID);
  end
end;

procedure TForm1.Downloader1EndDownload(Sender: TObject; URL, AdditionalInfo: String;
  F: TStream);
var ID,AType,Page:Integer;
    Reason,S:String;
begin
try
  if Length(AdditionalInfo)=0 then AdditionalInfo:='UPDATE;';
  Reason:=Copy(AdditionalInfo,1,pos(';',AdditionalInfo)-1);
  Delete(AdditionalInfo,1,pos(';',AdditionalInfo));
  if Reason='UPDATE' then
    MiniTokyo.UpdateDataBase(F,False)
  else if Reason='BEGIN' then
    begin;
    MiniTokyo.UpdateDataBase(F,True);
    S:=Copy(AdditionalInfo,1,pos(';',AdditionalInfo)-1);
    Delete(AdditionalInfo,1,pos(';',AdditionalInfo));
    ID:=StrToInt(S);
    CurrentDownloads.Items[ID]:=CurrentDownloads.Items[ID]-1;
    S:=Copy(AdditionalInfo,1,pos(';',AdditionalInfo)-1);
    Delete(AdditionalInfo,1,pos(';',AdditionalInfo));
    AType:=StrToInt(S);
    ContinueDownload(ID,AType,2);
    end
  else if Reason='CHECK' then
    begin;
    MiniTokyo.UpdateDataBase(F,True);
    S:=Copy(AdditionalInfo,1,pos(';',AdditionalInfo)-1);
    Delete(AdditionalInfo,1,pos(';',AdditionalInfo));
    ID:=StrToInt(S);
    CurrentDownloads.Items[ID]:=CurrentDownloads.Items[ID]-1;
    BeginDownload(ID);
    end
  else if Reason='SLOWCONTINUE' then
    begin;
    MiniTokyo.UpdateDataBase(F,True);
    S:=Copy(AdditionalInfo,1,pos(';',AdditionalInfo)-1);
    Delete(AdditionalInfo,1,pos(';',AdditionalInfo));
    ID:=StrToInt(S);
    CurrentDownloads.Items[ID]:=CurrentDownloads.Items[ID]-1;
    S:=Copy(AdditionalInfo,1,pos(';',AdditionalInfo)-1);
    Delete(AdditionalInfo,1,pos(';',AdditionalInfo));
    AType:=StrToInt(S);
    S:=Copy(AdditionalInfo,1,pos(';',AdditionalInfo)-1);
    Delete(AdditionalInfo,1,pos(';',AdditionalInfo));
    Page:=StrToInt(S);
    ContinueDownload(ID,AType,Page);
    end
  else
    MiniTokyo.UpdateDataBase(F,True)
except on E:Exception do begin;end;
end;
end;

procedure TForm1.EventedListBox1Call(Sender: TObject);
var Name:String;
    ID:Integer;
begin
if EventedListBox1.ItemIndex<>-1 then
  begin;
  Name:=EventedListBox1.Items[EventedListBox1.ItemIndex];
  ID:=MiniTokyo.Items.ItemsByName[Name].ID;
  HintLabel1.Caption:=Name;
  HintLabel2.Caption:=MyIntToStr(ID);
  HintLabel3.Caption:=MyIntToStr(MiniTokyo.Items[ID].Images.Count[0])+' (of '+MyIntToStr(MiniTokyo.Items[ID].ImagesCount[0])+')';
  HintLabel5.Caption:=MiniTokyo.Items[ID].Images.AsText;
  HintLabel6.Caption:=DateTimeToStr(MiniTokyo.Items[ID].LastUpdate);
  HintLabel4.Caption:=MyIntToStr(MiniTokyo.Items[ID].Images.Count[ComboBox2.ItemIndex])+' (of '+MyIntToStr(MiniTokyo.Items[ID].ImagesCount[ComboBox2.ItemIndex])+')';
  HintLabel8.Caption:=MiniTokyo.Items[ID].Images.AsText(ComboBox2.ItemIndex);
  end
else
  begin;
  HintLabel1.Caption:='';
  HintLabel2.Caption:='';
  HintLabel3.Caption:='';
  HintLabel4.Caption:='';
  HintLabel5.Caption:='';
  HintLabel6.Caption:='';
  HintLabel8.Caption:='';
  end;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
var ID,i:Integer;
    Name:String;
    List:TStringList;
begin
if EventedListBox1.ItemIndex<>-1 then
  begin;
  Name:=EventedListBox1.Items[EventedListBox1.ItemIndex];
  ID:=MiniTokyo.Items.ItemsByName[Name].ID;
  SaveDialog1.FileName:=Name+DownloadListTypes[DownloadListType];
  SaveDialog1.Filter:=DownloadListTypeNames[DownloadListType]+'|*'+DownloadListTypes[DownloadListType];
  if SaveDialog1.Execute then
    begin;
    List:=TStringList.Create;
    for i:=0 to MiniTokyo.Items.Items[ID].Images.Count[0]-1 do
      if (not DoNotReDownload) or (not FileExists(MakeFileName(MiniTokyo.Items.Items[ID].Images[i]))) then
        List.Add('http://download.minitokyo.net/_'+IntToStr(MiniTokyo.Items.Items[ID].Images[i])+'.jpg');
    CreateDownloadListFile(SaveDialog1.FileName,List);
    List.Free;
    end;
  end;
end;

procedure TForm1.BitBtn4Click(Sender: TObject);
var Dir:String;
begin
if SelectDirectory('Browse...','',Dir) then
  begin;
  if Dir[Length(Dir)]<>'\' then Dir:=Dir+'\';
  OptionsEdit4.Text:=Dir;
  end;
end;

procedure TForm1.BitBtn7Click(Sender: TObject);
var i,Index:Integer;
begin
if OpenDialog2.Execute then
  for i:=0 to OpenDialog2.Files.Count-1 do
    begin;
    try
      Index:=GetNumber(OpenDialog2.Files[i],-1);
      MoveFile(OpenDialog2.Files[i],
               MakeFileName(Index,ExtractFileExt(OpenDialog2.Files[i])));
    except
      on E:Exception do begin;end;
    end;
    end;
end;

procedure TForm1.BitBtn6Click(Sender: TObject);
var Name:String;
    ID:Integer;
begin
EventedListBox1.Call;
if EventedListBox1.ItemIndex<>-1 then
  begin;
  Name:=EventedListBox1.Items[EventedListBox1.ItemIndex];
  ID:=MiniTokyo.Items.ItemsByName[Name].ID;
  Downloader1.ProxyParams:=MakeProxyParams(ProxyServer,ProxyUsername,ProxyPassword,BasicAuthentication,UseProxy,ProxyPort);
  BeginDownload(ID);
  end
end;

procedure TForm1.BitBtn5Click(Sender: TObject);
var i:Integer;
begin
if OpenDialog3.Execute then
  for i:=0 to OpenDialog3.Files.Count-1 do
    begin;
    try
      MiniTokyo.MergeWithFile(OpenDialog3.Files[i]);
    except
      on E:Exception do begin;end;
    end;
    end;
end;

procedure TForm1.BitBtn8Click(Sender: TObject);
var Name:String;
    ID:Integer;
begin
if Application.MessageBox(PChar('А вы уверены?'),PChar('Вопросик'),MB_YESNO or MB_ICONQUESTION)=IDYES then
  begin;
  Name:=EventedListBox1.Items[EventedListBox1.ItemIndex];
  ID:=MiniTokyo.Items.ItemsByName[Name].ID;
  MiniTokyo.Items[ID].Clear;
  EventedListBox1Call(Self);
  end;
end;



end.
