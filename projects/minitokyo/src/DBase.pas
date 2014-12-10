unit DBase;

interface
{$I debug.inc}
uses Classes, DBaseList, SysUtils, IntegerList, StdCtrls, EventedListBox;

type
  FileOfChar = File of Char;

  TMiniTokyo = class(TObject)
  private
    FDataBaseFileName:String;
    FBase:TDBaseList;
    FCategories:TStringList;
    FTypes: TStringList;
    
    FNameListBox:TEventedListBox;
    FCategoryComboBox:TComboBox;
    FTypeComboBox:TComboBox;
  protected
    // Work with HTML File
    function GetNextTag(F:TStream):String;
    function GetWord(F:TStream; Blocker:TSysCharSet):String;
    function GetNextTagProperty(F:TStream; var Prop,Value:String):Boolean;
    procedure GetTagProperties(F:TStream; PropList,ValueList:TStringList);
    function GetBlockedSequence(F:TStream; Blocker:TSysCharSet):String;
    procedure CloseTag(F:TStream);
    function GetMidTagText(F:TStream):String;

    function GetCategoryNumber(Index:String):Integer;
    procedure CategoryComboBoxChange(Sender: TObject);
    procedure TypeComboBoxChange(Sender: TObject);
    procedure DataChanged(ImagesOnly:Boolean);
    procedure SetTypeName(Index:Integer; Value:String);
    function GetTypeName(Index:Integer):String;
    function GetTypesCount:Integer;
    function GetType(Index:Integer):String;

    property CategoryNumbers[Index: String]:Integer read GetCategoryNumber;
  public
    constructor Create(const FileName:String; ListBox:TEventedListBox; ComboBox, TypeBox:TComboBox); overload;
    constructor Create(const FileName:String); overload;
    destructor Destroy; override;
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);
    procedure MergeWithFile(const FileName: String);
    procedure UpdateDataBase(F:TStream; ImagesOnly:Boolean);
    property Categories:TStringList read FCategories;
    property Items:TDBaseList read FBase;
    property TypeNames[Index: Integer]:String read GetTypeName write SetTypeName;
    property Types[Index: Integer]:String read GetType;
    property TypesCount:Integer read GetTypesCount;
  end;

function GetNumber(const S:String; No:Integer):Integer;

var
  MiniTokyo:TMiniTokyo;

const
  DBaseHeader='ArXMTDDBv1.0.0 ';
  DBaseVersion='v1.0.0';

implementation
uses Debug, Forms, Windows;

procedure ErrorLoadDBase(Header,FileName:String);
begin;
if Header<>DBaseHeader then
  Application.MessageBox(PChar('Неправильная версия базы "'+FileName+'"!'#10#13'База не будет загружена.'#10#13'Необходимый заголовок базы: "'+DBaseHeader+'".'#13#10'Имеющийся заголовок: "'+Header+'".'),'Ошибка',MB_ICONERROR or MB_OK)
else
  Application.MessageBox(PChar('Ошибка при загрузке базы "'+FileName+'"!'#10#13'База имеет неправильную структуру.'),'Ошибка',MB_ICONERROR or MB_OK);
end;

type
  TMyStringList=class(TStringList)
  public
    function Find(const S: string; var Index: Integer): Boolean; override;
  end;

function TMyStringList.Find(const S: string; var Index: Integer): Boolean;
begin;
if Self.Sorted then Result:=inherited Find(S, Index)
else
  begin;
  Index:=Self.IndexOf(S);
  Result:=(Index<>-1);
  end;
end;

function FromCapitalLetter(const S:String):String;
begin;
Result:=UpCase(S[1])+LowerCase(Copy(s,2,Length(s)-1));
end;

function GetNumber(const S:String; No:Integer):Integer;
var i,n,q:Integer;
    Found:Boolean;
begin;
n:=0;
q:=0;
Found:=False;
if No=-1 then
  begin;
  for i:=Length(S) downto 1 do
    begin;
    if S[i] in ['0','1','2','3','4','5','6','7','8','9'] then
      begin;
      Found:=True;
      Inc(n);
      end
    else
      if Found then
        begin;
        Result:=StrToInt(Copy(S,i+1,n));
        Exit;
        end;
    end;
  if Found then
    Result:=StrToInt(Copy(S,1,n))
  else
    Result:=-1;
  end
else
  begin;
  for i:=1 to Length(S) do
    begin;
    if S[i] in ['0','1','2','3','4','5','6','7','8','9'] then
      begin;
      Found:=True;
      Inc(n);
      end
    else
      if Found then
        begin;
        Inc(q);
        if q=No then
          begin;
          Result:=StrToInt(Copy(S,i-n,n));
          Exit;
          end
        else
          begin;
          Found:=False;
          n:=0;
          end;
        end;
    end;
  if (Found)and(q+1=No) then
    Result:=StrToInt(Copy(S,1,n))
  else
    Result:=-1;
  end;
end;

//============================================================================\\
// TMiniTokyo
//============================================================================\\
function TMiniTokyo.GetType(Index:Integer):String;
begin;
if Index=0 then
  Result:='all'
else
  Result:=IntToStr(Index);
end;

function TMiniTokyo.GetTypesCount:Integer;
begin;
Result:=FTypes.Count;
end;

procedure TMiniTokyo.SetTypeName(Index:Integer; Value:String);
var i:Integer;
begin;
if Index>FTypes.Count-1 then
  for i:=FTypes.Count to Index do
    FTypes.Add('');
FTypes[Index]:=Value;    
end;

function TMiniTokyo.GetTypeName(Index:Integer):String;
begin;
try
  Result:=FTypes[Index];
except
  on E:Exception do Result:='';
end;
end;

procedure TMiniTokyo.CategoryComboBoxChange(Sender: TObject);
begin;
DataChanged(False);
end;

procedure TMiniTokyo.TypeComboBoxChange(Sender: TObject);
begin;
DataChanged(True);
end;

procedure TMiniTokyo.DataChanged(ImagesOnly:Boolean);
var
  OldName:String;
  i:Integer;
begin;
if Assigned(FTypeComboBox) then
  begin;
  if not(ImagesOnly) then
    begin;
    OldName:=FTypeComboBox.Text;
    FTypeComboBox.Items.Clear;
    For i:=0 to FTypes.Count-1 do
      FTypeComboBox.Items.Add(FTypes[i]);
    FTypeComboBox.ItemIndex:=FTypeComboBox.Items.IndexOf(OldName);
    if FTypeComboBox.ItemIndex=-1 then FTypeComboBox.ItemIndex:=0;
    end;
  end;
if Assigned(FCategoryComboBox) then
  begin;
  if not ImagesOnly then
    begin;
    OldName:=FCategoryComboBox.Text;
    FCategoryComboBox.Items.Clear;
    For i:=0 to FCategories.Count-1 do
      FCategoryComboBox.Items.Add(FromCapitalLetter(FCategories[i]));
    FCategoryComboBox.ItemIndex:=FCategoryComboBox.Items.IndexOf(OldName);
    if FCategoryComboBox.ItemIndex=-1 then FCategoryComboBox.ItemIndex:=0;
    end;

  if Assigned(FNameListBox) then
    begin;
    if not ImagesOnly then
      begin;
      if FNameListBox.ItemIndex<>-1 then
        OldName:=FNameListBox.Items[FNameListBox.ItemIndex]
      else
        OldName:='';
      FNameListBox.Items.Clear;
      For i:=0 to FBase.Count-1 do
        if FBase.ItemsByIndex[i].Category=FCategoryComboBox.ItemIndex then
          FNameListBox.Items.Add(FBase.ItemsByIndex[i].Name);
      FNameListBox.ItemIndex:=FNameListBox.Items.IndexOf(OldName);
      FNameListBox.Sorted:=True;
      end;
    FNameListBox.Call;
    end;
  end;
end;



function TMiniTokyo.GetCategoryNumber(Index:String):Integer;
var i:Integer;
begin;
for i:=0 to FCategories.Count-1 do
  if Index=FCategories[i] then
    begin;
    Result:=i;
    Exit;
    end;
Result:=FCategories.Add(Index);
end;

procedure TMiniTokyo.SaveToFile(const FileName: String);
var i,Count:Integer;
    F:TFileStream;
begin;
try
  F:=TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  WriteString(F,DBaseHeader,Length(DBaseHeader));
  Count:=FCategories.Count;F.Write(Count,SizeOf(Count));
  For i:=0 to Count-1 do WriteString(F,FCategories[i]);
  Count:=FTypes.Count;F.Write(Count,SizeOf(Count));
  For i:=0 to Count-1 do WriteString(F,FTypes[i]);
  FBase.Save(F);
  F.Free;
except
  on E:Exception do begin;end;
end;    
end;

procedure TMiniTokyo.LoadFromFile(const FileName: String);
var i,Count:Integer;
    F:TFileStream;
    Header:String;
begin;
(*
DB File Structure:
int CategoriesCount
  {
  int Length
  char[] Category
  }
int RecordsCount
  {
  record
  }
*)
FTypes.Free;
FTypes:=TStringList.Create;
FCategories.Free;
FCategories:=TStringList.Create;
FBase.Free;
FDataBaseFileName:=FileName;
if FileExists(FileName) then
  begin;
  try
    F:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    Header:=ReadString(F,Length(DBaseHeader));
    if Header<>DBaseHeader then
      begin;
      F.Free;
      raise Exception.Create('');
      end;
    Count:=0;F.Read(Count,SizeOf(Count));
    For i:=1 to Count do FCategories.Add(ReadString(F));
    Count:=0;F.Read(Count,SizeOf(Count));
    For i:=1 to Count do FTypes.Add(ReadString(F));
    FBase:=TDBaseList.Load(F);
    F.Free;
  except
    on E:Exception do
      begin;
      ErrorLoadDBase(Header, FileName);
      FCategories.Free;
      FCategories:=TStringList.Create;
      FBase.Free;
      FBase:=TDBaseList.Create;
      FTypes.Free;
      FTypes:=TStringList.Create;
      end;
  end;
  end
else
  begin;
  FBase:=TDBaseList.Create;
  end;
if Assigned(FCategoryComboBox.OnChange) then FCategoryComboBox.OnChange(Self);
end;

procedure TMiniTokyo.MergeWithFile(const FileName: String);
var i,Count:Integer;
    F:TFileStream;
    Header:STring;
begin;
if FileExists(FileName) then
  begin;
  try
    F:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    Header:=ReadString(F,Length(DBaseHeader));
    if Header<>DBaseHeader then
      begin;
      F.Free;
      raise Exception.Create('');
      end;
    Count:=0;F.Read(Count,SizeOf(Count));
    For i:=1 to Count do GetCategoryNumber(ReadString(F));
    Count:=0;F.Read(Count,SizeOf(Count));
    For i:=0 to Count-1 do
      TypeNames[i]:=ReadString(F);
    FBase.Merge(F);
    F.Free;
  except
    on E:Exception do begin;ErrorLoadDBase(Header, FileName);end;
  end;
  end;
if Assigned(FCategoryComboBox.OnChange) then FCategoryComboBox.OnChange(Self);
end;


constructor TMiniTokyo.Create(const FileName:String; ListBox:TEventedListBox; ComboBox, TypeBox:TComboBox);
begin;
Inherited Create;
FDataBaseFileName:=FileName;
FCategories:=TStringList.Create;
FTypes:=TStringList.Create;
FNameListBox:=ListBox;
FCategoryComboBox:=ComboBox;
FTypeComboBox:=TypeBox;
if Assigned(FCategoryComboBox) then
  FCategoryComboBox.OnChange:=CategoryComboBoxChange;
if Assigned(FTypeComboBox) then
  FTypeComboBox.OnChange:=TypeComboBoxChange;
LoadFromFile(FileName);
end;

constructor TMiniTokyo.Create(const FileName:String);
begin;
Create(FileName,nil,nil,nil);
end;

destructor TMiniTokyo.Destroy;
begin;
SaveToFile(FDataBaseFileName);
FCategories.Free;
FTypes.Free;
FBase.Free;
inherited Destroy;
end;

procedure TMiniTokyo.UpdateDataBase(F:TStream; ImagesOnly:Boolean);
var Tag:String;
    State1,State2,State3,State4,SelID,ItemType,State4ItemType,State2Count:Integer;
    State1OptGroup,State1ID,State1Name,State4Value:String;
    Props,Values:TMyStringList;
    Index:Integer;
begin;
if (not Assigned(F))or(F.Size=0) then Exit;
F.Position:=0;
Props:=TMyStringList.Create;
Values:=TMyStringList.Create;
State1:=0;
State2:=0;
State3:=0;
State4:=0;
SelID:=-1;
State4ItemType:=0;
State2Count:=0;
ItemType:=0;
{$ifdef DEBUG}
Props.CommaText;
ST(f,10);
{$endif}

repeat
  Tag:=GetNextTag(f);
  GetTagProperties(F,Props,Values);

  //state1 - load list
  if State1=0 then
    if Tag='SELECT' then
      begin;
      if Props.Find('TITLE',Index) then
        if Values[Index]='CATEGORY' then
          Inc(State1);
      end;
  if State1=1 then
    if Tag='OPTGROUP' then
      begin;
      if Props.Find('LABEL',Index) then
        begin;
        State1OptGroup:=Values[Index];
        Inc(State1);
        end;
      end
    else if Tag='/SELECT' then
      Dec(State1);
  if State1=2 then
    if Tag='OPTION' then
      begin;
      if Props.Find('VALUE',Index) then
        begin;
        State1ID:=Values[Index];
        if Props.Find('SELECTED',Index) then
          begin;
          try
            SelID:=StrToInt(State1ID);
          except
            on E:Exception do begin;end;
          end;
          end;
        CloseTag(F);
        if not ImagesOnly then
          begin;
          State1Name:=GetMidTagText(F);
          try
            FBase.Add(TDBaseRecord.Create(State1Name,StrToInt(State1ID),CategoryNumbers[State1OptGroup]));
          except
            on E:Exception do begin;end;
          end;
          end;
        end;
      end
    else if Tag='/OPTGROUP' then
      Dec(State1);

  //state2 - load links
  if SelID<>-1 then
  if State2=0 then
    if Tag='TABLE' then
      begin;
      if Props.Find('CLASS',Index) then
        if Values[Index]='GALLERY' then
          Inc(State2);
      end;
  if State2=1 then
    if Tag='TR' then
      Inc(State2)
    else if Tag='/TABLE' then
      Dec(State2);
  if State2=2 then
    if Tag='TD' then
      Inc(State2)
    else if Tag='/TR' then
      Dec(State2);
  if State2=3 then
    if Tag='P' then
      Inc(State2)
    else if Tag='/TD' then
      Dec(State2);
  if State2=4 then
    if Tag='A' then
      begin;
      if Props.Find('HREF',Index) then
        begin;
        FBase[SelID].Images.Add(GetNumber(Values[Index],-1),ItemType);
        Inc(State2Count);
        end;
      end
    else if Tag='/P' then
      Dec(State2);

  //State3 - load ImageCount
  if SelID<>-1 then
  if State3=0 then
    if Tag='DIV' then
      begin;
      if Props.Find('CLASS',Index) then
        if Values[Index]='OUTER ' then
          Inc(State3);
      end;
  if State3=1 then
    if Tag='DIV' then
      begin;
      if Props.Find('CLASS',Index) then
        if Values[Index]='INNER ' then
          Inc(State3);
      end
    else if Tag='/DIV' then
      Dec(State3);
  if State3=2 then
    if Tag='TABLE' then
      begin;
      if Props.Find('CLASS',Index) then
        if Values[Index]='WIDE' then
          Inc(State3);
      end
    else if Tag='/DIV' then
      Dec(State3);
  if State3=3 then
    if Tag='TR' then
      Inc(State3)
    else if Tag='/TABLE' then
      Dec(State3);
  if State3=4 then
    if Tag='TD' then
      Inc(State3)
    else if Tag='/TR' then
      Dec(State3);
  if State3=5 then
    if Tag='P' then
      begin;
      if Props.Find('CLASS',Index) then
        if Values[Index]='R M1' then
          Inc(State3);
      end
    else if Tag='/TD' then
      Dec(State3);
  if State3=6 then
    if Tag='SPAN' then
      begin;
      if (not(Props.Find('CLASS',Index)))and(Props.Find('TITLE',Index)) then
        begin;
        FBase[SelID].ImagesCount[ItemType]:=GetNumber(Values[Index],3);
        end;
      end
    else if Tag='/P' then
      Dec(State3);

  //State4 - Get ItemType
  if State4=0 then
    if Tag='SELECT' then
      begin;
      if Props.Find('TITLE',Index) then
        if Values[Index]='TYPE' then
          Inc(State4);
      end;
  if State4=1 then
    if Tag='OPTGROUP' then
      Inc(State4)
    else if Tag='/SELECT' then
      Dec(State4);
  if State4=2 then
    if Tag='OPTION' then
      begin;
      if Props.Find('VALUE',Index) then
        begin;
        State4Value:=Values[Index];
        try
          State4ItemType:=StrToInt(State4Value);
        except
          on E:Exception do begin;State4ItemType:=0;end;
        end;
        if Props.Find('SELECTED',Index) then
            ItemType:=State4ItemType;
        if not ImagesOnly then
          begin;
          CloseTag(F);          
          TypeNames[State4ItemType]:=GetMidTagText(F);
          end;
        end;
      end
    else if Tag='/OPTGROUP' then
      Dec(State4);

until F.Position>=F.Size;

if FBase[SelID].ImagesCount[ItemType]=-1 then
  FBase[SelID].ImagesCount[ItemType]:=State2Count;
Props.Free;
Values.Free;
DataChanged(ImagesOnly);
end;

procedure TMiniTokyo.GetTagProperties(F:TStream; PropList,ValueList:TStringList);
var Prop,Value:String;
    C:Char;
begin;
PropList.Clear;
ValueList.Clear;
repeat
  if GetNextTagProperty(F,Prop,Value) then
    begin;
    PropList.Add(Prop);
    ValueList.Add(Value);
    end;
  F.Read(c,1);
until (c='>')or(F.Position>=F.Size);
if (c='>')and(F.Position<>F.Size) then F.Seek(F.Position-1,soFromBeginning);
end;

function TMiniTokyo.GetNextTagProperty(F:TStream; var Prop,Value:String):Boolean;
var C:Char;
begin;
Result:=False;
Prop:='';
Prop:=GetWord(F,['>','=']);
F.Read(C,1);
if c='=' then
  begin;
  F.Read(C,1);
  F.Seek(F.Position-1,soFromBeginning);
  If C in ['''','"'] then
    Value:=GetBlockedSequence(F,['>'])
  else
    Value:=GetWord(F,['>']);
  Result:=True;
  end
else
  F.Seek(F.Position-1,soFromBeginning);
end;


function TMiniTokyo.GetNextTag(F:TStream):String;
var C:Char;
begin;
Result:='';
repeat
  F.Read(c,1);
  if c='<' then
    begin;
    Result:=GetWord(f,['>']);
    Exit;
    end;
until F.Position>=F.Size;
end;

function TMiniTokyo.GetBlockedSequence(F:TStream; Blocker:TSysCharSet):String;
var C,BlockerChar:Char;
begin;
Result:='';
F.Read(BlockerChar,1);
repeat
  F.Read(C,1);
  if (C<>BlockerChar)and(not(C in Blocker)) then Result:=Result+C;
until (C=BlockerChar)or(C in Blocker)or(F.Position>=F.Size);
if c in Blocker then F.Seek(F.Position-1,soFromBeginning);
Result:=UpperCase(Result);
end;

function TMiniTokyo.GetWord(F:TStream; Blocker:TSysCharSet):String;
var C:Char;
begin;
Result:='';
repeat
  F.Read(c,1);
  if (not(c in [' ',#13,#10]))and(not(c in Blocker)) then Result:=Result+c;
until (c in [' ',#13,#10])or(c in Blocker)or(F.Position>=F.Size);
if c in Blocker then F.Seek(F.Position-1,soFromBeginning);
Result:=UpperCase(Result);
end;

procedure TMiniTokyo.CloseTag(F:TStream);
var C:Char;
begin;
repeat
  F.Read(C,1);
until (c='>')or(F.Position>=F.Size);
end;

function TMiniTokyo.GetMidTagText(F:TStream):String;
var C:Char;
begin;
Result:='';
repeat
  F.Read(C,1);
  if C<>'<' then Result:=Result+C;
until (c='<')or(F.Position>=F.Size);
if c='<' then F.Seek(F.Position-1,soFromBeginning);
end;

end.
