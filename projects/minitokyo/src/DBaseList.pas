unit DBaseList;

interface
uses Classes, SysUtils, IntegerSet;

type
  TDBaseRecord = class;

  TDBaseRecord = class(TObject)
  private
    FID:Integer;
    FCategory:Byte;
    FLastUpdate:TDateTime;
    FName:String;
    FImages:TMiniTokyoIntegerSet;
    FCounts:TMap_Int_Int;
  protected
    function GetImages:TMiniTokyoIntegerSet;
    function GetLastUpdate:TDateTime;
  public
    constructor Create(const Name:String; ID,Category:Integer);
    destructor Destroy; override;
    constructor Load(F: TStream);
    procedure Save(F: TStream);
    procedure Clear;
    property ID:Integer read FID write FID;
    property Category:Byte read FCategory write FCategory;
    property ImagesCount:TMap_Int_Int read FCounts;
    property Name:String read FName write FName;
    property Images:TMiniTokyoIntegerSet read GetImages;
    property LastUpdate:TDateTime read GetLastUpdate;
  end;

  TDBaseList = class(TObject)
  private
    FList:TList;
    FLastFoundID:Integer;
    FLastFoundIndex:Integer;
  protected
    function Find(ID:Integer; var Index:Integer):Boolean; overload;
    function Find(Name: String; var Index:Integer):Boolean; overload;
    function GetCount:Integer;
    function Get(ID: Integer): TDBaseRecord;
    function GetByName(Name:String):TDBaseRecord;
    function GetByIndex(Index:Integer):TDBaseRecord;
    function GetByImage(Index:Integer):TDBaseRecord;
  public
    constructor Create;
    constructor Load(F:TStream);
    procedure Save(F:TStream);
    procedure Merge(F:TStream);
    destructor Destroy; override;
    property Items[ID: Integer]: TDBaseRecord read Get; default;
    property ItemsByName[Name: String]: TDBaseRecord read GetByName;
    property ItemsByIndex[Index: Integer]: TDBaseRecord read GetByIndex;
    property ItemsByImage[Index: Integer]: TDBaseRecord read GetByImage;
    procedure Add(Item: TDBaseRecord);
    property Count:Integer read GetCount;
  end;

function ReadString(F:TStream):String; overload;
function ReadString(F:TStream; Length:Integer):String; overload;
procedure WriteString(F:TStream; const S:String); overload;
procedure WriteString(F:TStream; const S:String; const Length:Integer); overload;


implementation
Uses Math;

function ReadString(F:TStream):String; overload;
var
  Buf:PChar;
  Length: Integer;
begin;
Length:=0;F.Read(Length,SizeOf(Length));
if Length<>0 then
  begin;
  GetMem(Buf,Length+1);
  F.Read(Buf^,Length);
  Buf[Length]:=#0;
  Result:=Buf;
  FreeMem(Buf,Length+1);
  end
else
  Result:='';
end;

function ReadString(F:TStream; Length:Integer):String; overload;
var
  Buf:PChar;
begin;
if Length>0 then
  begin;
  GetMem(Buf,Length+1);
  F.Read(Buf^,Length);
  Buf[Length]:=#0;
  Result:=Buf;
  FreeMem(Buf,Length+1);
  end
else
  Result:='';
end;

procedure WriteString(F:TStream; const S:String); overload;
var i,L:Integer;
begin;
L:=Length(S);F.Write(L,SizeOf(L));
for i:=1 to Length(S) do F.Write(S[i],SizeOf(S[i]));
end;

procedure WriteString(F:TStream; const S:String; const Length:Integer); overload;
var i:Integer;
begin;
for i:=1 to Length do F.Write(S[i],SizeOf(S[i]));
end;

//============================================================================\\
// TDBaseRecord
//============================================================================\\
procedure TDBaseRecord.Clear;
begin;
FImages.Clear;
FCounts.Clear;
end;

destructor TDBaseRecord.Destroy;
begin;
FCounts.Free;
inherited Destroy;
end;

constructor TDBaseRecord.Create(const Name:String; ID,Category:Integer);
begin;
inherited Create;
FName:=Name;
FCategory:=Category;
FillChar(FCounts,Sizeof(FCounts),-1);
FID:=ID;
FImages:=nil;
FCounts:=TMap_Int_Int.Create;
FCounts.ErrorValue:=-1;
FLastUpdate:=Now;
end;

constructor TDBaseRecord.Load(F: TStream);
var i,Count,Value,AType:Integer;
begin;
Create('',0,0);
(*
  int ID
  byte Category
  int TypeCountsCount
  int[] TypeCounts
  tdatetime LastUpdate
  int Length
  char[] Name
  int Images.Count
  int[] Images[]
*)
F.Read(FID, Sizeof(FID));
F.Read(FCategory, Sizeof(FCategory));
Count:=0;
F.Read(Count,Sizeof(Count));
for i:=1 to Count do
  begin;
  F.Read(Value,SizeOf(Value));
  F.Read(AType,SizeOf(AType));
  Images.Add(Value,AType);
  end;
Count:=0;
F.Read(Count,Sizeof(Count));
for i:=1 to Count do
  begin;
  F.Read(Value,SizeOf(Value));
  F.Read(AType,SizeOf(AType));
  ImagesCount.Add(AType,Value,True);
  end;
F.Read(FLastUpdate, Sizeof(FLastUpdate));
FName:=ReadString(F);
Images.Changed:=False;
end;

procedure TDBaseRecord.Save(F: TStream);
var i,Count,Value,AType:Integer;
begin;
F.Write(FID, Sizeof(FID));
F.Write(FCategory, Sizeof(FCategory));
Count:=Images.Count[0];
F.Write(Count,Sizeof(Count));
for i:=0 to Count-1 do
  begin;
  Value:=Images.ItemsByIndex[i];
  AType:=Images.TypesByIndex[i];
  F.Write(Value,SizeOf(Value));
  F.Write(AType,SizeOf(AType));
  end;
Count:=ImagesCount.Count;
F.Write(Count,Sizeof(Count));
for i:=0 to Count-1 do
  begin;
  AType:=ImagesCount.ItemsByIndex[i];
  Value:=ImagesCount.ValuesByIndex[i];
  F.Write(Value,SizeOf(Value));
  F.Write(AType,SizeOf(AType));
  end;
F.Write(FLastUpdate, Sizeof(FLastUpdate));
WriteString(F, FName);
end;

function TDBaseRecord.GetLastUpdate:TDateTime;
begin;
if Assigned(FImages) then if FImages.Changed then
  begin;
  FImages.Changed:=False;
  FLastUpdate:=Now;
  end;
Result:=FLastUpdate;
end;

function TDBaseRecord.GetImages:TMiniTokyoIntegerSet;
begin;
if not Assigned(FImages) then
  FImages:=TMiniTokyoIntegerSet.Create;
Result:=FImages;
end;

//============================================================================\\
// TDBaseList
//============================================================================\\
function TDBaseList.Find(ID:Integer; var Index:Integer):Boolean;
var
  L, H, I: Integer;
begin
if FLastFoundID=ID then
  begin;
  Index:=FLastFoundIndex;
  Result:=True;
  Exit;
  end;
Result:=False;
L:=0;
H:=FList.Count - 1;
while L<=H do
  begin
  I:=(L+H)shr 1;
  if ID<TDBaseRecord(FList[I]).ID then L:=I+1
  else if ID>TDBaseRecord(FList[I]).ID then H:=I-1
  else
    begin
    Result:=True;
    L:=I;
    FLastFoundID:=ID;
    FLastFoundIndex:=L;
    Break;
    end;
  end;
Index:=L;
end;

function TDBaseList.Find(Name: String; var Index:Integer):Boolean;
var i:Integer;
begin;
Result:=False;
for i:=0 to FList.Count-1 do
  if TDBaseRecord(FList[I]).Name=Name then
    begin;
    Result:=True;
    Index:=i;
    Exit;
    end;
end;

function TDBaseList.GetByImage(Index:Integer):TDBaseRecord;
var i:Integer;
begin;
if ItemsByIndex[FLastFoundIndex].Images.Includes(Index) then
  Result:=ItemsByIndex[FLastFoundIndex]
else
  begin;
  Result:=nil;
  for i:=0 to Count-1 do if ItemsByIndex[i].Images.Includes(Index) then
    begin;
    Result:=ItemsByIndex[i];
    FLastFoundIndex:=i;
    FLastFoundID:=ItemsByIndex[i].ID;
    Break;
    end;
  end;
end;

function TDBaseList.GetByIndex(Index:Integer):TDBaseRecord;
begin;
Result:=TDBaseRecord(FList[Index]);
end;

function TDBaseList.GetCount:Integer;
begin;
Result:=FList.Count;
end;

constructor TDBaseList.Load(F:TStream);
var i,Count:Integer;
begin;
Create;
Count:=0;F.Read(Count,SizeOf(Count));
For i:=1 to Count do
  Add(TDBaseRecord.Load(F));
end;

procedure TDBaseList.Merge(F:TStream);
var i,Count:Integer;
begin;
Count:=0;F.Read(Count,SizeOf(Count));
For i:=1 to Count do
  Add(TDBaseRecord.Load(F));
end;

procedure TDBaseList.Save(F:TStream);
var i,C:Integer;
begin;
C:=Count;
F.Write(C,SizeOf(C));
For i:=0 to Count-1 do ItemsByIndex[i].Save(F);
end;

constructor TDBaseList.Create;
begin;
inherited Create;
FLastFoundID:=-1;
FLastFoundIndex:=0;
FList:=TList.Create;
end;

destructor TDBaseList.Destroy;
var I:Integer;
begin;
For i:=0 to Count-1 do ItemsByIndex[i].Free;
FList.Free;
inherited Destroy;
end;

function TDBaseList.Get(ID: Integer): TDBaseRecord;
var Index:Integer;
begin;
if Find(ID, Index) then
  Result:=TDBaseRecord(FList[Index])
else
  Result:=nil;
end;

function TDBaseList.GetByName(Name:String):TDBaseRecord;
var Index:Integer;
begin;
if Find(Name, Index) then
  Result:=TDBaseRecord(FList[Index])
else
  Result:=nil;
end;

procedure TDBaseList.Add(Item: TDBaseRecord);
var Index,i:Integer;
begin;
if not Find(Item.ID, Index) then
  FList.Insert(Index,Pointer(Item))
else
  begin;
  for i:=0 to Item.ImagesCount.Count-1 do
    TDBaseRecord(FList[Index]).ImagesCount[Item.ImagesCount.ItemsByIndex[i]]:=
      Max(Item.ImagesCount.ValuesByIndex[i],TDBaseRecord(FList[Index]).ImagesCount[Item.ImagesCount.ItemsByIndex[i]]);
  if (Assigned(Item.Images)) then
    For i:=0 to Item.Images.Count[0]-1 do TDBaseRecord(FList[Index]).Images.Add(Item.Images[i], Item.Images.TypesByIndex[i]);
  Item.Free;
  end;
end;

end.

