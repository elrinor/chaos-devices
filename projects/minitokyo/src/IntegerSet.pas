unit IntegerSet;

interface
uses Classes;

type
  TIntegerSet = class(TObject)
  private
    FList: TList;
  protected
    function GetCount:Integer;
    function GetItem(Index:Integer):Integer;
    function Find(Item:Integer; var Index:Integer):Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Integer);
    procedure Clear;
    function Includes(Item:Integer):Boolean;
    property Count:Integer read GetCount;
    property Items[Index: Integer]:Integer read GetItem; default;
  end;

type
  TMap_Int_Int = class(TIntegerSet)
  private
    FErrorValue:Integer;
    FValues: TList;
  protected
    function GetItem2(Item:Integer):Integer;
    function GetValue(Index:Integer):Integer;
    procedure SetItem2(Item:Integer; Value:Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(Item: Integer; Value: Integer; Replace:Boolean):Boolean;
    property Items[Item:Integer]:Integer read GetItem2 write SetItem2; default;
    property ItemsByIndex[Index:Integer]:Integer read GetItem;
    property ValuesByIndex[Index:Integer]:Integer read GetValue;
    property ErrorValue:Integer read FErrorValue write FErrorValue;
  end;

type
  TMiniTokyoIntegerSet = class(TObject)
  private
    FMap: TMap_Int_Int;
    FTypeCounts: TMap_Int_Int;
    FChanged: Boolean;
  protected
    function GetCountByType(AType:Integer):Integer;
    function GetItemByIndex(Index:Integer):Integer;
    function GetItemTypeByItem(Item:Integer):Integer;
    function GetTypeByIndex(Index:Integer):Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Item: Integer; AType: Integer);
    function AsText:String; overload;
    function AsText(AType:Integer):String; overload;
    function Includes(Item:Integer):Boolean;
    property Changed:Boolean read FChanged write FChanged;
    property Count[AType:Integer]:Integer read GetCountByType;
    property ItemsByIndex[Index:Integer]:Integer read GetItemByIndex; default;
    property TypesByIndex[Index:Integer]:Integer read GetTypeByIndex;
    property Types[Item:Integer]:Integer read GetItemTypeByItem;
  end;

implementation
uses SysUtils;

//============================================================================\\
// TMiniTokyoIntegerSet
//============================================================================\\
function TMiniTokyoIntegerSet.GetTypeByIndex(Index:Integer):Integer;
begin;
Result:=FMap.ValuesByIndex[Index];
end;

function TMiniTokyoIntegerSet.Includes(Item:Integer):Boolean;
var i:Integer;
begin;
Result:=FMap.Find(Item,i);
end;

function TMiniTokyoIntegerSet.GetItemByIndex(Index:Integer):Integer;
begin;
Result:=FMap.ItemsByIndex[Index];
end;

function TMiniTokyoIntegerSet.GetItemTypeByItem(Item:Integer):Integer;
begin;
Result:=FMap.Items[Item];
end;

function TMiniTokyoIntegerSet.GetCountByType(AType:Integer):Integer;
begin;
Result:=FTypeCounts[AType];
end;

function TMiniTokyoIntegerSet.AsText(AType:Integer):String;
var i:integer;
begin;
if AType=0 then
  Result:=AsText
else
  begin;
  Result:='';
  for i:=0 to FMap.Count-1 do if FMap.ValuesByIndex[i]=AType then
    Result:=Result+IntToStr(FMap.ItemsByIndex[i])+', ';
  if Length(Result)>0 then
    begin;
    Delete(Result,Length(Result),1);
    Result[Length(Result)]:='.';
    end;
  end;  
end;

function TMiniTokyoIntegerSet.AsText:String;
var i:integer;
begin;
Result:='';
for i:=0 to FMap.Count-2 do Result:=Result+IntToStr(FMap.ItemsByIndex[i])+', ';
if FMap.Count>0 then Result:=Result+IntToStr(FMap.ItemsByIndex[FMap.Count-1])+'.';
end;

constructor TMiniTokyoIntegerSet.Create;
begin;
inherited Create;
FTypeCounts:=TMap_Int_Int.Create;
FMap:=TMap_Int_Int.Create;
FChanged:=False;
end;

procedure TMiniTokyoIntegerSet.Clear;
begin;
FMap.Clear;
FTypeCounts.Clear;
FChanged:=True;
end;

procedure TMiniTokyoIntegerSet.Add(Item: Integer; AType: Integer);
var Index:Integer;
begin;
if not FMap.Find(Item, Index) then
  begin;
  FMap.FList.Insert(Index, Pointer(Item));
  FMap.FValues.Insert(Index,Pointer(AType));
  FTypeCounts[AType]:=FTypeCounts[AType]+1;
  if AType<>0 then FTypeCounts[0]:=FTypeCounts[0]+1;
  FChanged:=True;
  end
else if AType<>0 then
  begin;
  FTypeCounts[AType]:=FTypeCounts[AType]+1;
  if (Integer(FMap.FValues[Index])<>0) then
    FTypeCounts[Integer(FMap.FValues[Index])]:=FTypeCounts[Integer(FMap.FValues[Index])]-1;
  FMap.FValues[Index]:=Pointer(AType);
  FChanged:=True;
  end;
end;

destructor TMiniTokyoIntegerSet.Destroy;
begin;
FTypeCounts.Free;
FMap.Free;
inherited Destroy;
end;


//============================================================================\\
// TMap_Int_Int
//============================================================================\\
procedure TMap_Int_Int.SetItem2(Item:Integer; Value:Integer);
begin;
Add(Item, Value, True);
end;

function TMap_Int_Int.GetValue(Index:Integer):Integer;
begin;
Result:=Integer(FValues[Index]);
end;

procedure TMap_Int_Int.Clear;
begin;
inherited Clear;
FValues.Clear;
end;

function TMap_Int_Int.GetItem2(Item:Integer):Integer;
var i:Integer;
begin;
if Find(Item,i) then
  Result:=Integer(FValues[i])
else
  Result:=ErrorValue;
end;

function TMap_Int_Int.Add(Item: Integer; Value: Integer; Replace:Boolean):Boolean;
var Index:Integer;
begin;
if not Find(Item, Index) then
  begin;
  FList.Insert(Index, Pointer(Item));
  FValues.Insert(Index,Pointer(Value));
  Result:=True;
  end
else if Replace then
  begin;
  FValues[Index]:=Pointer(Value);
  Result:=True;
  end;
end;

constructor TMap_Int_Int.Create;
begin;
inherited Create;
FValues:=TList.Create;
FErrorValue:=0;
end;

destructor TMap_Int_Int.Destroy;
begin;
FValues.Free;
inherited Destroy;
end;


//============================================================================\\
// TIntegerSet
//============================================================================\\
function TIntegerSet.Find(Item:Integer; var Index:Integer):Boolean;
var
  L, H, I: Integer;
begin
Result := False;
L := 0;
H := FList.Count - 1;
while L <= H do
  begin
  I := (L + H) shr 1;
  if Item<Items[I] then L := I + 1
  else if Item>Items[I] then H := I - 1
  else
    begin
    H := I - 1;
    Result := True;
    L := I;
    end;
  end;
Index := L;
end;

procedure TIntegerSet.Clear;
begin;
FList.Clear;
end;

function TIntegerSet.GetItem(Index:Integer):Integer;
begin;
Result:=Integer(FList[Index]);
end;

function TIntegerSet.GetCount:Integer;
begin;
Result:=FList.Count;
end;

procedure TIntegerSet.Add(Item: Integer);
var Index:Integer;
begin;
if not Find(Item, Index) then
  FList.Insert(Index, Pointer(Item));
end;

constructor TIntegerSet.Create;
begin;
inherited Create;
FList:=TList.Create;
end;

destructor TIntegerSet.Destroy;
begin;
FList.Free;
inherited Destroy;
end;

function TIntegerSet.Includes(Item:Integer):Boolean;
var Index:Integer;
begin;
Result:=Self.Find(Item,Index);
end;


end.
