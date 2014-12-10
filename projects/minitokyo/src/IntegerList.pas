unit IntegerList;

interface
uses Classes;

type
  PIntegerList = ^TIntegerArray;
  TIntegerArray = array[0..MaxListSize - 1] of Integer;


  TIntegerList = class(TList)
  private
  protected
    function Get(Index: Integer): Integer;
    procedure Put(Index: Integer; Item: Integer);
    function GetIntegerList: PIntegerList;

    function Extract(Item: Pointer): Pointer; virtual; abstract;
    function IndexOf(Item: Pointer): Integer; virtual; abstract;
    procedure Insert(Index: Integer; Item: Pointer); virtual; abstract;
    function Last: Pointer; virtual; abstract;
    function Remove(Item: Pointer): Integer; virtual; abstract;
    procedure Sort(Compare: TListSortCompare); virtual; abstract;
  public
    property Items[Index: Integer]: Integer read Get write Put; default;
    property List: PIntegerList read GetIntegerList;
    function Add(Item: Integer): Integer;
    //procedure Sort;
  end;

implementation

{procedure TIntegerList.Sort;
begin;

end;}

function TIntegerList.Get(Index: Integer): Integer;
begin;
Result:=Integer(inherited Get(Index));
end;

procedure TIntegerList.Put(Index: Integer; Item: Integer);
begin;
inherited Put(Index,Pointer(Item));
end;

function TIntegerList.Add(Item: Integer): Integer;
begin;
Result:=inherited Add(Pointer(Item));
end;

function TIntegerList.GetIntegerList: PIntegerList;
begin;
Result:=PIntegerList(inherited List);
end;

end.
