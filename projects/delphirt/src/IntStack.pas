unit IntStack;

interface
uses Classes;

type
  TIntStack = Class(TObject)
  private
    FList: TList;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(Item: Integer);
    function Pop:Integer;
    function Last:Integer;
    procedure Clear;
  end;

implementation

constructor TIntStack.Create;
begin;
inherited Create;
FList:=TList.Create;
end;

destructor TIntStack.Destroy;
begin;
FList.Free;
inherited Destroy;
end;

procedure TIntStack.Push(Item: Integer);
begin;
FList.Add(Pointer(Item));
end;

function TIntStack.Pop:Integer;
begin;
Result:=Integer(FList[FList.Count-1]);
FList.Delete(FList.Count-1);
end;

function TIntStack.Last:Integer;
begin;
if (FList.Count=0) then
  Result:=-1
else
  Result:=Integer(FList[FList.Count-1]);
end;

procedure TIntStack.Clear;
begin;
FList.Clear;
end;


end.
