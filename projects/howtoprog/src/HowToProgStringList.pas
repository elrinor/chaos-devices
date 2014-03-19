unit HowToProgStringList;

interface
uses HTPConstants,ConsoleUtils,Options,Output;

type THowToProgStringList = class(TObject)
     private
       FSyntax:array[1..MAX_STRING_NUMBER,1..MAX_STRING_LENGTH] of byte;
       FStrings:array[1..MAX_STRING_NUMBER] of String;
     public
       // procedures&functions
       Function GetString(Index: Integer): String;
       Procedure SetString(Index: Integer; S:String);

       Procedure ReDrawStrings(const Index: integer);
       Procedure ReDrawString(const Index: integer);

       Procedure InsertString(Index: Integer; const S: string);
       Procedure InsertCharacter(const StrNo,SymbNo:integer; const C:Char);
       Procedure InsertSubStr(const StrNo,SymbNo:integer; const SubStr:String);

       Procedure DeleteCharacters(const StrNo,SymbNo,Count:Integer);
       Procedure RemoveString(const StrNo:Integer);

       Procedure ReCountSyntax(Index: Integer);

       Constructor CreateList;
       //properties
       property Strings[Index: Integer]: string read GetString write SetString; default;
     end;

var Strings:THowToProgStringList;


implementation
uses math,sysutils;
//============================================================================\\
//  Функции и процедуры обьекта THowToProgStringList
//============================================================================\\
// Конструктор
Constructor THowToProgStringList.CreateList;
var i:Integer;
begin;
for i:=1 to MAX_STRING_NUMBER do FStrings[i]:=CEmptyString78;
FillChar(FSyntax,SizeOf(FSyntax),0);
end;

// Считывание строки
Function THowToProgStringList.GetString(Index:Integer): String;
begin;
  if (Index>0)and(Index<=MAX_STRING_NUMBER) then Result:=FStrings[Index]
  else Result:='';
end;

// Присваивание строке значения
Procedure THowToProgStringList.SetString(Index:Integer; S:String);
begin;
  if (Index>0)and(Index<=MAX_STRING_NUMBER) then FStrings[Index]:=S;
end;

// Вставка строки в лист
procedure THowToProgStringList.InsertString(Index: Integer; const S: string);
var i:integer;
begin;
if (Index>0)and(Index<=MAX_STRING_NUMBER) then
  begin;
  for i:=MAX_STRING_NUMBER-1 downto Index do FStrings[i+1]:=FStrings[i];
  for i:=MAX_STRING_NUMBER-1 downto Index do FSyntax[i+1]:=FSyntax[i];
  FStrings[Index]:=S;
  ReCountSyntax(Index);
  ReDrawStrings(Index);
  end;
end;

// Вставка подстроки
Procedure THowToProgStringList.InsertSubStr(const StrNo,SymbNo:integer; const SubStr:String);
begin;
if (StrNo>0)and(StrNo<=MAX_STRING_NUMBER)and(SymbNo>0)then
  begin;
  Insert(SubStr,FStrings[StrNo],SymbNo);
  end;
end;

// Удаление Некоторого количества символов из строки
Procedure THowToProgStringList.DeleteCharacters(const StrNo,SymbNo,Count:Integer);
begin;
if (StrNo>0)and(StrNo<=MAX_STRING_NUMBER)and(SymbNo>0)then
  begin;
  Delete(FStrings[StrNo],SymbNo,Count);
  ReCountSyntax(StrNo);
  end;
end;

// Удаление строки из листа
Procedure THowToProgStringList.RemoveString(const StrNo:Integer);
var i:integer;
begin;
if (StrNo>0)and(StrNo<=MAX_STRING_NUMBER) then
  begin;
  for i:=StrNo to MAX_STRING_NUMBER-1 do FStrings[i]:=FStrings[i+1];
  for i:=StrNo to MAX_STRING_NUMBER-1 do FSyntax[i]:=FSyntax[i+1];
  ReDrawStrings(StrNo);
  end;
end;

// Перерисовка всех строк начиная с номера Index
Procedure THowToProgStringList.ReDrawStrings(const Index:integer);
var i:integer;
begin;
if (Index>=1)and(Index<=ScreenPosY+SCREEN_HEIGHT-1) then
  begin;
  for i:=Index to (ScreenPosY+SCREEN_HEIGHT-1) do ReDrawString(i);
  end;
end;

// Перерисовка строки с номером Index
Procedure THowToProgStringList.ReDrawString(const Index:integer);
var i:integer;
begin;
if (Index>=ScreenPosY)and(Index<=ScreenPosY+SCREEN_HEIGHT-1) then
  begin;
  if (SyntaxHighlight=True) then
    begin;
    Put(2,Index-ScreenPosY+2,Colors[clCodeWindowBack],Colors[clCodeWindowBack],CEmptyString78);
    for i:=ScreenPosX to Min(ScreenPosX+SCREEN_WIDTH-1,Length(FStrings[Index])) do
      if FStrings[Index][i]<>' ' then Put(i-ScreenPosX+2,Index-ScreenPosY+2,Colors[FSyntax[Index,i]],Colors[clCodeWindowBack],FStrings[Index][i]);
    end
  else
    begin;
    Put(2,Index-ScreenPosY+2,DefaultColor,Colors[clCodeWindowBack],Copy(FStrings[Index],ScreenPosX,SCREEN_WIDTH));
    end;
  end;
end;

// Вставка символа в строку
Procedure THowToProgStringList.InsertCharacter(const StrNo,SymbNo:integer; const C:Char);
begin;
If (StrNo>0)and(StrNo<=MAX_STRING_NUMBER)and(SymbNo>0)and(SymbNo<=MAX_STRING_LENGTH) then
  begin;
  Insert(C,FStrings[StrNo],SymbNo);
  ReCountSyntax(StrNo);
  end;
end;

//============================================================================\\
// Пересчет синтаксиса
//============================================================================\\
Procedure THowToProgStringList.ReCountSyntax(Index: Integer);
var i,IdStart,i1,Id_SyntaxType:integer;
    bComment,bString,bIdentifier,bReservedWord:Boolean;
    Ident:String;
begin;

IdStart:=1;
bIdentifier:=False;
bString:=False;
bComment:=False;
FillChar(FSyntax[Index],SizeOf(FSyntax[Index]),0);

// comments, strings & symbols
For i:=1 to Min(Length(FStrings[Index]),MAX_STRING_LENGTH) do
  begin;
  if (FStrings[Index][i]='{')and(bString=False) then
    bComment:=True;
  if (FStrings[Index][i]='}')and(bComment=True) then
    begin;
    bComment:=False;
    FSyntax[Index,i]:=clCOMMENT;
    end;
  if (FStrings[Index][i]='''')and(bComment=False) then
    begin;
    bString:=not(bString);
    FSyntax[Index,i]:=clSTRING;
    end;

  if FSyntax[Index][i]=clWHITESPACE then
  if bComment=True then FSyntax[Index,i]:=clCOMMENT
  else if bString=True then FSyntax[Index,i]:=clSTRING
  else if not(FStrings[Index][i] in NonSymbolCharacters) then FSyntax[Index,i]:=clSYMBOL;
  end;

// Identifiers, Numbers & Reserved Words
For i:=1 to Min(Length(FStrings[Index]),MAX_STRING_LENGTH) do
  begin;
  if ((FSyntax[Index,i]<>clWHITESPACE)or(FStrings[Index][i]=' '))and(bIdentifier=True) then
    begin;
    Ident:=UpperCase(Copy(FStrings[Index],IdStart,i-IdStart));
    bReservedWord:=False;
    bIdentifier:=False;
    for i1:=1 to 48 do if Ident=ReservedWords[i1] then begin;bReservedWord:=True;Break;end;
    if bReservedWord=True then Id_SyntaxType:=clRESERVEDWORD else Id_SyntaxType:=clIDENTIFIER;
    for i1:=IdStart to i-1 do FSyntax[Index,i1]:=Id_SyntaxType;
    end;
  if (bIdentifier=False)and(FStrings[Index][i] in NumberCharacters)and(FSyntax[Index,i]=clWHITESPACE) then
    FSyntax[Index,i]:=clNUMBER;
  if (bIdentifier=False)and(FStrings[Index][i] in IdentifierStartCharacters)and(FSyntax[Index,i]=clWHITESPACE) then
    begin;
    IdStart:=i;
    bIdentifier:=True;
    end;
  end;

end;

initialization
Strings:=THowToProgStringList.CreateList;
finalization
Strings.Destroy;
end.
