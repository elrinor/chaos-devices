unit Commands;

//============================================================================\\
// Interface
//============================================================================\\
interface

procedure ExecuteCommand(Command:String);

var ExecutionResult:record
      EndFile:Boolean;
      Error:Boolean;
    end;

//============================================================================\\
// Implementation
//============================================================================\\
implementation
uses SysUtils,HTPConstants,Options,Output,HowToProgStringList,ConsoleUtils,Log,ConsoleInterface,Math;

//============================================================================\\
// Конвертация и прочее полезное
//============================================================================\\
// Конвертация строки в boolean
Function StrToBool(const s:string):boolean;
begin;
if (s='1')or(UpperCase(s)='TRUE') then Result:=True
else Result:=False;
end;

// Удаляет Ведущие пробелы
function RemoveLeadingSpaces(const S:String):string;
begin;
Result:=s;
if (s<>'') then while (Result[1]=' ') do Delete(Result,1,1);
if (s<>'') then while (Result[Length(Result)]=' ') do Delete(Result,Length(Result),1);
end;

// Вычленяет комманду
function ExtractCommand(const s:String):string;
begin;
Result:=UpperCase(RemoveLeadingSpaces(Copy(s,1,Pos('(',S)-1)));
end;

// Вычленяет аргументы
function ExtractArguments(const s:String):string;
begin;
Result:=RemoveLeadingSpaces(Copy(s,Pos('(',S)+1,Length(S)-Pos('(',S)-1));
end;

//============================================================================\\
// Проверки
//============================================================================\\
// Проверка на правильность Boolean
Function ItIsRigthBoolean(const s:String):Boolean;
begin;
if (s='1')or(s='0')or(UpperCase(s)='TRUE')or(UpperCase(s)='FALSE') then Result:=True
else Result:=False;
end;

// Проверка на правильность Color
Function ItIsRigthColor(const s:String):Boolean;
begin;
if (S='0')or(S='1')or(S='2')or(S='3')or(S='4')or(S='5')or(S='6')or(S='7')or(S='8')or(S='9')or(S='10')or(S='11')or(S='12')or(S='13')or(S='14')or(S='15') then Result:=True
else Result:=False;
end;

// Проверка на правильность Integer
Function ItIsRightInt(const s:String):Boolean;
var i:integer;
begin;
if s='' then Result:=False
else
  begin;
  Result:=True;
  for i:=1 to Length(s) do if not(s[i] in NumberCharacters) then begin;Result:=False;Break;end;
  end;
end;

// Проверка: правильное ли это присваивание
Function ItIsRightSetVarCommand(const s:string;var EqPos:Integer;var Variable,Value:String):Boolean;
begin;
EqPos:=Pos('=',s);
Result:=True;

if EqPos=0 then
  begin;
  AddLogMessage('Ошибка: '''+s+''' - неправильное присваивание. Отсутствует символ ''=''');
  Result:=False
  end
else
  begin;
  Variable:=RemoveLeadingSpaces(Copy(s,1,EqPos-1));
  Value:=RemoveLeadingSpaces(Copy(s,EqPos+1,Length(s)-EqPos));
  if (Variable<>'BASEDELAYFACTOR')and(Variable<>'HOMEALLOWED')and(Variable<>'DOSTEXT')and(Variable<>'WHITESPACE_COLOR')and(Variable<>'COMMENT_COLOR')and(Variable<>'STRING_COLOR')and(Variable<>'IDENTIFIER_COLOR')and(Variable<>'NUMBER_COLOR')and(Variable<>'RESERVED_WORD_COLOR')and(Variable<>'SYMBOL_COLOR')and(Variable<>'SYNTAXHIGHLIGHT')and(Variable<>'CODEWINDOW_BACK_COLOR')and(Variable<>'CODEWINDOW_BORDER_COLOR')and(Variable<>'CODEWINDOW_HEADER_COLOR')and(Variable<>'CODEWINDOW_SCROLLER_COLOR')and(Variable<>'CODEWINDOW_SPRAINER_COLOR')and(Variable<>'CODEWINDOW_BUTTONS_COLOR')and(Variable<>'COMMENTWINDOW_BACK_COLOR')and(Variable<>'COMMENTWINDOW_BORDER_COLOR')and(Variable<>'COMMENTWINDOW_HEADER_COLOR')and(Variable<>'COMMENTWINDOW_TEXT_COLOR')and(Variable<>'DEFAULT_TEXT_COLOR')then
    begin;
    AddLogMessage('Ошибка: '''+Variable+''' - неизвестная переменная. Проверьте правильность написания имени переменной.');
    Result:=False;
    end;

  if Result=True then
    begin;
    if (Variable='WHITESPACE_COLOR')or(Variable='COMMENT_COLOR')or(Variable='STRING_COLOR')or(Variable='IDENTIFIER_COLOR')or(Variable='NUMBER_COLOR')or(Variable='RESERVED_WORD_COLOR')or(Variable='SYMBOL_COLOR')or(Variable='CODEWINDOW_BACK_COLOR')or(Variable='CODEWINDOW_BORDER_COLOR')or(Variable='CODEWINDOW_HEADER_COLOR')or(Variable='CODEWINDOW_SCROLLER_COLOR')or(Variable='CODEWINDOW_SPRAINER_COLOR')or(Variable='CODEWINDOW_BUTTONS_COLOR')or(Variable='COMMENTWINDOW_BACK_COLOR')or(Variable='COMMENTWINDOW_BORDER_COLOR')or(Variable='COMMENTWINDOW_HEADER_COLOR')or(Variable='COMMENTWINDOW_TEXT_COLOR')or(Variable='DEFAULT_TEXT_COLOR')then
      if not(ItIsRigthColor(Value)) then
        begin;
        AddLogMessage('Ошибка: '''+Value+''' - неправильный цвет. Цвет может принимать значения от 0 до 15.');
        Result:=False;
        end;
    if (Variable='HOMEALLOWED')or(Variable='DOSTEXT')or(Variable='SYNTAXHIGHLIGHT') then
      if not(ItIsRigthBoolean(Value)) then
        begin;
        AddLogMessage('Ошибка: '''+Value+''' - неправильное значение логической переменной. Разрешенные значения: TRUE,FALSE или 0,1.');
        Result:=False;
        end;
    if (Variable='BASEDELAYFACTOR') then
      if not(ItIsRightInt(Value)) then
        begin;
        AddLogMessage('Ошибка: '''+Value+''' - неправильное значение целочисленной переменной.');
        Result:=False;
        end;
    end;
  end;
end;

// Проверка на то, является ли команда присваиванием
function ItIsSetVarCommand(const S:string):Boolean;
var EqPos,i:Integer;
begin;
Result:=True;
if pos('(',S)=0 then
  begin;
  EqPos:=Pos('=',S);
  if EqPos<>0 then
    begin;
    for i:=EqPos-1 DownTo 1 do if not ((S[i] in IdentifierCharacters)or(S[i]=' ')) then begin;Result:=False;Break;end;
    end
  else
    Result:=False;
  end
else
  Result:=False;
end;

Function ItIsCommand(const s:string):Boolean;
begin;
If ((Pos('(',S)<>0)and(S[Length(s)]=')'))or(UpperCase(s)='REFRESH')or(Uppercase(s)='END')or(Uppercase(s)='MANUALCODEVIEW') then Result:=True
else Result:=False;
end;


//============================================================================\\
// Выполнение комманд
//============================================================================\\
// КодоСамоИзучение
Procedure ManualCodeView;
var LastBaseDelayFactor:Integer;
    c:char;
begin;
Put(2,23,Colors[clCommentColor],Colors[clCommentWindowBack],Ascii('Используйте курсор чтобы перемещаться по коду.'));
Put(2,24,Colors[clCommentColor],Colors[clCommentWindowBack],Ascii('Нажмите ESC чтобы выйти из режима самостоятельного изучения кода.'));
LastBaseDelayFactor:=BaseDelayFactor;
BaseDelayFactor:=1;
  repeat
  c:=Readkey;
  case c of
    '8': if(PosY-1<=MAX_STRING_NUMBER)and(PosY-1>=1)then MoveCursor(PosX,PosY,PosX,PosY-1);
    '2': if(PosY+1<=MAX_STRING_NUMBER)and(PosY+1>=1)then MoveCursor(PosX,PosY,PosX,PosY+1);
    '6': if(PosX+1<=MAX_STRING_LENGTH)and(PosX+1>=1)then MoveCursor(PosX,PosY,PosX+1,PosY);
    '4': if(PosX-1<=MAX_STRING_LENGTH)and(PosX-1>=1)then MoveCursor(PosX,PosY,PosX-1,PosY);
    #45: begin;PosY:=max(1,PosY-SCREEN_HEIGHT);SpecGotoXY(PosX+1,PosY+1);end;
    #43: begin;PosY:=min(MAX_STRING_NUMBER,PosY+SCREEN_HEIGHT);SpecGotoXY(PosX+1,PosY+1);end;
    #30: begin;PosX:=1;SpecGotoXY(PosX+1,PosY+1);end;
    #31: begin;PosX:=Length(TrimRight(Strings[PosY]))+1;SpecGotoXY(PosX+1,PosY+1);end;
  end;
  until  c=#27;
Put(2,23,Colors[clCommentColor],Colors[clCommentWindowBack],CEmptyString78);
Put(2,24,Colors[clCommentColor],Colors[clCommentWindowBack],CEmptyString78);
BaseDelayFactor:=LastBaseDelayFactor;
end;

// Присваивание
Procedure SetVariable(s:string);
var EqPos:Integer;
    Variable,Value:String;
begin;
s:=UpperCase(s);
if ItIsRightSetVarCommand(s,EqPos,Variable,Value) then
  begin;
  if Variable='HOMEALLOWED' then HomeAllowed:=StrToBool(Value);
  if Variable='DOSTEXT' then DosText:=StrToBool(Value);
  if Variable='BASEDELAYFACTOR' then BaseDelayFactor:=StrToInt(Value);
  if Variable='SYNTAXHIGHLIGHT' then SyntaxHighlight:=StrToBool(Value);
  if (Variable='WHITESPACE_COLOR') then    begin;Colors[0]:=StrToInt(Value);end;
  if (Variable='COMMENT_COLOR') then       begin;Colors[1]:=StrToInt(Value);end;
  if (Variable='STRING_COLOR') then        begin;Colors[2]:=StrToInt(Value);end;
  if (Variable='IDENTIFIER_COLOR') then    begin;Colors[3]:=StrToInt(Value);end;
  if (Variable='NUMBER_COLOR') then        begin;Colors[4]:=StrToInt(Value);end;
  if (Variable='RESERVED_WORD_COLOR') then begin;Colors[5]:=StrToInt(Value);end;
  if (Variable='SYMBOL_COLOR') then        begin;Colors[6]:=StrToInt(Value);end;
  if (Variable='CODEWINDOW_BACK_COLOR') then      begin;Colors[ 7]:=StrToInt(Value);end;
  if (Variable='CODEWINDOW_BORDER_COLOR') then    begin;Colors[ 8]:=StrToInt(Value);end;
  if (Variable='CODEWINDOW_HEADER_COLOR') then    begin;Colors[ 9]:=StrToInt(Value);end;
  if (Variable='CODEWINDOW_SCROLLER_COLOR') then  begin;Colors[10]:=StrToInt(Value);end;
  if (Variable='CODEWINDOW_SPRAINER_COLOR') then  begin;Colors[11]:=StrToInt(Value);end;
  if (Variable='CODEWINDOW_BUTTONS_COLOR') then   begin;Colors[12]:=StrToInt(Value);end;
  if (Variable='COMMENTWINDOW_BACK_COLOR') then   begin;Colors[13]:=StrToInt(Value);end;
  if (Variable='COMMENTWINDOW_BORDER_COLOR') then begin;Colors[14]:=StrToInt(Value);end;
  if (Variable='COMMENTWINDOW_HEADER_COLOR') then begin;Colors[15]:=StrToInt(Value);end;
  if (Variable='COMMENTWINDOW_TEXT_COLOR') then   begin;Colors[16]:=StrToInt(Value);end;
  if (Variable='DEFAULT_TEXT_COLOR') then         begin;DefaultColor:=StrToInt(Value);end;

  ExecutionResult.Error:=False;
  end;
end;

// Выполнение комманды
Procedure DoCommand(const s:string);
var Command,Arguments,arg1,arg2,arg3:String;
    i,x,y,n:Integer;
begin;
if (UpperCase(s)='REFRESH')or(Uppercase(s)='END')or(Uppercase(s)='MANUALCODEVIEW') then
  Command:=UpperCase(s)
else
  begin;
  Command:=ExtractCommand(s);
  Arguments:=ExtractArguments(s);
  end;

if (Command='INSERTSTRING') then
  begin;
  if ItIsRightInt(Arguments) then
    begin;
    y:=StrToInt(Arguments);
    MoveCursor(PosX,PosY,1,y);
    Strings.InsertString(y,CEmptyString78);
    Delay(BaseDelayFactor);
    ExecutionResult.Error:=False;
    end
  else
    AddLogMessage('Ошибка: '''+Arguments+''' - неправильное значение целочисленной переменной.');
  end;

if (Command='WRITE') then
  begin;
  if (pos(',',Arguments)>0)then arg1:=RemoveLeadingSpaces(Copy(Arguments,1,pos(',',Arguments)-1));
  Delete(Arguments,1,pos(',',Arguments));
  if (pos(',',Arguments)>0)then arg2:=RemoveLeadingSpaces(Copy(Arguments,1,pos(',',Arguments)-1));
  Delete(Arguments,1,pos(',',Arguments));
  arg3:=RemoveLeadingSpaces(Arguments);
  if (ItIsRightInt(arg1))and(ItIsRightInt(arg2))and(arg3[1]='''')and(arg3[Length(arg3)]='''') then
    begin;
    Delete(arg3,1,1);
    Delete(arg3,Length(arg3),1);
    x:=StrToInt(arg1);
    y:=StrToInt(arg2);
    MoveCursor(PosX,PosY,x,y);
    DrawStringOutput(arg3);
    ExecutionResult.Error:=False;
    end
  else
    AddLogMessage('Ошибка: '''+arg1+', '+arg2+', '+arg3+''' - неправильные аргументы процедуры Write.');
  end;

If (Command='REMOVESTRING') then
  begin;
  if ItIsRightInt(Arguments) then
    begin;
    y:=StrToInt(Arguments);
    MoveCursor(PosX,PosY,1,y);
    Strings.RemoveString(y);
    Delay(BaseDelayFactor);
    ExecutionResult.Error:=False;
    end
  else
    AddLogMessage('Ошибка: '''+Arguments+''' - неправильное значение целочисленной переменной.');
  end;

if (Command='SHOWHINT') then
  begin;
  if (Arguments[1]='''')and(Arguments[Length(Arguments)]='''') then
    begin;
    Delete(Arguments,1,1);
    Delete(Arguments,Length(Arguments),1);
    ShowHint(Arguments);
    ExecutionResult.Error:=False;
    end
  else
    AddLogMessage('Ошибка: '''+Arguments+''' - неправильное значение строковой переменной.');
  end;

If (Command='GOTOXY') then
  begin;
  if (pos(',',Arguments)>0)then arg1:=RemoveLeadingSpaces(Copy(Arguments,1,pos(',',Arguments)-1));
  Delete(Arguments,1,pos(',',Arguments));
  arg2:=RemoveLeadingSpaces(Arguments);
  if (ItIsRightInt(arg1))and(ItIsRightInt(arg2))then
    begin;
    x:=StrToInt(arg1);
    y:=StrToInt(arg2);
    if (x>0)and(x<=MAX_STRING_LENGTH)and(y>0)and(y<=MAX_STRING_NUMBER) then
      begin;
      MoveCursor(PosX,PosY,x,y);
      ExecutionResult.Error:=False;
      end;
    end
  else
    begin;
    AddLogMessage('Ошибка: '''+arg1+', '+arg2+''' - неправильные аргументы процедуры GotoXY.');
    end;
  end;

if (Command='JUMPTOXY') then
  begin;
  if (pos(',',Arguments)>0)then arg1:=RemoveLeadingSpaces(Copy(Arguments,1,pos(',',Arguments)-1));
  Delete(Arguments,1,pos(',',Arguments));
  arg2:=RemoveLeadingSpaces(Arguments);
  if (ItIsRightInt(arg1))and(ItIsRightInt(arg2))then
    begin;
    x:=StrToInt(arg1);
    y:=StrToInt(arg2);
    if (x>0)and(x<=MAX_STRING_LENGTH)and(y>0)and(y<=MAX_STRING_NUMBER) then
      begin;
      SpecGotoXY(x+1,y+1);
      PosX:=x;
      PosY:=y;
      Delay(BaseDelayFactor);
      ExecutionResult.Error:=False;
      end
    else
      AddLogMessage('Ошибка: '''+arg1+', '+arg2+'''- неправильные аргументы процедуры JumptoXY. Числа выходят за допустимые рамки (от 1 до '+IntToStr(MAX_STRING_LENGTH)+' и от 1 до '+IntToStr(MAX_STRING_NUMBER)+' соответственно).')
    end
  else
    begin;
    AddLogMessage('Ошибка: '''+arg1+', '+arg2+''' - неправильные аргументы процедуры JumptoXY.');
    end;
  end;

if (Command='DELETE') then
  begin;
  if (pos(',',Arguments)>0)then arg1:=RemoveLeadingSpaces(Copy(Arguments,1,pos(',',Arguments)-1));
  Delete(Arguments,1,pos(',',Arguments));
  if (pos(',',Arguments)>0)then arg2:=RemoveLeadingSpaces(Copy(Arguments,1,pos(',',Arguments)-1));
  Delete(Arguments,1,pos(',',Arguments));
  arg3:=RemoveLeadingSpaces(Arguments);
  if (ItIsRightInt(arg1))and(ItIsRightInt(arg2))and(ItIsRightInt(arg3))then
    begin;
    x:=StrToInt(arg1);
    y:=StrToInt(arg2);
    n:=StrToInt(arg3);
    MoveCursor(PosX,PosY,x,y);
    for i:=1 to n do
      begin;
      Strings.DeleteCharacters(y,x,1);
      Strings.ReDrawString(y);
      Delay(BaseDelayFactor);
      end;
    ExecutionResult.Error:=False;
    end
  else
    begin;
    AddLogMessage('Ошибка: '''+arg1+', '+arg2+', '+arg3+''' - неправильные аргументы процедуры Delete.');
    end;
  end;

if (Command='BACKSPACE') then
  begin;
  if (pos(',',Arguments)>0)then arg1:=RemoveLeadingSpaces(Copy(Arguments,1,pos(',',Arguments)-1));
  Delete(Arguments,1,pos(',',Arguments));
  if (pos(',',Arguments)>0)then arg2:=RemoveLeadingSpaces(Copy(Arguments,1,pos(',',Arguments)-1));
  Delete(Arguments,1,pos(',',Arguments));
  arg3:=RemoveLeadingSpaces(Arguments);
  if (ItIsRightInt(arg1))and(ItIsRightInt(arg2))and(ItIsRightInt(arg3))then
    begin;
    x:=StrToInt(arg1);
    y:=StrToInt(arg2);
    n:=StrToInt(arg3);
    MoveCursor(PosX,PosY,x,y);
    for i:=1 to n do
      begin;
      Strings.DeleteCharacters(y,x-i,1);
      Strings.ReDrawString(y);
      SpecGotoXY(x-i+1,y+1);
      Delay(BaseDelayFactor);
      end;
    PosX:=x-n;
    ExecutionResult.Error:=False;
    end
  else
    begin;
    AddLogMessage('Ошибка: '''+arg1+', '+arg2+', '+arg3+''' - неправильные аргументы процедуры Backspace.');
    end;
  end;

if (Command='WAIT') then
  begin;
  if ItIsRightInt(Arguments) then
    begin;
    Delay(BaseDelayFactor*StrToInt(Arguments));
    ExecutionResult.Error:=False;
    end
  else
    AddLogMessage('Ошибка: '''+Arguments+''' - неправильное значение целочисленной переменной.');
  end;

if (Command='DELAY') then
  begin;
  if ItIsRightInt(Arguments) then
    begin;
    Delay(StrToInt(Arguments));
    ExecutionResult.Error:=False;
    end
  else
    AddLogMessage('Ошибка: '''+Arguments+''' - неправильное значение целочисленной переменной.');
  end;

if (Command='REFRESH') then
  begin;
  DrawInterface;
  DrawScrollBars;
  Strings.ReDrawStrings(1);
  ExecutionResult.Error:=False;
  end;

if (Command='END') then
  begin;
  Delay(EndDelay);
  ExecutionResult.EndFile:=True;
  ExecutionResult.Error:=False;
  end;

if (Command='MANUALCODEVIEW') then
  begin;
  ManualCodeView;
  ExecutionResult.Error:=False;
  end;

if (Command='LOADCOLORSET') then
  begin;
  if (Arguments[1]='''')and(Arguments[Length(Arguments)]='''') then
    begin;
    Delete(Arguments,1,1);
    Delete(Arguments,Length(Arguments),1);
    ReadColorOptions(Arguments);
    ExecutionResult.Error:=False;
    end
  else
    AddLogMessage('Ошибка: '''+Arguments+''' - неправильное значение строковой переменной.');
  end;
end;


// Обработка комманды
procedure ExecuteCommand(Command:String);
begin;
ExecutionResult.Error:=False;
Command:=RemoveLeadingSpaces(Command);
if Command<>'' then if Command[Length(Command)]=';' then
  begin;
  Delete(Command,Length(Command),1);
  Command:=RemoveLeadingSpaces(Command);
  end;
if Command<>'' then
  begin;
  ExecutionResult.Error:=True;
  if ItIsSetVarCommand(Command) then SetVariable(Command)
  else
  If ItIsCommand(Command) then DoCommand(Command);
  end;
end;


begin;
ExecutionResult.EndFile:=False;
end.
