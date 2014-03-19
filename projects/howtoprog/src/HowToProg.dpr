program HowToProg;

{$APPTYPE CONSOLE}
{$R *.RES}

//============================================================================\\
//  Используемые модули
//============================================================================\\
uses
  SysUtils,
  ConsoleUtils,
  Options,
  ConsoleInterface,
  HTPConstants,
  HowToProgStringList,
  Commands,
  Output,
  Log;

//============================================================================\\
// Основной цикл
//============================================================================\\
var f:text;
    Command:string;
    StrNo:Integer;
begin
{Put(1,1,15,0,#45#43#31#30#52#56#54#50#47#42);
repeat Put(1,1,15,0,readkey); until 1=2;}
SetConsTitle('Система обучения программированию');
StrNo:=0;
AddLogMessage('Анализ файла '+InFileFullName);
AssignFile(f,InFileFullName);
reset(f);


repeat
Inc(StrNo);
readln(f,Command);

try
  ExecuteCommand(Command);
except
  on E:Exception do AddLogMessage('!!!Критическая ошибка в строке '+IntToStr(StrNo)+': '+E.Message);
end;

if ExecutionResult.Error=False then AddLogMessage('Команда выполнена без ошибок: '+Command)
else AddLogMessage('Команда содержала ошибки: '+Command);

until (ExecutionResult.EndFile=True)or(eof(f));


if (eof(f))and(ExecutionResult.EndFile=False) then AddLogMessage('Ошибка: Достигнут конец файла, но процедура End не была вызвана.');
close(f);
end.
