program HowToProg;

{$APPTYPE CONSOLE}
{$R *.RES}

//============================================================================\\
//  ������������ ������
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
// �������� ����
//============================================================================\\
var f:text;
    Command:string;
    StrNo:Integer;
begin
{Put(1,1,15,0,#45#43#31#30#52#56#54#50#47#42);
repeat Put(1,1,15,0,readkey); until 1=2;}
SetConsTitle('������� �������� ����������������');
StrNo:=0;
AddLogMessage('������ ����� '+InFileFullName);
AssignFile(f,InFileFullName);
reset(f);


repeat
Inc(StrNo);
readln(f,Command);

try
  ExecuteCommand(Command);
except
  on E:Exception do AddLogMessage('!!!����������� ������ � ������ '+IntToStr(StrNo)+': '+E.Message);
end;

if ExecutionResult.Error=False then AddLogMessage('������� ��������� ��� ������: '+Command)
else AddLogMessage('������� ��������� ������: '+Command);

until (ExecutionResult.EndFile=True)or(eof(f));


if (eof(f))and(ExecutionResult.EndFile=False) then AddLogMessage('������: ��������� ����� �����, �� ��������� End �� ���� �������.');
close(f);
end.
