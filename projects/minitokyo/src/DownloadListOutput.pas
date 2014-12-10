unit DownloadListOutput;

interface
uses Classes;

const
  DownloadListTypeCount=3;
  DownloadListTypes:array[0..DownloadListTypeCount-1] of String = ('.txt','.txt','.kgt');
  DownloadListTypeNames:array[0..DownloadListTypeCount-1] of String = ('Windows Text (*.txt)','Linux Text (*.txt)','Linux KGet (*.kgt)');
  KGetDefDir='/flash/';

procedure CreateDownloadListFile(const FileName:String; const List:TStringList);

implementation
uses Options, SysUtils;

var c10:Char=#10;

function MyExtractFileName(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('/\:', FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

procedure LWriteStr(fc:TFileStream; s:String);
var i:Integer;
begin;
for i:=1 to Length(s) do fc.Write(s[i],1);
fc.Write(c10,1);
end;

function LMake(const S:String):String;
var i:Integer;
begin;
Result:='';
for i:=1 to Length(S) do
  begin;
  case S[i] of
  '[': Result:=Result+'%5B';
  ']': Result:=Result+'%5D';
  ' ': Result:=Result+'%20';
  else Result:=Result+S[i];
  end;
  end;
end;


procedure CreateDownloadListFile(const FileName:String; const List:TStringList);
var f:TextFile;
    fc:TFileStream;
    i:Integer;
    Dest:String;
begin;
if Assigned(List) then
  begin;
  case DownloadListType of
  0: {wintext}
    begin;
    AssignFile(f,FileName);
    ReWrite(f);
    for i:=0 to List.Count-1 do
      Writeln(f,List[i]);
    CloseFile(f);
    end;
  1: {linux text}
    begin;
    fc:=TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    for i:=0 to List.Count-1 do
      LWriteStr(fc, List[i]);
    fc.Free;
    end;
  2: {kget}
    begin;
    if (Length(MainFolder)=0) then
      Dest:=KGetDefDir
    else
      begin;
      Dest:=MainFolder;
      if Dest[Length(Dest)]<>'/' then Dest:=Dest+'/';
      end;
    fc:=TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    LWriteStr(fc,'[Common]');
    LWriteStr(fc,'Count='+IntToStr(List.Count));
    LWriteStr(fc,'');
    for i:=0 to List.Count-1 do
      begin;
      LWriteStr(fc,'[Item'+IntToStr(i)+']');
      LWriteStr(fc,'CanResume=true'); //???
      LWriteStr(fc,'Dest=file://'+Dest+LMake(MyExtractFileName(List[i])));
      LWriteStr(fc,'Mode=1'); //???
      LWriteStr(fc,'ProcessedSize=0');
      LWriteStr(fc,'ScheduledTime=2005,10,19,18,17,10');
      LWriteStr(fc,'Source='+List[i]);
      LWriteStr(fc,'Status=0'); //???
      //LWriteStr(fc,'TotalSize=0');
      LWriteStr(fc,'');
      end;
    fc.Free;
    end;
  end;
  end;
end;

end.
