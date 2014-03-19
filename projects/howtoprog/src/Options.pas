unit Options;

//============================================================================\
// implementation
//============================================================================\
interface

var BaseDelayFactor:Integer;
    HomeAllowed:Boolean;
    InFileFullName,InFileName,ExeDir:String;
    DosText:Boolean;
    SyntaxHighlight:Boolean;
    Colors:array[0..16] of Byte;
    DefaultColor:Integer;
    EndDelay:Integer;

Procedure ReadColorOptions(const ColorOptionsFileName:String);

//============================================================================\
// implementation
//============================================================================\
implementation
uses SysUtils,IniFiles,HTPConstants;


//============================================================================\
// —читывание опций из ini-файла
//============================================================================\
Procedure ReadColorOptions(const ColorOptionsFileName:String);
var OptionsFile:TIniFile;
begin;
OptionsFile:=TIniFile.Create(ExeDir+ColorOptionsFileName);
OptionsFile.UpdateFile;

Colors[clWHITESPACE]:=          OptionsFile.ReadInteger('HowToProg','Whitespace_Color',          Colors[clWHITESPACE]);
Colors[clCOMMENT]:=             OptionsFile.ReadInteger('HowToProg','Comment_Color',             Colors[clCOMMENT]);
Colors[clSTRING]:=              OptionsFile.ReadInteger('HowToProg','String_Color',              Colors[clSTRING]);
Colors[clIDENTIFIER]:=          OptionsFile.ReadInteger('HowToProg','Identifier_Color',          Colors[clIDENTIFIER]);
Colors[clNUMBER]:=              OptionsFile.ReadInteger('HowToProg','Number_Color',              Colors[clNUMBER]);
Colors[clRESERVEDWORD]:=        OptionsFile.ReadInteger('HowToProg','Reserved_Word_Color',       Colors[clRESERVEDWORD]);
Colors[clSYMBOL]:=              OptionsFile.ReadInteger('HowToProg','Symbol_Color',              Colors[clSYMBOL]);
Colors[clCodeWindowBack]:=      OptionsFile.ReadInteger('HowToProg','CODEWINDOW_BACK_COLOR',     Colors[clCodeWindowBack]);
Colors[clCodeWindowBorder]:=    OptionsFile.ReadInteger('HowToProg','CODEWINDOW_BORDER_COLOR',   Colors[clCodeWindowBorder]);
Colors[clCodeWindowHeader]:=    OptionsFile.ReadInteger('HowToProg','CODEWINDOW_HEADER_COLOR',   Colors[clCodeWindowHeader]);
Colors[clCodeWindowScroller]:=  OptionsFile.ReadInteger('HowToProg','CODEWINDOW_SCROLLER_COLOR', Colors[clCodeWindowScroller]);
Colors[clCodeWindowSprainer]:=  OptionsFile.ReadInteger('HowToProg','CODEWINDOW_SPRAINER_COLOR', Colors[clCodeWindowSprainer]);
Colors[clCodeWindowButtons]:=   OptionsFile.ReadInteger('HowToProg','CODEWINDOW_BUTTONS_COLOR',  Colors[clCodeWindowButtons]);
Colors[clCommentWindowBack]:=   OptionsFile.ReadInteger('HowToProg','COMMENTWINDOW_BACK_COLOR',  Colors[clCommentWindowBack]);
Colors[clCommentWindowBorder]:= OptionsFile.ReadInteger('HowToProg','COMMENTWINDOW_BORDER_COLOR',Colors[clCommentWindowBorder]);
Colors[clCommentWindowHeader]:= OptionsFile.ReadInteger('HowToProg','COMMENTWINDOW_HEADER_COLOR',Colors[clCommentWindowHeader]);
Colors[clCommentColor]:=        OptionsFile.ReadInteger('HowToProg','COMMENTWINDOW_TEXT_COLOR',  Colors[clCommentColor]);
DefaultColor:=                  OptionsFile.ReadInteger('HowToProg','DEFAULT_TEXT_COLOR',        DefaultColor);

OptionsFile.Free;
end;


Procedure ReadOptions;
var OptionsFile:TIniFile;
begin;
ExeDir:=ExtractFilePath(ParamStr(0));
OptionsFile:=TIniFile.Create(ExeDir+'options.ini');
OptionsFile.UpdateFile;

InFileFullName:=OptionsFile.ReadString('HowToProg','InFileName','simple.proga');
InFileName:=UpperCase(ExtractFileName(InFileFullName));
BaseDelayFactor:=OptionsFile.ReadInteger('HowToProg','BaseDelayFactor',100);
HomeAllowed:=OptionsFile.ReadBool('HowToProg','HomeAllowed',TRUE);
DosText:=OptionsFile.ReadBool('HowToProg','DosText',FALSE);
SyntaxHighlight:=OptionsFile.ReadBool('HowToProg','SyntaxHighlight',TRUE);
EndDelay:=OptionsFile.ReadInteger('HowToProg','EndDelay',1000);
if EndDelay>5000 then EndDelay:=5000; if EndDelay<0 then EndDelay:=0;
OptionsFile.Free;

ReadColorOptions('options.ini');
end;




{var OptionsFile:TIniFile;}
begin;
{OptionsFile:=TIniFile.Create(ExtractFilePath(ParamStr(0))+'options.ini');
OptionsFile.UpdateFile;
OptionsFile.WriteInteger('HowToProg','CodeWindow_Back_Color',      1);
OptionsFile.WriteInteger('HowToProg','CodeWindow_Border_Color',   15);
OptionsFile.WriteInteger('HowToProg','CodeWindow_Header_Color',   15);
OptionsFile.WriteInteger('HowToProg','CodeWindow_Scroller_Color',  3);
OptionsFile.WriteInteger('HowToProg','CodeWindow_Sprainer_Color', 10);
OptionsFile.WriteInteger('HowToProg','CodeWindow_Buttons_Color',  10);
OptionsFile.WriteInteger('HowToProg','CommentWindow_Back_Color',   0);
OptionsFile.WriteInteger('HowToProg','CommentWindow_Border_Color', 7);
OptionsFile.WriteInteger('HowToProg','CommentWindow_Header_Color', 7);
OptionsFile.WriteInteger('HowToProg','CommentWindow_Text_Color',   7);
OptionsFile.Free;}

Colors[clWHITESPACE]:=14;
Colors[clCOMMENT]:=7;
Colors[clSTRING]:=10;
Colors[clIDENTIFIER]:=14;
Colors[clNUMBER]:=10;
Colors[clRESERVEDWORD]:=15;
Colors[clSYMBOL]:=15;
Colors[clCodeWindowBack]:=1;
Colors[clCodeWindowBorder]:=15;
Colors[clCodeWindowHeader]:=15;
Colors[clCodeWindowScroller]:=3;
Colors[clCodeWindowSprainer]:=10;
Colors[clCodeWindowButtons]:=10;
Colors[clCommentWindowBack]:=0;
Colors[clCommentWindowBorder]:=7;
Colors[clCommentWindowHeader]:=7;
Colors[clCommentColor]:=7;

ReadOptions;

{Colors[clWHITESPACE]:=14;
Colors[clCOMMENT]:=7;
Colors[clSTRING]:=2;
Colors[clIDENTIFIER]:=0;
Colors[clNUMBER]:=2;
Colors[clRESERVEDWORD]:=8;
Colors[clSYMBOL]:=1;
Colors[clCodeWindowBack]:=15;
Colors[clCodeWindowBorder]:=8;
Colors[clCodeWindowHeader]:=0;
Colors[clCodeWindowScroller]:=7;
Colors[clCodeWindowSprainer]:=2;
Colors[clCodeWindowButtons]:=2;
Colors[clCommentWindowBack]:=15;
Colors[clCommentWindowBorder]:=8;
Colors[clCommentWindowHeader]:=0;
Colors[clCommentColor]:=0;}
end.
