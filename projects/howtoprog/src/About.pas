unit About;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Label3: TLabel;
    BitBtn1: TBitBtn;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

uses Mainform;

{$R *.DFM}

procedure TAboutBox.FormCreate(Sender: TObject);
begin
AboutBox.Left:=Form1.Left+(Form1.Width div 2)-(AboutBox.Width div 2);
AboutBox.Top:=Form1.Top+(Form1.Height div 2)-(AboutBox.Height div 2);
end;

end.
 
