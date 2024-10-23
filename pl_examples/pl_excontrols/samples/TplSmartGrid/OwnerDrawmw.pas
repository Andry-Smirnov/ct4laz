unit OwnerDrawmw;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TplSmartGridUnit;

type
  TForm1 = class(TForm)
    SmartGrid1: TplSmartGrid;
    procedure FormCreate(Sender: TObject);
    procedure SmartGrid1DrawCell(Sender: TObject; ACanvas: TCanvas; X,
      Y: Integer; Rc: TRect; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  x: Integer;
begin
  for x := 0 to 19 do
  begin
    SmartGrid1.Cells[0, x] := IntToStr(Random(100));
    SmartGrid1.Cells[1, x] := IntToStr(Random(100));
    SmartGrid1.Cells[2, x] := IntToStr(Random(100));
    SmartGrid1.Cells[3, x] := IntToStr(Random(100));
    SmartGrid1.Cells[4, x] := IntToStr(Random(100));
  end;
end;

procedure TForm1.SmartGrid1DrawCell(Sender: TObject; ACanvas: TCanvas; X,
  Y: Integer; Rc: TRect; var Handled: Boolean);
var
  i: Integer;
begin
  i := StrToIntDef(SmartGrid1.Cells[X, Y], 0);
  if Odd(i)
    then ACanvas.Font.Color := clRed;
  if ((i mod 10) = 0)
    then ACanvas.Brush.Color := clYellow;
end;

end.
