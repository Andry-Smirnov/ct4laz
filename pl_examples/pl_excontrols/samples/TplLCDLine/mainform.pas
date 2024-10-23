{***************************************************************
  This Application is part of CodeTyphon Studio
  by PiloLogic Software House (https://www.pilotlogic.com/)
****************************************************************}
unit mainform;

interface

uses
  Messages,LResources, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, TplLCDLineUnit, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    LCDLine1: TplLCDLine;
    LCDLine2: TplLCDLine;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  CurDate: String;
  LenDate: Integer;
  StartOffset: Integer;
  EndOffset: Integer;
  vOffset: Longint;
  NC: Integer;

implementation


procedure TForm1.FormCreate(Sender: TObject);
begin
  StartOffset := LCDLine2.GlobalColCount + 1;
  vOffset := StartOffset;
  LCDLine2.Offset := StartOffset;
  NC := LCDLine2.ColCount;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin

  LCDLine1.Text := TimeToStr(Time);
  CurDate := 'Date ' + FormatDateTime('dddd dddddd', Date);
  LCDLine2.Text := CurDate;
  LenDate := Length(CurDate);
  EndOffset := LenDate * NC + LenDate;

  vOffset := vOffset - 1;
  if vOffset < (-EndOffset) then
     vOffset := StartOffset;
  LCDLine2.Offset := vOffset;
end;

initialization
  {$I mainform.lrs}

end.
