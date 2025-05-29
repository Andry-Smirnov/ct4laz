unit formcyMathParser;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBaseSpeedButton, cyMathParser;

type
  TFrmCyMathParser = class(TForm)
    CyPanel1: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    StatusBar1: TStatusBar;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    cyLabel1: TcyLabel;
    LblWebSite: TcyHotLabel;
    Panel1: TPanel;
    cyMathParser1: TcyMathParser;
    Label1: TLabel;
    Label3: TLabel;
    MemExprs: TMemo;
    MemRslts: TMemo;
    Label2: TLabel;
    Label4: TLabel;
    MemGraf: TMemo;
    TrackBar1: TTrackBar;
    Bevel1: TBevel;
    Bevel2: TBevel;
    cyLabel2: TcyLabel;
    cyLabel3: TcyLabel;
    CyPanel3: TCyPanel;
    Image1: TImage;
    Bevel3: TBevel;
    Label5: TLabel;
    EditCalc: TEdit;
    LblResult: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MemExprsChange(Sender: TObject);
    procedure MemGrafChange(Sender: TObject);
    procedure EditCalcChange(Sender: TObject);
  private
    { Private declarations }
    XAxisCoord, YAxisCoord, savX, savY: Integer;
  public
    { Public declarations }
  end;

var
  FrmCyMathParser: TFrmCyMathParser;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyMathParser.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyMathParser.EditCalcChange(Sender: TObject);
begin
  cyMathParser1.Expression := EditCalc.Text;
  cyMathParser1.Parse;

  if cyMathParser1.GetLastError = 0
  then LblResult.Caption := FloatToStr(cyMathParser1.ParserResult)
  else LblResult.Caption := cyMathParser1.GetLastErrorString;
end;

procedure TFrmCyMathParser.FormCreate(Sender: TObject);
var
  L: Integer;
  cyCompName: String;
  RtfFile: TFilename;
begin
  L := Length(Name);
  cyCompName := Copy(Name, 4, L-3);
  Caption := cyCompName + ' demo';
  RtfFile := pathMedia+ cyCompName+'.txt';

  try
    RichEditInfo.Lines.LoadFromFile(RtfFile);
  finally

  end;
end;

procedure TFrmCyMathParser.FormShow(Sender: TObject);
begin
  XAxisCoord := Image1.Height div 2;
  YAxisCoord := Image1.Width div 2;

  EditCalcChange(nil);
  //MemExprsChange(nil);
 // MemGrafChange(nil);
end;

procedure TFrmCyMathParser.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  savX := X;
  savY := Y;
end;

procedure TFrmCyMathParser.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  YAxisCoord := YAxisCoord + (X - savX);
  XAxisCoord := XAxisCoord + (Y - savY);
  MemGrafChange(nil);
end;

procedure TFrmCyMathParser.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyMathParser.MemExprsChange(Sender: TObject);
var
 i:integer;
 CurrentLine, CurrentExpression, Variable: String;
 MathParser: TcyMathParser;
begin
 MemRslts.Clear;
 MathParser := TcyMathParser.create(self);

 for i:=0 to MemExprs.Lines.Count-1 do
 begin
   CurrentLine := MemExprs.Lines[i];

   // If "=" exists, we will save variable value:
   if pos('=', CurrentLine) <> 0
   then begin
     Variable := trim(copy(CurrentLine, 1, pos('=', CurrentLine)-1));
     CurrentExpression := copy(CurrentLine, pos('=', CurrentLine)+1,length(CurrentLine));

     // Valid variable name ?
     if not MathParser.ValidVariableName(Variable)
     then begin
       MemRslts.Lines.add('Invalid variable name!');
       Continue;
     end;
    end
   else begin
     // Simple calc expression :
     Variable := '';
     CurrentExpression := CurrentLine;
   end;

   MathParser.Expression := CurrentExpression;
   MathParser.Parse;       // Evaluate the expression

   if MathParser.GetLastError = 0
   then begin
     if Variable <> ''
     then begin
       // Display Variable and its value:
       MemRslts.Lines.Add(Variable + '=' + FloatToStr(MathParser.ParserResult));
       // Save variable value into Variables list :
       MathParser.Variables.SetValue(Variable, MathParser.ParserResult);
     end
     else
       // Display expression result:
       MemRslts.Lines.Add(FloatToStr(MathParser.ParserResult));
   end
   else
     MemRslts.Lines.add(MathParser.GetLastErrorString);
 end;

 MathParser.Free;
end;

procedure TFrmCyMathParser.MemGrafChange(Sender: TObject);
var
  pX, pY, j: integer;
  x, fx: Extended;
  MathParser: TcyMathParser;
begin
  // Clean image :
  image1.Canvas.FillRect(image1.ClientRect);
  // Draw axis :
  image1.canvas.Pen.Color:=clgray;
  image1.Canvas.MoveTo(0, XAxisCoord);
  image1.Canvas.LineTo(Image1.Width, XAxisCoord);
  image1.Canvas.MoveTo(YAxisCoord, 0);
  image1.Canvas.LineTo(YAxisCoord, Image1.Height);

  MathParser := TcyMathParser.create(self);

  for j := 0 to MemGraf.Lines.Count-1 do
  begin
    MathParser.Expression := MemGraf.lines[j];
    if MathParser.GetLastError <> 0 then continue;

    for pX := 0 to Image1.Width do
    begin
      // Calc X :
      x := (pX - YAxisCoord) / TrackBar1.Position;
      MathParser.Variables.SetValue('x', x);
      // Calc f(x):
      MathParser.Parse;

      if MathParser.GetLastError = 0
      then begin
       fx := MathParser.ParserResult;
       pY := Round( -(-XAxisCoord + fx * TrackBar1.Position) );
       image1.Canvas.Pixels[pX, pY] := clBlue;
      end
      else
        if MathParser.GetLastError < cCalcError  // Expression error
        then begin
          image1.Canvas.TextOut(2, 2 + j * 16, MathParser.GetLastErrorString);
          Break;
        end;
    end;
  end;

  MathParser.Free;
end;

procedure TFrmCyMathParser.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
