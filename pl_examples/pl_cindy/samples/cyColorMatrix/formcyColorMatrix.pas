unit formcyColorMatrix;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons, Math,
  cyGraphics, cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyColorMatrix, cyBaseSpeedButton, cyBaseColorMatrix, cyBaseButton, cyBitBtn;

type
  TFrmCyColorMatrix = class(TForm)
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
    SBDoubleBuffer: TcySpeedButton;
    CBRandomDemo: TCheckBox;
    cyColorMatrix1: TcyColorMatrix;
    CBGaugeDemo: TCheckBox;
    BtnLoadFromBitmap: TcyBitBtn;
    ImageLoad: TImage;
    BtnDrawGraphic: TcyBitBtn;
    BtnSaveToBitmap: TcyBitBtn;
    ImageSave: TImage;
    ImageDrawGraphic: TImage;
    BtnDrawText: TcyBitBtn;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure SBDoubleBufferClick(Sender: TObject);
    procedure cyColorMatrix1CellClick(Sender: TObject; aRow, aCol: Integer; aColor: TColor);
    procedure CBRandomDemoClick(Sender: TObject);
    procedure CBGaugeDemoClick(Sender: TObject);
    procedure cySpeedButton1Click(Sender: TObject);
    procedure BtnLoadFromBitmapClick(Sender: TObject);
    procedure BtnDrawGraphicClick(Sender: TObject);
    procedure BtnSaveToBitmapClick(Sender: TObject);
    procedure BtnDrawTextClick(Sender: TObject);
    procedure cyColorMatrix1Paint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyColorMatrix: TFrmCyColorMatrix;

  Statistics: String;

implementation

{$R *.lfm}
const

// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyColorMatrix.cyColorMatrix1CellClick(Sender: TObject; aRow, aCol: Integer; aColor: TColor);
begin
  Caption := 'Cell clicked: Row = ' + intToStr(aRow) + '   Col = ' + intToStr(aCol);
end;

procedure TFrmCyColorMatrix.cyColorMatrix1Paint(Sender: TObject);
begin
  // You can only change cyColorMatrix1.Canvas here!

  if CBRandomDemo.Checked or CBGaugeDemo.Checked
  then begin
    cyColorMatrix1.Canvas.Brush.Color := clBlack;
    cyColorMatrix1.Canvas.Font.Color := clWhite;
    cyColorMatrix1.Canvas.TextOut(10, cyColorMatrix1.Height-25, Statistics);
  end;
end;

procedure TFrmCyColorMatrix.cySpeedButton1Click(Sender: TObject);
var aRow: Integer;
begin
  aRow := cyColorMatrix1.ValueToRow(cyColorMatrix1.TopRowValue);
  cyColorMatrix1.SetColorGridRange(aRow, aRow, 0, 10, clRed);

  aRow := cyColorMatrix1.ValueToRow(cyColorMatrix1.BottomRowValue);
  cyColorMatrix1.SetColorGridRange(aRow, aRow, 0, 10, clLime);
end;

procedure TFrmCyColorMatrix.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyColorMatrix.FormCreate(Sender: TObject);
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

  Randomize;
  Statistics := 'Calculating ...';
end;

procedure TFrmCyColorMatrix.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyColorMatrix.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCyColorMatrix.SBDoubleBufferClick(Sender: TObject);
begin
  DoubleBuffered := SBDoubleBuffer.Down;
end;

procedure TFrmCyColorMatrix.CBRandomDemoClick(Sender: TObject);
var
  c, r, _r, _g, _b: integer;
  DrawCount: Integer;
  SaveTime: Cardinal;
begin
  DrawCount := 0;
  SaveTime := GetTickCount64;
  while (CBRandomDemo.Checked) and (not Application.Terminated) do
  begin
    cyColorMatrix1.BeginUpdate;

    for r := 0 to cyColorMatrix1.RowCount - 1 do
    begin
      for c := 0 to cyColorMatrix1.ColCount -1 do
      begin
        _r := RandomRange(0, 254);
        _g := RandomRange(0, 254);
        _b := RandomRange(0, 254);

        cyColorMatrix1.SetColorGrid(r, c, RGB(_r, _g, _b));

        Inc(DrawCount, 1);
        if GetTickCount64 - SaveTime > 1000
        then begin
          Statistics := intToStr(DrawCount) + ' boxes painted per second.';
          // Frames per second:
          Statistics := Statistics + '   '
            + intToStr(DrawCount div (cyColorMatrix1.RowCount*cyColorMatrix1.ColCount)) + ' frames per second.';
          DrawCount := 0;
          SaveTime := GetTickCount64;
        end;
      end;
    end;

    cyColorMatrix1.Canvas.Brush.Style := bsSolid;
    cyColorMatrix1.Canvas.Brush.Color := clWhite;
    cyColorMatrix1.Canvas.Font.Color := clBlack;
    cyColorMatrix1.DrawText('Random demo', 0, 0, cyColorMatrix1.RowCount-1, cyColorMatrix1.ColCount-1, DT_WORDBREAK);
    cyColorMatrix1.EndUpdate;

    Application.ProcessMessages;
  end;
end;

procedure TFrmCyColorMatrix.CBGaugeDemoClick(Sender: TObject);
var
  c: integer;
  OldValue, NewValue: Double;
  OldRow, OldCol, NewRow: Integer;
begin
  Statistics := 'No statistics because of Sleep() used to slow down ...';

  while (CBGaugeDemo.Checked) and (not Application.Terminated) do
  begin
    cyColorMatrix1.BeginUpdate;

    for c := 0 to cyColorMatrix1.ColCount -1 do
    begin
      // Red cell animation:
      if not cyColorMatrix1.FindCellColor(0, cyColorMatrix1.RowCount - 1, c, c, clRed, OldRow, OldCol)
      then OldValue := cyColorMatrix1.BottomRowValue
      else OldValue := cyColorMatrix1.RowToValue(OldRow);

      if OldValue > (cyColorMatrix1.TopRowValue - cyColorMatrix1.BottomRowValue) / 2
      then NewValue := OldValue + RandomRange(-20, 5)
      else NewValue := OldValue + RandomRange(-5, 20);

      if NewValue < cyColorMatrix1.BottomRowValue
      then NewValue := cyColorMatrix1.BottomRowValue;
      if NewValue > cyColorMatrix1.TopRowValue
      then NewValue := cyColorMatrix1.TopRowValue;

      // Draw column level :
      NewRow := cyColorMatrix1.ValueToRow(NewValue);
      cyColorMatrix1.SetColorGridRange(0, NewRow, c, c, cyColorMatrix1.DefaultColor);     // Light off ...
      cyColorMatrix1.SetColorGridRange(NewRow, cyColorMatrix1.RowCount-1, c, c, clLime);  // Light on ...

      // Draw Red Cell:
      if NewValue < OldValue
      then begin
        OldRow := OldRow + 1;  // Indicator is falling ...
        if OldRow > cyColorMatrix1.RowCount-1
        then OldRow := cyColorMatrix1.RowCount-1;
      end
      else
        OldRow := NewRow;
      cyColorMatrix1.SetColorGrid(OldRow, c, clRed);
    end;

    Sleep(100);
    cyColorMatrix1.Canvas.Brush.Style := bsClear;
    cyColorMatrix1.Canvas.Font.Color := clBlack;
    cyColorMatrix1.DrawText('Gauge demo', 0, 0, cyColorMatrix1.RowCount-1, cyColorMatrix1.ColCount-1, DT_WORDBREAK);
    cyColorMatrix1.EndUpdate;
    Application.ProcessMessages;
  end;
end;

procedure TFrmCyColorMatrix.BtnLoadFromBitmapClick(Sender: TObject);
begin
  cyColorMatrix1.LoadFromGraphic(ImageLoad.Picture.Bitmap);
end;

procedure TFrmCyColorMatrix.BtnSaveToBitmapClick(Sender: TObject);
begin
  if not Assigned(ImageSave.Picture.Bitmap)
  then ImageSave.Picture.Bitmap := TBitmap.Create;

  cyColorMatrix1.SaveToBitmap(ImageSave.Picture.Bitmap);
end;

procedure TFrmCyColorMatrix.BtnDrawGraphicClick(Sender: TObject);
begin
  cyColorMatrix1.DrawGraphic(0, 0, ImageDrawGraphic.Picture.Graphic, clWhite, true);
end;

procedure TFrmCyColorMatrix.BtnDrawTextClick(Sender: TObject);
begin
  cyColorMatrix1.Canvas.Brush.Style := bsClear;
  cyColorMatrix1.DrawText(Edit1.Text, 50, 50, cyColorMatrix1.RowCount-1, cyColorMatrix1.ColCount-1, DT_SINGLELINE);
end;

end.
