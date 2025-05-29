unit formcyVirtualGrid;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, Spin, cyVirtualGrid, cyBaseSpeedButton;

type
  TFrmCyVirtualGrid = class(TForm)
    CyPanel1: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    StatusBar1: TStatusBar;
    SBGenerate: TcySpeedButton;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    cyLabel1: TcyLabel;
    LblWebSite: TcyHotLabel;
    Panel1: TPanel;
    GroupBox2: TGroupBox;
    Label53: TLabel;
    RBColAutoFixed: TRadioButton;
    RBColAutoStretched: TRadioButton;
    RBColManual: TRadioButton;
    SEColWidth: TSpinEdit;
    GroupBox3: TGroupBox;
    Label54: TLabel;
    RBRowAutoFixed: TRadioButton;
    RBRowAutoStretched: TRadioButton;
    RBRowManual: TRadioButton;
    SERowHeight: TSpinEdit;
    cyVirtualGrid1: TcyVirtualGrid;
    LblVirtualGridInfo: TLabel;
    Shape1: TShape;
    PaintBox1: TPaintBox;
    Image1: TImage;
    Label51: TLabel;
    SERows: TSpinEdit;
    Label52: TLabel;
    SEColumns: TSpinEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure SBGenerateClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    CoordX, CoordY: Integer;
  end;

var
  FrmCyVirtualGrid: TFrmCyVirtualGrid;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyVirtualGrid.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyVirtualGrid.FormCreate(Sender: TObject);
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

procedure TFrmCyVirtualGrid.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyVirtualGrid.PaintBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  aRect: TRect;
begin
  if cyVirtualGrid1.ValidCells
  then begin
    if cyVirtualGrid1.GetCellCoord(X, Y, CoordX, CoordY)
    then begin
      LblVirtualGridInfo.Caption := Format(' Row: %d  Column: %d', [CoordY, CoordX]);
      aRect := cyVirtualGrid1.GetCellRect(CoordX, CoordY);

      // Put the shape on the selected cell:
      Shape1.Top := PaintBox1.Top + aRect.Top;
      Shape1.Left := PaintBox1.Left + aRect.Left;
      Shape1.Width := aRect.Right - aRect.Left;
      Shape1.Height := aRect.Bottom - aRect.Top;

      if not Shape1.Visible
      then Shape1.Visible := true;
    end
    else
      Shape1.Visible := false;
  end
  else
    Shape1.Visible := false;
end;

procedure TFrmCyVirtualGrid.PaintBox1Paint(Sender: TObject);
var
  x, y: Integer;
  aRect: TRect;
begin
  // Draw background :
  PaintBox1.Canvas.Brush.Color := clBlack;
  PaintBox1.Canvas.FillRect(PaintBox1.ClientRect);
  PaintBox1.Canvas.Draw(0, 0, Image1.Picture.Graphic);

  // Draw virtual grid lines :
  if cyVirtualGrid1.ValidCells
  then
    for x := cyVirtualGrid1.FromCoordX to cyVirtualGrid1.ToCoordX do
      for y := cyVirtualGrid1.FromCoordY to cyVirtualGrid1.ToCoordY do
      begin
        aRect := cyVirtualGrid1.GetCellRect(x, y);
        PaintBox1.Canvas.MoveTo(aRect.Right, aRect.Top);
        PaintBox1.Canvas.LineTo(aRect.Right, aRect.Bottom);
        PaintBox1.Canvas.LineTo(aRect.Left, aRect.Bottom);
      end;
end;

procedure TFrmCyVirtualGrid.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCyVirtualGrid.SBGenerateClick(Sender: TObject);
begin
  // Columns definition :
  cyVirtualGrid1.ToCoordX := SEColumns.Value-1;

  if RBColAutoFixed.Checked
  then
    cyVirtualGrid1.CellWidthMode := smAutoFixed
  else
    if RBColAutoStretched.Checked
    then
      cyVirtualGrid1.CellWidthMode := smAutoStretched
    else begin
      cyVirtualGrid1.CellWidthMode := smManual;
      cyVirtualGrid1.CellWidth := SEColWidth.Value;
    end;

  // Rows definition :
  cyVirtualGrid1.ToCoordY := SERows.Value-1;

  if RBRowAutoFixed.Checked
  then
    cyVirtualGrid1.CellHeightMode := smAutoFixed
  else
    if RBRowAutoStretched.Checked
    then
      cyVirtualGrid1.CellHeightMode := smAutoStretched
    else begin
      cyVirtualGrid1.CellHeightMode := smManual;
      cyVirtualGrid1.CellHeight := SERowHeight.Value;
    end;

  cyVirtualGrid1.GenerateCells(PaintBox1.ClientRect);
  CoordX := -1;
  CoordY := -1;
  Shape1.Visible := false;
  PaintBox1.Invalidate;
end;

end.
