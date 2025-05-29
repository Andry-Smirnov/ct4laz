unit formcyColorGrid;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cyTypes, cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyColorGrid, cyGraphics, cyBaseSpeedButton;

type
  TFrmCyColorGrid = class(TForm)
    CyPanel1: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    StatusBar1: TStatusBar;
    SBDoubleBuffer: TcySpeedButton;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    cyLabel1: TcyLabel;
    LblWebSite: TcyHotLabel;
    Panel1: TPanel;
    Label34: TLabel;
    cyColorGridPick: TcyColorGrid;
    cyColorGrid1: TcyColorGrid;
    PanSelected: TPanel;
    PanUnderMouse: TPanel;
    cyColorGridCustom: TcyColorGrid;
    cyColorGridFill: TcyColorGrid;
    SBFill: TcySpeedButton;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure cyColorGridPickBoxClick(Sender: TObject; aRow, aCol: Integer;
      aColor: TColor);
    procedure cyColorGridPickMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SBFillClick(Sender: TObject);
    procedure cyColorGridCustomCustomDrawBox(Sender: TObject; aRect: TRect; aState: TBoxState; aRow: integer; aCol: integer; aColor: TColor);
    procedure cyColorGridCustomCustomDrawBkgnd(Sender: TObject);
    procedure SBDoubleBufferClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyColorGrid: TFrmCyColorGrid;

implementation

{$R *.lfm}
const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyColorGrid.cyColorGridCustomCustomDrawBkgnd(
  Sender: TObject);
var x, y: Integer;
begin
  for x := 0 to 2 do
    for y := 0 to 3 do
    begin
      cyColorGridCustom.Canvas.Draw(x * Image1.Picture.Bitmap.Width,
                                    y * Image1.Picture.Bitmap.Height,
                                    Image1.Picture.Bitmap);
    end;
end;

procedure TFrmCyColorGrid.cyColorGridCustomCustomDrawBox(Sender: TObject; aRect: TRect; aState: TBoxState; aRow: integer; aCol: integer; aColor: TColor);

begin
  cyGradientFill(cyColorGridCustom.Canvas, aRect,
                 clWhite, aColor, dgdVertical, 50,0, bmNormal, 255, 90);

  cyColorGridCustom.Canvas.FrameRect(aRect);
end;

procedure TFrmCyColorGrid.cyColorGridPickBoxClick(Sender: TObject; aRow,
  aCol: Integer; aColor: TColor);
begin
  PanSelected.Color := aColor;
end;

procedure TFrmCyColorGrid.cyColorGridPickMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  PanUnderMouse.Color := TcyColorGrid(Sender).HotColor;
end;

procedure TFrmCyColorGrid.SBDoubleBufferClick(Sender: TObject);
begin
  DoubleBuffered := SBDoubleBuffer.Down;
end;

procedure TFrmCyColorGrid.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyColorGrid.FormCreate(Sender: TObject);
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

procedure TFrmCyColorGrid.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyColorGrid.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCyColorGrid.SBFillClick(Sender: TObject);
var
  r, g, b, Red, Green, Blue: Integer;
  aColor: TColor;
begin
  cyColorGridFill.Cursor := crNone;
  cyColorGridFill.BoxCols   := 64;
  cyColorGridFill.BoxRows   := 64;
  cyColorGridFill.BoxWidth  := 4;
  cyColorGridFill.BoxHeight := 4;
  cyColorGridFill.ColorList.Clear;

  for g := 0 to 63 do
    for b := 0 to 63 do
    begin
      r := 63 - (g + b) div 2;
      Red := r * 4;
      Green := g * 4;
      Blue := b * 4;
      aColor := RGB(Red, Green, Blue);
      cyColorGridFill.AddColor(aColor);
    end;
end;

end.
