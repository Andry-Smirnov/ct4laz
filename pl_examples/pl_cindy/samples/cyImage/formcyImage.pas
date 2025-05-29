unit formcyImage;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBevel, cyBaseSpeedButton, cyCustomImage;

type
  TFrmCyImage = class(TForm)
    PanTop: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    StatusBar1: TStatusBar;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    cyLabel1: TcyLabel;
    LblWebSite: TcyHotLabel;
    ScrollBox1: TScrollBox;
    cyImage1: TcyImage;
    Label1: TLabel;
    CBStretch: TCheckBox;
    CBProportional: TCheckBox;
    Panel1: TPanel;
    Image1: TImage;
    cyImage2: TcyImage;
    Label2: TLabel;
    Label3: TLabel;
    Panel3: TPanel;
    cyImage3: TcyImage;
    Label4: TLabel;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure cyImage1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CBProportionalClick(Sender: TObject);
    procedure CBStretchClick(Sender: TObject);
    procedure cyImage3MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure cyImage1Paint(Sender: TObject);
  private
    ImageView_Zoom: Extended;
  public
  end;

var
  FrmCyImage: TFrmCyImage;

implementation

{$R *.lfm}
     const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ENDIF}
{$IFDEF UNIX}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyImage.CBProportionalClick(Sender: TObject);
begin
  cyImage1.Proportional := CBProportional.Checked;
end;

procedure TFrmCyImage.CBStretchClick(Sender: TObject);
begin
  if CBStretch.Checked
  then cyImage1.Align := alClient
  else cyImage1.Align := alNone;

  cyImage1.AutoSize := not CBStretch.Checked;
  cyImage1.Stretch := CBStretch.Checked;
end;

procedure TFrmCyImage.cyImage1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  percX, percY: Extended;

  CursorPos: TPoint;
  aColor: TColor;
begin
  if cyImage1.Stretch then Exit;

  // Where clicked (percentage of TcyImage size) :
  percX := X / cyImage1.Width;
  percY := Y / cyImage1.Height;

  // Apply zoom :
  if Button = mbLeft then
  begin
    if ImageView_Zoom < 20
    then ImageView_Zoom := ImageView_Zoom * 2
    else Exit;
  end;

  if Button = mbRight then
    if ImageView_Zoom > 0.25
    then ImageView_Zoom := ImageView_Zoom / 2
    else Exit;

  if Button = mbMiddle then
    ImageView_Zoom := 1;

  cyImage1.AutoSize := false;
  cyImage1.Zoom := ImageView_Zoom;

  cyImage1.Width  := Round(cyImage1.Picture.Bitmap.Width * ImageView_Zoom);
  cyImage1.Height := Round(cyImage1.Picture.Bitmap.Height * ImageView_Zoom);

    // Adjust ScrollBox position based on where we clicked :
    ScrollBox1.HorzScrollBar.Position := Round(ScrollBox1.HorzScrollBar.Range * percX) - ScrollBox1.Width div 2;  // Range = valor Maximo (Left + Width) de um child control
    ScrollBox1.VertScrollBar.Position := Round(ScrollBox1.VertScrollBar.Range * percY) - ScrollBox1.Height div 2; // Range = valor Maximo (Top + Height) de um child control
end;

procedure TFrmCyImage.cyImage1Paint(Sender: TObject);
begin
  cyImage1.CanvasControl.Font.Color := clWhite;
  cyImage1.CanvasControl.Font.Size := 12;
  cyImage1.CanvasControl.TextOut(10, 10, 'Post paint exemple');
end;

procedure TFrmCyImage.cyImage3MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  OffsetPixel, CurrentPixel, OffsetPos: TPoint;
begin
  if cyImage3.MouseIsDown then
    cyImage3.DragImageFromMouseDown(Point(X, Y));

end;

procedure TFrmCyImage.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyImage.FormCreate(Sender: TObject);
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

  ImageView_Zoom := 1;

  cyImage2.Picture.Graphic := cyImage1.Picture.Graphic;
  Image1.Picture.Graphic := cyImage1.Picture.Graphic;
end;

procedure TFrmCyImage.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  aPt: TPoint;
begin
//  aPt := Self.ClientToScreen(MousePos);
  aPt := cyImage3.ScreenToClient(MousePos);
  cyImage3.ZoomFromMousePos(aPt, cyImage3.Zoom + 0.2);
  Handled := true;
end;

procedure TFrmCyImage.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  aPt: TPoint;
begin
  if cyImage3.Zoom <= 0.4 then Exit;

  aPt := Self.ClientToScreen(MousePos);
  aPt := cyImage3.ScreenToClient(aPt);
  cyImage3.ZoomFromMousePos(aPt, cyImage3.Zoom - 0.2);
  Handled := true;
end;

procedure TFrmCyImage.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyImage.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
