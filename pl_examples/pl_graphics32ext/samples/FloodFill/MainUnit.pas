unit MainUnit;

interface

uses
  LCLIntf, LCLType,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtDlgs, GR32, GR32_Image, GR32_Layers, ExtCtrls, StdCtrls,
  XGR32_FloodFill, ComCtrls, Spin;

type

  { TFrmFloodFill }

  TFrmFloodFill = class(TForm)
    PnlTop: TPanel;
    IvwMain: TImgView32;
    OpenPictDlg: TOpenPictureDialog;
    ColorDialog: TColorDialog;
    PnlColorOuter: TPanel;
    PnlColorInner: TPanel;
    BtnPicture: TButton;
    BtnColor: TButton;
    BtnExit: TButton;
    EdtTolerance: TSpinEdit;
    StatusBar1: TStatusBar;
    LblTolerance: TLabel;
    procedure BtnExitClick(Sender: TObject);
    procedure BtnPictureClick(Sender: TObject);
    procedure BtnColorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IvwMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    UndoList: array [0..5] of TBitmap32;
    UndoBase: Integer;
    UndoPos : Integer;
    procedure ResetUndo;
    procedure PushUndo;
    procedure PopUndo;
  public
    { Public declarations }
  end;

var
  FrmFloodFill: TFrmFloodFill;

implementation

{$R *.lfm}

const
// CodeTyphon: Different platforms store resource files on different locations

{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmFloodFill.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to high(UndoList) do
    UndoList[I] := TBitmap32.Create;
end;

procedure TFrmFloodFill.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to high(UndoList) do
    UndoList[I].Free;
end;

procedure TFrmFloodFill.ResetUndo;
var
  I: Integer;
begin
  UndoBase := 0;
  UndoPos := 0;
  for I := 0 to high(UndoList) do
    UndoList[I].Delete;
end;

procedure TFrmFloodFill.PushUndo;
var
  HighI: Integer;
begin
  HighI := High(UndoList);
  UndoList[UndoPos].Assign(IvwMain.Bitmap);
  Inc(UndoPos);
  if UndoPos > HighI then UndoPos := 0;
  if UndoPos = UndoBase then Inc(UndoBase);
  if UndoBase > HighI then UndoBase := 0;
end;

procedure TFrmFloodFill.PopUndo;
var
  HighI: Integer;
begin
  HighI := High(UndoList);
  if UndoPos = UndoBase then Exit;
  Dec(UndoPos);
  if UndoPos < 0 then UndoPos := HighI;
  IvwMain.Bitmap.Assign(UndoList[UndoPos]);
  IvwMain.Refresh;
end;

procedure TFrmFloodFill.BtnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmFloodFill.BtnPictureClick(Sender: TObject);
begin
  if OpenPictDlg.FileName='' then  OpenPictDlg.InitialDir:=SetDirSeparators(pathMedia);
  if OpenPictDlg.Execute then
  begin
    IvwMain.Bitmap.LoadFromFile(OpenPictDlg.FileName);
    ResetUndo;
  end;
end;

procedure TFrmFloodFill.BtnColorClick(Sender: TObject);
begin
  if ColorDialog.Execute then
    PnlColorInner.Color := ColorDialog.Color;
end;

procedure TFrmFloodFill.IvwMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  Pt: TPoint;
begin
  PushUndo;
  Pt := IvwMain.ControlToBitmap(Point(X,Y));
  with TFloodFill.Create do
  try
    Tolerance := StrToIntDef(EdtTolerance.Text, 0);;
    Execute(IvwMain.Bitmap, Pt, Color32(PnlColorInner.Color));
    IvwMain.Refresh;
  finally
    Free;
  end;
end;

procedure TFrmFloodFill.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = Ord('Z')) then PopUndo;
end;

end.
