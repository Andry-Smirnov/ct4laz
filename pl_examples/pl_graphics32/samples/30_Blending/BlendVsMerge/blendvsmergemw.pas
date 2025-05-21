unit BlendVsMergemw;

interface

uses
  LCLIntf, LResources, Buttons,
  SysUtils, Classes, Graphics, Controls, Forms, Math, StdCtrls, ExtCtrls,
  GR32, GR32_Blend, GR32_Image;

type
  TMainForm = class(TForm)
    LabelBlendSettings: TLabel;
    LabelOverlay: TLabel;
    DstImg: TImage32;
    RadioButtonBlend: TRadioButton;
    RadioButtonMerge: TRadioButton;
    LabelVisible: TLabel;
    CheckBoxForeground: TCheckBox;
    CheckBoxBackground: TCheckBox;
    CheckBoxTransparent: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure RadioButtonBlendClick(Sender: TObject);
    procedure RadioButtonMergeClick(Sender: TObject);
    procedure DstImgPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure RadioButtonNoneClick(Sender: TObject);
    procedure CheckBoxImageClick(Sender: TObject);
  private
    FForeground: TBitmap32;
    FBackground: TBitmap32;
    FBackgroundOpaque: TBitmap32;
    FBlendFunc: TBlendReg;
    procedure ModifyAlphaValues;
    procedure DrawBitmap;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
{$IFDEF Darwin}
  MacOSAll,
{$ENDIF}
  GR32_Resamplers, GR32_LowLevel;


//-------------------------------------------------- 9999
const
// CodeTyphon: Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  with DstImg do
  begin
    with PaintStages[0]^ do //Set up custom paintstage to draw checkerboard
    begin
      Stage := PST_CUSTOM;
      Parameter := 1; // use parameter to tag the stage, we inspect this in OnPaintStage
    end;
  end;


    FForeground := TBitmap32.Create;
    FForeground.LoadFromFile(pathMedia+'texture_a.jpg');

    FBackground := TBitmap32.Create;
    FBackground.LoadFromFile(pathMedia+'texture_b.jpg');


  FBackgroundOpaque := TBitmap32.Create;
  FBackgroundOpaque.Assign(FBackground);
  ModifyAlphaValues;

  DstImg.Bitmap.SetSize(FForeground.Width, FForeground.Height);
  FBlendFunc := MergeReg;
  DrawBitmap;
end;

procedure TMainForm.ModifyAlphaValues;
var
  X, Y: Integer;
  Line: PColor32EntryArray;
begin
  for Y := 0 to FForeground.Height - 1 do
  begin
    Line := PColor32EntryArray(FForeground.ScanLine[Y]);
    for X := 0 to FForeground.Width - 1 do
    begin
      Line^[X].A := X;
    end;
  end;

  for Y := 0 to FBackground.Height - 1 do
  begin
    Line := PColor32EntryArray(FBackground.ScanLine[Y]);
    for X := 0 to FBackground.Width - 1 do
    begin
      Line^[X].A := Y;
    end;
  end;
end;

procedure TMainForm.DstImgPaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
const            //0..1
  Colors: array [Boolean] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  R: TRect;
  I, J: Integer;
  OddY: Integer;
  TilesHorz, TilesVert: Integer;
  TileX, TileY: Integer;
  TileHeight, TileWidth: Integer;
begin
  with TImgView32(Sender) do
  begin
    BeginUpdate;
    R := GetViewportRect;
    TileHeight := 8;
    TileWidth := 8;
    TilesHorz := (R.Right - R.Left) div TileWidth;
    TilesVert := (R.Bottom - R.Top) div TileHeight;
    TileY := 0;
    for J := 0 to TilesVert do
    begin
      TileX := 0;
      OddY := J and $1;
      for I := 0 to TilesHorz do
      begin
        Buffer.FillRectS(TileX, TileY, TileX + TileWidth, TileY +
          TileHeight, Colors[I and $1 = OddY]);
        Inc(TileX, TileWidth);
      end;
      Inc(TileY, TileHeight);
    end;
    EndUpdate;
  end;
end;

procedure TMainForm.RadioButtonNoneClick(Sender: TObject);
begin
  DstImg.Bitmap.Clear(0);

  // Needed under Mac OS X
  DstImg.Invalidate;
end;

procedure TMainForm.RadioButtonBlendClick(Sender: TObject);
begin
  FBlendFunc := BlendReg;

  DrawBitmap;
end;

procedure TMainForm.RadioButtonMergeClick(Sender: TObject);
begin
  FBlendFunc := MergeReg;

  DrawBitmap;
end;

procedure TMainForm.CheckBoxImageClick(Sender: TObject);
begin
  DrawBitmap;
end;

procedure TMainForm.DrawBitmap;
var
  X, Y: Integer;
  PSrcF, PSrcB, PDst: PColor32Array;
  Background: TBitmap32;
begin
  if CheckBoxTransparent.Checked then
    Background := FBackground
  else
    Background := FBackgroundOpaque;

  if CheckBoxForeground.Checked then
  begin
    if CheckBoxBackground.Checked then
      for Y := 0 to FForeground.Height - 1 do
      begin
        PSrcF := PColor32Array(FForeground.ScanLine[Y]);
        PSrcB := PColor32Array(Background.ScanLine[Y]);
        PDst := PColor32Array(DstImg.Bitmap.ScanLine[Y]);
        for X := 0 to FForeground.Width - 1 do
          PDst[X] := FBlendFunc(PSrcF[X], PSrcB[X]);
      end
    else
      for Y := 0 to FForeground.Height - 1 do
      begin
        PSrcF := PColor32Array(FForeground.ScanLine[Y]);
        PDst := PColor32Array(DstImg.Bitmap.ScanLine[Y]);
        for X := 0 to FForeground.Width - 1 do
          PDst[X] := PSrcF[X];
      end
  end
  else
  begin
    if CheckBoxBackground.Checked then
      for Y := 0 to FForeground.Height - 1 do
      begin
        PSrcB := PColor32Array(Background.ScanLine[Y]);
        PDst := PColor32Array(DstImg.Bitmap.ScanLine[Y]);
        for X := 0 to FForeground.Width - 1 do
          PDst[X] := PSrcB[X];
      end
    else
      DstImg.Bitmap.Clear(0);
  end;

  //This is needed because we may use MMX
  EMMS;

  // Needed under Mac OS X
  DstImg.Invalidate;
end;

end.
