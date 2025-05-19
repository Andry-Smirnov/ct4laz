unit UnsharpMaskDemomw;

{$MODE Delphi}

interface

uses
{ Delphi }
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ExtDlgs, StdCtrls,
{ Graphics32 }
  GR32, GR32_Image, GR32_RangeBars;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    OpenPictureDialog: TOpenPictureDialog;
    Panel1: TPanel;
    imgvwWorkArea: TImgView32;
    btnLoadImage: TButton;
    grpbxOptions: TGroupBox;
    lblAmount: TLabel;
    ggbrAmount: TGaugeBar;
    lblRadius: TLabel;
    ggbrRadius: TGaugeBar;
    lblThreshold: TLabel;
    ggbrThreshold: TGaugeBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadImageClick(Sender: TObject);
    procedure ggbrAmountChange(Sender: TObject);
    procedure ggbrRadiusChange(Sender: TObject);
    procedure ggbrThresholdChange(Sender: TObject);
    procedure GaugeBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FSourceBitmap: TBitmap32;

    procedure ExecuteUnsharpMask;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
{ GraphicsMagic }
  gmUnsharpMask;

{$R *.lfm}

const
  AMOUNT_MID_POS = 100;
  RADIUS_MID_POS = 10;

procedure TfrmMain.ExecuteUnsharpMask;
var
  LAmount: Double;
  LRadius: Double;
begin
  LAmount := ggbrAmount.Position / AMOUNT_MID_POS;
  LRadius := ggbrRadius.Position / RADIUS_MID_POS;

  imgvwWorkArea.Bitmap.Assign(FSourceBitmap);
  UnsharpMask(imgvwWorkArea.Bitmap, LAmount, LRadius, ggbrThreshold.Position);
end; 

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSourceBitmap := nil;

  lblAmount.Caption    := 'Amount: ' + IntToStr(ggbrAmount.Position) + '%';
  lblRadius.Caption    := Format('Radius: %.1f pixels', [ggbrRadius.Position / RADIUS_MID_POS]);
  lblThreshold.Caption := 'Threshold: ' + IntToStr(ggbrThreshold.Position) + ' levels';
end; 

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FSourceBitmap.Free;
end;

procedure TfrmMain.LoadImageClick(Sender: TObject);
begin


  if OpenPictureDialog.FileName='' then
     OpenPictureDialog.InitialDir:=ExpandFileName(SetDirSeparators('..\..\..\pl_Graphics32\samples\xmedia\'));

  if OpenPictureDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      if Assigned(FSourceBitmap) then
      begin
        FreeAndNil(FSourceBitmap);
      end;

      FSourceBitmap := TBitmap32.Create;
      FSourceBitmap.LoadFromFile(OpenPictureDialog.FileName);
      ExecuteUnsharpMask;

      grpbxOptions.Visible := True;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.ggbrAmountChange(Sender: TObject);
begin
  lblAmount.Caption := 'Amount: ' + IntToStr(ggbrAmount.Position) + '%';
end;

procedure TfrmMain.ggbrRadiusChange(Sender: TObject);
begin
  lblRadius.Caption := Format('Radius: %.1f pixels', [ggbrRadius.Position / RADIUS_MID_POS]);
end;

procedure TfrmMain.ggbrThresholdChange(Sender: TObject);
begin
  lblThreshold.Caption := 'Threshold: ' + IntToStr(ggbrThreshold.Position) + ' levels';
end;

procedure TfrmMain.GaugeBarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crHourGlass;
  try
    ExecuteUnsharpMask;
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
