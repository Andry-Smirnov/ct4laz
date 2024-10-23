{***************************************************************
  This Application is part of CodeTyphon Studio
  by PiloLogic Software House (https://www.pilotlogic.com/)
****************************************************************}

unit mainform;

interface

uses
  Classes, Graphics,LResources, Forms, Buttons, Dialogs,Controls, StdCtrls, ComCtrls, ExtCtrls,
  SysUtils, TplLCDScreenUnit;

type
  TForm1 = class(TForm)
    CycleLCDAnimator: TplLCDAnimator;
    WaveLCDAnimator: TplLCDAnimator;
    FlashLCDAnimator: TplLCDAnimator;
    GravityLCDAnimator: TplLCDAnimator;
    ScrollLCDAnimator: TplLCDAnimator;
    Animation: TGroupBox;
    DecSpB: TSpeedButton;
    GoSpB: TSpeedButton;
    IncSpB: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    CycleSpB: TSpeedButton;
    FlashSpB: TSpeedButton;
    WaveSpB: TSpeedButton;
    GravitySpB: TSpeedButton;
    Look: TGroupBox;
    IntensityTB: TTrackBar;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    DotSizeCB: TComboBox;
    ColorCB: TComboBox;
    PixShapeCB: TComboBox;
    Label1: TLabel;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton23: TSpeedButton;
    SpeedButton24: TSpeedButton;
    SpeedButton25: TSpeedButton;
    SpeedButton26: TSpeedButton;
    LCDAnimator1: TplLCDAnimator;
    Label7: TLabel;
    DisplayModeCB: TComboBox;
    Label2: TLabel;
    LinesGB: TGroupBox;
    Label11: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    FontNameCB: TComboBox;
    FontSizeCB: TComboBox;
    BlinkChckB: TCheckBox;
    InverseChckB: TCheckBox;
    UnderlineChckB: TCheckBox;
    StrikeChckB: TCheckBox;
    ItalicChckB: TCheckBox;
    BoldChckB: TCheckBox;
    LinesModeCB: TComboBox;
    LinesXTB: TTrackBar;
    LinesYTB: TTrackBar;
    BitmapGB: TGroupBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    BitmapModeCB: TComboBox;
    BitmapXTB: TTrackBar;
    BitmapYTB: TTrackBar;
    LCDScreen1: TplLCDScreen;
    RadioGroup1: TRadioGroup;
    NTrspRB: TRadioButton;
    TrspRB: TRadioButton;

    procedure IncSpBClick(Sender: TObject);
    procedure DecSpBClick(Sender: TObject);
    procedure GoSpBClick(Sender: TObject);
    procedure DotSizeCBChange(Sender: TObject);
    procedure ColorCBChange(Sender: TObject);
    procedure IntensityTBChange(Sender: TObject);
    procedure SpeedButton15Click(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure SpeedButton17Click(Sender: TObject);
    procedure SpeedButton16Click(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure PixShapeCBChange(Sender: TObject);
    procedure CyclingClick(Sender: TObject);
    procedure WavingClick(Sender: TObject);
    procedure FlashingClick(Sender: TObject);
    procedure GravityClick(Sender: TObject);
    procedure BlinkChckBClick(Sender: TObject);
    procedure InverseChckBClick(Sender: TObject);
    procedure SpeedButton21Click(Sender: TObject);
    procedure SpeedButton23Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton24Click(Sender: TObject);
    procedure SpeedButton25Click(Sender: TObject);
    procedure SpeedButton19Click(Sender: TObject);
    procedure SpeedButton18Click(Sender: TObject);
    procedure UnderlineChckBClick(Sender: TObject);
    procedure StrikeChckBClick(Sender: TObject);
    procedure SpeedButton26Click(Sender: TObject);
    procedure FontNameCBChange(Sender: TObject);
    procedure FontSizeCBChange(Sender: TObject);

    procedure BoldChckBClick(Sender: TObject);
    procedure ItalicChckBClick(Sender: TObject);
    procedure DisplayModeCBChange(Sender: TObject);
    procedure BitmapModeCBChange(Sender: TObject);
    procedure LinesXTBChange(Sender: TObject);
    procedure LinesYTBChange(Sender: TObject);
    procedure BitmapXTBChange(Sender: TObject);
    procedure BitmapYTBChange(Sender: TObject);
    procedure LinesModeCBChange(Sender: TObject);
    procedure NTrspRBClick(Sender: TObject);
    procedure TrspRBClick(Sender: TObject);
    
  private
    { Dιclarations privιes }
  public
    { Dιclarations publiques }
  end;

var
  Form1: TForm1;

const
  FPixSize:  array[1..7] of TPixelSize  = (pix1x1, pix2x2, pix3x3, pix4x4, pix5x5,
                                           pix6x6, pix7x7);

  FPixShape: array[1..2] of TPixelShape = (psSquare, psRound);

  FAnimMode: array[1..2] of TAnimMode = (amStatic, amDynamic);

  FDisplayMode: array[1..3] of TDisplayMode = (dmText, dmBitmap, dmBoth);

implementation

procedure TForm1.IncSpBClick(Sender: TObject);
begin
  if LCDScreen1.AnimationDelay <> 1
  then LCDScreen1.AnimationDelay := LCDScreen1.AnimationDelay - 50;
end;

procedure TForm1.DecSpBClick(Sender: TObject);
begin
  LCDScreen1.AnimationDelay := LCDScreen1.AnimationDelay + 50;
end;

procedure TForm1.GoSpBClick(Sender: TObject);
begin
  if LCDScreen1.AnimationEnabled = False
  then begin
         GoSpB.Caption := 'STOP';
         GoSpB.Font.Color := clMaroon;
         LCDScreen1.AnimationEnabled := True;
         end
  else begin
         GoSpB.Caption := 'GO!';
         GoSpB.Font.Color := clGreen;
         LCDScreen1.AnimationEnabled := False;
         end;
end;


procedure TForm1.DotSizeCBChange(Sender: TObject);
begin
  LCDScreen1.PixelSize := FPixSize[DotSizeCB.ItemIndex + 1];
end;

procedure TForm1.PixShapeCBChange(Sender: TObject);
begin
  LCDScreen1.PixelShape := FPixShape[PixShapeCB.ItemIndex + 1];
end;

procedure TForm1.ColorCBChange(Sender: TObject);
begin
  case ColorCB.ItemIndex of
       0 : begin  {Blue}
             LCDScreen1.PixelOff := clTeal;
             LCDScreen1.Font.Color  := clAqua;
             LCDScreen1.Color    := clBlack;
             end;
       1 : begin  {Gray}
             LCDScreen1.PixelOff := $00AAAAAA;
             LCDScreen1.Font.Color  := clBlack;
             LCDScreen1.Color    := clSilver
             end;
       2 : begin {Green}
             LCDScreen1.PixelOff := clGreen;
             LCDScreen1.Font.Color  := clLime;
             LCDScreen1.Color    := clBlack;
             end;
       3 : begin {Pink}
             LCDScreen1.PixelOff := clPurple;
             LCDScreen1.Font.Color  := clFuchsia;
             LCDScreen1.Color    := clBlack;
             end;
       4 : begin {Red}
             LCDScreen1.PixelOff := clMaroon;
             LCDScreen1.Font.Color  := clRed;
             LCDScreen1.Color    := clBlack;
             end;
       5 : begin {Yellow}
             LCDScreen1.PixelOff := clOlive;
             LCDScreen1.Font.Color  := clYellow;
             LCDScreen1.Color    := clBlack;
             end;
   end;

end;

procedure TForm1.IntensityTBChange(Sender: TObject);
begin
  LCDScreen1.Intensity := - IntensityTB.Position;
end;

procedure TForm1.SpeedButton15Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[HorzScroll(1); ]');
  LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton14Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[HorzScroll(2); ]');
  LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton13Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[HorzScroll(4); ]');
  LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton17Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[HorzScroll(-1); ]');   LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton16Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[HorzScroll(-2); ]');  LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton12Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[HorzScroll(-4); ]');  LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton8Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[VertScroll(-1); ]');   LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[VertScroll(-2); ]');   LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[VertScroll(-4); ]');  LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton11Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[VertScroll(1); ]');   LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton10Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[VertScroll(2); ]');      LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton9Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[VertScroll(4); ]');   LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.CyclingClick(Sender: TObject);
begin
  LCDScreen1.LCDAnimator := CycleLCDAnimator;
end;

procedure TForm1.WavingClick(Sender: TObject);
begin
  LCDScreen1.LCDAnimator := WaveLCDAnimator;
end;

procedure TForm1.FlashingClick(Sender: TObject);
begin
  LCDScreen1.LCDAnimator := FlashLCDAnimator;
end;

procedure TForm1.GravityClick(Sender: TObject);
begin
  LCDScreen1.LCDAnimator := GravityLCDAnimator;
end;

procedure TForm1.BlinkChckBClick(Sender: TObject);
begin
  if BlinkChckB.Checked
  then LCDScreen1.SpecialEffects := LCDScreen1.SpecialEffects + [spBlink]
  else LCDScreen1.SpecialEffects := LCDScreen1.SpecialEffects - [spBlink];
end;

procedure TForm1.InverseChckBClick(Sender: TObject);
begin
  if InverseChckB.Checked
  then LCDScreen1.SpecialEffects := LCDScreen1.SpecialEffects + [spInverse]
  else LCDScreen1.SpecialEffects := LCDScreen1.SpecialEffects - [spInverse];
end;

procedure TForm1.SpeedButton21Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[HorzScroll(1); VertScroll(-1);]');   LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton23Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[HorzScroll(2); VertScroll(-2);]');   LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[HorzScroll(1); VertScroll(1);]');   LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton7Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[HorzScroll(2); VertScroll(2);]');   LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;
procedure TForm1.SpeedButton24Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[HorzScroll(-1); VertScroll(1);]');   LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton25Click(Sender: TObject);
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[HorzScroll(-2); VertScroll(2);]');   LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;

procedure TForm1.SpeedButton19Click(Sender: TObject);
begin
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[HorzScroll(-1); VertScroll(-1);]');   LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;
end;

procedure TForm1.SpeedButton18Click(Sender: TObject);
begin
begin
  ScrollLcdAnimator.Code.Clear;
  ScrollLcdAnimator.Code.Add('[HorzScroll(-2); VertScroll(-2);]');   LCDScreen1.LCDAnimator := ScrollLCDAnimator;
end;
end;


procedure TForm1.UnderlineChckBClick(Sender: TObject);
begin
  if UnderlineChckB.Checked
  then LCDScreen1.SpecialEffects := LCDScreen1.SpecialEffects + [spUnderline]
  else LCDScreen1.SpecialEffects := LCDScreen1.SpecialEffects - [spUnderline];
end;


procedure TForm1.StrikeChckBClick(Sender: TObject);
begin
  if StrikeChckB.Checked
  then LCDScreen1.SpecialEffects := LCDScreen1.SpecialEffects + [spStrike]
  else LCDScreen1.SpecialEffects := LCDScreen1.SpecialEffects - [spStrike];
end;

procedure TForm1.SpeedButton26Click(Sender: TObject);
begin
    LCDScreen1.Reset(rmDisplay);
end;

procedure TForm1.FontNameCBChange(Sender: TObject);
begin
  LcdScreen1.Font.Name := FontNameCB.Text;
end;

procedure TForm1.FontSizeCBChange(Sender: TObject);
begin
  LcdScreen1.Font.Size := StrtoInt(FontSizeCB.Text);
end;

procedure TForm1.BoldChckBClick(Sender: TObject);
begin
  if BoldChckB.Checked
  then LCDScreen1.SpecialEffects := LCDScreen1.SpecialEffects + [spBold]
  else LCDScreen1.SpecialEffects := LCDScreen1.SpecialEffects - [spbold];
end;

procedure TForm1.ItalicChckBClick(Sender: TObject);
begin
  if ItalicChckB.Checked
  then LCDScreen1.SpecialEffects := LCDScreen1.SpecialEffects + [spItalic]
  else LCDScreen1.SpecialEffects := LCDScreen1.SpecialEffects - [spItalic];
end;

procedure TForm1.DisplayModeCBChange(Sender: TObject);
begin
  case DisplayModeCB.ItemIndex of
  0: begin LinesGB.Show; BitmapGB.Hide; end;
  1: begin LinesGB.Hide; BitmapGB.Show; end;
  2: begin LinesGB.Show; BitmapGB.Show; end;
  end;

  LCDScreen1.DisplayMode := FDisplayMode[DisplayModeCB.ItemIndex + 1];
end;

procedure TForm1.BitmapModeCBChange(Sender: TObject);
begin
  LCDScreen1.BitmapAnimMode := FAnimMode[BitmapModeCB.ItemIndex + 1];
end;

procedure TForm1.LinesXTBChange(Sender: TObject);
begin
  Label11.Caption := 'LinesXOffset:                           ' + InttoStr(LinesXTB.Position);
  LCDScreen1.LinesXOffset := LinesXTB.Position;
end;

procedure TForm1.LinesYTBChange(Sender: TObject);
begin
  Label10.Caption := 'LinesYOffset:                           ' + InttoStr(LinesYTB.Position);
  LCDScreen1.LinesYOffset := LinesYTB.Position;
end;

procedure TForm1.BitmapXTBChange(Sender: TObject);
begin
  Label13.Caption := 'BitmapXOffset:                           ' + InttoStr(BitmapXTB.Position);
  LCDScreen1.BitmapXOffset := BitmapXTB.Position;
end;

procedure TForm1.BitmapYTBChange(Sender: TObject);
begin
  Label15.Caption := 'BitmapYOffset:                           ' + InttoStr(BitmapYTB.Position);
  LCDScreen1.BitmapYOffset := BitmapYTB.Position;
end;

procedure TForm1.LinesModeCBChange(Sender: TObject);
begin
  LCDScreen1.LinesAnimMode := FAnimMode[LinesModeCB.ItemIndex + 1];
end;

procedure TForm1.NTrspRBClick(Sender: TObject);
begin
  if NTrspRB.Checked then LCDScreen1.BitmapCopyMode := cmNotTransparent;
end;

procedure TForm1.TrspRBClick(Sender: TObject);
begin
  if TrspRB.Checked then LCDScreen1.BitmapCopyMode := cmTransparent;
end;

initialization
  {$I mainform.lrs}
end.
