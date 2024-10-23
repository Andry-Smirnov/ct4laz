unit A3nalogGauge1mw;

interface

uses
  Messages,SysUtils, Classes, Graphics, Controls,LResources,
  Forms, Dialogs, ExtCtrls, StdCtrls, Spin, TplA3nalogGaugeUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    Label1: TLabel;
    Timer: TTimer;
    ColorDialog: TColorDialog;
    AnalogGauge1: TplA3nalogGauge;
    AnalogGauge2: TplA3nalogGauge;
    AnalogGauge3: TplA3nalogGauge;
    FPSLabel: TLabel;
    FreqLabel: TLabel;
    FreqEdit: TSpinEdit;
    FaceColorButton: TButton;
    CenterColorButton: TButton;
    CircleColorButton: TButton;
    MinimLabel: TLabel;
    MinimEdit: TSpinEdit;
    MaximLabel: TLabel;
    MaximEdit: TSpinEdit;
    MinColorButton: TButton;
    MidColorButton: TButton;
    MaxColorButton: TButton;
    TicksColorButton: TButton;
    ValueColorButton: TButton;
    CaptionColorButton: TButton;
    ArrowColorButton: TButton;
    MarginColorButton: TButton;
    MarginLabel: TLabel;
    MarginEdit: TSpinEdit;
    CenterLabel: TLabel;
    CenterRadEdit: TSpinEdit;
    CircleLabel: TLabel;
    CircleRadEdit: TSpinEdit;
    ScaleLabel: TLabel;
    ScaleEdit: TSpinEdit;
    AngleLabel: TLabel;
    AngleEdit: TSpinEdit;
    WidthLabel: TLabel;
    WidthEdit: TSpinEdit;
    NumMainLabel: TLabel;
    NumMainEdit: TSpinEdit;
    MainLenLabel: TLabel;
    MainLenEdit: TSpinEdit;
    SubLenLabel: TLabel;
    SubLenEdit: TSpinEdit;
    MarginBox: TCheckBox;
    MainTicksBox: TCheckBox;
    SubticksBox: TCheckBox;
    IndMinBox: TCheckBox;
    IndMidBox: TCheckBox;
    IndMaxBox: TCheckBox;
    CirclesBox: TCheckBox;
    ValuesBox: TCheckBox;
    CenterBox: TCheckBox;
    FrameBox: TCheckBox;
    Draw3DBox: TCheckBox;
    CaptionBox: TCheckBox;
    CaptionLabel: TLabel;
    CaptionEdit: TEdit;
    CloseButton: TButton;
    AAModeBox: TComboBox;
    AAModeLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure AAModeBoxChange(Sender: TObject);
    procedure FreqEditChange(Sender: TObject);
    procedure FaceColorButtonClick(Sender: TObject);
    procedure CenterColorButtonClick(Sender: TObject);
    procedure CircleColorButtonClick(Sender: TObject);
    procedure MinimEditChange(Sender: TObject);
    procedure MaximEditChange(Sender: TObject);
    procedure MinColorButtonClick(Sender: TObject);
    procedure MidColorButtonClick(Sender: TObject);
    procedure MaxColorButtonClick(Sender: TObject);
    procedure TicksColorButtonClick(Sender: TObject);
    procedure ValueColorButtonClick(Sender: TObject);
    procedure CaptionColorButtonClick(Sender: TObject);
    procedure ArrowColorButtonClick(Sender: TObject);
    procedure MarginColorButtonClick(Sender: TObject);
    procedure MarginEditChange(Sender: TObject);
    procedure CenterRadEditChange(Sender: TObject);
    procedure CircleRadEditChange(Sender: TObject);
    procedure ScaleEditChange(Sender: TObject);
    procedure AngleEditChange(Sender: TObject);
    procedure WidthEditChange(Sender: TObject);
    procedure NumMainEditChange(Sender: TObject);
    procedure MainLenEditChange(Sender: TObject);
    procedure SubLenEditChange(Sender: TObject);
    procedure MarginBoxClick(Sender: TObject);
    procedure MainTicksBoxClick(Sender: TObject);
    procedure SubticksBoxClick(Sender: TObject);
    procedure IndMinBoxClick(Sender: TObject);
    procedure IndMidBoxClick(Sender: TObject);
    procedure IndMaxBoxClick(Sender: TObject);
    procedure CirclesBoxClick(Sender: TObject);
    procedure ValuesBoxClick(Sender: TObject);
    procedure CenterBoxClick(Sender: TObject);
    procedure FrameBoxClick(Sender: TObject);
    procedure Draw3DBoxClick(Sender: TObject);
    procedure CaptionBoxClick(Sender: TObject);
    procedure CaptionEditChange(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
  private
    FDelta: Double;
  end;

var
  MainForm: TMainForm;

implementation


procedure TMainForm.FormCreate(Sender: TObject);
begin
  CenterRadEdit.Value := AnalogGauge1.CenterRadius;
  CircleRadEdit.Value := AnalogGauge1.CircleRadius;
  MarginEdit.Value := AnalogGauge1.Margin;
  ScaleEdit.Value := AnalogGauge1.Scale;
  AngleEdit.Value := AnalogGauge1.Angle;
  MinimEdit.Value := AnalogGauge1.IndMinimum;
  MaximEdit.Value := AnalogGauge1.IndMaximum;
  WidthEdit.Value := AnalogGauge1.ArrowWidth;
  NumMainEdit.Value := AnalogGauge1.NumberMainTicks;
  MainLenEdit.Value := AnalogGauge1.LengthMainTicks;
  SubLenEdit.Value := AnalogGauge1.LengthSubTicks;
  MarginBox.Checked := ShowMargin in AnalogGauge1.FaceOptions;
  CirclesBox.Checked := ShowCircles in AnalogGauge1.FaceOptions;
  MainTicksBox.Checked := ShowMainTicks in AnalogGauge1.FaceOptions;
  SubTicksBox.Checked := ShowSubTicks in AnalogGauge1.FaceOptions;
  IndMinBox.Checked := ShowIndicatorMin in AnalogGauge1.FaceOptions;
  IndMidBox.Checked := ShowIndicatorMid in AnalogGauge1.FaceOptions;
  IndMaxBox.Checked := ShowIndicatorMax in AnalogGauge1.FaceOptions;
  ValuesBox.Checked := ShowValues in AnalogGauge1.FaceOptions;
  CenterBox.Checked := ShowCenter in AnalogGauge1.FaceOptions;
  FrameBox.Checked := ShowFrame in AnalogGauge1.FaceOptions;
  Draw3DBox.Checked := Show3D in AnalogGauge1.FaceOptions;
  CaptionBox.Checked := ShowCaption in AnalogGauge1.FaceOptions;
  CaptionEdit.Text := AnalogGauge1.Caption;
  FreqEdit.Value := Timer.Interval;
  AAModeBox.ItemIndex := 0;  

end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  V: Double;
begin
  V := AnalogGauge1.Position; if FDelta = 0 then FDelta := 1; V := V + FDelta;
  if V < 0 then begin V := 0; FDelta := -FDelta end else
  if V > AnalogGauge1.Scale then begin V := AnalogGauge1.Scale; FDelta := -FDelta end;
  AnalogGauge1.Position := V; AnalogGauge2.Position := V; AnalogGauge3.Position := V;
end;


procedure TMainForm.AAModeBoxChange(Sender: TObject);
var
  AA: TAntialiased;
begin
  case AAModeBox.ItemIndex of
    1: AA := aaBiline;
    2: AA := aaTriline;
    3: AA := aaQuadral;
    else AA := aaNone
  end;
  AnalogGauge1.AntiAliased := AA;
  AnalogGauge2.AntiAliased := AA;
  AnalogGauge3.AntiAliased := AA;
end;

procedure TMainForm.FreqEditChange(Sender: TObject);
begin
  Timer.Interval := FreqEdit.Value
end;

procedure TMainForm.FaceColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.FaceColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.FaceColor := ColorDialog.Color;
    AnalogGauge2.FaceColor := ColorDialog.Color;
    AnalogGauge3.FaceColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.CenterColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.CenterColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.CenterColor := ColorDialog.Color;
    AnalogGauge2.CenterColor := ColorDialog.Color;
    AnalogGauge3.CenterColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.CircleColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.CircleColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.CircleColor := ColorDialog.Color;
    AnalogGauge2.CircleColor := ColorDialog.Color;
    AnalogGauge3.CircleColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.MinimEditChange(Sender: TObject);
begin
  AnalogGauge1.IndMinimum := MinimEdit.Value;
  AnalogGauge2.IndMinimum := MinimEdit.Value;
  AnalogGauge3.IndMinimum := MinimEdit.Value;
end;

procedure TMainForm.MaximEditChange(Sender: TObject);
begin
  AnalogGauge1.IndMaximum := MaximEdit.Value;
  AnalogGauge2.IndMaximum := MaximEdit.Value;
  AnalogGauge3.IndMaximum := MaximEdit.Value;
end;

procedure TMainForm.MinColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.MinColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.MinColor := ColorDialog.Color;
    AnalogGauge2.MinColor := ColorDialog.Color;
    AnalogGauge3.MinColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.MidColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.MidColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.MidColor := ColorDialog.Color;
    AnalogGauge2.MidColor := ColorDialog.Color;
    AnalogGauge3.MidColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.MaxColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.MaxColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.MaxColor := ColorDialog.Color;
    AnalogGauge2.MaxColor := ColorDialog.Color;
    AnalogGauge3.MaxColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.TicksColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.TicksColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.TicksColor := ColorDialog.Color;
    AnalogGauge2.TicksColor := ColorDialog.Color;
    AnalogGauge3.TicksColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.ValueColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.ValueColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.ValueColor := ColorDialog.Color;
    AnalogGauge2.ValueColor := ColorDialog.Color;
    AnalogGauge3.ValueColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.CaptionColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.CaptionColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.CaptionColor := ColorDialog.Color;
    AnalogGauge2.CaptionColor := ColorDialog.Color;
    AnalogGauge3.CaptionColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.ArrowColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.ArrowColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.ArrowColor := ColorDialog.Color;
    AnalogGauge2.ArrowColor := ColorDialog.Color;
    AnalogGauge3.ArrowColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.MarginColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := AnalogGauge1.MarginColor;
  if ColorDialog.Execute then begin
    AnalogGauge1.MarginColor := ColorDialog.Color;
    AnalogGauge2.MarginColor := ColorDialog.Color;
    AnalogGauge3.MarginColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.MarginEditChange(Sender: TObject);
begin
  AnalogGauge1.Margin := MarginEdit.Value;
  AnalogGauge2.Margin := MarginEdit.Value;
  AnalogGauge3.Margin := MarginEdit.Value;
end;

procedure TMainForm.CenterRadEditChange(Sender: TObject);
begin
  AnalogGauge1.CenterRadius := CenterRadEdit.Value;
  AnalogGauge2.CenterRadius := CenterRadEdit.Value;
  AnalogGauge3.CenterRadius := CenterRadEdit.Value;
end;

procedure TMainForm.CircleRadEditChange(Sender: TObject);
begin
  AnalogGauge1.CircleRadius := CircleRadEdit.Value;
  AnalogGauge2.CircleRadius := CircleRadEdit.Value;
  AnalogGauge3.CircleRadius := CircleRadEdit.Value;
end;

procedure TMainForm.ScaleEditChange(Sender: TObject);
begin
  AnalogGauge1.Scale := ScaleEdit.Value;
  AnalogGauge2.Scale := ScaleEdit.Value;
  AnalogGauge3.Scale := ScaleEdit.Value;
end;

procedure TMainForm.AngleEditChange(Sender: TObject);
begin
  AnalogGauge1.Angle := AngleEdit.Value;
  AnalogGauge2.Angle := AngleEdit.Value;
  AnalogGauge3.Angle := AngleEdit.Value;
end;

procedure TMainForm.WidthEditChange(Sender: TObject);
begin
  AnalogGauge1.ArrowWidth := WidthEdit.Value;
  AnalogGauge2.ArrowWidth := WidthEdit.Value;
  AnalogGauge3.ArrowWidth := WidthEdit.Value;
end;

procedure TMainForm.NumMainEditChange(Sender: TObject);
begin
  AnalogGauge1.NumberMainTicks := NumMainEdit.Value;
  AnalogGauge2.NumberMainTicks := NumMainEdit.Value;
  AnalogGauge3.NumberMainTicks := NumMainEdit.Value;
end;

procedure TMainForm.MainLenEditChange(Sender: TObject);
begin
  AnalogGauge1.LengthMainTicks := MainLenEdit.Value;
  AnalogGauge2.LengthMainTicks := MainLenEdit.Value;
  AnalogGauge3.LengthMainTicks := MainLenEdit.Value;
end;

procedure TMainForm.SubLenEditChange(Sender: TObject);
begin
  AnalogGauge1.LengthSubTicks := SubLenEdit.Value;
  AnalogGauge2.LengthSubTicks := SubLenEdit.Value;
  AnalogGauge3.LengthSubTicks := SubLenEdit.Value;
end;

procedure TMainForm.MarginBoxClick(Sender: TObject);
begin
  if MarginBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [ShowMargin];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [ShowMargin];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [ShowMargin];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [ShowMargin];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [ShowMargin];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [ShowMargin];
  end;
end;

procedure TMainForm.MainTicksBoxClick(Sender: TObject);
begin
  if MainTicksBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [ShowMainTicks];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [ShowMainTicks];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [ShowMainTicks];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [ShowMainTicks];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [ShowMainTicks];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [ShowMainTicks];
  end;
end;

procedure TMainForm.SubTicksBoxClick(Sender: TObject);
begin
  if SubTicksBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [ShowSubTicks];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [ShowSubTicks];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [ShowSubTicks];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [ShowSubTicks];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [ShowSubTicks];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [ShowSubTicks];
  end;
end;

procedure TMainForm.IndMinBoxClick(Sender: TObject);
begin
  if IndMinBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [ShowIndicatorMin];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [ShowIndicatorMin];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [ShowIndicatorMin];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [ShowIndicatorMin];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [ShowIndicatorMin];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [ShowIndicatorMin];
  end;
end;

procedure TMainForm.IndMidBoxClick(Sender: TObject);
begin
  if IndMidBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [ShowIndicatorMid];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [ShowIndicatorMid];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [ShowIndicatorMid];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [ShowIndicatorMid];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [ShowIndicatorMid];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [ShowIndicatorMid];
  end;
end;

procedure TMainForm.IndMaxBoxClick(Sender: TObject);
begin
  if IndMaxBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [ShowIndicatorMax];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [ShowIndicatorMax];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [ShowIndicatorMax];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [ShowIndicatorMax];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [ShowIndicatorMax];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [ShowIndicatorMax];
  end;
end;

procedure TMainForm.CirclesBoxClick(Sender: TObject);
begin
  if CirclesBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [ShowCircles];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [ShowCircles];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [ShowCircles];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [ShowCircles];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [ShowCircles];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [ShowCircles];
  end;
end;

procedure TMainForm.ValuesBoxClick(Sender: TObject);
begin
  if ValuesBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [ShowValues];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [ShowValues];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [ShowValues];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [ShowValues];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [ShowValues];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [ShowValues];
  end;
end;

procedure TMainForm.CenterBoxClick(Sender: TObject);
begin
  if CenterBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [ShowCenter];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [ShowCenter];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [ShowCenter];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [ShowCenter];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [ShowCenter];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [ShowCenter];
  end;
end;

procedure TMainForm.FrameBoxClick(Sender: TObject);
begin
  if FrameBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [ShowFrame];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [ShowFrame];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [ShowFrame];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [ShowFrame];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [ShowFrame];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [ShowFrame];
  end;
end;

procedure TMainForm.Draw3DBoxClick(Sender: TObject);
begin
  if Draw3DBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [Show3D];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [Show3D];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [Show3D];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [Show3D];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [Show3D];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [Show3D];
  end;
end;

procedure TMainForm.CaptionBoxClick(Sender: TObject);
begin
  if CaptionBox.Checked then begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions + [ShowCaption];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions + [ShowCaption];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions + [ShowCaption];
  end else begin
    AnalogGauge1.FaceOptions := AnalogGauge1.FaceOptions - [ShowCaption];
    AnalogGauge2.FaceOptions := AnalogGauge2.FaceOptions - [ShowCaption];
    AnalogGauge3.FaceOptions := AnalogGauge3.FaceOptions - [ShowCaption];
  end;
end;

procedure TMainForm.CaptionEditChange(Sender: TObject);
begin
  AnalogGauge1.Caption := CaptionEdit.Text;
  AnalogGauge2.Caption := CaptionEdit.Text;
  AnalogGauge3.Caption := CaptionEdit.Text;
end;

procedure TMainForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;



initialization
  {$I a3naloggauge1mw.lrs}

end.

