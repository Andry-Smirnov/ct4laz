unit Benchmarkmw;

interface

uses
  LCLIntf, LCLType, LMessages,
  SysUtils, Classes, Graphics, StdCtrls, Controls, Forms, Dialogs, ExtCtrls,
  GR32_Image, GR32_Paths, GR32, GR32_Polygons;

const
  TEST_DURATION = 4000;  // test for 4 seconds

type
  TTestProc = procedure(Canvas: TCanvas32);

  { TMainForm }

  TMainForm = class(TForm)
    BtnBenchmark: TButton;
    BtnExit: TButton;
    CbxAllRenderers: TCheckBox;
    CbxAllTests: TCheckBox;
    CmbRenderer: TComboBox;
    CmbTest: TComboBox;
    GbxResults: TGroupBox;
    GbxSettings: TGroupBox;
    Img: TImage32;
    LblRenderer: TLabel;
    LblTest: TLabel;
    MemoLog: TMemo;
    PnlBenchmark: TPanel;
    PnlBottom: TPanel;
    PnlSpacer: TPanel;
    PnlTop: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure BtnBenchmarkClick(Sender: TObject);
    procedure ImgResize(Sender: TObject);
    procedure BtnExitClick(Sender: TObject);
  private
    procedure RunTest(TestProc: TTestProc; TestTime: Int64 = TEST_DURATION);
    procedure WriteTestResult(OperationsPerSecond: Integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Math, GR32_System, GR32_LowLevel, GR32_Resamplers, GR32_Brushes,
  GR32_Backends, GR32_VPR2, GR32_PolygonsAggLite;

const
  GridScale: Integer = 40;

var
  TestRegistry: TStringList;

procedure RegisterTest(const TestName: string; Test: TTestProc);
begin
  if not Assigned(TestRegistry) then
    TestRegistry := TStringList.Create;
  TestRegistry.AddObject(TestName, TObject(@Test));
end;

procedure TMainForm.WriteTestResult(OperationsPerSecond: Integer);
begin
  MemoLog.Lines.Add(Format('%s: %d op/s', [cmbRenderer.Text,
    OperationsPerSecond]));
end;

procedure TMainForm.RunTest(TestProc: TTestProc; TestTime: Int64);
var
  Canvas: TCanvas32;
  i, t: Int64;
begin
  TestTime := TestTime * 1000;
  RandSeed := 0;
  Img.Bitmap.Clear(clWhite32);
  Update;
  Canvas := TCanvas32.Create(Img.Bitmap);
  try try
    Canvas.Brushes.Add(TSolidBrush);
    Canvas.Brushes.Add(TStrokeBrush);
    Canvas.Brushes[0].Visible := True;
    Canvas.Brushes[1].Visible := False;
    i := 0;
    GlobalPerfTimer.Start;
    repeat
      TestProc(Canvas);
      TestProc(Canvas);
      TestProc(Canvas);
      TestProc(Canvas);
      TestProc(Canvas);
      TestProc(Canvas);
      TestProc(Canvas);
      TestProc(Canvas);
      TestProc(Canvas);
      TestProc(Canvas);
      t := GlobalPerfTimer.ReadValue;
      Inc(i, 10);
    until t > TestTime;
    WriteTestResult((i*1000000) div t);
  except
      MemoLog.Lines.Add(Format('%s: Failed', [cmbRenderer.Text]));
  end;
  finally
    Canvas.Free;
  end;
  Img.Invalidate; // VPR2 and VPR2X doesn't call TBitmap32.Changed when they draw
end;

function RandColor: TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := Random($FFFFFF) or Random($ff) shl 24;
end;

//----------------------------------------------------------------------------//
// ellipses
//----------------------------------------------------------------------------//
procedure EllipseTest(Canvas: TCanvas32);
var
  W, H: Integer;
begin
  W := Canvas.Bitmap.Width;
  H := Canvas.Bitmap.Height;
  (Canvas.Brushes[0] as TSolidBrush).FillColor := RandColor;
  Canvas.Path.Ellipse(Random(W), Random(H), Random(W shr 1), Random(H shr 1));
end;

//----------------------------------------------------------------------------//
// thin lines
//----------------------------------------------------------------------------//
procedure ThinLineTest(Canvas: TCanvas32);
var
  W, H: Integer;
begin
  W := Canvas.Bitmap.Width;
  H := Canvas.Bitmap.Height;
  Canvas.Brushes[0].Visible := False;
  Canvas.Brushes[1].Visible := True;
  with Canvas.Brushes[1] as TStrokeBrush do
  begin
    StrokeWidth := 1;
    FillColor := RandColor;
  end;
  Canvas.Path.BeginPath;
  Canvas.Path.MoveTo(Random(W), Random(H));
  Canvas.Path.LineTo(Random(W), Random(H));
  Canvas.Path.EndPath;
end;

//----------------------------------------------------------------------------//
// thick lines
//----------------------------------------------------------------------------//
procedure ThickLineTest(Canvas: TCanvas32);
var
  W, H: Integer;
begin
  W := Canvas.Bitmap.Width;
  H := Canvas.Bitmap.Height;
  Canvas.Brushes[0].Visible := False;
  Canvas.Brushes[1].Visible := True;
  with Canvas.Brushes[1] as TStrokeBrush do
  begin
    StrokeWidth := 10;
    FillColor := RandColor;
  end;
  Canvas.Path.BeginPath;
  Canvas.Path.MoveTo(Random(W), Random(H));
  Canvas.Path.LineTo(Random(W), Random(H));
  Canvas.Path.EndPath;
end;

//----------------------------------------------------------------------------//
// text
//----------------------------------------------------------------------------//
const
  STRINGS: array [0..5] of string = (
    'Graphics32',
    'Excellence endures!',
    'Hello World!',
    'Lorem ipsum dolor sit amet, consectetur adipisicing elit,' + #13#10 +
    'sed do eiusmod tempor incididunt ut labore et dolore magna' + #13#10 +
    'aliqua. Ut enim ad minim veniam, quis nostrud exercitation' + #13#10 +
    'ullamco laboris nisi ut aliquip ex ea commodo consequat.',
    'The quick brown fox jumps over the lazy dog.',
    'Jackdaws love my big sphinx of quartz.');

type
  TFontEntry = record
    Name: string;
    Size: Integer;
    Style: TFontStyles;
  end;

const
  FACES: array [0..5] of TFontEntry = (
    (Name: 'Trebuchet MS'; Size: 24; Style: [fsBold]),
    (Name: 'Tahoma'; Size: 20; Style: [fsItalic]),
    (Name: 'Courier New'; Size: 14; Style: []),
    (Name: 'Georgia'; Size: 8; Style: [fsItalic]),
    (Name: 'Times New Roman'; Size: 12; Style: []),
    (Name: 'Garamond'; Size: 12; Style: [])
  );

procedure TextTest(Canvas: TCanvas32);
var
  W, H, I: Integer;
  Font: TFont;
begin
  W := Canvas.Bitmap.Width;
  H := Canvas.Bitmap.Height;
  (Canvas.Brushes[0] as TSolidBrush).FillColor := RandColor;

  I := Random(5);
  Font := Canvas.Bitmap.Font;
  Font.Name := FACES[I].Name;
  Font.Size := FACES[I].Size;
  Font.Style := FACES[I].Style;

  Canvas.RenderText(Random(W), Random(H), STRINGS[I]);
end;

//----------------------------------------------------------------------------//
// splines
//----------------------------------------------------------------------------//
function MakeCurve(const Points: TArrayOfFloatPoint; Kernel: TCustomKernel;
  Closed: Boolean; StepSize: Integer): TArrayOfFloatPoint;
var
  I, J, F, H, Index, LastIndex, Steps, R: Integer;
  K, V, W, X, Y: TFloat;
  Delta: TFloatPoint;
  Filter: TFilterMethod;
  WrapProc: TWrapProc;
  PPoint: PFloatPoint;
const
  WRAP_PROC: array[Boolean] of TWrapProc = (Clamp, Wrap);
begin
  WrapProc := Wrap_PROC[Closed];
  Filter := Kernel.Filter;
  R := Ceil(Kernel.GetWidth);
  H := High(Points);

  LastIndex := H - Ord(not Closed);
  Steps := 0;
  for I := 0 to LastIndex do
  begin
    Index := WrapProc(I + 1, H);
    Delta.X := Points[Index].X - Points[I].X;
    Delta.Y := Points[Index].Y - Points[I].Y;
    Inc(Steps, Floor(Hypot(Delta.X, Delta.Y) / StepSize) + 1);
  end;

  SetLength(Result, Steps);
  PPoint := @Result[0];

  for I := 0 to LastIndex do
  begin
    Index := WrapProc(I + 1, H);
    Delta.X := Points[Index].X - Points[I].X;
    Delta.Y := Points[Index].Y - Points[I].Y;
    Steps := Floor(Hypot(Delta.X, Delta.Y) / StepSize);
    if Steps > 0 then
    begin
      K := 1 / Steps;
      V := 0;
      for J := 0 to Steps do
      begin
        X := 0; Y := 0;
        for F := -R to R do
        begin
          Index := WrapProc(I - F, H);
          W := Filter(F + V);
          X := X + W * Points[Index].X;
          Y := Y + W * Points[Index].Y;
        end;
        PPoint^ := FloatPoint(X, Y);
        Inc(PPoint);
        V := V + K;
      end;
    end;
  end;
end;

procedure SplinesTest(Canvas: TCanvas32);
var
  Input, Points: TArrayOfFloatPoint;
  K: TSplineKernel;
  W, H, I: Integer;
begin
  W := Canvas.Bitmap.Width;
  H := Canvas.Bitmap.Height;
  SetLength(Input, 10);
  for I := 0 to High(Input) do
  begin
    Input[I].X := Random(W);
    Input[I].Y := Random(H);
  end;
  K := TSplineKernel.Create;
  try
    Points := MakeCurve(Input, K, True, 3);
  finally
    K.Free;
  end;
  (Canvas.Brushes[0] as TSolidBrush).FillColor := RandColor;
  Canvas.Path.Polygon(Points);
end;

//----------------------------------------------------------------------------//

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // set priority class and thread priority for better accuracy
{.$IFDEF MSWindows}
 // SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);
 // SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_HIGHEST);
{.$ENDIF}

  CmbTest.Items := TestRegistry;
  CmbTest.ItemIndex := 0;
  PolygonRendererList.GetClassNames(CmbRenderer.Items);
  CmbRenderer.ItemIndex := 0;
  Img.SetupBitmap(True, clWhite32);
end;


procedure TMainForm.BtnBenchmarkClick(Sender: TObject);

  procedure TestRenderer;
  begin
    DefaultPolygonRendererClass := TPolygonRenderer32Class(
      PolygonRendererList[CmbRenderer.ItemIndex]);
    RunTest(TTestProc(cmbTest.Items.Objects[cmbTest.ItemIndex]));
  end;

  procedure TestAllRenderers;
  var
    I: Integer;
  begin
    for I := 0 to CmbRenderer.Items.Count - 1 do
    begin
      CmbRenderer.ItemIndex := I;
      TestRenderer;
    end;
    MemoLog.Lines.Add('');
  end;

  procedure PerformTest;
  begin
    MemoLog.Lines.Add(Format('=== Test: %s ===', [cmbTest.Text]));
    if CbxAllRenderers.Checked then
      TestAllRenderers
    else
      TestRenderer;
  end;

  procedure PerformAllTests;
  var
    I: Integer;
  begin
    for I := 0 to CmbTest.Items.Count - 1 do
    begin
      CmbTest.ItemIndex := I;
      Repaint;
      PerformTest;
    end;
    MemoLog.Lines.Add('');
  end;

begin
  Screen.Cursor := crHourGlass;
  try
    if CbxAllTests.Checked then
      PerformAllTests
    else
      PerformTest;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ImgResize(Sender: TObject);
begin
  Img.SetupBitmap(True, clWhite32);
end;

procedure TMainForm.BtnExitClick(Sender: TObject);
begin
  Close;
end;

initialization
  RegisterTest('Ellipses', EllipseTest);
  RegisterTest('Thin Lines', ThinLineTest);
  RegisterTest('Thick Lines', ThickLineTest);
  RegisterTest('Splines', SplinesTest);
  if Assigned(TBitmap32.GetPlatformBackendClass.GetInterfaceEntry(ITextToPathSupport)) then
    RegisterTest('Text', TextTest);

end.
