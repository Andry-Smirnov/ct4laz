unit ArrowHeadmf;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is ArrowHead Example for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Angus Johnson < http://www.angusj.com >
 *
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface


uses
   LCLIntf, LResources, SysUtils, Classes, Graphics,
  Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, GR32, GR32_Image, GR32_Layers,
  GR32_Paths, GR32_Polygons, GR32_ArrowHeads;

type
  TFmArrowHead = class(TForm)
    Animation: TTimer;
    BtnClose: TButton;
    CbxAnimate: TCheckBox;
    EdtArrowSize: TEdit;
    ImgView32: TImgView32;
    LblArrowSize: TLabel;
    LblLineWidth: TLabel;
    PnlControl: TPanel;
    RgpArrowStyle: TRadioGroup;
    RgpPosition: TRadioGroup;
    TbrAnimationSpeed: TTrackBar;
    TbrLineWidth: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AnimationTimer(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure CbxAnimateClick(Sender: TObject);
    procedure EdtArrowSizeChange(Sender: TObject);
    procedure ImgView32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgView32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgView32MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer; Layer: TCustomLayer);
    procedure ImgView32Resize(Sender: TObject);
    procedure RgpArrowStyleClick(Sender: TObject);
    procedure TbrAnimationSpeedChange(Sender: TObject);
    procedure TbrLineWidthChange(Sender: TObject);
  private
    FArrowSize: Integer;
    FBoxIndex: Integer;
    FLastPos: TPoint;
    FDashes: TArrayOfFloat;
    FAnimationSpeed: Integer;
    FBoxCenter: array [0..1] of TFloatPoint;
    FVelocity: array [0..1] of TFloatPoint;
    FPattern: array [0..1] of TBitmap32;
    FBitmapFiller: TBitmapPolygonFiller;
    procedure SetArrowSize(const Value: Integer);
  protected
    procedure ArrowSizeChanged; virtual;
  public
    procedure ReDraw;

    property ArrowSize: Integer read FArrowSize write SetArrowSize;
  end;

var
  FmArrowHead: TFmArrowHead;

const
  CBoxSize = 60;
  CBorderSize = 10;
  CBoxSizePlus = CBoxSize + CBorderSize;
  CRad = (CBoxSize + CBorderSize) div 2;

implementation

{$R *.lfm}

uses
  Math, GR32_LowLevel, GR32_Geometry, GR32_VectorUtils, GR32_ColorGradients;

const
// CodeTyphon: Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ENDIF}
{$IFDEF UNIX}
  pathMedia = '../xmedia/';
{$ENDIF}

{ Miscellaneous functions }

procedure ChangeSign(var Value: TFloat); {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Value := -Value;
end;

procedure SwapVelocities(var Value1, Value2: TFloat); {$IFDEF USEINLINING} inline; {$ENDIF}
var
  Val: TFloat;
begin
  Val := Value1;
  Value1 := Value2;
  Value2 := Val;
end;

function GetNearestPointOnBox(const Pt, BoxCenter: TFloatPoint;
  const BoxPts: array of TFloatPoint): TFloatPoint;
var
  I, Index: Integer;
  DistSqrd, DS: TFloat;
begin
  Index := 0;
  DistSqrd := SqrDistance(BoxPts[0], Pt);
  for I := 1 to High(BoxPts) do
  begin
    DS := SqrDistance(BoxPts[I], Pt);
    if DS >= DistSqrd then Continue;
    DistSqrd := DS;
    Index := I;
  end;
  if Index = High(BoxPts) then I := 0 else I := Index + 1;
  if not SegmentIntersect(Pt, BoxCenter, BoxPts[Index], BoxPts[I], Result) then
  begin
    if Index = 0 then I := High(BoxPts) else I := Index - 1;
    if not SegmentIntersect(Pt, BoxCenter, BoxPts[Index], BoxPts[I], Result) then
      Result := Pt;
  end;
end;

function BoxesOverlap(const Box1Center, Box2Center: TFloatPoint;
  BoxSize: TFloat): Boolean;
begin
  Result := (Abs(Box1Center.X - Box2Center.X) <= BoxSize) and
    (Abs(Box1Center.Y - Box2Center.Y) <= BoxSize);
end;

function MakeArrayOfFloat(const Data: array of TFloat): TArrayOfFloat;
var
  Index, Len: Integer;
begin
  Len := Length(Data);
  SetLength(Result, Len);
  for Index := 0 to Len - 1
    do Result[Index] := Data[Index];
end;

function MakeBezierCurve(const CtrlPts: TArrayOfFloatPoint): TArrayOfFloatPoint;
var
  Index: Integer;
begin
  with TFlattenedPath.Create do
  try
    MoveTo(CtrlPts[0]);
    for Index := 0 to (High(CtrlPts) - 3) div 3 do
      CurveTo(CtrlPts[Index * 3 + 1], CtrlPts[Index * 3 + 2],
        CtrlPts[Index * 3 + 3]);
    Result := Points;
  finally
    Free;
  end;
end;

function MakeBox(CenterPt: TFloatPoint; Size: TFloat): TArrayOfFloatPoint;
begin
  Size := Size * 0.5;
  SetLength(Result, 4);
  Result[0] := OffsetPoint(CenterPt, -Size, -Size);
  Result[1] := OffsetPoint(CenterPt,  Size, -Size);
  Result[2] := OffsetPoint(CenterPt,  Size,  Size);
  Result[3] := OffsetPoint(CenterPt, -Size,  Size);
end;

{ TFmArrowHead }

procedure TFmArrowHead.FormCreate(Sender: TObject);
begin
  ImgView32.Bitmap.DrawMode := dmOpaque;
  ImgView32.SetupBitmap(True, clWhite32);

  FBoxIndex := -1;
  FArrowSize := 20;
  FDashes := MakeArrayOfFloat([14, 3, 3, 3, 3, 3]);
  FBoxCenter[0] := FloatPoint(120, 100);
  FBoxCenter[1] := FloatPoint(240, 300);
  FAnimationSpeed := TbrAnimationSpeed.Position;
  CbxAnimateClick(nil);

  FPattern[0] := TBitmap32.Create;
  FPattern[0].loadfromfile(pathMedia+'Pattern1.bmp');
  FPattern[1] := TBitmap32.Create;
  FPattern[1].loadfromfile(pathMedia+'Pattern2.bmp');

  FBitmapFiller := TBitmapPolygonFiller.Create;

  Redraw;
end;

procedure TFmArrowHead.FormDestroy(Sender: TObject);
begin
  FPattern[0].Free;
  FPattern[1].Free;
  FBitmapFiller.Free;
end;

procedure TFmArrowHead.ImgView32MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Index: Integer;
begin
  FBoxIndex := -1;
  for Index := 0 to High(FBoxCenter) do
    if PtInRect(FloatRect(FBoxCenter[Index].X - CRad,
      FBoxCenter[Index].Y - CRad, FBoxCenter[Index].X + CRad,
      FBoxCenter[Index].Y + CRad), Point(X, Y)) then
    begin
      FLastPos := Point(X, Y);
      FBoxIndex := Index;
      Exit;
    end;
end;

procedure TFmArrowHead.ImgView32MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Index: Integer;
begin
  if FBoxIndex >= 0 then
  begin
    FBoxCenter[FBoxIndex].X := EnsureRange(FBoxCenter[FBoxIndex].X + X -
      FLastPos.X, CRad, ImgView32.Width - CRad);
    FBoxCenter[FBoxIndex].Y := EnsureRange(FBoxCenter[FBoxIndex].Y + Y -
      FLastPos.Y, CRad, ImgView32.Height - CRad);
    ReDraw;
    FLastPos := Point(X, Y);
  end
  else
  begin
    for Index := 0 to High(FBoxCenter) do
      if PtInRect(FloatRect(FBoxCenter[Index].X - CRad,
        FBoxCenter[Index].Y - CRad, FBoxCenter[Index].X + CRad,
        FBoxCenter[Index].Y + CRad), Point(X, Y)) then
      begin
        ImgView32.Cursor := crHandPoint;
        Exit;
      end;
    ImgView32.Cursor := crArrow;
  end;
end;

procedure TFmArrowHead.ImgView32MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  FBoxIndex := -1;
end;

procedure TFmArrowHead.ImgView32Resize(Sender: TObject);
begin
  ImgView32.Bitmap.SetSize(ImgView32.Width, ImgView32.Height);
  ReDraw;
end;

procedure TFmArrowHead.ReDraw;
var
  Box : array [0..1] of TArrayOfFloatPoint;
  Poly, ArrowPts: TArrayOfFloatPoint;
  StartPoint, EndPoint, StartOffsetPt, EndOffsetPt: TFloatPoint;
  Delta: TFloatPoint;
  Arrow: TArrowHeadAbstract;
  GradientFiller: TLinearGradientPolygonFiller;
const
  StartArrowColor: TColor32 = $60009900;
  StartArrowPenColor: TColor32 = $FF339900;
  EndArrowColor: TColor32 = $600000AA;
  EndArrowPenColor: TColor32 = $FF0033AA;
begin
  ImgView32.Bitmap.Clear(clWhite32);

  case RgpArrowStyle.ItemIndex of
    1: Arrow := TArrowHeadSimple.Create(ArrowSize);
    2: Arrow := TArrowHeadFourPt.Create(ArrowSize);
    3: Arrow := TArrowHeadDiamond.Create(ArrowSize);
    4: Arrow := TArrowHeadCircle.Create(ArrowSize);
    else Arrow := nil;
  end;

  Box[0] := MakeBox(FBoxCenter[0], CBoxSize);
  Box[1] := MakeBox(FBoxCenter[1], CBoxSize);

  FBitmapFiller.Pattern := FPattern[0];
  DashLineFS(ImgView32.Bitmap, Box[0], FDashes, FBitmapFiller, EndArrowPenColor,
    True, CBorderSize, 1.5);

  FBitmapFiller.Pattern := FPattern[1];
  DashLineFS(ImgView32.Bitmap, Box[1], FDashes, FBitmapFiller, EndArrowPenColor,
    True, CBorderSize, 1.5);

  // now accommodate for CBorderSize width as above ...
  Box[0] := MakeBox(FBoxCenter[0], CBoxSizePlus);
  Box[1] := MakeBox(FBoxCenter[1], CBoxSizePlus);
  if BoxesOverlap(FBoxCenter[0], FBoxCenter[1], CBoxSizePlus) then
  begin
    StartPoint := FBoxCenter[0];
    EndPoint := FBoxCenter[1];
  end else
  begin
    StartPoint := GetNearestPointOnBox(FBoxCenter[1], FBoxCenter[0], Box[0]);
    EndPoint := GetNearestPointOnBox(FBoxCenter[0], FBoxCenter[1], Box[1]);
  end;

  Delta.X := StartPoint.X - FBoxCenter[0].X;
  Delta.Y := StartPoint.Y - FBoxCenter[0].Y;
  if Abs(Delta.X) > Abs(Delta.Y) then
    StartOffsetPt := FloatPoint(StartPoint.X + Delta.X * 2, StartPoint.Y)
  else
    StartOffsetPt := FloatPoint(StartPoint.X, StartPoint.Y + Delta.Y *2);

  Delta.X := EndPoint.X - FBoxCenter[1].X;
  Delta.Y := EndPoint.Y - FBoxCenter[1].Y;
  if Abs(Delta.X) > Abs(Delta.Y) then
    EndOffsetPt := FloatPoint(EndPoint.X + Delta.X * 2, EndPoint.Y)
  else
    EndOffsetPt := FloatPoint(EndPoint.X, EndPoint.Y + Delta.Y * 2);

  Poly := BuildPolygonF([StartPoint.X, StartPoint.Y,
    StartOffsetPt.X, StartOffsetPt.Y,
    EndOffsetPt.X, EndOffsetPt.Y,
    EndPoint.X, EndPoint.Y]);
  Poly := MakeBezierCurve(Poly);

  if Assigned(Arrow) then
  begin
    // shorten path at specified end(s) and draw ...
    case RgpPosition.ItemIndex of
      0: Poly := Shorten(Poly, ArrowSize, lpStart);
      1: Poly := Shorten(Poly, ArrowSize, lpEnd);
      2: Poly := Shorten(Poly, ArrowSize, lpBoth);
    end;
    // draw the connecting line ...
    GradientFiller := TLinearGradientPolygonFiller.Create;
    try
      GradientFiller.SimpleGradient(Poly[0], StartArrowPenColor,
        Poly[High(Poly)], EndArrowPenColor);
      PolylineFS(ImgView32.Bitmap, Poly, GradientFiller, False,
        TbrLineWidth.Position);
    finally
      GradientFiller.Free;
    end;

    // draw specified arrows ...
    if RgpPosition.ItemIndex <> 1 then
    begin
      ArrowPts := Arrow.GetPoints(Poly, False);
      PolygonFS(ImgView32.Bitmap, ArrowPts, StartArrowColor);
      PolylineFS(ImgView32.Bitmap, ArrowPts, StartArrowPenColor, True,
        TbrLineWidth.Position);
    end;
    if RgpPosition.ItemIndex <> 0 then
    begin
      ArrowPts := Arrow.GetPoints(Poly, True);
      PolygonFS(ImgView32.Bitmap, ArrowPts, EndArrowColor);
      PolylineFS(ImgView32.Bitmap, ArrowPts, EndArrowPenColor, True,
        TbrLineWidth.Position);
    end;
  end else
    PolylineFS(ImgView32.Bitmap, Poly, clBlack32, False,
      TbrLineWidth.Position);
end;

procedure TFmArrowHead.RgpArrowStyleClick(Sender: TObject);
begin
  ReDraw;
end;

procedure TFmArrowHead.EdtArrowSizeChange(Sender: TObject);
begin
  ArrowSize := EnsureRange(StrToIntDef(EdtArrowSize.Text, ArrowSize), 5, 40);
end;

procedure TFmArrowHead.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFmArrowHead.CbxAnimateClick(Sender: TObject);
begin
  Animation.Enabled := CbxAnimate.Checked;
  Randomize;
  FVelocity[0] := FloatPoint((2 * Random - 1) * FAnimationSpeed,
    (2 * Random -1) * FAnimationSpeed);
  FVelocity[1] := FloatPoint((2 * Random - 1) * FAnimationSpeed,
    (2 * Random -1) * FAnimationSpeed);
end;

procedure TFmArrowHead.TbrAnimationSpeedChange(Sender: TObject);
var
  SpeedRatio: TFloat;
begin
  if not Animation.Enabled then Exit;
  SpeedRatio := TbrAnimationSpeed.Position / FAnimationSpeed;
  FAnimationSpeed := TbrAnimationSpeed.Position;
  with FVelocity[0] do
  begin
    X := X * SpeedRatio;
    Y := Y * SpeedRatio;
  end;
  with FVelocity[1] do
  begin
    X := X * SpeedRatio;
    Y := Y * SpeedRatio;
  end;
end;

procedure TFmArrowHead.AnimationTimer(Sender: TObject);
var
  Index: Integer;
  NextCenter: array [0..1] of TFloatPoint;
begin
  if FBoxIndex >= 0 then Exit;

  // move boxes ...
  FBoxCenter[0] := OffsetPoint(FBoxCenter[0], FVelocity[0].X, FVelocity[0].Y);
  FBoxCenter[1] := OffsetPoint(FBoxCenter[1], FVelocity[1].X, FVelocity[1].Y);
  ReDraw;

  // update velocities where there are collisions ...

  NextCenter[0] := OffsetPoint(FBoxCenter[0], FVelocity[0].X, FVelocity[0].Y);
  NextCenter[1] := OffsetPoint(FBoxCenter[1], FVelocity[1].X, FVelocity[1].Y);
  if BoxesOverlap(NextCenter[0], NextCenter[1], CBoxSizePlus) then
  begin
    // manage box collisions ...
    if (Abs(FBoxCenter[0].X - FBoxCenter[1].X) > CBoxSizePlus) then
        SwapVelocities(FVelocity[0].X, FVelocity[1].X);
    if (Abs(FBoxCenter[0].Y - FBoxCenter[1].Y) > CBoxSizePlus) then
        SwapVelocities(FVelocity[0].Y, FVelocity[1].Y);
    NextCenter[0] := OffsetPoint(FBoxCenter[0], FVelocity[0].X, FVelocity[0].Y);
    NextCenter[1] := OffsetPoint(FBoxCenter[1], FVelocity[1].X, FVelocity[1].Y);
  end;

  // manage wall collisions ...
  for Index := 0 to High(FBoxCenter) do
  begin
    if (NextCenter[Index].X + CRad > ImgView32.Width) or
      (NextCenter[Index].X < CRad) then
        ChangeSign(FVelocity[Index].X);

    if (NextCenter[Index].Y + CRad > ImgView32.Height) or
      (NextCenter[Index].Y < CRad) then
        ChangeSign(FVelocity[Index].Y);
  end;
end;

procedure TFmArrowHead.SetArrowSize(const Value: Integer);
begin
  if FArrowSize <> Value then
  begin
    FArrowSize := Value;
    ArrowSizeChanged;
  end;
end;

procedure TFmArrowHead.ArrowSizeChanged;
begin
  Redraw;
end;

procedure TFmArrowHead.TbrLineWidthChange(Sender: TObject);
begin
  Redraw;
end;

end.
