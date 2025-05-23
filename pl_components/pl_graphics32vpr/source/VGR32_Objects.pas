
{**********************************************************************
 Package pl_Graphics32VPR.pkg
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

unit VGR32_Objects;


(* BEGIN LICENSE BLOCK *********************************************************
 * Version: MPL 1.1
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
 * The Initial Developer of the GR32_Objects Code is Angus Johnson
 * <angus@angusj.com>. The GR32_Objects Code is Copyright (C) 2009-2010
 * All rights reserved
 *
 * Version 0.58 alpha (Last updated 10-Nov-2010)
 *
 * END LICENSE BLOCK **********************************************************)

interface

uses
  LCLIntf, LCLType, Controls, Graphics, Forms,
  Classes, SysUtils, Math,
  GR32, GR32_Blend, GR32_Filters,
  GR32_Layers, GR32_LowLevel, GR32_MicroTiles,
  GR32_Polygons,
  GR32_Rasterizers,
  GR32_Resamplers, GR32_System, GR32_Transforms, GR32_VPR,
  VGR32_PolygonsEx, VGR32_Pictures,
  VGR32_Misc, VGR32_Lines,
  VGR32_Text,
  TypInfo;

const
  crVPRHandPointing = 1;
  crVPRHandOpen = 2;
  crVPRHandClosed = 3;

  DRAG_MOVE = -2;
  DRAG_NONE = -1;

  MIN_OBJ_SIZE = 6;
  DEFAULT_BTN_SIZE = 3;
  DEFAULT_MARGIN = DEFAULT_BTN_SIZE + 1;
  DEFAULT_OBJ_SIZE = 100;

type
  TDesignerMovingEvent = procedure(Sender: TObject; const OldLocation: TFloatRect; var NewLocation: TFloatRect;
    DragState: integer; Shift: TShiftState) of object;

  TDragStateEvent = procedure(Sender: TObject; var DragState: integer) of object;

  TArrayOfBoolean = array of boolean;
  TUpdatePendingInfo = (upiNoRedraw, upiRedraw, upiEndUpdate);
  TUpdatePendingInfos = set of TUpdatePendingInfo;
  TFillStyle = (fsPlain, fsGradiant, fsRadial, fsCustom);

  TArrow = class(TPersistent)
  private
    FStyle: TArrowHeadStyle;
    FSize: single;
    FColor: TColor32;
    FOnChange: TNotifyEvent;
    procedure SetStyle(Value: TArrowHeadStyle);
    procedure SetSize(Value: single);
    procedure SetColor(Value: TColor32);
  published
    property Style: TArrowHeadStyle read FStyle write SetStyle;
    property Size: single read FSize write SetSize;
    property Color: TColor32 read FColor write SetColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDesignerLayer = class;
  TDrawObjLayerBaseClass = class of TDrawObjLayerBase;

  TDrawObjLayerBase = class(TBitmapLayer)
  private
    FLine32: TLine32;
    FControlBtns: TArrayOfFixedPoint;
    FUnrotatedOutline: TArrayOfFixedPoint;
    FText: UnicodeString;
    FTextPadding: integer;
    FStrokeWidth: single;
    FStrokeColor: TColor32;
    FStrokeStyle: TPenStyle;
    FShadowColor: TColor32;
    FShadowOffset: TPoint;
    FFillColors: TArrayOfColor32;
    FFillStyle: TFillStyle;
    FRotationPoint: TFloatPoint;
    FButtonsLocked: boolean;
    FRefreshNeeded: boolean;
    FDesigner: TDesignerLayer;
    FUpdateCount: integer;
    FUnpublishedProps: TStringList;
    procedure SetFillColor(color: TColor32);
    function GetFillColor: TColor32;
    function GetLeft: single;
    function GetTop: single;
    procedure SetLeft(Value: single);
    procedure SetTop(Value: single);
  protected
    procedure SetText(const Text: UnicodeString); virtual;
    function GetFont: TFont;
    procedure SetControlBtns(const Value: TArrayOfFixedPoint); overload;
    procedure SetControlBtns(const Value: TArrayOfFloatPoint); overload;
    procedure DoSetLocation(const NewLocation: TFloatRect); override;
    procedure SetLocationToButtons;
    function ButtonCanMove(BtnIdx: integer): boolean; virtual;
    function ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean; virtual;
    procedure SetShadowOffset(Value: integer); virtual;
    procedure SetShadowOffsetX(Value: integer); virtual;
    procedure SetShadowOffsetY(Value: integer); virtual;
    function GetMargin: integer; virtual;
    function GetRotatedOutline: TArrayOfFixedPoint; virtual;
    function GetUnrotatedOutline: TArrayOfFixedPoint; virtual;
    procedure DoShadow(aBitmap: TBitmap32; const pts: TArrayOfFixedPoint; dx, dy: integer; closed: boolean); virtual;
    procedure DoStroke(aBitmap: TBitmap32; const pts: TArrayOfFixedPoint; Width, scaling: single); virtual;
    procedure DoFill(aBitmap: TBitmap32; const pts: TArrayOfFixedPoint; scaling: single); virtual;
    procedure AddControlBtns(const newBtns: TArrayOfFixedPoint; ToEnd: boolean);
    procedure DrawDesigner(Designer: TDesignerLayer; Buffer: TBitmap32); virtual;
    procedure SetStrokeWidth(Value: single);
    procedure SetStrokeColor(Value: TColor32);
    procedure SetStrokeStyle(Value: TPenStyle);
    procedure SetShadowColor(Value: TColor32);
    procedure SetFillStyle(Value: TFillStyle);
    procedure SetTextPadding(Value: integer);
    procedure SetDesigner(Designer: TDesignerLayer);

    property Line32: TLine32 read fLine32;
    property RefreshNeeded: boolean write FRefreshNeeded;
    property UnrotatedOutline: TArrayOfFixedPoint read FUnrotatedOutline write FUnrotatedOutline;
    property RotationPoint: TFloatPoint read FRotationPoint write FRotationPoint;
    property UnpublishedProps: TStringList read FUnpublishedProps;
    property ControlBtns: TArrayOfFixedPoint read FControlBtns;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    function ControlBtnCount: integer;
    procedure Rotate(angle_degrees: integer); virtual;
    procedure RePaint;
    procedure DrawTo(aBitmap: TBitmap32; offset: TFloatPoint; scalingFactor: single = 1); virtual;
    procedure MoveButton(BtnIdx: integer; newPt: TFixedPoint);
    property Designer: TDesignerLayer read FDesigner;
    property Outline: TArrayOfFixedPoint read GetRotatedOutline;
    property ShadowOffset: integer write SetShadowOffset;
  published
    property FillColor: TColor32 read GetFillColor write SetFillColor default clWhite32;
    property FillColors: TArrayOfColor32 read FFillColors write FFillColors;
    property FillStyle: TFillStyle read FFillStyle write SetFillStyle default fsPlain;
    property Font: TFont read GetFont;
    property Left: single read GetLeft write SetLeft;
    property StrokeWidth: single read FStrokeWidth write SetStrokeWidth;
    property StrokeColor: TColor32 read FStrokeColor write SetStrokeColor default clBlack32;
    property StrokeStyle: TPenStyle read FStrokeStyle write SetStrokeStyle default psSolid;
    property ShadowColor: TColor32 read FShadowColor write SetShadowColor default $CC808080;
    property ShadowOffsetX: integer read FShadowOffset.X write SetShadowOffsetX default 0;
    property ShadowOffsetY: integer read FShadowOffset.Y write SetShadowOffsetY default 0;
    property Text: UnicodeString read FText write SetText;
    property TextPadding: integer read FTextPadding write SetTextPadding default 2;
    property Top: single read GetTop write SetTop;
  end;

  TDrawObjGraphic = class(TDrawObjLayerBase)
  private
    FPic: TBitmap32;
    FAllowResizing: boolean;
    FScale: integer;
    FProportionalSizing: boolean;
    FAngle: integer;
    FTransparent: boolean;
    FTransparentColor: TColor32;
    procedure SetAngle(degrees: integer);
    procedure SetScale(percent: integer);
    procedure SetTransparent(Value: boolean);
  protected
    function GetUnrotatedOutline: TArrayOfFixedPoint; override;
    function GetRotatedOutline: TArrayOfFixedPoint; override;
    function DoHitTest(X, Y: integer): boolean; override;
    function ButtonCanMove(BtnIdx: integer): boolean; override;
    function ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean; override;
    procedure DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32); override;
    property Pic: TBitmap32 read FPic;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    procedure Position(const rec: TFloatRect; angle_degrees: integer; stream: TStream);
    procedure DrawTo(aBitmap: TBitmap32; offset: TFloatPoint; scalingFactor: single = 1); override;
    procedure Rotate(degrees: integer); override;
    procedure SetImage(imageStream: TStream);
  published
    property AllowResizing: boolean read FAllowResizing write FAllowResizing default True;
    property Angle: integer read FAngle write SetAngle default 0;
    property ProportionalSizing: boolean read FProportionalSizing write FProportionalSizing default True;
    property Scale: integer read FScale write SetScale default 100;
    property Transparent: boolean read FTransparent write SetTransparent default False;
  end;

  TDrawObjNonTextBase = class(TDrawObjLayerBase)
  private
    FillLast: boolean;
    IsClosed: boolean;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    procedure DrawTo(aBitmap: TBitmap32; offset: TFloatPoint; scalingFactor: single = 1); override;
  end;

  TDrawObjPoint = class(TDrawObjNonTextBase)
  private
    FRadius: single;
    procedure SetRadius(Value: single);
  protected
    function GetMargin: integer; override;
    function ButtonCanMove(BtnIdx: integer): boolean; override;
    function GetUnrotatedOutline: TArrayOfFixedPoint; override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    procedure DrawTo(aBitmap: TBitmap32; offset: TFloatPoint; scalingFactor: single = 1); override;
    procedure DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32); override;
    procedure Position(const pt: TFloatPoint);
  published
    property Radius: single read FRadius write SetRadius;
  end;

  TDrawObjLine = class(TDrawObjLayerBase)
  private
    FArrowStart: TArrow;
    FArrowEnd: TArrow;
    FReverseText: boolean;
    FAutoReverseText: boolean;
    procedure ArrowUpdated(Sender: TObject);
    procedure SetReverseText(Value: boolean);
    procedure SetAutoReverseText(Value: boolean);
    procedure FontChange(Sender: TObject);
  protected
    IsClosed: boolean;
    function GetMargin: integer; override;
    function DoHitTest(X, Y: integer): boolean; override;
    procedure DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    procedure Position(const pts: TArrayOfFloatPoint);
    procedure DrawTo(aBitmap: TBitmap32; offset: TFloatPoint; scalingFactor: single = 1); override;
    procedure Extend(toEnd: boolean = True; distance: single = 50); virtual;
  published
    property ArrowStart: TArrow read FArrowStart;
    property ArrowEnd: TArrow read FArrowEnd;
    property AutoReverseText: boolean read FAutoReverseText write SetAutoReverseText default True;
    property ReverseText: boolean read FReverseText write SetReverseText default False;
  end;

  TDrawObjBezier = class(TDrawObjLine)
  private
    FSmooth: boolean;
    procedure SetSmooth(Value: boolean);
  protected
    function GetUnrotatedOutline: TArrayOfFixedPoint; override;
    procedure DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32); override;
    function ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean; override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    procedure Position(const pts: TArrayOfFloatPoint);
    procedure Extend(toEnd: boolean = True; distance: single = 50); override;
  published
    property Smooth: boolean read FSmooth write SetSmooth default True;
  end;

  TDrawObjWideBezier = class(TDrawObjNonTextBase)
  private
    FFillWidth: single;
    FSmooth: boolean;
    procedure SetFillWidth(Value: single);
    procedure SetSmooth(Value: boolean);
  protected
    function ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean; override;
    procedure DoFill(aBitmap: TBitmap32; const pts: TArrayOfFixedPoint; scaling: single); override;
    function DoHitTest(X, Y: integer): boolean; override;
    function GetUnrotatedOutline: TArrayOfFixedPoint; override;
    procedure DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    procedure Position(const pts: TArrayOfFloatPoint);
    procedure Extend(toEnd: boolean = True; distance: single = 50);
  published
    property FillWidth: single read FFillWidth write SetFillWidth;
    property Smooth: boolean read FSmooth write SetSmooth default True;
  end;

  TDrawObjPolygon = class(TDrawObjNonTextBase)
  private
    FRegular: boolean;
    procedure SetRegular(Value: boolean);
    function GetPointCnt: integer;
    procedure SetPointCnt(Value: integer);
  protected
    function DoHitTest(X, Y: integer): boolean; override;
    procedure DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32); override;
    function ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean; override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    procedure Position(const pts: TArrayOfFixedPoint); overload;
    procedure Position(const rec: TFloatRect; pointCnt: integer); overload;
  published
    property PointCount: integer read GetPointCnt write SetPointCnt default 5;
    property Regular: boolean read FRegular write SetRegular default False;
  end;

  TDrawObjStar = class(TDrawObjNonTextBase)
  private
    FRegular: boolean;
    procedure SetRegular(Value: boolean);
    function GetPointCnt: integer;
    procedure SetPointCnt(Value: integer);
  protected
    function DoHitTest(X, Y: integer): boolean; override;
    procedure DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32); override;
    function ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean; override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    procedure Position(const center: TFloatPoint; innerRadius, outerRadius: single; pointCnt: integer; rotateDegrees: integer = 0);
  published
    property PointCount: integer read GetPointCnt write SetPointCnt default 7;
    property Regular: boolean read FRegular write SetRegular default False;
  end;

  TDrawObjArrow = class(TDrawObjNonTextBase)
  private
    procedure ResetPoints(const rec: TFloatRect);
  protected
    function DoHitTest(X, Y: integer): boolean; override;
    procedure DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32); override;
    function ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean; override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    procedure Position(const rec: TFloatRect; rotateDegrees: integer); overload;
    procedure Position(const pts: TArrayOfFixedPoint); overload;
  end;

  TDrawObjTextBase = class(TDrawObjLayerBase)
  private
    FAngle: integer;
    FRegular: boolean; //ie square, circle etc
    procedure SetRegular(Value: boolean);
  protected
    function DoHitTest(X, Y: integer): boolean; override;
    procedure SetAngle(degrees: integer);
    function GetRotatedOutline: TArrayOfFixedPoint; override;
    procedure DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32); override;
    function ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean; override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    procedure Position(const rec: TFloatRect; angleDegrees: integer);
    procedure Rotate(degrees: integer); override;
    procedure DrawTo(aBitmap: TBitmap32; offset: TFloatPoint; scalingFactor: single = 1); override;
  published
    property Angle: integer read FAngle write SetAngle default 0;
    property Regular: boolean read FRegular write SetRegular default False;
  end;

  TDrawObjRectangle = class(TDrawObjTextBase)
  private
    fRounded: boolean;
    procedure SetRounded(Value: boolean);
  protected
    function GetUnrotatedOutline: TArrayOfFixedPoint; override;
  published
    property Rounded: boolean read fRounded write SetRounded default False;
  end;

  TDrawObjEllipse = class(TDrawObjTextBase)
  private
    fBalloonPos: TBalloonPos;
    procedure SetBalloonPos(Value: TBalloonPos);
  protected
    function GetUnrotatedOutline: TArrayOfFixedPoint; override;
  published
    property BalloonPos: TBalloonPos read fBalloonPos write SetBalloonPos default bpNone;
  end;

  TDrawObjArc = class(TDrawObjTextBase)
  private
    fAngleStart: integer;
    fAngleEnd: integer;
    procedure SetAngleStart(Value: integer);
    procedure SetAngleEnd(Value: integer);
  protected
    function GetUnrotatedOutline: TArrayOfFixedPoint; override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
  published
    property AngleStart: integer read fAngleStart write SetAngleStart default 45;
    property AngleEnd: integer read fAngleEnd write SetAngleEnd default 0;
  end;

  TDrawObjDiamond = class(TDrawObjTextBase)
  protected
    function GetUnrotatedOutline: TArrayOfFixedPoint; override;
  end;

  TDesignerLayer = class(TPositionedLayer)
  private
    FChildLayer: TDrawObjLayerBase;
    FButtonSize: integer;
    FDragState: integer;
    FOnMoving: TDesignerMovingEvent;
    FOnBtnMoving: TNotifyEvent;
    FOnValidateDragState: TDragStateEvent;
    procedure SetChildLayer(Value: TDrawObjLayerBase);
    procedure SetButtonSize(Value: integer);
  protected
    MouseDownLoc: TFloatRect;
    MouseDownPt: TFloatPoint;
    function DoHitTest(X, Y: integer): boolean; override;
    procedure DoMoving(OldLocation: TFloatRect; var NewLocation: TFloatRect; DragState: integer; Shift: TShiftState); virtual;
    procedure DoButtonMoving; virtual;
    procedure DoSetLocation(const NewLocation: TFloatRect); override;
    function GetDragBtnState(X, Y: integer): integer; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure Notification(ALayer: TCustomLayer); override;
    procedure Paint(Buffer: TBitmap32); override;
    procedure SetLayerOptions(Value: cardinal); override;
    procedure UpdateChildLayer;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    function ButtonRect(btnIdx: integer): TRect;
    property ButtonSize: integer read FButtonSize write SetButtonSize;
    property ChildLayer: TDrawObjLayerBase read FChildLayer write SetChildLayer;
    property DragState: integer read FDragState;
    property OnButtonMoving: TNotifyEvent read FOnBtnMoving write FOnBtnMoving;
    property OnMoving: TDesignerMovingEvent read FOnMoving write FOnMoving;
    property OnValidateDragState: TDragStateEvent read FOnValidateDragState write FOnValidateDragState;
  end;

procedure BitmapToScreen(LayerCollection: TLayerCollection; var pt: TFixedPoint); overload;
procedure BitmapToScreen(LayerCollection: TLayerCollection; var pts: TArrayOfFixedPoint); overload;
procedure ScreenToBitmap(LayerCollection: TLayerCollection; var pt: TFixedPoint); overload;
procedure ScreenToBitmap(LayerCollection: TLayerCollection; var pt: TPoint); overload;

function GetInflatedOutline(const pts: TArrayOfFixedPoint; closed: boolean; delta: single): TArrayOfFixedPoint;

//Runtime load Cursors
procedure LoadObjectsCursors;  // ct9999 for CodeTyphon

implementation

{$R VGR32_Objects.res}

function RoundFixed(val: TFixed): TFixed;
begin
  Result := round(val);
end;

//------------------------------------------------------------------------------

procedure BitmapToScreen(LayerCollection: TLayerCollection; var pt: TFixedPoint);
var
  ScaleX, ScaleY, ShiftX, ShiftY: single;
begin
  if Assigned(LayerCollection) then
  begin
    LayerCollection.GetViewportShift(ShiftX, ShiftY);
    LayerCollection.GetViewportScale(ScaleX, ScaleY);
    pt.X := round(pt.X * ScaleX + ShiftX * FixedOne);
    pt.Y := round(pt.Y * ScaleY + ShiftY * FixedOne);
  end;
end;
//------------------------------------------------------------------------------

procedure BitmapToScreen(LayerCollection: TLayerCollection; var pts: TArrayOfFixedPoint);
var
  i: integer;
  ScaleX, ScaleY, ShiftX, ShiftY: single;
begin
  if Assigned(LayerCollection) then
  begin
    LayerCollection.GetViewportShift(ShiftX, ShiftY);
    LayerCollection.GetViewportScale(ScaleX, ScaleY);
    ShiftX := ShiftX * FixedOne;
    ShiftY := ShiftY * FixedOne;
    for i := 0 to high(pts) do
    begin
      pts[i].X := round(pts[i].X * ScaleX + ShiftX);
      pts[i].Y := round(pts[i].Y * ScaleY + ShiftY);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure ScreenToBitmap(LayerCollection: TLayerCollection; var pt: TFixedPoint);
var
  ScaleX, ScaleY, ShiftX, ShiftY: single;
begin
  if Assigned(LayerCollection) then
  begin
    LayerCollection.GetViewportShift(ShiftX, ShiftY);
    LayerCollection.GetViewportScale(ScaleX, ScaleY);
    pt.X := round((pt.X - ShiftX * FixedOne) / ScaleX);
    pt.Y := round((pt.Y - ShiftY * FixedOne) / ScaleY);
  end;
end;
//------------------------------------------------------------------------------

procedure ScreenToBitmap(LayerCollection: TLayerCollection; var pt: TPoint);
var
  fp: TFixedPoint;
begin
  fp := FixedPoint(pt);
  ScreenToBitmap(LayerCollection, fp);
  pt := MakePoint(fp);
end;
//------------------------------------------------------------------------------

function GetInflatedOutline(const pts: TArrayOfFixedPoint; closed: boolean; delta: single): TArrayOfFixedPoint;
begin
  //if not closed then
  delta := delta * 2;
  with TLine32.Create do
    try
      if closed then
        EndStyle := esClosed
      else
        EndStyle := esRounded;
      JoinStyle := jsRounded;
      SetPoints(pts);
      Result := GetOuterEdge(delta);
    finally
      Free;
    end;
end;
//------------------------------------------------------------------------------

procedure ScalePoints(var pts: TArrayOfFixedPoint; scaling: single); overload;
var
  i: integer;
begin
  if scaling = 1 then
    exit;
  for i := 0 to high(pts) do
  begin
    pts[i].X := round(pts[i].X * scaling);
    pts[i].Y := round(pts[i].Y * scaling);
  end;
end;
//------------------------------------------------------------------------------

procedure ScalePoints(var ppts: TArrayOfArrayOfArrayOfFixedPoint; scaling: single); overload;
var
  i, j, k: integer;
begin
  if scaling = 1 then
    exit;
  for i := 0 to high(ppts) do
    for j := 0 to high(ppts[i]) do
      for k := 0 to high(ppts[i][j]) do
      begin
        ppts[i][j][k].X := round(ppts[i][j][k].X * scaling);
        ppts[i][j][k].Y := round(ppts[i][j][k].Y * scaling);
      end;
end;
//------------------------------------------------------------------------------

function StripLF(const Text: UnicodeString): UnicodeString;
var
  i: integer;
begin
  Result := Text;
  for i := length(Result) downto 1 do
    if Result[i] = #13 then
      Delete(Result, i, 1);
end;

//------------------------------------------------------------------------------
// TArrow methods
//------------------------------------------------------------------------------

procedure TArrow.SetStyle(Value: TArrowHeadStyle);
begin
  FStyle := Value;
  if Assigned(FOnChange) then
    FOnChange(self);
end;
//------------------------------------------------------------------------------

procedure TArrow.SetSize(Value: single);
begin
  FSize := Value;
  if Assigned(FOnChange) then
    FOnChange(self);
end;
//------------------------------------------------------------------------------

procedure TArrow.SetColor(Value: TColor32);
begin
  FColor := Value;
  if Assigned(FOnChange) then
    FOnChange(self);
end;

//------------------------------------------------------------------------------
// TDrawObjLayerBase methods
//------------------------------------------------------------------------------

constructor TDrawObjLayerBase.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FUnpublishedProps := TStringList.Create;
  FUnpublishedProps.Add('ControlBtns');
  FUnpublishedProps.Add('RotationPoint');

  FLine32 := TLine32.Create;
  FLine32.JoinStyle := jsRounded;
  FLine32.EndStyle := esClosed;

  Scaled := True;
  Cropped := True;
  FStrokeWidth := 2;
  FStrokeColor := clBlack32;
  FShadowColor := $CC808080;
  Bitmap.Font.Name := 'Arial';
  Bitmap.Font.Size := 9;
  FTextPadding := 2;
  Bitmap.DrawMode := dmBlend;
  Bitmap.CombineMode := cmMerge;
  FRotationPoint := FloatPoint(DEFAULT_OBJ_SIZE / 2, DEFAULT_OBJ_SIZE / 2);
  //FillColor := clWhite32;
end;
//------------------------------------------------------------------------------

destructor TDrawObjLayerBase.Destroy;
begin
  FUnpublishedProps.Free;
  FLine32.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TDrawObjLayerBase.ControlBtnCount: integer;
begin
  Result := length(FControlBtns);
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetControlBtns(const Value: TArrayOfFixedPoint);
begin
  FControlBtns := CopyPoints(Value);
  SetLocationToButtons;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetControlBtns(const Value: TArrayOfFloatPoint);
var
  i, len: integer;
begin
  len := length(Value);
  setlength(FControlBtns, len);
  if len > 0 then
    for i := 0 to len - 1 do
      FControlBtns[i] := FixedPoint(Value[i]);
  SetLocationToButtons;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.AddControlBtns(const newBtns: TArrayOfFixedPoint; ToEnd: boolean);
var
  cnt, cntNew: integer;
begin
  cntNew := length(newBtns);
  if cntNew = 0 then
    exit;
  cnt := length(FControlBtns);
  setLength(FControlBtns, cnt + cntNew);
  if not ToEnd then
  begin
    move(FControlBtns[0], FControlBtns[cntNew], cnt * sizeof(TFixedPoint));
    move(newBtns[0], FControlBtns[0], cntNew * sizeof(TFixedPoint));
  end
  else
  begin
    move(newBtns[0], FControlBtns[cnt], cntNew * sizeof(TFixedPoint));
  end;
  SetLocationToButtons;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.BeginUpdate;
begin
  Inc(FUpdateCount);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and FRefreshNeeded then
    RePaint;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.DoSetLocation(const NewLocation: TFloatRect);
var
  oldLoc, newLoc: TFloatRect;
  dx, dy: single;
begin
  oldLoc := Location;
  newLoc := NewLocation;

  //don't allow resizing here ...
  if round(newLoc.Right - newLoc.Left) <> Bitmap.Width then
    newLoc.Right := newLoc.Left + Bitmap.Width;
  if round(newLoc.Bottom - newLoc.Top) <> Bitmap.Height then
    newLoc.Bottom := newLoc.Top + Bitmap.Height;
  inherited DoSetLocation(newLoc);

  //update (move) buttons only when this call originates
  //from an external Location adjustment ...
  if FButtonsLocked then
    exit;

  //move the buttons to the new location ...
  dx := NewLoc.Left - OldLoc.Left;
  dy := NewLoc.Top - OldLoc.Top;
  OffsetPoints(fControlBtns, dx, dy);
  OffsetPoints(FUnrotatedOutline, dx, dy);
  OffsetPoint(FRotationPoint, dx, dy);

  if assigned(Designer) and (Designer.ChildLayer = self) and not IsDuplicateRect(Designer.Location, Location) then
    Designer.Location := Location;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetLocationToButtons;
var
  rotatedOutline: TArrayOfFixedPoint;
  oldLoc, newLoc: TFloatRect;
  marg: integer;
begin
  if (ControlBtnCount = 0) then
    exit;

  //first, update FUnrotatedOutline ...
  FUnrotatedOutline := GetUnrotatedOutline;

  //get the new location ...
  rotatedOutline := GetRotatedOutline;
  newLoc := GetBoundsFloatRect(rotatedOutline);
  marg := GetMargin;
  InflateRect(newLoc, marg, marg);

  //resize bitmap ...
  oldLoc := Location;
  if (newLoc.Right - newLoc.Left <> oldLoc.Right - oldLoc.Left) or (newLoc.Bottom - newLoc.Top <> oldLoc.Bottom - oldLoc.Top) then
    with newLoc do
      Bitmap.SetSize(round(right - left), round(bottom - top));

  //adjust Location ...
  FButtonsLocked := True;
  try
    Location := newLoc;
  finally
    FButtonsLocked := False;
  end;

  FRefreshNeeded := True;
  //RePaint;

  //finally, update the designer location ...
  if assigned(Designer) and (Designer.ChildLayer = self) then
    Designer.Location := Location;
end;
//------------------------------------------------------------------------------

function TDrawObjLayerBase.GetUnrotatedOutline: TArrayOfFixedPoint;
begin
  Result := CopyPoints(FControlBtns);
end;
//------------------------------------------------------------------------------

function TDrawObjLayerBase.GetRotatedOutline: TArrayOfFixedPoint;
begin
  Result := GetUnrotatedOutline;
end;
//------------------------------------------------------------------------------

function TDrawObjLayerBase.GetMargin: integer;
begin
  Result := max(DEFAULT_MARGIN, max(abs(FShadowOffset.X), abs(FShadowOffset.Y))) + ceil(StrokeWidth / 2);
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetShadowOffset(Value: integer);
begin
  Value := Constrain(Value, -20, 20);
  if (FShadowOffset.X = Value) and (FShadowOffset.Y = Value) then
    exit;
  FShadowOffset.X := Value;
  FShadowOffset.Y := Value;
  SetLocationToButtons;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetShadowOffsetX(Value: integer);
begin
  Value := Constrain(Value, -20, 20);
  if FShadowOffset.X = Value then
    exit;
  FShadowOffset.X := Value;
  SetLocationToButtons;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetShadowOffsetY(Value: integer);
begin
  Value := Constrain(Value, -20, 20);
  if FShadowOffset.Y = Value then
    exit;
  FShadowOffset.Y := Value;
  SetLocationToButtons;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.Rotate(angle_degrees: integer);
begin
  if angle_degrees = 0 then
    exit;
  FControlBtns := RotatePoints(FControlBtns, FixedPoint(FRotationPoint), angle_degrees * DegToRad);
  SetLocationToButtons;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetStrokeWidth(Value: single);
begin
  if Value < 0 then
    Value := 0
  else if Value > 100 then
    Value := 100;
  if FStrokeWidth = Value then
    exit;
  FStrokeWidth := Value;
  SetLocationToButtons;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetFillStyle(Value: TFillStyle);
begin
  if FFillStyle = Value then
    exit;
  FFillStyle := Value;
  FRefreshNeeded := True;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetTextPadding(Value: integer);
begin
  if FTextPadding = Value then
    exit;
  FTextPadding := Value;
  FRefreshNeeded := True;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetStrokeColor(Value: TColor32);
begin
  if FStrokeColor = Value then
    exit;
  FStrokeColor := Value;
  FRefreshNeeded := True;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetStrokeStyle(Value: TPenStyle);
begin
  if FStrokeStyle = Value then
    exit;
  FStrokeStyle := Value;
  FRefreshNeeded := True;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetShadowColor(Value: TColor32);
begin
  if FShadowColor = Value then
    exit;
  FShadowColor := Value;
  FRefreshNeeded := True;
end;
//------------------------------------------------------------------------------

function TDrawObjLayerBase.GetLeft: single;
begin
  Result := Location.Left;
end;
//------------------------------------------------------------------------------

function TDrawObjLayerBase.GetTop: single;
begin
  Result := Location.Top;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetLeft(Value: single);
begin
  if Location.Left <> Value then
    with Location do
      Location := FloatRect(Value, Top, Right, Bottom);
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetTop(Value: single);
begin
  if Location.Top <> Value then
    with Location do
      Location := FloatRect(Left, Value, Right, Bottom);
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetText(const Text: UnicodeString);
begin
  //for some reason #13 chars cause all sorts of grief so ...
  FText := StripLF(Text);
  FRefreshNeeded := True;
end;
//------------------------------------------------------------------------------

function TDrawObjLayerBase.GetFont: TFont;
begin
  Result := Bitmap.Font;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetDesigner(Designer: TDesignerLayer);
begin
  FDesigner := Designer;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.MoveButton(BtnIdx: integer; newPt: TFixedPoint);
var
  rec: TFloatRect;
  pts: TArrayOfFixedPoint;
  updateRotatePt: boolean;
begin
  updateRotatePt := True;
  if (BtnIdx < 0) or (BtnIdx >= ControlBtnCount) or not ValidateMoveButton(BtnIdx, newPt, updateRotatePt) then
    exit;
  FControlBtns[BtnIdx] := newPt;
  if updateRotatePt then
  begin
    pts := GetUnrotatedOutline;
    rec := GetBoundsFloatRect(pts);
    with rec do
      FRotationPoint := FloatPoint((left + right) / 2, (top + bottom) / 2);
  end;
  SetLocationToButtons;
  RePaint;
end;
//------------------------------------------------------------------------------

function TDrawObjLayerBase.ButtonCanMove(BtnIdx: integer): boolean;
begin
  Result := True;
end;
//------------------------------------------------------------------------------

function TDrawObjLayerBase.ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean;
var
  pt: TFixedPoint;
  rec: TFloatRect;
begin
  pt := FControlBtns[BtnIdx];
  FControlBtns[BtnIdx] := newPt;
  rec := GetBoundsFloatRect(FControlBtns);
  FControlBtns[BtnIdx] := pt;
  with rec do
    Result := ((right - left) > MIN_OBJ_SIZE) or ((bottom - top) > MIN_OBJ_SIZE);
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.DoStroke(aBitmap: TBitmap32; const pts: TArrayOfFixedPoint; Width, scaling: single);
begin
  if (AlphaComponent(FStrokeColor) = 0) or (Width < 0.5) then
    exit;
  Width := Width * scaling;
  with FLine32 do
  begin
    SetPoints(pts);
    case FStrokeStyle of
      psDash: Draw(aBitmap, Width, [FStrokeColor, FStrokeColor, FStrokeColor, $0, $0], 1 / Width);
      psDot:  Draw(aBitmap, Width, [FStrokeColor, FStrokeColor, $0, $0], 2 / Width);
      psDashDot: Draw(aBitmap, Width, [FStrokeColor, $0, $0, FStrokeColor, FStrokeColor, FStrokeColor, $0, $0], 1 / Width);
      psDashDotDot: Draw(aBitmap, Width, [FStrokeColor, $0, $0, FStrokeColor, $0, $0, FStrokeColor, FStrokeColor, FStrokeColor, $0, $0], 1 / Width);
      else
        Draw(aBitmap, Width, FStrokeColor);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.DoFill(aBitmap: TBitmap32; const pts: TArrayOfFixedPoint; scaling: single);

var
  pts2: TArrayOfFloatPoint;

begin
  if length(FFillColors) = 0 then
    exit;
  if (FFillStyle = fsPlain) or (length(FFillColors) = 1) then
  begin
    if (AlphaComponent(FFillColors[0]) > 0) then

    begin
      pts2 := MakeArrayOfFloatPoints(pts);
      PolygonFS(Bitmap, pts2, FFillColors[0]);
    end;

  end
  else
  begin
    case FFillStyle of
      fsGradiant:
        if self is TDrawObjTextBase then
          SimpleGradientFill(aBitmap, pts, $0, FFillColors,
            TDrawObjTextBase(self).FAngle)
        else
          SimpleGradientFill(aBitmap, pts, $0, FFillColors, 0);
      fsRadial:
        if self is TDrawObjTextBase then
          SimpleRadialFill(aBitmap, pts, FFillColors,
            TDrawObjTextBase(self).FAngle)
        else
          SimpleRadialFill(aBitmap, pts, FFillColors, 0);
      fsCustom: ; //todo - create an event property for custom fills
    end;
  end;
end;
//------------------------------------------------------------------------------

function TDrawObjLayerBase.GetFillColor: TColor32;
begin
  if length(FFillColors) < 1 then
    Result := $00000000
  else
    Result := FFillColors[0];
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.SetFillColor(color: TColor32);
begin
  setlength(FFillColors, 1);
  FFillColors[0] := color;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.DoShadow(aBitmap: TBitmap32; const pts: TArrayOfFixedPoint; dx, dy: integer; closed: boolean);
var
  mask, orig: TBitmap32;
begin
  if (AlphaComponent(FShadowColor) = 0) then
    exit;
  if closed then
  begin
    //nb: this avoids shadowing inside semi-opaque polygons ...
    orig := TBitmap32.Create;
    mask := CreateMaskFromPolygon(aBitmap, pts);
    try
      orig.Assign(aBitmap);
      SimpleShadow(aBitmap, pts, dx, dy, MINIMUM_SHADOW_FADE, FShadowColor, closed);
      ApplyMask(aBitmap, orig, mask, True);
    finally
      Mask.Free;
      orig.Free;
    end;
  end
  else
    SimpleShadow(aBitmap, pts, dx, dy, MINIMUM_SHADOW_FADE, FShadowColor, closed);
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.RePaint;
begin
  if FUpdateCount <> 0 then
    exit;
  Bitmap.Clear($00FFFFFF);
  DrawTo(Bitmap, Location.TopLeft);
  FRefreshNeeded := False;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.DrawTo(aBitmap: TBitmap32; offset: TFloatPoint; scalingFactor: single = 1);
begin
  //implemented in descendant classes
end;
//------------------------------------------------------------------------------

procedure TDrawObjLayerBase.DrawDesigner(Designer: TDesignerLayer; Buffer: TBitmap32);
var
  i: integer;
begin
  //just draw the buttons ...
  for i := 0 to high(FControlBtns) do
    if ButtonCanMove(i) then
    begin
      Buffer.FillRectTS(Designer.ButtonRect(i), clRed32);
      Buffer.FrameRectTS(Designer.ButtonRect(i), clBlack32);
    end;
end;

//------------------------------------------------------------------------------
// TDrawObjGraphic methods
//------------------------------------------------------------------------------

constructor TDrawObjGraphic.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  UnpublishedProps.Add('PictureData');
  FPic := TBitmap32.Create;

  FPic.ResamplerClassName := 'TKernelResampler';
  TKernelResampler(FPic.Resampler).KernelClassName := 'TLanczosKernel';
  FFillColors := nil;
  FStrokeColor := $0;
  FScale := 100;
  FAllowResizing := True;
  FProportionalSizing := True;
  FTransparentColor := clWhite32;
  SetControlBtns(MakeArrayOfFloatPoints([DEFAULT_MARGIN, DEFAULT_MARGIN, DEFAULT_OBJ_SIZE + DEFAULT_MARGIN * 2, DEFAULT_OBJ_SIZE + DEFAULT_MARGIN * 2]));
end;
//------------------------------------------------------------------------------

destructor TDrawObjGraphic.Destroy;
begin
  FPic.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TDrawObjGraphic.Position(const rec: TFloatRect; angle_degrees: integer; stream: TStream);
var
  i: integer;
  pts: TArrayOfFixedPoint;
  w, h: single;
  autoSize: boolean;
begin
  LoadPicFromStream(stream, FPic);
  FTransparent := False;
  if FPic.Empty then
    FTransparentColor := clWhite32
  else
  begin
    FTransparentColor := Pic.bits^[0];
    {$R-}
    for i := 0 to Pic.Width * Pic.Height - 1 do
      if Pic.Bits^[i] and $FF000000 = $0 then
      begin
        FTransparent := True;
        FTransparentColor := Pic.Bits^[i] or $FF000000;
        break;
      end;
    {$R+}
  end;

  with rec do
  begin
    w := Right - Left;
    h := Bottom - Top;
    RotationPoint := FloatPoint((left + right) / 2, (top + bottom) / 2);
  end;

  if (w = 0) or (h = 0) then
  begin
    w := MIN_OBJ_SIZE;
    h := MIN_OBJ_SIZE;
    FScale := 100;
    autoSize := True;
  end
  else if FPic.Empty then
  begin
    FScale := 100;
    autoSize := False;
  end
  else
  begin
    FScale := round(min(w * 100 / FPic.Width, h * 100 / FPic.Height));
    if ProportionalSizing then
    begin
      w := FPic.Width * FScale / 100;
      h := FPic.Height * FScale / 100;
    end;
    autoSize := False;
  end;

  with FPic, RotationPoint do
    if empty then
      pts := MakeArrayOfFixedPoints([X - w / 2, Y - h / 2, X + w / 2, Y + h / 2])
    else if autoSize then
      pts := MakeArrayOfFixedPoints([X - Width * FScale / 200, Y - Height * FScale / 200, X + Width * FScale / 200, Y + Height * FScale / 200])
    else
      pts := MakeArrayOfFixedPoints([X - w / 2, Y - h / 2, X + w / 2, Y + h / 2]);
  FAngle := angle_degrees;
  if angle_degrees <> 0 then
    pts := RotatePoints(pts, FixedPoint(RotationPoint), angle_degrees * DegToRad);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjGraphic.SetImage(imageStream: TStream);
var
  i: integer;
  w, h: single;
  pts: TArrayOfFixedPoint;
begin
  FTransparent := False;

  LoadPicFromStream(imageStream, FPic);
  if FPic.Empty then
    exit;

  FTransparentColor := Pic.bits^[0];
  {$R-}
  for i := 0 to Pic.Width * Pic.Height - 1 do
    if Pic.Bits^[i] and $FF000000 = $0 then
    begin
      FTransparent := True;
      FTransparentColor := Pic.Bits^[i] or $FF000000;
      break;
    end;
  {$R+}

  with Location do
  begin
    w := right - left;
    h := bottom - top;
  end;

  //FScale := round(min(w*100/FPic.Width, h*100/FPic.Height));
  if ProportionalSizing then
  begin
    w := FPic.Width * FScale / 100;
    h := FPic.Height * FScale / 100;
  end;

  with FPic, RotationPoint do
    pts := MakeArrayOfFixedPoints([X - w / 2, Y - h / 2, X + w / 2, Y + h / 2]);
  if FAngle <> 0 then
    pts := RotatePoints(pts, FixedPoint(RotationPoint), FAngle * DegToRad);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

function TDrawObjGraphic.GetUnrotatedOutline: TArrayOfFixedPoint;
var
  rec: TFixedRect;
begin
  Result := CopyPoints(FControlBtns);
  if FAngle <> 0 then
    Result := RotatePoints(Result, FixedPoint(RotationPoint), -FAngle * DegToRad);
  rec := GetBoundsFixedRect(Result);
  Result := MakeArrayOfFixedPoints(rec);
end;
//------------------------------------------------------------------------------

function TDrawObjGraphic.GetRotatedOutline: TArrayOfFixedPoint;
begin
  Result := GetUnrotatedOutline;
  Result := RotatePoints(Result, FixedPoint(RotationPoint), FAngle * DegToRad);
end;
//------------------------------------------------------------------------------

procedure TDrawObjGraphic.SetAngle(degrees: integer);
var
  rotateDegrees: integer;
begin
  while degrees <= -180 do
    Inc(degrees, 360);
  while degrees > 180 do
    Dec(degrees, 360);
  if FAngle = degrees then
    exit;
  rotateDegrees := degrees - FAngle;
  FAngle := degrees;
  inherited Rotate(rotateDegrees);
end;
//------------------------------------------------------------------------------

procedure TDrawObjGraphic.Rotate(degrees: integer);
begin
  if degrees <> 0 then
    SetAngle(FAngle + degrees);
end;
//------------------------------------------------------------------------------

procedure TDrawObjGraphic.SetScale(percent: integer);
var
  pts: TArrayOfFixedPoint;
begin
  if percent > 1000 then
    percent := 1000
  else if percent < 10 then
    percent := 10;
  FScale := percent;
  if FPic.Empty then
    exit;
  //resize object ...
  with FPic, RotationPoint do
    pts := MakeArrayOfFixedPoints([X - Width * FScale / 200, Y - Height * FScale / 200, X + Width * FScale / 200, Y + Height * FScale / 200]);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjGraphic.SetTransparent(Value: boolean);
var
  i: integer;
  c: TColor32;
begin
  if Value = FTransparent then
    exit;
  FTransparent := Value and not Pic.Empty;
  if Pic.Empty then
    exit;

  {$R-}
  if FTransparent then
  begin
    c := FTransparentColor and $00FFFFFF;
    for i := 0 to Pic.Width * Pic.Height - 1 do
      if Pic.Bits^[i] and $00FFFFFF = c then
        Pic.Bits^[i] := Pic.Bits^[i] and $00FFFFFF;
  end
  else
  begin
    c := FTransparentColor or $FF000000;
    for i := 0 to Pic.Width * Pic.Height - 1 do
      if Pic.Bits^[i] or $FF000000 = c then
        Pic.Bits^[i] := Pic.Bits^[i] or $FF000000;
  end;
  {$R+}
  RefreshNeeded := True;
end;
//------------------------------------------------------------------------------

function TDrawObjGraphic.DoHitTest(X, Y: integer): boolean;
var
  pt: TFixedPoint;
  pts: TArrayOfFixedPoint;
  rec: TFloatRect;
  marg: integer;
begin
  if (FAngle <> 0) then
  begin
    pt := FixedPoint(X, Y);
    ScreenToBitmap(LayerCollection, pt);
    rec := GetBoundsFloatRect(FUnrotatedOutline);
    marg := GetMargin;
    InflateRect(rec, marg, marg);
    pts := MakeArrayOfFixedPoints(rec);
    pts := RotatePoints(pts, FixedPoint(RotationPoint), FAngle * DegToRad);
    Result := PtInPolygon(pt, pts);
  end
  else
    Result := inherited DoHitTest(X, Y);
end;
//------------------------------------------------------------------------------

function TDrawObjGraphic.ButtonCanMove(BtnIdx: integer): boolean;
begin
  Result := FAllowResizing;
end;
//------------------------------------------------------------------------------

function TDrawObjGraphic.ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean;
var
  altPt, pt: TFixedPoint;
  XYratio: single;
begin
  Result := inherited ValidateMoveButton(BtnIdx, newPt, updateRotationPt);
  if not Result or FPic.Empty or not FProportionalSizing then
    exit;

  //force proportional sizing ...
  with FPic do
    XYratio := Width / Height;
  pt := newPt;
  if BtnIdx = 0 then
    altPt := ControlBtns[1]
  else
    altPt := ControlBtns[0];
  if FAngle <> 0 then
  begin
    pt := RotatePoint(pt, FixedPoint(RotationPoint), -FAngle * DegToRad);
    altPt := RotatePoint(altPt, FixedPoint(RotationPoint), -FAngle * DegToRad);
  end;
  if abs(pt.Y - altPt.Y) < Fixed(MIN_OBJ_SIZE) then
  begin
    Result := False;
    exit;
  end;
  if abs((pt.X - altPt.X) / (pt.Y - altPt.Y)) < XYratio then
    pt.Y := Round((pt.X - altPt.X) / XYratio) + altPt.Y
  else
    pt.X := Round((pt.Y - altPt.Y) * XYratio) + altPt.X;
  if FAngle <> 0 then
    pt := RotatePoint(pt, FixedPoint(RotationPoint), FAngle * DegToRad);
  newPt := pt;
end;
//------------------------------------------------------------------------------

procedure TDrawObjGraphic.DrawTo(aBitmap: TBitmap32; offset: TFloatPoint; scalingFactor: single = 1);
var
  pts: TArrayOfFixedPoint;
  affine: TAffineTransformation;
  rec: TRect;
  scaleX, scaleY, x, y: single;
  b: TBitmap32;
begin
  if FPic.Empty then
  begin
    if scalingFactor = 1 then
    begin
      //draw an outline of where Pic would be if there was one ...
      pts := CopyPoints(FUnrotatedOutline);
      if FAngle <> 0 then
        pts := RotatePoints(pts, FixedPoint(RotationPoint), FAngle * DegToRad);
      OffsetPoints(pts, -offset.X, -offset.Y);
      aBitmap.SetStipple([clRed32, $0, $0]);
      PolylineXSP(aBitmap, pts, True);
    end;
  end
  else
  begin
    if FAngle = 0 then
    begin
      rec := MakeRect(Location);
      InflateRect(rec, -DEFAULT_MARGIN, -DEFAULT_MARGIN);
      OffsetRect(rec, round(-offset.X), round(-offset.Y));
      rec.Left := round(rec.Left * scalingFactor);
      rec.Top := round(rec.Top * scalingFactor);
      rec.Right := round(rec.Right * scalingFactor);
      rec.Bottom := round(rec.Bottom * scalingFactor);
      aBitmap.Draw(rec, FPic.BoundsRect, FPic);
    end
    else
    begin
      pts := GetUnrotatedOutline;
      rec := GetBoundsRect(pts);
      scaleX := (rec.right - rec.left) / FPic.Width * scalingFactor;
      scaleY := (rec.bottom - rec.top) / FPic.Height * scalingFactor;
      with Location do
      begin
        x := (right + left) / 2 - offset.X;
        y := (bottom + top) / 2 - offset.Y;
      end;

      //nb: this temporary bitmap seems necessary for those times when
      //the target bitmap isn't a TBitmapLayer.bitmap (eg when printing) ...
      b := TBitmap32.Create;
      b.SetSize(aBitmap.Width, aBitmap.Height);
      b.DrawMode := dmBlend;
      affine := TAffineTransformation.Create;
      try
        affine.SrcRect := FloatRect(0, 0, FPic.Width, FPic.Height);
        affine.Translate(-FPic.Width / 2, -FPic.Height / 2);
        affine.Scale(scaleX, scaleY);
        affine.Rotate(0, 0, FAngle);
        affine.Translate(x * scalingFactor, y * scalingFactor);
        Transform(b, FPic, affine);
        aBitmap.Draw(0, 0, b.BoundsRect, b);
      finally
        affine.Free;
        b.Free;
      end;

    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TDrawObjGraphic.DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32);
var
  pts: TArrayOfFixedPoint;
  rec: TFloatRect;
  i, marg: integer;
begin
  rec := GetBoundsFloatRect(FUnrotatedOutline);
  marg := GetMargin;
  InflateRect(rec, marg, marg);
  pts := MakeArrayOfFixedPoints(rec);
  if FAngle <> 0 then
    pts := RotatePoints(pts, FixedPoint(RotationPoint), FAngle * DegToRad);
  BitmapToScreen(LayerCollection, pts);

  //workaround that avoids suboptimal antialiasing of design lines ...
  for i := 0 to high(pts) do
  begin
    pts[i].X := RoundFixed(pts[i].X);
    pts[i].Y := RoundFixed(pts[i].Y);
  end;
  PolylineXSP(Buffer, pts, True);
  inherited; //draw the buttons
end;

//------------------------------------------------------------------------------
// TDrawObjNonTextBase methods
//------------------------------------------------------------------------------

constructor TDrawObjNonTextBase.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  IsClosed := True;
end;
//------------------------------------------------------------------------------

procedure TDrawObjNonTextBase.DrawTo(aBitmap: TBitmap32; offset: TFloatPoint; scalingFactor: single = 1);
var
  sw: single;
  pts, shadowPts: TArrayOfFixedPoint;
  orig, mask: TBitmap32;
  savedClr: TColor32;
  savedFillStyle: TFillStyle;
begin
  pts := CopyPoints(FUnrotatedOutline);
  OffsetPoints(pts, -offset.X, -offset.Y);
  ScalePoints(pts, scalingFactor);
  if FillLast then
  begin
    //get a copy of the current bitmap ready for masking ...
    orig := TBitmap32.Create;
    orig.Assign(aBitmap);
  end
  else
    orig := nil;

  try
    if (FShadowOffset.X <> 0) or (FShadowOffset.Y <> 0) then
    begin
      sw := FStrokeWidth - 1;
      if sw > 0 then
        if not IsClockwise(pts) then
          shadowPts := InflatePoints(pts, -sw * scalingFactor, IsClosed)
        else
          shadowPts := InflatePoints(pts, sw * scalingFactor, IsClosed);
      with FShadowOffset do
        DoShadow(aBitmap, shadowPts, round(X * scalingFactor), round(Y * scalingFactor), IsClosed);
    end;
    if FillLast and (FFillColors <> nil) then
    begin
      //the fill may significantly overlap the stroke so mask out the fill region
      //to avoid color overlap. (See TDrawObjWideBezier for why this is necessary)
      mask := TBitmap32.Create;
      try
        with aBitmap do
          Mask.SetSize(Width, Height);

        //build the mask ...
        savedClr := FFillColors[0];
        savedFillStyle := FFillStyle;
        FFillColors[0] := $FFFFFFFF;
        FFillStyle := fsPlain;
        try
          DoFill(Mask, pts, scalingFactor);
        finally
          FFillColors[0] := savedClr;
          FFillStyle := savedFillStyle;
        end;

        DoStroke(aBitmap, pts, FStrokeWidth, scalingFactor);
        //apply the mask to remove the fill region ...
        ApplyMask(aBitmap, orig, mask);
        DoFill(aBitmap, pts, scalingFactor);
      finally
        Mask.Free;
      end;
    end
    else
    begin
      DoFill(aBitmap, pts, scalingFactor);
      DoStroke(aBitmap, pts, FStrokeWidth, scalingFactor);
    end;
  finally
    orig.Free;
  end;
end;

//------------------------------------------------------------------------------
// TDrawObjPoint methods
//------------------------------------------------------------------------------

constructor TDrawObjPoint.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FillColor := clBlack32;
  FRadius := 2;
  Position(FloatPoint(0, 0));
end;
//------------------------------------------------------------------------------

procedure TDrawObjPoint.Position(const pt: TFloatPoint);
var
  pts: TArrayOfFixedPoint;
begin
  setlength(pts, 1);
  RotationPoint := pt;
  pts[0] := FixedPoint(pt);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjPoint.SetRadius(Value: single);
begin
  if FRadius = Value then
    exit;
  FRadius := Value;
  RefreshNeeded := True;
end;
//------------------------------------------------------------------------------

function TDrawObjPoint.GetUnrotatedOutline: TArrayOfFixedPoint;
var
  rec: TFloatRect;
begin
  with FloatPoint(ControlBtns[0]) do
    rec := FloatRect(X - radius, Y - radius, X + radius, Y + radius);
  Result := GetEllipsePoints(rec);
end;
//------------------------------------------------------------------------------

function TDrawObjPoint.GetMargin: integer;
begin
  Result := max(DEFAULT_MARGIN, max(abs(FShadowOffset.X), abs(FShadowOffset.Y))) + ceil(FRadius);
end;
//------------------------------------------------------------------------------

function TDrawObjPoint.ButtonCanMove(BtnIdx: integer): boolean;
begin
  Result := False;
end;
//------------------------------------------------------------------------------

procedure TDrawObjPoint.DrawTo(aBitmap: TBitmap32; offset: TFloatPoint; scalingFactor: single = 1);
var
  sw: single;
  pts, shadowPts: TArrayOfFixedPoint;
begin
  pts := CopyPoints(FUnrotatedOutline);
  OffsetPoints(pts, -offset.X, -offset.Y);
  ScalePoints(pts, scalingFactor);
  if (FShadowOffset.X <> 0) or (FShadowOffset.Y <> 0) then
  begin
    sw := FStrokeWidth - 1;
    if sw > 0 then
      if not IsClockwise(pts) then
        shadowPts := InflatePoints(pts, -sw * scalingFactor, IsClosed)
      else
        shadowPts := InflatePoints(pts, sw * scalingFactor, True);
    with FShadowOffset do
      DoShadow(aBitmap, shadowPts, round(X * scalingFactor), round(Y * scalingFactor), True);
  end;
  if length(pts) = 1 then
  begin
    with pts[0] do
      aBitmap.PixelXS[X, Y] := StrokeColor;
  end
  else
  begin
    DoFill(aBitmap, pts, scalingFactor);
    DoStroke(aBitmap, pts, FStrokeWidth, scalingFactor);
  end;
end;
//------------------------------------------------------------------------------

procedure TDrawObjPoint.DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32);
var
  pt: TFixedPoint;
  pts: TArrayOfFixedPoint;
  rec: TFloatRect;
  marg: integer;
begin
  pt := ControlBtns[0];
  with FloatPoint(pt) do
    rec := FloatRect(X, Y, X, Y);
  marg := GetMargin;
  InflateRect(rec, marg, marg);
  pts := MakeArrayOfFixedPoints(rec);
  BitmapToScreen(LayerCollection, pts);
  PolylineXSP(Buffer, pts, True);
end;

//------------------------------------------------------------------------------
// TDrawObjLine methods
//------------------------------------------------------------------------------

constructor TDrawObjLine.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  IsClosed := False;
  FArrowStart := TArrow.Create;
  FArrowStart.Size := 12;
  FArrowStart.Color := clWhite32;
  FArrowStart.FOnChange := @ArrowUpdated;
  FArrowEnd := TArrow.Create;
  FArrowEnd.Size := 12;
  FArrowEnd.Color := clWhite32;
  FArrowEnd.FOnChange := @ArrowUpdated;
  FAutoReverseText := True;
  Font.OnChange := @FontChange;
  Line32.EndStyle := esRounded;
  SetControlBtns(MakeArrayOfFixedPoints([DEFAULT_MARGIN, DEFAULT_MARGIN, DEFAULT_OBJ_SIZE + DEFAULT_MARGIN, DEFAULT_OBJ_SIZE + DEFAULT_MARGIN]));
end;
//------------------------------------------------------------------------------

destructor TDrawObjLine.Destroy;
begin
  FArrowStart.Free;
  FArrowEnd.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLine.FontChange(Sender: TObject);
begin
  SetLocationToButtons; //ie forces margin readjustment;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLine.Position(const pts: TArrayOfFloatPoint);
var
  rec: TFloatRect;
begin
  rec := GetBoundsFloatRect(pts);
  with rec do
    RotationPoint := FloatPoint((left + right) / 2, (top + bottom) / 2);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjLine.Extend(toEnd: boolean = True; distance: single = 50);
var
  cnt: integer;
  pts: TArrayOfFixedPoint;
  a: single;
begin
  cnt := ControlBtnCount;
  if cnt < 2 then
    exit; //should never happen
  setLength(pts, 1);
  if toEnd then
  begin
    a := GetAngleOfPt2FromPt1(controlBtns[cnt - 2], controlBtns[cnt - 1]);
    pts[0] := GetPointAtAngleFromPoint(controlBtns[cnt - 1], distance, a);
    AddControlBtns(pts, True);
  end
  else
  begin
    a := GetAngleOfPt2FromPt1(controlBtns[1], controlBtns[0]);
    pts[0] := GetPointAtAngleFromPoint(controlBtns[0], distance, a);
    AddControlBtns(pts, False);
  end;
  Repaint;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLine.DrawTo(aBitmap: TBitmap32; offset: TFloatPoint; scalingFactor: single = 1);
var
  i: integer;
  sw: single;
  pts, shadowPts: TArrayOfFixedPoint;
  ppts: TArrayOfArrayOfArrayOfFixedPoint;
  ttf: TTrueTypeFontAnsiCache;
begin
  Line32.ArrowStart.Style := ArrowStart.Style;
  if ArrowStart.Style <> asNone then
  begin
    Line32.ArrowStart.Pen.Color := FStrokeColor;
    Line32.ArrowStart.Color := ArrowStart.Color;
    Line32.ArrowStart.Pen.Width := FStrokeWidth * scalingFactor;
    Line32.ArrowStart.Size := ArrowStart.Size * scalingFactor;
  end;
  Line32.ArrowEnd.Style := ArrowEnd.Style;
  if ArrowEnd.Style <> asNone then
  begin
    Line32.ArrowEnd.Pen.Color := FStrokeColor;
    Line32.ArrowEnd.Color := ArrowEnd.Color;
    Line32.ArrowEnd.Pen.Width := FStrokeWidth * scalingFactor;
    Line32.ArrowEnd.Size := ArrowEnd.Size * scalingFactor;
  end;

  pts := CopyPoints(UnrotatedOutline);
  OffsetPoints(pts, -offset.X, -offset.Y);
  ScalePoints(pts, scalingFactor);
  Line32.SetPoints(pts);
  if (ShadowOffsetX <> 0) or (ShadowOffsetY <> 0) then
  begin
    sw := FStrokeWidth - 1;
    shadowPts := Line32.GetArrowTruncatedPoints;
    if sw > 0 then
      if not IsClockwise(shadowPts) then
        shadowPts := InflatePoints(shadowPts, -sw * scalingFactor, IsClosed)
      else
        shadowPts := InflatePoints(shadowPts, sw * scalingFactor, False);
    with FShadowOffset do
      DoShadow(aBitmap, shadowPts, round(X * scalingFactor), round(Y * scalingFactor), True);

    if ArrowStart.Style <> asNone then
    begin
      shadowPts := Line32.ArrowStart.OutlinePoints(0);
      with FShadowOffset do
        DoShadow(aBitmap, shadowPts, round(X * scalingFactor), round(Y * scalingFactor), True);
    end;
    if ArrowEnd.Style <> asNone then
    begin
      shadowPts := Line32.ArrowEnd.OutlinePoints(0);
      with FShadowOffset do
        DoShadow(aBitmap, shadowPts, round(X * scalingFactor), round(Y * scalingFactor), True);
    end;
  end;
  DoStroke(aBitmap, pts, FStrokeWidth, scalingFactor);

  //now if there's any text, draw it along the line ...
  if trim(Text) = '' then
    exit;

  pts := Line32.GetArrowTruncatedPoints;
  ScalePoints(pts, 1 / scalingFactor);

  if AutoReverseText then
  begin
    if pts[0].X > pts[high(pts)].X then
      pts := ReversePoints(pts);
  end
  else if ReverseText then
    pts := ReversePoints(pts);

  ttf := TTrueTypeFontAnsiCache.Create(Font);
  with TText32.Create do
    try
      ppts := GetEx(pts, Text, ttf, aCenter, aBottom, True, StrokeWidth / 2 + FTextPadding);
      if scalingFactor = 1 then
      begin
        // LCDPolygonSmoothing(bitmap, nil, ppts, Color32(Font.Color), FillColor);  // 7777
      end
      else
      begin
        ScalePoints(ppts, scalingFactor);
        for i := 0 to high(ppts) do
          PolyPolygonXS(abitmap, ppts[i], Color32(Font.Color), pfWinding);
      end;
    finally
      Free;
      ttf.Free;
    end;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLine.DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32);
var
  i, highI: integer;
  pts: TArrayOfFixedPoint;
begin
  pts := GetInflatedOutline(UnrotatedOutline, False, StrokeWidth / 2 + DEFAULT_MARGIN);
  BitmapToScreen(LayerCollection, pts);
  PolylineXSP(Buffer, pts, True);

  //draw the buttons (with the last one colored yellow) ...
  highI := high(FControlBtns);
  Buffer.FillRectTS(aDesigner.ButtonRect(highI), clYellow32);
  Buffer.FrameRectTS(aDesigner.ButtonRect(highI), clBlack32);
  for i := 0 to highI - 1 do
  begin
    Buffer.FillRectTS(aDesigner.ButtonRect(i), clRed32);
    Buffer.FrameRectTS(aDesigner.ButtonRect(i), clBlack32);
  end;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLine.ArrowUpdated(Sender: TObject);
begin
  RefreshNeeded := True;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLine.SetReverseText(Value: boolean);
begin
  if FReverseText = Value then
    exit;
  FReverseText := Value;
  RefreshNeeded := True;
end;
//------------------------------------------------------------------------------

procedure TDrawObjLine.SetAutoReverseText(Value: boolean);
begin
  if FAutoReverseText = Value then
    exit;
  FAutoReverseText := Value;
  RefreshNeeded := True;
end;
//------------------------------------------------------------------------------

function TDrawObjLine.GetMargin: integer;
var
  ttf: TTrueTypeFontAnsiCache;
  tm: TTextMetric; //LCLtypes
  extra, textIncrement, arrowIncrement1, arrowIncrement2: integer;
begin
  Result := max(DEFAULT_MARGIN, max(abs(FShadowOffset.X), abs(FShadowOffset.Y))) + ceil(StrokeWidth / 2);

  //make sure there's room for text and arrows too ...
  if trim(Text) <> '' then
  begin
    ttf := TTrueTypeFontAnsiCache.Create(Font);
    ttf.GetTextMetrics(tm);
    ttf.Free;
    textIncrement := TextPadding + tm.tmHeight;
  end
  else
    textIncrement := 0;
  if Line32.ArrowStart.Style <> asNone then
    arrowIncrement1 := ceil(Line32.ArrowStart.Size / 2 + StrokeWidth / 2)
  else
    arrowIncrement1 := 0;
  if Line32.ArrowEnd.Style <> asNone then
    arrowIncrement2 := ceil(Line32.ArrowEnd.Size / 2 + StrokeWidth / 2)
  else
    arrowIncrement2 := 0;
  extra := max(max(arrowIncrement1, arrowIncrement2), textIncrement);
  Inc(Result, extra);
end;
//------------------------------------------------------------------------------


function TDrawObjLine.DoHitTest(X, Y: integer): boolean;
var
  pt: TFixedPoint;
  p: TPoint;
  pts: TArrayOfFixedPoint;
begin
  //first, check if we're inside the line's bounding rect ...
  Result := inherited DoHitTest(X, Y);
  if not Result then
    exit;

  pt := FixedPoint(X, Y);
  ScreenToBitmap(LayerCollection, pt);

  //now see if an opaque portion of the line is clicked ...
  //(do this first because PtInPolygon can fail when a line overlaps itself)
  p := MakePoint(pt);
  with location do
  begin
    Dec(p.X, round(left));
    Dec(p.Y, round(top));
  end;
  Result := Bitmap.PixelS[p.X, p.Y] and $FF000000 <> 0;
  if Result then
    exit;

  //finally, see if the click is inside the outline ...
  pts := GetInflatedOutline(UnrotatedOutline, False, DEFAULT_MARGIN);
  Result := PtInPolygon(pt, pts);
end;

//------------------------------------------------------------------------------
// TDrawObjBezier methods
//------------------------------------------------------------------------------

constructor TDrawObjBezier.Create(ALayerCollection: TLayerCollection);
var
  pts: TArrayOfFixedPoint;
begin
  inherited;
  IsClosed := False;
  setlength(pts, 4);
  pts[0] := ControlBtns[0];
  pts[3] := ControlBtns[1];
  pts[1].X := pts[0].X + round((pts[3].X - pts[0].X) / 3);
  pts[1].Y := pts[0].Y + round((pts[3].Y - pts[0].Y) / 3);
  pts[2].X := pts[3].X + round((pts[0].X - pts[3].X) / 3);
  pts[2].Y := pts[3].Y + round((pts[0].Y - pts[3].Y) / 3);
  SetControlBtns(pts);
  FSmooth := True;
end;
//------------------------------------------------------------------------------

procedure TDrawObjBezier.Position(const pts: TArrayOfFloatPoint);
var
  cnt: integer;
begin
  cnt := length(pts);
  if (cnt < 4) or ((cnt - 1) mod 3 <> 0) then
    exit
  else
    inherited Position(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjBezier.Extend(toEnd: boolean = True; distance: single = 50);
var
  cnt: integer;
  pts: TArrayOfFixedPoint;
  a, dist: single;
begin
  cnt := ControlBtnCount;
  if cnt < 4 then
    exit; //should never happen
  setLength(pts, 3);
  if toEnd then
  begin
    a := GetAngleOfPt2FromPt1(controlBtns[cnt - 2], controlBtns[cnt - 1]);
    dist := DistBetweenPoints(controlBtns[cnt - 2], controlBtns[cnt - 1]);
    pts[0] := GetPointAtAngleFromPoint(controlBtns[cnt - 1], dist, a);
    pts[1] := GetPointAtAngleFromPoint(controlBtns[cnt - 1], distance * 2 / 3, a);
    pts[2] := GetPointAtAngleFromPoint(controlBtns[cnt - 1], distance, a);
    AddControlBtns(pts, True);
  end
  else
  begin
    a := GetAngleOfPt2FromPt1(controlBtns[1], controlBtns[0]);
    dist := DistBetweenPoints(controlBtns[1], controlBtns[0]);
    pts[0] := GetPointAtAngleFromPoint(controlBtns[0], distance, a);
    pts[1] := GetPointAtAngleFromPoint(controlBtns[0], distance * 2 / 3, a);
    pts[2] := GetPointAtAngleFromPoint(controlBtns[0], dist, a);
    AddControlBtns(pts, False);
  end;
  Repaint;
end;
//------------------------------------------------------------------------------

procedure TDrawObjBezier.DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32);
var
  i: integer;
  pts: TArrayOfFixedPoint;
begin
  setLength(pts, 2);
  i := high(FControlBtns) - 1;
  while i > 0 do
  begin
    pts[0] := FControlBtns[i];
    pts[1] := FControlBtns[i + 1];
    BitmapToScreen(LayerCollection, pts);
    PolylineXSP(Buffer, pts, True);
    pts[0] := FControlBtns[i - 1];
    pts[1] := FControlBtns[i - 2];
    BitmapToScreen(LayerCollection, pts);
    PolylineXSP(Buffer, pts, True);
    Dec(i, 3);
  end;
  inherited;
end;
//------------------------------------------------------------------------------

function TDrawObjBezier.GetUnrotatedOutline: TArrayOfFixedPoint;
begin
  if (ControlBtnCount - 1) mod 3 <> 0 then
    Result := nil
  else
    Result := GetCBezierPoints(FControlBtns);
end;
//------------------------------------------------------------------------------

procedure TDrawObjBezier.SetSmooth(Value: boolean);
begin
  if FSmooth = Value then
    exit;
  FSmooth := Value;
  //RefreshNeeded := true; //not needed because no buttons moved
end;
//------------------------------------------------------------------------------

function TDrawObjBezier.ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean;
var
  rotBtnIdx: integer;
  dx, dy: TFixed;
begin
  Result := inherited ValidateMoveButton(BtnIdx, newPt, updateRotationPt);
  if not Result then
    exit;

  //keep bezier joins smooth ...
  rotBtnIdx := (BtnIdx div 3) * 3;
  if BtnIdx - rotBtnIdx > 1 then
    Inc(rotBtnIdx, 3);
  case BtnIdx - rotBtnIdx of
    -1:
    begin
      if not FSmooth or (BtnIdx < 2) or (BtnIdx > ControlBtnCount - 3) then
        exit;
      dx := newPt.X - ControlBtns[rotBtnIdx].X;
      dy := newPt.Y - ControlBtns[rotBtnIdx].Y;
      ControlBtns[rotBtnIdx + 1].X := ControlBtns[rotBtnIdx].X - dx;
      ControlBtns[rotBtnIdx + 1].Y := ControlBtns[rotBtnIdx].Y - dy;
    end;
    0:
    begin
      dx := newPt.X - ControlBtns[BtnIdx].X;
      dy := newPt.Y - ControlBtns[BtnIdx].Y;
      if (BtnIdx > 0) then
      begin
        ControlBtns[BtnIdx - 1].X := ControlBtns[BtnIdx - 1].X + dx;
        ControlBtns[BtnIdx - 1].Y := ControlBtns[BtnIdx - 1].Y + dy;
      end;
      if (BtnIdx < ControlBtnCount - 1) then
      begin
        ControlBtns[BtnIdx + 1].X := ControlBtns[BtnIdx + 1].X + dx;
        ControlBtns[BtnIdx + 1].Y := ControlBtns[BtnIdx + 1].Y + dy;
      end;
    end;
    1:
    begin
      if not FSmooth or (BtnIdx < 2) or (BtnIdx > ControlBtnCount - 3) then
        exit;
      dx := newPt.X - ControlBtns[rotBtnIdx].X;
      dy := newPt.Y - ControlBtns[rotBtnIdx].Y;
      ControlBtns[rotBtnIdx - 1].X := ControlBtns[rotBtnIdx].X - dx;
      ControlBtns[rotBtnIdx - 1].Y := ControlBtns[rotBtnIdx].Y - dy;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TDrawObjWideBezier methods
//------------------------------------------------------------------------------

constructor TDrawObjWideBezier.Create(ALayerCollection: TLayerCollection);
var
  pts: TArrayOfFixedPoint;
begin
  inherited;
  IsClosed := False;
  FillLast := True;
  Line32.EndStyle := esRounded;
  FFillWidth := 18;
  StrokeWidth := 25;
  FSmooth := True;
  setlength(pts, 4);
  pts[0] := FixedPoint(DEFAULT_MARGIN, DEFAULT_MARGIN);
  pts[3] := FixedPoint(DEFAULT_OBJ_SIZE + DEFAULT_MARGIN, DEFAULT_OBJ_SIZE + DEFAULT_MARGIN);
  pts[1].X := pts[0].X + round((pts[3].X - pts[0].X) / 3);
  pts[1].Y := pts[0].Y + round((pts[3].Y - pts[0].Y) / 3);
  pts[2].X := pts[3].X + round((pts[0].X - pts[3].X) / 3);
  pts[2].Y := pts[3].Y + round((pts[0].Y - pts[3].Y) / 3);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjWideBezier.Position(const pts: TArrayOfFloatPoint);
var
  rec: TFloatRect;
  cnt: integer;
begin
  cnt := length(pts);
  if (cnt < 4) or ((cnt - 1) mod 3 <> 0) then
    exit;
  rec := GetBoundsFloatRect(pts);
  with rec do
    RotationPoint := FloatPoint((left + right) / 2, (top + bottom) / 2);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjWideBezier.DoFill(aBitmap: TBitmap32; const pts: TArrayOfFixedPoint; scaling: single);
var
  p: TArrayOfFixedPoint;
begin
  if (FillWidth < 1) or (FFillColors = nil) then
    exit;
  p := GetInflatedOutline(pts, False, FillWidth * scaling / 2);
  case FFillStyle of
    fsPlain: if (AlphaComponent(FFillColors[0]) > 0) then
        PolygonXS(aBitmap, p, FFillColors[0], pfWinding);
    fsGradiant: SimpleGradientFill(aBitmap, p, $0, FFillColors, 0);
    fsRadial: SimpleRadialFill(aBitmap, p, FFillColors, 0);
    fsCustom: ;
  end;
end;
//------------------------------------------------------------------------------

procedure TDrawObjWideBezier.DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32);
var
  i, highI: integer;
  pts: TArrayOfFixedPoint;
begin
  pts := GetInflatedOutline(UnrotatedOutline, False, StrokeWidth / 2 + DEFAULT_MARGIN);
  BitmapToScreen(LayerCollection, pts);
  PolylineXSP(Buffer, pts, True);

  setLength(pts, 2);
  i := high(FControlBtns) - 1;
  while i > 0 do
  begin
    pts[0] := FControlBtns[i];
    pts[1] := FControlBtns[i + 1];
    BitmapToScreen(LayerCollection, pts);
    PolylineXSP(Buffer, pts, True);
    pts[0] := FControlBtns[i - 1];
    pts[1] := FControlBtns[i - 2];
    BitmapToScreen(LayerCollection, pts);
    PolylineXSP(Buffer, pts, True);
    Dec(i, 3);
  end;

  //draw the buttons (with the last one colored yellow) ...
  highI := high(FControlBtns);
  Buffer.FillRectTS(aDesigner.ButtonRect(highI), clYellow32);
  Buffer.FrameRectTS(aDesigner.ButtonRect(highI), clBlack32);
  for i := 0 to highI - 1 do
  begin
    Buffer.FillRectTS(aDesigner.ButtonRect(i), clRed32);
    Buffer.FrameRectTS(aDesigner.ButtonRect(i), clBlack32);
  end;
end;
//------------------------------------------------------------------------------

function TDrawObjWideBezier.GetUnrotatedOutline: TArrayOfFixedPoint;
begin
  if (ControlBtnCount - 1) mod 3 <> 0 then
    Result := nil
  else
    Result := GetCBezierPoints(FControlBtns);
end;
//------------------------------------------------------------------------------

procedure TDrawObjWideBezier.SetFillWidth(Value: single);
begin
  if FFillWidth = Value then
    exit;
  FFillWidth := Value;
  RefreshNeeded := True;
end;
//------------------------------------------------------------------------------

procedure TDrawObjWideBezier.SetSmooth(Value: boolean);
begin
  if FSmooth = Value then
    exit;
  FSmooth := Value;
  //RefreshNeeded := true; //not needed because no buttons moved
end;
//------------------------------------------------------------------------------

procedure TDrawObjWideBezier.Extend(toEnd: boolean = True; distance: single = 50);
var
  cnt: integer;
  pts: TArrayOfFixedPoint;
  a, dist: single;
begin
  cnt := ControlBtnCount;
  if cnt < 4 then
    exit; //should never happen
  setLength(pts, 3);
  if toEnd then
  begin
    a := GetAngleOfPt2FromPt1(controlBtns[cnt - 2], controlBtns[cnt - 1]);
    dist := DistBetweenPoints(controlBtns[cnt - 2], controlBtns[cnt - 1]);
    pts[0] := GetPointAtAngleFromPoint(controlBtns[cnt - 1], dist, a);
    pts[1] := GetPointAtAngleFromPoint(controlBtns[cnt - 1], distance * 2 / 3, a);
    pts[2] := GetPointAtAngleFromPoint(controlBtns[cnt - 1], distance, a);
    AddControlBtns(pts, True);
  end
  else
  begin
    a := GetAngleOfPt2FromPt1(controlBtns[1], controlBtns[0]);
    dist := DistBetweenPoints(controlBtns[1], controlBtns[0]);
    pts[0] := GetPointAtAngleFromPoint(controlBtns[0], distance, a);
    pts[1] := GetPointAtAngleFromPoint(controlBtns[0], distance * 2 / 3, a);
    pts[2] := GetPointAtAngleFromPoint(controlBtns[0], dist, a);
    AddControlBtns(pts, False);
  end;
  Repaint;
end;
//------------------------------------------------------------------------------

function TDrawObjWideBezier.ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean;
var
  rotBtnIdx: integer;
  dx, dy: TFixed;
begin
  Result := inherited ValidateMoveButton(BtnIdx, newPt, updateRotationPt);
  if not Result then
    exit;

  rotBtnIdx := (BtnIdx div 3) * 3;
  if BtnIdx - rotBtnIdx > 1 then
    Inc(rotBtnIdx, 3);
  case BtnIdx - rotBtnIdx of
    -1:
    begin
      if not FSmooth or (BtnIdx < 2) or (BtnIdx > ControlBtnCount - 3) then
        exit;
      dx := newPt.X - ControlBtns[rotBtnIdx].X;
      dy := newPt.Y - ControlBtns[rotBtnIdx].Y;
      ControlBtns[rotBtnIdx + 1].X := ControlBtns[rotBtnIdx].X - dx;
      ControlBtns[rotBtnIdx + 1].Y := ControlBtns[rotBtnIdx].Y - dy;
    end;
    0:
    begin
      dx := newPt.X - ControlBtns[BtnIdx].X;
      dy := newPt.Y - ControlBtns[BtnIdx].Y;
      if (BtnIdx > 0) then
      begin
        ControlBtns[BtnIdx - 1].X := ControlBtns[BtnIdx - 1].X + dx;
        ControlBtns[BtnIdx - 1].Y := ControlBtns[BtnIdx - 1].Y + dy;
      end;
      if (BtnIdx < ControlBtnCount - 1) then
      begin
        ControlBtns[BtnIdx + 1].X := ControlBtns[BtnIdx + 1].X + dx;
        ControlBtns[BtnIdx + 1].Y := ControlBtns[BtnIdx + 1].Y + dy;
      end;
    end;
    1:
    begin
      if not FSmooth or (BtnIdx < 2) or (BtnIdx > ControlBtnCount - 3) then
        exit;
      dx := newPt.X - ControlBtns[rotBtnIdx].X;
      dy := newPt.Y - ControlBtns[rotBtnIdx].Y;
      ControlBtns[rotBtnIdx - 1].X := ControlBtns[rotBtnIdx].X - dx;
      ControlBtns[rotBtnIdx - 1].Y := ControlBtns[rotBtnIdx].Y - dy;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TDrawObjWideBezier.DoHitTest(X, Y: integer): boolean;
var
  pt: TFixedPoint;
  p: TPoint;
  pts: TArrayOfFixedPoint;
begin
  //first, check if we're inside the line's bounding rect ...
  Result := inherited DoHitTest(X, Y);
  if not Result then
    exit;

  pt := FixedPoint(X, Y);
  ScreenToBitmap(LayerCollection, pt);

  //now see if an opaque portion of the line is clicked ...
  //(do this first because PtInPolygon can fail when a line overlaps itself)
  p := MakePoint(pt);
  with location do
  begin
    Dec(p.X, round(left));
    Dec(p.Y, round(top));
  end;
  Result := Bitmap.PixelS[p.X, p.Y] and $FF000000 <> 0;
  if Result then
    exit;

  //finally, see if the click is inside the outline ...
  pts := GetInflatedOutline(UnrotatedOutline, False, DEFAULT_MARGIN);
  Result := PtInPolygon(pt, pts);
end;

//------------------------------------------------------------------------------
// TDrawObjPolygon methods
//------------------------------------------------------------------------------

constructor TDrawObjPolygon.Create(ALayerCollection: TLayerCollection);
var
  pts: TArrayOfFixedPoint;
  rec: TFloatRect;
begin
  inherited;
  rec := Location;
  InflateRect(rec, -DEFAULT_MARGIN, -DEFAULT_MARGIN);
  pts := GetPointsAroundEllipse(rec, 5);
  with rec do
    RotationPoint := FloatPoint((left + right) / 2, (top + bottom) / 2);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjPolygon.Position(const pts: TArrayOfFixedPoint);
var
  rec: TFloatRect;
begin
  rec := GetBoundsFloatRect(pts);
  with rec do
    RotationPoint := FloatPoint((left + right) / 2, (top + bottom) / 2);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjPolygon.Position(const rec: TFloatRect; pointCnt: integer);
var
  pts: TArrayOfFixedPoint;
begin
  with rec do
    RotationPoint := FloatPoint((left + right) / 2, (top + bottom) / 2);
  pts := GetPointsAroundEllipse(rec, pointCnt);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

function TDrawObjPolygon.ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean;
var
  i: integer;
  dx, angle, angleInc: single;
  center: TFixedPoint;
begin
  Result := inherited ValidateMoveButton(BtnIdx, newPt, updateRotationPt);
  if Result and FRegular then
  begin
    updateRotationPt := False;
    center := Fixedpoint(RotationPoint);
    dx := DistBetweenPoints(center, newPt);
    angleInc := rad360 / ControlBtnCount;
    angle := GetAngleOfPt2FromPt1(center, newPt) - angleInc * BtnIdx;
    for i := BtnIdx + 1 to ControlBtnCount - 1 do
      ControlBtns[i] := GetPointAtAngleFromPoint(center, dx, angle + angleInc * i);
    for i := BtnIdx - 1 downto 0 do
      ControlBtns[i] := GetPointAtAngleFromPoint(center, dx, angle + angleInc * i);
  end;
end;
//------------------------------------------------------------------------------

procedure TDrawObjPolygon.SetRegular(Value: boolean);
var
  marg: integer;
  sx: single;
  rec: TFloatRect;
  pts: TArrayOfFixedPoint;
begin
  if FRegular = Value then
    exit;
  FRegular := Value;
  RefreshNeeded := True;

  if not FRegular then
    exit;
  rec := Location;
  marg := GetMargin;
  InflateRect(rec, -marg, -marg);
  with rec, RotationPoint do
  begin
    sx := ((Bottom - top) + (Right - Left)) / 4;
    Left := X - sx;
    Top := Y - sx;
    right := X + sx;
    Bottom := Y + sx;
  end;
  pts := GetPointsAroundEllipse(rec, ControlBtnCount);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

function TDrawObjPolygon.GetPointCnt: integer;
begin
  Result := ControlBtnCount;
end;
//------------------------------------------------------------------------------

procedure TDrawObjPolygon.SetPointCnt(Value: integer);
var
  pts: TArrayOfFixedPoint;
  RotationPt: TFixedPoint;
  i: integer;
  dist, sqrDist: single;
  rec: TFloatRect;
begin
  Value := min(30, max(3, Value));
  RotationPt := FixedPoint(RotationPoint);
  sqrDist := 0;
  for i := 0 to ControlBtnCount - 1 do
    sqrDist := sqrDist + SquaredDistBetweenPoints(controlBtns[i], RotationPt);
  sqrDist := sqrDist / ControlBtnCount;
  dist := Sqrt(sqrDist);
  with RotationPoint do
    rec := FloatRect(X - dist, Y - dist, X + dist, Y + dist);
  pts := GetPointsAroundEllipse(rec, Value);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjPolygon.DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32);
var
  pts: TArrayOfFixedPoint;
begin
  pts := GetConvexHull(UnrotatedOutline);
  pts := GetInflatedOutline(pts, True, DEFAULT_MARGIN);
  BitmapToScreen(LayerCollection, pts);
  PolylineXSP(Buffer, pts, True);
  inherited; //draw the buttons
end;
//------------------------------------------------------------------------------

function TDrawObjPolygon.DoHitTest(X, Y: integer): boolean;
var
  pt: TFixedPoint;
  pts: TArrayOfFixedPoint;
begin
  Result := inherited DoHitTest(X, Y);
  if not Result then
    exit;
  pt := FixedPoint(X, Y);
  ScreenToBitmap(LayerCollection, pt);
  pts := GetConvexHull(UnrotatedOutline);
  pts := GetInflatedOutline(pts, True, DEFAULT_MARGIN);
  Result := PtInPolygon(pt, pts);
end;

//------------------------------------------------------------------------------
// TDrawObjStar methods
//------------------------------------------------------------------------------

constructor TDrawObjStar.Create(ALayerCollection: TLayerCollection);
var
  pts: TArrayOfFixedPoint;
begin
  inherited;
  with location do
    pts := GetStarPoints(FixedPoint(RotationPoint), 7, (right - left) / 8, (right - left) / 2 - DEFAULT_MARGIN * 2);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjStar.Position(const center: TFloatPoint; innerRadius, outerRadius: single; pointCnt: integer; rotateDegrees: integer = 0);
var
  pts: TArrayOfFixedPoint;
begin
  RotationPoint := center;
  pointCnt := max(5, pointCnt);
  pts := GetStarPoints(FixedPoint(center), pointCnt, innerRadius, outerRadius);
  if rotateDegrees <> 0 then
    pts := RotatePoints(pts, FixedPoint(RotationPoint), rotateDegrees * DegToRad);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

function TDrawObjStar.GetPointCnt: integer;
begin
  Result := ControlBtnCount div 2;
end;
//------------------------------------------------------------------------------

procedure TDrawObjStar.SetPointCnt(Value: integer);
var
  pts: TArrayOfFixedPoint;
  RotationPt: TFixedPoint;
  innerRadius, outerRadius: single;
begin
  Value := min(25, max(5, Value));
  RotationPt := FixedPoint(RotationPoint);
  OuterRadius := DistBetweenPoints(controlBtns[0], RotationPt);
  innerRadius := DistBetweenPoints(controlBtns[1], RotationPt);
  pts := GetStarPoints(RotationPt, Value, innerRadius, outerRadius);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjStar.DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32);
var
  pts: TArrayOfFixedPoint;
begin
  pts := GetInflatedOutline(UnrotatedOutline, True, DEFAULT_MARGIN);
  BitmapToScreen(LayerCollection, pts);
  PolylineXSP(Buffer, pts, True);
  inherited; //draw the buttons
end;
//------------------------------------------------------------------------------

procedure TDrawObjStar.SetRegular(Value: boolean);
var
  i: integer;
  d, dx1, dx2: single;
  pt: TFixedPoint;
begin
  if FRegular = Value then
    exit;
  FRegular := Value;
  RefreshNeeded := True;
  if not FRegular then
    exit;

  //update buttons
  with location do
    dx2 := Max(Right - Left, Bottom - Top) / 2;
  dx1 := dx2;
  pt := FixedPoint(RotationPoint);
  for i := 0 to ControlBtnCount - 1 do
  begin
    d := DistBetweenPoints(pt, ControlBtns[i]);
    if d < dx1 then
      dx1 := d;
  end;
  Position(RotationPoint, dx1, dx2, ControlBtnCount div 2);
end;
//------------------------------------------------------------------------------

function TDrawObjStar.DoHitTest(X, Y: integer): boolean;
var
  pt: TFixedPoint;
  pts: TArrayOfFixedPoint;
begin
  Result := inherited DoHitTest(X, Y);
  if not Result then
    exit;
  pt := FixedPoint(X, Y);
  ScreenToBitmap(LayerCollection, pt);
  pts := GetInflatedOutline(UnrotatedOutline, True, DEFAULT_MARGIN);
  Result := PtInPolygon(pt, pts);
end;
//------------------------------------------------------------------------------

function TDrawObjStar.ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean;
var
  i, prevI: integer;
  angle, a, prevA, nextA, dx: single;
  center: TFixedPoint;
begin
  Result := inherited ValidateMoveButton(BtnIdx, newPt, updateRotationPt);
  if not Result then
    exit;

  i := BtnIdx;
  updateRotationPt := False;
  center := Fixedpoint(RotationPoint);
  dx := DistBetweenPoints(center, newPt);
  if FRegular then
  begin
    if i = 0 then
      prevI := ControlBtnCount - 1
    else
      prevI := i - 1;
    prevA := GetAngleOfPt2FromPt1(center, ControlBtns[prevI]);
    nextA := prevA + rad360 / (ControlBtnCount / 2);
    angle := (prevA + nextA) / 2;
    newPt := GetPointAtAngleFromPoint(center, dx, angle);
  end
  else
    angle := GetAngleOfPt2FromPt1(center, newPt);

  a := angle;
  while i >= 2 do
  begin
    Dec(i, 2);
    a := a - (rad360 / ControlBtnCount) * 2;
    FControlBtns[i] := GetPointAtAngleFromPoint(center, dx, a);
  end;
  i := BtnIdx;
  a := angle;
  while i < ControlBtnCount - 2 do
  begin
    Inc(i, 2);
    a := a + (rad360 / ControlBtnCount) * 2;
    FControlBtns[i] := GetPointAtAngleFromPoint(center, dx, a);
  end;
end;

//------------------------------------------------------------------------------
// TDrawObjArrow methods
//------------------------------------------------------------------------------

constructor TDrawObjArrow.Create(ALayerCollection: TLayerCollection);
var
  rec: TFloatRect;
begin
  inherited;
  rec := Location;
  InflateRect(rec, -DEFAULT_MARGIN, -DEFAULT_MARGIN);
  ResetPoints(rec);
end;
//------------------------------------------------------------------------------

procedure TDrawObjArrow.ResetPoints(const rec: TFloatRect);
var
  pts: TArrayOfFixedPoint;
  w, h: single;
begin
  with rec do
  begin
    RotationPoint := FloatPoint((left + right) / 2, (top + bottom) / 2);
    w := right - left;
    h := bottom - top;
    pts := MakeArrayOfFixedPoints([right, RotationPoint.Y, left + w / 3, top, left + w / 2, top +
      h * 5 / 12, left, top + h / 4, left + w / 5, RotationPoint.Y, left, bottom - h / 4, left + w / 2, bottom - h * 5 / 12, left + w / 3, bottom]);
  end;
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjArrow.Position(const rec: TFloatRect; rotateDegrees: integer);
begin
  with rec do
    RotationPoint := FloatPoint((left + right) / 2, (top + bottom) / 2);
  ResetPoints(rec);
  if rotateDegrees <> 0 then
    Rotate(rotateDegrees);
end;
//------------------------------------------------------------------------------

procedure TDrawObjArrow.Position(const pts: TArrayOfFixedPoint);
var
  rec: TFloatRect;
begin
  if length(pts) <> 8 then
    exit;
  rec := GetBoundsFloatRect(pts);
  with rec do
    RotationPoint := FloatPoint((left + right) / 2, (top + bottom) / 2);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjArrow.DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32);
var
  pts: TArrayOfFixedPoint;
begin
  pts := GetInflatedOutline(UnrotatedOutline, True, DEFAULT_MARGIN);
  BitmapToScreen(LayerCollection, pts);
  PolylineXSP(Buffer, pts, True);
  inherited; //draw the buttons
end;
//------------------------------------------------------------------------------

function TDrawObjArrow.DoHitTest(X, Y: integer): boolean;
var
  pt: TFixedPoint;
  pts: TArrayOfFixedPoint;
begin
  Result := inherited DoHitTest(X, Y);
  if not Result then
    exit;
  pt := FixedPoint(X, Y);
  ScreenToBitmap(LayerCollection, pt);
  pts := GetInflatedOutline(UnrotatedOutline, True, DEFAULT_MARGIN);
  Result := PtInPolygon(pt, pts);
end;
//------------------------------------------------------------------------------

function TDrawObjArrow.ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean;
var
  alt: integer;
  c: TFixedPoint;
  d, a, a2, m, b: single;
  crossesMidline: boolean;
begin
  Result := inherited ValidateMoveButton(BtnIdx, newPt, updateRotationPt);
  if not Result then
    exit;

  //nb: FControlBtns[0] = arrow point; FControlBtns[4] = arrow rear;
  c := MidPoint(FControlBtns[1], FControlBtns[7]);
  alt := 8 - BtnIdx;
  case BtnIdx of
    0:
    begin
      d := DistBetweenPoints(newPt, FControlBtns[4]);
      if d < DistBetweenPoints(c, FControlBtns[4]) + 3 then
        d := DistBetweenPoints(c, FControlBtns[4]) + 3;
      a := GetAngleOfPt2FromPt1(FControlBtns[4], c);
      newPt := GetPointAtAngleFromPoint(FControlBtns[4], d, a);
    end;
    1, 2, 3, 5, 6, 7:
    begin
      //make sure the point isn't crossing the arrow axis ...
      if (FControlBtns[0].X = FControlBtns[4].X) then
      begin
        crossesMidline := (newPt.X - FControlBtns[0].X) * FixedToFloat * (FControlBtns[alt].X - FControlBtns[0].X) * FixedToFloat >= 0;
        //          m := MaxSingle;
        //          b := FControlBtns[0].X *FixedToFloat;
      end
      else
      begin
        m := (FControlBtns[0].Y - FControlBtns[4].Y) / (FControlBtns[0].X - FControlBtns[4].X);
        b := FControlBtns[0].Y - m * FControlBtns[0].X;
        //both points above or both points below the axis line when ...
        crossesMidline := (newPt.Y - m * newPt.X - b) * (FControlBtns[alt].Y - m * FControlBtns[alt].X - b) >= 0;
      end;
      if crossesMidline then
      begin
        Result := False;
        exit;
      end;

      d := DistBetweenPoints(newPt, c);
      if (BtnIdx in [2, 6]) then
      begin
        if d >= DistBetweenPoints(FControlBtns[1], c) then
          d := DistBetweenPoints(FControlBtns[1], c) - 1;
      end
      else if d < 5 then
        d := 5;

      a := GetAngleOfPt2FromPt1(c, newPt);
      a2 := GetAngleOfPt2FromPt1(c, FControlBtns[4]);
      newPt := GetPointAtAngleFromPoint(c, d, a);
      FControlBtns[alt] := GetPointAtAngleFromPoint(c, d, a2 + (a2 - a));
    end;
    4:
    begin
      d := DistBetweenPoints(newPt, FControlBtns[0]);
      if d >= DistBetweenPoints(FControlBtns[3], FControlBtns[0]) then
        d := DistBetweenPoints(FControlBtns[3], FControlBtns[0]);
      a := GetAngleOfPt2FromPt1(FControlBtns[0], c);
      newPt := GetPointAtAngleFromPoint(FControlBtns[0], d, a);
    end;
  end;
end;

//------------------------------------------------------------------------------
// TDrawObjTextBase methods
//------------------------------------------------------------------------------

constructor TDrawObjTextBase.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  TextPadding := 2;
  SetControlBtns(MakeArrayOfFixedPoints([DEFAULT_MARGIN, DEFAULT_MARGIN, DEFAULT_OBJ_SIZE + DEFAULT_MARGIN * 2, DEFAULT_OBJ_SIZE + DEFAULT_MARGIN * 2]));
end;
//------------------------------------------------------------------------------

function TDrawObjTextBase.GetRotatedOutline: TArrayOfFixedPoint;
begin
  Result := GetUnrotatedOutline;
  Result := RotatePoints(Result, FixedPoint(RotationPoint), FAngle * DegToRad);
end;
//------------------------------------------------------------------------------

procedure TDrawObjTextBase.SetRegular(Value: boolean);
var
  sx: single;
  rec: TFloatRect;
  pts: TArrayOfFixedPoint;
begin
  if FRegular = Value then
    exit;
  FRegular := Value;
  RefreshNeeded := True;
  if not FRegular then
    exit;

  rec := GetBoundsFloatRect(FUnrotatedOutline);
  with rec, RotationPoint do
  begin
    sx := ((Bottom - top) + (Right - Left)) / 4;
    Left := X - sx;
    Top := Y - sx;
    right := X + sx;
    Bottom := Y + sx;
  end;
  setlength(pts, 2);
  pts[0] := FixedPoint(rec.TopLeft);
  pts[1] := FixedPoint(rec.BottomRight);
  if FAngle <> 0 then
    pts := RotatePoints(pts, FixedPoint(RotationPoint), FAngle * DegToRad);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

procedure TDrawObjTextBase.Position(const rec: TFloatRect; angleDegrees: integer);
var
  pts: TArrayOfFixedPoint;
begin
  with rec do
  begin
    RotationPoint := FloatPoint((left + right) / 2, (top + bottom) / 2);
    setlength(pts, 2);
    pts[0] := FixedPoint(rec.TopLeft);
    pts[1] := FixedPoint(rec.BottomRight);
  end;
  FAngle := angleDegrees;
  if angleDegrees <> 0 then
    pts := RotatePoints(pts, FixedPoint(RotationPoint), angleDegrees * DegToRad);
  SetControlBtns(pts);
end;
//------------------------------------------------------------------------------

function TDrawObjTextBase.DoHitTest(X, Y: integer): boolean;
var
  pt: TFixedPoint;
  pts: TArrayOfFixedPoint;
  rec: TFloatRect;
  marg: integer;
begin
  if (FAngle <> 0) then
  begin
    pt := FixedPoint(X, Y);
    ScreenToBitmap(LayerCollection, pt);
    rec := GetBoundsFloatRect(FUnrotatedOutline);
    marg := GetMargin;
    InflateRect(rec, marg, marg);
    pts := MakeArrayOfFixedPoints(rec);
    pts := RotatePoints(pts, FixedPoint(RotationPoint), FAngle * DegToRad);
    Result := PtInPolygon(pt, pts);
  end
  else
    Result := inherited DoHitTest(X, Y);
end;
//------------------------------------------------------------------------------

procedure TDrawObjTextBase.Rotate(degrees: integer);
begin
  if degrees <> 0 then
    SetAngle(FAngle + degrees);
end;
//------------------------------------------------------------------------------

procedure TDrawObjTextBase.SetAngle(degrees: integer);
var
  rotateDegrees: integer;
begin
  while degrees <= -180 do
    Inc(degrees, 360);
  while degrees > 180 do
    Dec(degrees, 360);
  if FAngle = degrees then
    exit;
  rotateDegrees := degrees - FAngle;
  FAngle := degrees;
  inherited Rotate(rotateDegrees);
end;
//------------------------------------------------------------------------------

procedure TDrawObjTextBase.DrawDesigner(aDesigner: TDesignerLayer; Buffer: TBitmap32);
var
  pts: TArrayOfFixedPoint;
  rec: TFloatRect;
  i, marg: integer;
begin
  rec := GetBoundsFloatRect(FUnrotatedOutline);
  marg := GetMargin;
  InflateRect(rec, marg, marg);
  pts := MakeArrayOfFixedPoints(rec);
  if FAngle <> 0 then
    pts := RotatePoints(pts, FixedPoint(RotationPoint), FAngle * DegToRad);
  BitmapToScreen(LayerCollection, pts);

  //workaround that avoids suboptimal antialiasing of design lines ...
  for i := 0 to high(pts) do
  begin
    pts[i].X := RoundFixed(pts[i].X);
    pts[i].Y := RoundFixed(pts[i].Y);
  end;

  //  pts := GetRotatedOutline;
  //  BitmapToScreen(LayerCollection, pts);
  //  marg := GetMargin+DEFAULT_MARGIN;
  //  pts := InflatePoints(pts, marg, true);

  PolylineXSP(Buffer, pts, True);

  inherited; //draw the buttons
end;
//------------------------------------------------------------------------------

function TDrawObjTextBase.ValidateMoveButton(BtnIdx: integer; var newPt: TFixedPoint; var updateRotationPt: boolean): boolean;
var
  altBtnIdx: integer;
  pt: TFixedPoint;
  pts: TArrayOfFixedPoint;
  q: TFixed;
begin
  Result := inherited ValidateMoveButton(BtnIdx, newPt, updateRotationPt);
  if not Result or not Regular then
    exit;

  if BtnIdx = 0 then
    altBtnIdx := 1
  else
    altBtnIdx := 0;
  if FAngle = 0 then
  begin
    pts := copypoints(ControlBtns);
    pt := newPt;
  end
  else
  begin
    pts := RotatePoints(ControlBtns, FixedPoint(RotationPoint), -FAngle * DegToRad);
    pt := RotatePoint(newPt, FixedPoint(RotationPoint), -FAngle * DegToRad);
  end;
  q := min(abs(pt.X - pts[altBtnIdx].X), abs(pt.Y - pts[altBtnIdx].Y));
  if pt.X < pts[altBtnIdx].X then
    q := -q;
  newPt.X := pts[altBtnIdx].X + q;
  newPt.Y := pts[altBtnIdx].Y + q;
  if FAngle <> 0 then
    newPt := RotatePoint(newPt, FixedPoint(RotationPoint), FAngle * DegToRad);
end;
//------------------------------------------------------------------------------

procedure TDrawObjTextBase.DrawTo(aBitmap: TBitmap32; offset: TFloatPoint; scalingFactor: single = 1);
var
  pts, shadowPts: TArrayOfFixedPoint;
  sw: single;
  i: integer;
  ppts: TArrayOfArrayOfArrayOfFixedPoint;
  ttf: TTrueTypeFontAnsiCache;
  rec: TFloatRect;
begin
  pts := CopyPoints(FUnrotatedOutline);
  if FAngle <> 0 then
    pts := RotatePoints(pts, FixedPoint(RotationPoint), FAngle * DegToRad);
  OffsetPoints(pts, -offset.X, -offset.Y);
  ScalePoints(pts, scalingFactor);
  if (FShadowOffset.X <> 0) or (FShadowOffset.Y <> 0) then
  begin
    sw := FStrokeWidth - 1;
    if sw > 0 then
      if not IsClockwise(pts) then
        shadowPts := InflatePoints(pts, -sw * scalingFactor, True)
      else
        shadowPts := InflatePoints(pts, sw * scalingFactor, True);
    with FShadowOffset do
      DoShadow(aBitmap, shadowPts, round(X * scalingFactor), round(Y * scalingFactor), True);
  end;
  DoFill(aBitmap, pts, scalingFactor);
  DoStroke(aBitmap, pts, FStrokeWidth, scalingFactor);

  //draw text ...
  if trim(FText) = '' then
    exit;
  ttf := TTrueTypeFontAnsiCache.Create(Font);
  ttf.Hinted := round(Angle) = 0;
  with TText32.Create do
    try
      Angle := round(self.Angle);
      rec := GetBoundsFloatRect(FUnrotatedOutline);
      InflateRect(rec, -TextPadding - StrokeWidth / 2, -TextPadding - StrokeWidth / 2);
      OffsetRect(rec, -offset.X, -offset.Y);
      ppts := getEx(rec, FText, ttf, aCenter, aMiddle);
      if scalingFactor <> 1 then
        ScalePoints(ppts, scalingFactor);
    finally
      Free;
      ttf.Free;
    end;
  if scalingFactor = 1 then
  begin
    // LCDPolygonSmoothing(bitmap,nil,ppts,Color32(Font.Color));  // 7777
  end
  else
    for i := 0 to high(ppts) do
      PolyPolygonXS(bitmap, ppts[i], Color32(Font.Color), pfWinding);
end;

//------------------------------------------------------------------------------
//TDrawObjRectangle methods
//------------------------------------------------------------------------------

procedure TDrawObjRectangle.SetRounded(Value: boolean);
begin
  if FRounded = Value then
    exit;
  FRounded := Value;
  //update outline ...
  UnrotatedOutline := GetUnrotatedOutline;
  RefreshNeeded := True;
end;
//------------------------------------------------------------------------------

function TDrawObjRectangle.GetUnrotatedOutline: TArrayOfFixedPoint;
var
  rec: TFixedRect;
begin
  Result := CopyPoints(FControlBtns);
  if FAngle <> 0 then
    Result := RotatePoints(Result, FixedPoint(RotationPoint), -FAngle * DegToRad);
  rec := GetBoundsFixedRect(Result);
  if fRounded then
    Result := GetRoundedRectanglePoints(FloatRect(rec), 20)
  else
    Result := MakeArrayOfFixedPoints(rec);
end;

//------------------------------------------------------------------------------
// TDrawObjEllipse methods
//------------------------------------------------------------------------------

procedure TDrawObjEllipse.SetBalloonPos(Value: TBalloonPos);
begin
  if FBalloonPos = Value then
    exit;
  FBalloonPos := Value;
  //update outline ...
  UnrotatedOutline := GetUnrotatedOutline;
  RefreshNeeded := True;
end;
//------------------------------------------------------------------------------

function TDrawObjEllipse.GetUnrotatedOutline: TArrayOfFixedPoint;
var
  pts: TArrayOfFixedPoint;
  rec: TFloatRect;
begin
  pts := CopyPoints(FControlBtns);
  if FAngle <> 0 then
    pts := RotatePoints(pts, FixedPoint(RotationPoint), -FAngle * DegToRad);
  rec := GetBoundsFloatRect(pts);
  case fBalloonPos of
    bpNone: Result := GetEllipsePoints(rec);
    else
      Result := GetBalloonedEllipsePoints(rec, fBalloonPos);
  end;
end;

//------------------------------------------------------------------------------
// TDrawObjArc methods
//------------------------------------------------------------------------------

constructor TDrawObjArc.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  AngleStart := 45;
  AngleEnd := 0;
end;
//------------------------------------------------------------------------------

procedure TDrawObjArc.SetAngleStart(Value: integer);
begin
  while Value < 0 do
    Inc(Value, 360);
  while Value >= 360 do
    Dec(Value, 360);
  if FAngleStart = Value then
    exit;
  FAngleStart := Value;
  //update outline ...
  UnrotatedOutline := GetUnrotatedOutline;
  RefreshNeeded := True;
end;
//------------------------------------------------------------------------------

procedure TDrawObjArc.SetAngleEnd(Value: integer);
begin
  while Value < 0 do
    Inc(Value, 360);
  while Value >= 360 do
    Dec(Value, 360);
  if FAngleEnd = Value then
    exit;
  FAngleEnd := Value;
  //update outline ...
  UnrotatedOutline := GetUnrotatedOutline;
  RefreshNeeded := True;
end;
//------------------------------------------------------------------------------

function TDrawObjArc.GetUnrotatedOutline: TArrayOfFixedPoint;
var
  len: integer;
  pts: TArrayOfFixedPoint;
  rec: TFloatRect;
begin
  pts := CopyPoints(FControlBtns);
  if FAngle <> 0 then
    pts := RotatePoints(pts, FixedPoint(RotationPoint), -FAngle * DegToRad);
  rec := GetBoundsFloatRect(pts);
  Result := GetArcPoints(rec, fAngleStart, fAngleEnd);
  len := length(Result);
  setLength(Result, len + 1);
  Result[len] := FixedPoint((rec.Left + rec.Right) / 2, (rec.Top + rec.Bottom) / 2);
end;

//------------------------------------------------------------------------------
// TDrawObjDiamond methods
//------------------------------------------------------------------------------

function TDrawObjDiamond.GetUnrotatedOutline: TArrayOfFixedPoint;
var
  pts: TArrayOfFixedPoint;
  rec: TFloatRect;
  hMiddle, vMiddle: single;
begin
  pts := CopyPoints(FControlBtns);
  if FAngle <> 0 then
    pts := RotatePoints(pts, FixedPoint(RotationPoint), -FAngle * DegToRad);
  rec := GetBoundsFloatRect(pts);
  hMiddle := (rec.left + rec.right) / 2;
  vMiddle := (rec.top + rec.bottom) / 2;
  Result := MakeArrayOfFixedPoints([hMiddle, rec.top, rec.right, vMiddle, hMiddle, rec.bottom, rec.left, vMiddle]);
end;

//------------------------------------------------------------------------------
// TDesignerLayer methods
//------------------------------------------------------------------------------

constructor TDesignerLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  Scaled := True;
  FButtonSize := DEFAULT_BTN_SIZE;
  LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS;
  FDragState := DRAG_NONE;
end;
//------------------------------------------------------------------------------

destructor TDesignerLayer.Destroy;
begin
  if Assigned(FChildLayer) then
    FChildLayer.SetDesigner(nil);
  inherited;
end;
//------------------------------------------------------------------------------

function TDesignerLayer.GetDragBtnState(X, Y: integer): integer;
var
  i: integer;
begin
  //returns the button index if Point(X,Y) is over a button
  //else if ChildLayer HitTest(X,Y) = true then returns DRAG_MOVE
  //otherwise returns DRAG_NONE ...
  Result := DRAG_NONE;
  if not assigned(FChildLayer) then
    exit;

  with FChildLayer do
  begin
    for i := 0 to high(ControlBtns) do
    begin
      if PtInRect(ButtonRect(i), Point(X, Y)) and ButtonCanMove(i) then
      begin
        Result := i;
        break;
      end;
    end;
    if (Result = DRAG_NONE) and HitTest(X, Y) then
      Result := DRAG_MOVE;
  end;

  if assigned(FOnValidateDragState) and (Result <> DRAG_NONE) then
  begin
    FOnValidateDragState(self, Result);
    //now make sure the user hasn't done something silly ...
    if (Result < DRAG_MOVE) or (Result >= FChildLayer.ControlBtnCount) then
      Result := DRAG_NONE;
  end;

end;
//------------------------------------------------------------------------------

function TDesignerLayer.DoHitTest(X, Y: integer): boolean;
begin
  Result := GetDragBtnState(X, Y) <> DRAG_NONE;
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer.DoMoving(OldLocation: TFloatRect; var NewLocation: TFloatRect; DragState: integer; Shift: TShiftState);
begin
  if Assigned(FOnMoving) then
    FOnMoving(Self, OldLocation, NewLocation, DragState, Shift);
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer.DoButtonMoving;
begin
  if assigned(FOnBtnMoving) then
    FOnBtnMoving(self);
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer.DoSetLocation(const NewLocation: TFloatRect);
begin
  inherited;
  UpdateChildLayer;
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (FDragState <> DRAG_NONE) then
    Exit;
  //sometimes DesignerLayer is no longer over its ChildLayer ...
  if assigned(ChildLayer) and not IsDuplicateRect(location, ChildLayer.Location) then
    Location := ChildLayer.Location;

  FDragState := GetDragBtnState(X, Y);
  if (FDragState <> DRAG_NONE) then
  begin
    MouseDownLoc := Location;
    MouseDownPt := FloatPoint(X, Y);
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer.MouseMove(Shift: TShiftState; X, Y: integer);
var
  Delta: TFloatPoint;
  NewLoc: TFloatRect;
  pt: TFixedPoint;
  DragBtnState: integer;
  ScaleX, ScaleY: single;
begin
  if FDragState = DRAG_NONE then
  begin
    DragBtnState := GetDragBtnState(X, Y);
    //if over a button display HandPoint cursor ...
    if DragBtnState >= 0 then
      Screen.Cursor := crVPRHandPointing
    else if DragBtnState = DRAG_MOVE then
      Screen.Cursor := crVPRHandOpen
    else
      Screen.Cursor := crDefault;
  end
  else if FDragState = DRAG_MOVE then
  begin
    Screen.Cursor := crVPRHandClosed;
    if assigned(LayerCollection) then
    begin
      LayerCollection.GetViewportScale(ScaleX, ScaleY);
      Delta := FloatPoint((X - MouseDownPt.X) / ScaleX, (Y - MouseDownPt.Y) / ScaleY);
    end
    else
      Delta := FloatPoint(X - MouseDownPt.X, Y - MouseDownPt.Y);
    NewLoc := MouseDownLoc;
    OffsetRect(NewLoc, Delta.X, Delta.Y);
    DoMoving(Location, NewLoc, FDragState, Shift);
    if not IsDuplicatePoint(Location.TopLeft, NewLoc.TopLeft) then
      self.Location := NewLoc;
  end
  else //Button Move ...
  begin
    pt := FixedPoint(X, Y);
    ScreenToBitmap(LayerCollection, pt);
    FChildLayer.MoveButton(FDragState, pt);
    DoButtonMoving;
  end;
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FDragState := DRAG_NONE;
  inherited;
end;
//------------------------------------------------------------------------------

function TDesignerLayer.ButtonRect(btnIdx: integer): TRect;
var
  pt: TFixedPoint;
begin
  Result := Rect(0, 0, 0, 0);
  if not assigned(FChildLayer) or (btnIdx < 0) or (btnIdx >= FChildLayer.ControlBtnCount) then
    exit;

  //scale the button position ...
  pt := FChildLayer.ControlBtns[btnIdx];
  BitmapToScreen(LayerCollection, pt);

  //but don't scale the button size ...
  with Point(pt) do
  begin
    Result.Left := X - ButtonSize;
    Result.Right := X + ButtonSize;
    Result.Top := Y - ButtonSize;
    Result.Bottom := Y + ButtonSize;
  end;
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer.Paint(Buffer: TBitmap32);
begin
  if not Assigned(FChildLayer) then
    exit;
  Buffer.StippleCounter := 0;
  Buffer.SetStipple(([clRed32, $0, $0]));
  Buffer.StippleStep := 0.2;
  FChildLayer.DrawDesigner(self, Buffer);
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer.Notification(ALayer: TCustomLayer);
begin
  if ALayer = FChildLayer then
    FChildLayer := nil;
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer.SetChildLayer(Value: TDrawObjLayerBase);
begin
  if Assigned(FChildLayer) then
  begin
    FChildLayer.SetDesigner(nil);
    RemoveNotification(FChildLayer);
  end;

  FChildLayer := Value;

  if Assigned(Value) then
  begin
    Location := Value.Location;
    Scaled := Value.Scaled;
    AddNotification(FChildLayer);
    FChildLayer.SetDesigner(self);
  end;
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer.SetButtonSize(Value: integer);
begin
  if Value < 2 then
    Value := 2;
  if Value <> FButtonSize then
    FButtonSize := Value;
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer.UpdateChildLayer;
begin
  if Assigned(FChildLayer) and not IsDuplicateRect(Location, FChildLayer.Location) then
    FChildLayer.Location := self.Location;
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer.SetLayerOptions(Value: cardinal);
begin
  inherited SetLayerOptions(Value and not LOB_NO_UPDATE);
end;
//------------------------------------------------------------------------------

procedure RegisterDrawObjClasses;
begin
  RegisterClasses([TDrawObjGraphic, TDrawObjPoint, TDrawObjLine, TDrawObjBezier, TDrawObjWideBezier, TDrawObjPolygon,
    TDrawObjStar, TDrawObjArrow, TDrawObjArc, TDrawObjRectangle, TDrawObjEllipse, TDrawObjDiamond]);
end;
//------------------------------------------------------------------------------

procedure LoadObjectsCursors;  // ct9999
begin
  Screen.Cursors[crVPRHandPointing] := LoadCursor(HInstance, 'VPRXHANDPOINT');
  Screen.Cursors[crVPRHandOpen] := LoadCursor(HInstance, 'VPRXHANDOPEN');
  Screen.Cursors[crVPRHandClosed] := LoadCursor(HInstance, 'VPRXHANDCLOSED');
end;

initialization

  RegisterDrawObjClasses;

end.
