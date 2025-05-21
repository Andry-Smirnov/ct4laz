{*************************************************************************
                PilotLogic Software House

  Package pl_Graphics32
  This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)

 ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * https://www.mozilla.org/en-US/MPL/1.1/
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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All rights reserved
 *
 * ***** END LICENSE BLOCK *****
 ************************************************************************}

unit GR32_Polygons;

interface

{$I GR32.inc}

uses
  Classes, Types,
  GR32_AllStrings,
  GR32, GR32_Containers, GR32_VPR, GR32_Transforms, GR32_Resamplers,
  Math, SysUtils, GR32_Math, GR32_LowLevel, GR32_Blend;

type
  { Polygon join style - used by GR32_VectorUtils.Grow(). }
  { nb: jsRoundEx rounds both convex and concave joins unlike jsRound which
    only rounds convex joins. The depth of convex join rounding is controlled
    by Grow's MiterLimit parameter }
  TJoinStyle = (jsMiter, jsBevel, jsRound, jsRoundEx);

  { Polygon end style }
  TEndStyle = (esButt, esSquare, esRound);

  { Polygon fill mode }
  TPolyFillMode = (pfAlternate, pfWinding, pfEvenOdd = 0, pfNonZero);

  { TCustomPolygonRenderer }
  TCustomPolygonRenderer = class(TThreadPersistent)
  public
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation); overload; virtual;
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect); overload; virtual;
    procedure PolygonFS(const Points: TArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation); overload; virtual;
    procedure PolygonFS(const Points: TArrayOfFloatPoint;
      const ClipRect: TFloatRect); overload; virtual;

    // procedure PolyPolygonXS(const Points: TArrayOfArrayOfFixedPoint; const ClipRect: TFixedRect; Transformation: TTransformation); virtual; overload;
    // procedure PolyPolygonXS(const Points: TArrayOfArrayOfFixedPoint; const ClipRect: TFixedRect); virtual; overload;
  end;
  TCustomPolygonRendererClass = class of TCustomPolygonRenderer;

  TCustomPolygonFiller = class;

  { TPolygonRenderer32 }
  TPolygonRenderer32 = class(TCustomPolygonRenderer)
  private
    FBitmap: TBitmap32;
    FFillMode: TPolyFillMode;
    FColor: TColor32;
    FFiller: TCustomPolygonFiller;
    procedure SetColor(const Value: TColor32);
    procedure SetFillMode(const Value: TPolyFillMode);
    procedure SetFiller(const Value: TCustomPolygonFiller);
  protected
    procedure SetBitmap(const Value: TBitmap32); virtual;
  public
    constructor Create(Bitmap: TBitmap32; Fillmode: TPolyFillMode = pfWinding); reintroduce; overload;
    procedure PolygonFS(const Points: TArrayOfFloatPoint); overload; virtual;
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint); overload; virtual;

    property Bitmap: TBitmap32 read FBitmap write SetBitmap;
    property FillMode: TPolyFillMode read FFillMode write SetFillMode;
    property Color: TColor32 read FColor write SetColor;
    property Filler: TCustomPolygonFiller read FFiller write SetFiller;
  end;
  TPolygonRenderer32Class = class of TPolygonRenderer32;

  { TPolygonRenderer32VPR }
  { Polygon renderer based on VPR. Computes exact coverages for optimal anti-aliasing. }
  TFillProc = procedure(Coverage: PSingleArray; AlphaValues: PColor32Array; Count: Integer; Color: TColor32);

  TPolygonRenderer32VPR = class(TPolygonRenderer32)
  private
    FFillProc: TFillProc;
    procedure UpdateFillProcs;
  protected
    procedure RenderSpan(const Span: TValueSpan; DstY: Integer); virtual;
    procedure FillSpan(const Span: TValueSpan; DstY: Integer); virtual;
    function GetRenderSpan: TRenderSpanEvent; virtual;
  public
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect); override;
  end;

  { TPolygonRenderer32LCD }
  TPolygonRenderer32LCD = class(TPolygonRenderer32VPR)
  protected
    procedure RenderSpan(const Span: TValueSpan; DstY: Integer); override;
  public
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect); override;
  end;

  { TPolygonRenderer32LCD2 }
  TPolygonRenderer32LCD2 = class(TPolygonRenderer32LCD)
  public
    procedure RenderSpan(const Span: TValueSpan; DstY: Integer); override;
  end;

  { TCustomPolygonFiller }

  TFillLineEvent = procedure(Dst: PColor32; DstX, DstY, Length: Integer;
    AlphaValues: PColor32; CombineMode: TCombineMode) of object;

  TCustomPolygonFiller = class
  protected
    function GetFillLine: TFillLineEvent; virtual; abstract;
  public
    procedure BeginRendering; virtual;
    procedure EndRendering; virtual;

    property FillLine: TFillLineEvent read GetFillLine;
  end;

  { TCallbackPolygonFiller }
  TCallbackPolygonFiller = class(TCustomPolygonFiller)
  private
    FFillLineEvent: TFillLineEvent;
  protected
    function GetFillLine: TFillLineEvent; override;
  public
    property FillLineEvent: TFillLineEvent read FFillLineEvent write FFillLineEvent;
  end;

  { TInvertPolygonFiller }
  TInvertPolygonFiller = class(TCustomPolygonFiller)
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineBlend(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
  end;

  { TClearPolygonFiller }
  TClearPolygonFiller = class(TCustomPolygonFiller)
  private
    FColor: TColor32;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineClear(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
  public
    constructor Create(Color: TColor32 = $00808080); reintroduce; virtual;

    property Color: TColor32 read FColor write FColor;
  end;

  { TBitmapPolygonFiller }
  TBitmapPolygonFiller = class(TCustomPolygonFiller)
  private
    FPattern: TCustomBitmap32;
    FOffsetY: Integer;
    FOffsetX: Integer;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineOpaque(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineBlend(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineBlendMasterAlpha(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineCustomCombine(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
  public
    property Pattern: TCustomBitmap32 read FPattern write FPattern;
    property OffsetX: Integer read FOffsetX write FOffsetX;
    property OffsetY: Integer read FOffsetY write FOffsetY;
  end;

  { TSamplerFiller }
  TSamplerFiller = class(TCustomPolygonFiller)
  private
    FSampler: TCustomSampler;
    FGetSample: TGetSampleInt;
    procedure SetSampler(const Value: TCustomSampler);
  protected
    procedure SamplerChanged; virtual;
    function GetFillLine: TFillLineEvent; override;
    procedure SampleLineOpaque(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
  public
    constructor Create(Sampler: TCustomSampler = nil); reintroduce; virtual;
    procedure BeginRendering; override;
    procedure EndRendering; override;
    property Sampler: TCustomSampler read FSampler write SetSampler;
  end;

procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_LCD(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS_LCD(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;

procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_LCD(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS_LCD(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;


procedure PolyPolylineFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;
procedure PolyPolylineFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;

procedure PolylineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;
procedure PolylineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;

//Filled only Dashes ...
procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Color: TColor32;
  Closed: Boolean = False; Width: TFloat = 1.0); overload;
procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; FillColor, StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0); overload;
//Filled and stroked Dashes ...
procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller;
  Closed: Boolean = False; Width: TFloat = 1.0); overload;
procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller; StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0); overload;

procedure PolyPolygonXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonXS_LCD(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonXS_LCD(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil);
procedure PolyPolygonXS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonXS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil);

procedure PolyPolylineXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; StrokeWidth: TFixed = $10000;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;
procedure PolyPolylineXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFixed = $10000;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;

procedure PolylineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; StrokeWidth: TFixed = $10000;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;
procedure PolylineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFixed = $10000;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;

//Filled only Dashes ...
procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Color: TColor32;
  Closed: Boolean = False; Width: TFixed = $10000); overload;
procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; FillColor, StrokeColor: TColor32;
  Closed: Boolean; Width: TFixed; StrokeWidth: TFixed = $20000); overload;
//Filled and stroked Dashes ...
procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller;
  Closed: Boolean = False; Width: TFixed = $10000); overload;
procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller; StrokeColor: TColor32;
  Closed: Boolean; Width: TFixed; StrokeWidth: TFixed = $20000); overload;

// fill entire bitmap with a given polygon filler
procedure FillBitmap(Bitmap: TBitmap32; Filler: TCustomPolygonFiller);

{ Registration routines }
procedure RegisterPolygonRenderer(PolygonRendererClass: TCustomPolygonRendererClass);

//=== ct9999 =====================================
 
{ Polylines }

procedure PolylineTS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; Transformation: TTransformation = nil);
procedure PolylineAS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; Transformation: TTransformation = nil);
procedure PolylineXS2(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; Transformation: TTransformation = nil);
procedure PolylineXSP(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Closed: Boolean = False; Transformation: TTransformation = nil);

procedure PolyPolylineTS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; Transformation: TTransformation = nil);
procedure PolyPolylineAS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; Transformation: TTransformation = nil);
procedure PolyPolylineXS2(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; Transformation: TTransformation = nil);
procedure PolyPolylineXSP(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Closed: Boolean = False; Transformation: TTransformation = nil);

{ Polygons }

type
  TAntialiasMode = (am32times, am16times, am8times, am4times, am2times, amNone);

const
  DefaultAAMode = am8times; // Use 54 levels of transparency for antialiasing.

procedure PolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Mode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Mode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;

procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Mode: TPolyFillMode = pfAlternate;
  const AAMode: TAntialiasMode = DefaultAAMode; Transformation: TTransformation = nil); overload;
procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode = pfAlternate;
  const AAMode: TAntialiasMode = DefaultAAMode; Transformation: TTransformation = nil); overload;
procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Mode: TPolyFillMode = pfAlternate;
  const AAMode: TAntialiasMode = DefaultAAMode; Transformation: TTransformation = nil); overload;

procedure PolyPolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Mode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolyPolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolyPolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Mode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;

procedure PolyPolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode = pfAlternate;
  const AAMode: TAntialiasMode = DefaultAAMode; Transformation: TTransformation = nil); overload;
procedure PolyPolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Mode: TPolyFillMode = pfAlternate;
  const AAMode: TAntialiasMode = DefaultAAMode; Transformation: TTransformation = nil); overload;
procedure PolyPolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Mode: TPolyFillMode = pfAlternate;
  const AAMode: TAntialiasMode = DefaultAAMode; Transformation: TTransformation = nil); overload;

function PolygonBounds(const Points: TArrayOfFixedPoint; Transformation: TTransformation = nil): TFixedRect;
function PolyPolygonBounds(const Points: TArrayOfArrayOfFixedPoint; Transformation: TTransformation = nil): TFixedRect;

function PtInPolygon(const Pt: TFixedPoint; const Points: TArrayOfFixedPoint): Boolean;

{ TPolygon32 }
{ TODO : Bezier Curves, and QSpline curves for TrueType font rendering }
{ TODO : Check if QSpline is compatible with Type1 fonts }
type
  TPolygon32 = class(TThreadPersistent)
  private
    FAntialiased: Boolean;
    FClosed: Boolean;
    FFillMode: TPolyFillMode;
    FNormals: TArrayOfArrayOfFixedPoint;
    FPoints: TArrayOfArrayOfFixedPoint;
    FAntialiasMode: TAntialiasMode;
  protected
    procedure BuildNormals;
    procedure CopyPropertiesTo(Dst: TPolygon32); virtual;
    procedure AssignTo(Dst: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Add(const P: TFixedPoint);
    procedure AddPoints(var First: TFixedPoint; Count: Integer);
    function  ContainsPoint(const P: TFixedPoint): Boolean;
    procedure Clear;
    function  Grow(const Delta: TFixed; EdgeSharpness: Single = 0): TPolygon32;

    procedure Draw(Bitmap: TCustomBitmap32; OutlineColor, FillColor: TColor32; Transformation: TTransformation = nil); overload;
    procedure Draw(Bitmap: TCustomBitmap32; OutlineColor: TColor32; FillCallback: TFillLineEvent; Transformation: TTransformation = nil); overload;
    procedure Draw(Bitmap: TCustomBitmap32; OutlineColor: TColor32; Filler: TCustomPolygonFiller; Transformation: TTransformation = nil); overload;

    procedure DrawEdge(Bitmap: TCustomBitmap32; Color: TColor32; Transformation: TTransformation = nil);

    procedure DrawFill(Bitmap: TCustomBitmap32; Color: TColor32; Transformation: TTransformation = nil); overload;
    procedure DrawFill(Bitmap: TCustomBitmap32; FillCallback: TFillLineEvent; Transformation: TTransformation = nil); overload;
    procedure DrawFill(Bitmap: TCustomBitmap32; Filler: TCustomPolygonFiller; Transformation: TTransformation = nil); overload;

    procedure NewLine;
    procedure Offset(const Dx, Dy: TFixed);
    function  Outline: TPolygon32;
    procedure Transform(Transformation: TTransformation);
    function GetBoundingRect: TFixedRect;

    property Antialiased: Boolean read FAntialiased write FAntialiased;
    property AntialiasMode: TAntialiasMode read FAntialiasMode write FAntialiasMode;
    property Closed: Boolean read FClosed write FClosed;
    property FillMode: TPolyFillMode read FFillMode write FFillMode;

    property Normals: TArrayOfArrayOfFixedPoint read FNormals write FNormals;
    property Points: TArrayOfArrayOfFixedPoint read FPoints write FPoints;
  end;


//===============================================


var
  PolygonRendererList: TClassList;
  DefaultPolygonRendererClass: TPolygonRenderer32Class = TPolygonRenderer32VPR;


implementation

uses
  GR32_VectorUtils;

type
  TBitmap32Access = class(TBitmap32);

procedure RegisterPolygonRenderer(PolygonRendererClass: TCustomPolygonRendererClass);
begin
  if not Assigned(PolygonRendererList) then PolygonRendererList := TClassList.Create;
  PolygonRendererList.Add(PolygonRendererClass);
end;

// routines for color filling:

procedure MakeAlphaNonZeroUP(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  M, V: Cardinal;
  Last: TFloat;
  C: TColor32Entry absolute Color;
begin
  M := C.A * $101;
  Last := Infinity;
  for I := 0 to Count - 1 do
  begin
    if PInteger(@Last)^ <> PInteger(@Coverage[I])^ then
    begin
      Last := Coverage[I];
      V := Abs(Round(Last * $10000));
      if V > $10000 then V := $10000;
      V := V * M shr 24;
{$IFDEF USEGR32GAMMA}
      V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
      C.A := V;
    end;
    AlphaValues[I] := Color;
  end;
end;

(*
procedure MakeAlphaNonZeroUP(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  M, V, C: Cardinal;
begin
  M := Color shr 24 * $101;
  C := Color and $00ffffff;
  for I := 0 to Count - 1 do
  begin
    V := Abs(Round(Coverage[I] * $10000));
    if V > $10000 then V := $10000;
{$IFDEF USEGR32GAMMA}
    V := GAMMA_ENCODING_TABLE[V * M shr 24];
    AlphaValues[I] := (V shl 24) or C;
{$ELSE}
    AlphaValues[I] := (V * M and $ff000000) or C;
{$ENDIF}
  end;
end;
*)

procedure MakeAlphaEvenOddUP(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  M, V: Cardinal;
  Last: TFloat;
  C: TColor32Entry absolute Color;
begin
  M := C.A * $101;
  Last := Infinity;
  for I := 0 to Count - 1 do
  begin
    if PInteger(@Last)^ <> PInteger(@Coverage[I])^ then
    begin
      Last := Coverage[I];
      V := Abs(Round(Coverage[I] * $10000));
      V := V and $01ffff;
      if V >= $10000 then
        V := V xor $1ffff;
      V := V * M shr 24;
{$IFDEF USEGR32GAMMA}
      V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
      C.A := V;
    end;
    AlphaValues[I] := Color;
  end;
end;

procedure MakeAlphaNonZeroP(Value: Single; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  M, V: Cardinal;
  C: TColor32Entry absolute Color;
begin
  M := C.A * $101;
  V := Abs(Round(Value * $10000));
  if V > $10000 then V := $10000;
  V := V * M shr 24;
{$IFDEF USEGR32GAMMA}
  V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
  C.A := V;
  FillLongWord(AlphaValues[0], Count, Color);
end;

procedure MakeAlphaEvenOddP(Value: Single; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  M, V: Cardinal;
  C: TColor32Entry absolute Color;
begin
  M := C.A * $101;
  V := Abs(Round(Value * $10000));
  V := V and $01ffff;
  if V > $10000 then V := V xor $1ffff;
  V := V * M shr 24;
{$IFDEF USEGR32GAMMA}
  V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
  C.A := V;
  FillLongWord(AlphaValues[0], Count, Color);
end;


// polygon filler routines (extract alpha only):

procedure MakeAlphaNonZeroUPF(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  V: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    V := Clamp(Round(Abs(Coverage[I]) * 256));
{$IFDEF USEGR32GAMMA}
    V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
    AlphaValues[I] := V;
  end;
end;

procedure MakeAlphaEvenOddUPF(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  V: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    V := Round(Abs(Coverage[I]) * 256);
    V := V and $000001ff;
    if V >= $100 then V := V xor $1ff;
{$IFDEF USEGR32GAMMA}
    V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
    AlphaValues[I] := V;
  end;
end;

procedure MakeAlphaNonZeroPF(Value: Single; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  V: Integer;
begin
  V := Clamp(Round(Abs(Value) * 256));
{$IFDEF USEGR32GAMMA}
    V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
  FillLongWord(AlphaValues[0], Count, V);
end;

procedure MakeAlphaEvenOddPF(Value: Single; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  V: Integer;
begin
  V := Round(Abs(Value) * 256);
  V := V and $000001ff;
  if V >= $100 then V := V xor $1ff;
{$IFDEF USEGR32GAMMA}
    V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
  FillLongWord(AlphaValues[0], Count, V);
end;

procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    Renderer.PolyPolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    Renderer.PolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  if not Assigned(Filler) then Exit;
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    Renderer.PolyPolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  if not Assigned(Filler) then Exit;
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    Renderer.PolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonFS_LCD(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD;
begin
  Renderer := TPolygonRenderer32LCD.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonFS_LCD(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD;
begin
  Renderer := TPolygonRenderer32LCD.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolyPolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonFS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD2;
begin
  Renderer := TPolygonRenderer32LCD2.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonFS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD2;
begin
  Renderer := TPolygonRenderer32LCD2.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolyPolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
  IntersectedClipRect: TRect;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolyPolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
  IntersectedClipRect: TRect;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
  IntersectedClipRect: TRect;
begin
  if not Assigned(Filler) then Exit;
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolyPolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
  IntersectedClipRect: TRect;
begin
  if not Assigned(Filler) then Exit;
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonFS_LCD(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD;
  IntersectedClipRect: TRect;
begin
  Renderer := TPolygonRenderer32LCD.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonFS_LCD(Bitmap: TBitmap32;
  const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Color: TColor32;
  FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD;
  IntersectedClipRect: TRect;
begin
  Renderer := TPolygonRenderer32LCD.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolyPolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonFS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD2;
  IntersectedClipRect: TRect;
begin
  Renderer := TPolygonRenderer32LCD2.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonFS_LCD2(Bitmap: TBitmap32;
  const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Color: TColor32;
  FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD2;
  IntersectedClipRect: TRect;
begin
  Renderer := TPolygonRenderer32LCD2.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolyPolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolylineFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean; StrokeWidth: TFloat;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle;
  MiterLimit: TFloat; Transformation: TTransformation);
var
  Dst: TArrayOfArrayOfFloatPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle, MiterLimit);
  PolyPolygonFS(Bitmap, Dst, Color, pfWinding, Transformation);
end;

procedure PolyPolylineFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil);
var
  Dst: TArrayOfArrayOfFloatPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle, MiterLimit);
  PolyPolygonFS(Bitmap, Dst, Filler, pfWinding, Transformation);
end;

procedure PolylineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean; StrokeWidth: TFloat;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle;
  MiterLimit: TFloat; Transformation: TTransformation);
begin
  PolyPolylineFS(Bitmap, PolyPolygon(Points), Color, Closed, StrokeWidth,
    JoinStyle, EndStyle, MiterLimit, Transformation);
end;

procedure PolylineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil);
begin
  PolyPolylineFS(Bitmap, PolyPolygon(Points), Filler, Closed, StrokeWidth,
    JoinStyle, EndStyle, MiterLimit, Transformation);
end;

procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Color: TColor32;
  Closed: Boolean = False; Width: TFloat = 1.0);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineFS(Bitmap, MultiPoly, Color, False, Width);
end;

procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; FillColor, StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  MultiPoly := BuildPolyPolyLine(MultiPoly, False, Width);
  PolyPolygonFS(Bitmap, MultiPoly, FillColor);
  PolyPolylineFS(Bitmap, MultiPoly, StrokeColor, True, StrokeWidth);
end;

procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller;
  Closed: Boolean = False; Width: TFloat = 1.0);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineFS(Bitmap, MultiPoly, Filler, False, Width);
end;

procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller; StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  MultiPoly := BuildPolyPolyLine(MultiPoly, False, Width);
  PolyPolygonFS(Bitmap, MultiPoly, Filler);
  PolyPolylineFS(Bitmap, MultiPoly, StrokeColor, True, StrokeWidth);
end;


procedure PolyPolygonXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    Renderer.PolyPolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    Renderer.PolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    Renderer.PolyPolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    Renderer.PolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonXS_LCD(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD;
begin
  Renderer := TPolygonRenderer32LCD.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonXS_LCD(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD;
begin
  Renderer := TPolygonRenderer32LCD.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolyPolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonXS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD2;
begin
  Renderer := TPolygonRenderer32LCD2.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonXS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD2;
begin
  Renderer := TPolygonRenderer32LCD2.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolyPolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolylineXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean; StrokeWidth: TFixed;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle;
  MiterLimit: TFixed; Transformation: TTransformation);
var
  Dst: TArrayOfArrayOfFixedPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle,
    MiterLimit);
  PolyPolygonXS(Bitmap, Dst, Color, pfWinding, Transformation);
end;

procedure PolyPolylineXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False;
  StrokeWidth: TFixed = $10000; JoinStyle: TJoinStyle = jsMiter;
  EndStyle: TEndStyle = esButt; MiterLimit: TFixed = $40000;
  Transformation: TTransformation = nil);
var
  Dst: TArrayOfArrayOfFixedPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle,
    MiterLimit);
  PolyPolygonXS(Bitmap, Dst, Filler, pfWinding, Transformation);
end;

procedure PolylineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean; StrokeWidth: TFixed;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle;
  MiterLimit: TFixed; Transformation: TTransformation);
begin
  PolyPolylineXS(Bitmap, PolyPolygon(Points), Color,
    Closed, StrokeWidth, JoinStyle, EndStyle,
    MiterLimit, Transformation);
end;

procedure PolylineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False;
  StrokeWidth: TFixed = $10000; JoinStyle: TJoinStyle = jsMiter;
  EndStyle: TEndStyle = esButt; MiterLimit: TFixed = $40000;
  Transformation: TTransformation = nil);
begin
  PolyPolylineXS(Bitmap, PolyPolygon(Points), Filler, Closed, StrokeWidth,
    JoinStyle, EndStyle, MiterLimit, Transformation);
end;

procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Color: TColor32;
  Closed: Boolean = False; Width: TFixed = $10000);
var
  MultiPoly: TArrayOfArrayOfFixedPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineXS(Bitmap, MultiPoly, Color, False, Width);
end;

procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; FillColor, StrokeColor: TColor32;
  Closed: Boolean; Width: TFixed; StrokeWidth: TFixed = $20000);
var
  MultiPoly: TArrayOfArrayOfFixedPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineXS(Bitmap, MultiPoly, FillColor, False, Width);
  MultiPoly := BuildPolyPolyLine(MultiPoly, False, Width);
  PolyPolylineXS(Bitmap, MultiPoly, StrokeColor, True, strokeWidth);
end;

procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller;
  Closed: Boolean = False; Width: TFixed = $10000);
var
  MultiPoly: TArrayOfArrayOfFixedPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineXS(Bitmap, MultiPoly, Filler, False, Width);
end;

procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller; StrokeColor: TColor32;
  Closed: Boolean; Width: TFixed; StrokeWidth: TFixed = $20000);
var
  MultiPoly: TArrayOfArrayOfFixedPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineXS(Bitmap, MultiPoly, Filler, False, Width);
  MultiPoly := BuildPolyPolyLine(MultiPoly, False, Width);
  PolyPolylineXS(Bitmap, MultiPoly, StrokeColor, True, StrokeWidth);
end;

procedure FillBitmap(Bitmap: TBitmap32; Filler: TCustomPolygonFiller);
var
  AlphaValues: PColor32;
  Y: Integer;
begin
  {$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc(Bitmap.Width * SizeOf(TColor32));
  {$ELSE}
  GetMem(AlphaValues, Bitmap.Width * SizeOf(TColor32));
  {$ENDIF}
  FillLongword(AlphaValues^, Bitmap.Width, $FF);
  Filler.BeginRendering;
  for Y := 0 to Bitmap.Height - 1 do
    Filler.FillLine(PColor32(Bitmap.ScanLine[y]), 0, y, Bitmap.Width,
      AlphaValues, Bitmap.CombineMode);
  Filler.EndRendering;
  {$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
  {$ELSE}
  FreeMem(AlphaValues);
  {$ENDIF}
end;


{ LCD sub-pixel rendering (see http://www.grc.com/cttech.htm) }

type
  PRGBTriple = ^TRGBTriple;
  TRGBTriple = packed record
    B, G, R: Byte;
  end;

  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [0..0] of TRGBTriple;

  TMakeAlphaProcLCD = procedure(Coverage: PSingleArray; AlphaValues: SysUtils.PByteArray;
    Count: Integer; Color: TColor32);

procedure MakeAlphaNonZeroLCD(Coverage: PSingleArray; AlphaValues: SysUtils.PByteArray;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  M, V: Cardinal;
  Last: TFloat;
  C: TColor32Entry absolute Color;
begin
  M := C.A * 86;  // 86 = 258 / 3

  Last := Infinity;
  V := 0;
  AlphaValues[0] := 0;
  AlphaValues[1] := 0;
  for I := 0 to Count - 1 do
  begin
    if PInteger(@Last)^ <> PInteger(@Coverage[I])^ then
    begin
      Last := Coverage[I];
      V := Abs(Round(Last * $10000));
      if V > $10000 then V := $10000;
      V := V * M shr 24;
    end;
    Inc(AlphaValues[I], V);
{$IFDEF USEGR32GAMMA}
    AlphaValues[I] := GAMMA_ENCODING_TABLE[AlphaValues[I]];
{$ENDIF}
    Inc(AlphaValues[I + 1], V);
    AlphaValues[I + 2] := V;
  end;
  AlphaValues[Count + 2] := 0;
  AlphaValues[Count + 3] := 0;
end;

procedure MakeAlphaEvenOddLCD(Coverage: PSingleArray; AlphaValues: SysUtils.PByteArray;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  M, V: Cardinal;
  Last: TFloat;
begin
  M := Color shr 24 * 86;  // 86 = 258 / 3

  Last := Infinity;
  V := 0;
  AlphaValues[0] := 0;
  AlphaValues[1] := 0;
  for I := 0 to Count - 1 do
  begin
    if PInteger(@Last)^ <> PInteger(@Coverage[I])^ then
    begin
      Last := Coverage[I];
      V := Abs(Round(Coverage[I] * $10000));
      V := V and $01ffff;
      if V >= $10000 then V := V xor $1ffff;
      V := V * M shr 24;
    end;
    Inc(AlphaValues[I], V);
{$IFDEF USEGR32GAMMA}
    AlphaValues[I] := GAMMA_ENCODING_TABLE[AlphaValues[I]];
{$ENDIF}
    Inc(AlphaValues[I + 1], V);
    AlphaValues[I + 2] := V;
  end;
  AlphaValues[Count + 2] := 0;
  AlphaValues[Count + 3] := 0;
end;

procedure MakeAlphaNonZeroLCD2(Coverage: PSingleArray; AlphaValues: SysUtils.PByteArray;
  Count: Integer; Color: TColor32);
var
  I: Integer;
begin
  MakeAlphaNonZeroLCD(Coverage, AlphaValues, Count, Color);
  AlphaValues[Count + 2] := (AlphaValues[Count] + AlphaValues[Count + 1]) div 3;
  AlphaValues[Count + 3] := AlphaValues[Count + 1] div 3;
  for I := Count + 1 downto 2 do
  begin
    AlphaValues[I] := (AlphaValues[I] + AlphaValues[I - 1] + AlphaValues[I - 2]) div 3;
  end;
  AlphaValues[1] := (AlphaValues[0] + AlphaValues[1]) div 3;
  AlphaValues[0] := AlphaValues[0] div 3;
end;

procedure MakeAlphaEvenOddLCD2(Coverage: PSingleArray; AlphaValues: SysUtils.PByteArray;
  Count: Integer; Color: TColor32);
var
  I: Integer;
begin
  MakeAlphaEvenOddLCD(Coverage, AlphaValues, Count, Color);
  AlphaValues[Count + 2] := (AlphaValues[Count] + AlphaValues[Count + 1]) div 3;
  AlphaValues[Count + 3] := AlphaValues[Count + 1] div 3;
  for I := Count + 1 downto 2 do
  begin
    AlphaValues[I] := (AlphaValues[I] + AlphaValues[I - 1] + AlphaValues[I - 2]) div 3;
  end;
  AlphaValues[1] := (AlphaValues[0] + AlphaValues[1]) div 3;
  AlphaValues[0] := AlphaValues[0] div 3;
end;

procedure CombineLineLCD(Weights: PRGBTripleArray; Dst: PColor32Array; Color: TColor32; Count: Integer);
var
  I: Integer;
  {$IFDEF TEST_BLENDMEMRGB128SSE4}
  Weights64: UInt64;
  {$ENDIF}
begin
  I := 0;
  while Count <> 0 do
    {$IFDEF TEST_BLENDMEMRGB128SSE4}
    if (Count shr 1) = 0 then
    {$ENDIF}
    begin
      if PColor32(@Weights[I])^ = $FFFFFFFF then
        Dst[I] := Color
      else
        BlendMemRGB(Color, Dst[I], PColor32(@Weights[I])^);
      Dec(Count);
      Inc(I);
    end
    {$IFDEF TEST_BLENDMEMRGB128SSE4}
    else
    begin
      Weights64 := (UInt64(PColor32(@Weights[I + 1])^) shl 32) or
        PColor32(@Weights[I])^;
      if Weights64 = $FFFFFFFFFFFFFFFF then
      begin
        Dst[I] := Color;
        Dst[I + 1] := Color;
      end
      else
        BlendMemRGB128(Color, Dst[I], Weights64);
      Dec(Count, 2);
      Inc(I, 2);
    end
    {$ENDIF};
  EMMS;
end;

{ TCustomPolygonFiller }

procedure TCustomPolygonFiller.BeginRendering;
begin
  // implemented by descendants
end;

procedure TCustomPolygonFiller.EndRendering;
begin
  // implemented by descendants
end;

{ TCallbackPolygonFiller }

function TCallbackPolygonFiller.GetFillLine: TFillLineEvent;
begin
  Result := FFillLineEvent;
end;


{ TInvertPolygonFiller }

procedure TInvertPolygonFiller.FillLineBlend(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: Integer;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(InvertColor(Dst^), Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

function TInvertPolygonFiller.GetFillLine: TFillLineEvent;
begin
  Result := FillLineBlend;
end;


{ TClearPolygonFiller }

constructor TClearPolygonFiller.Create(Color: TColor32 = $00808080);
begin
  inherited Create;
  FColor := Color;
end;

procedure TClearPolygonFiller.FillLineClear(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
begin
  FillLongword(Dst^, Length, FColor);
end;

function TClearPolygonFiller.GetFillLine: TFillLineEvent;
begin
  Result := FillLineClear;
end;


{ TBitmapPolygonFiller }

procedure TBitmapPolygonFiller.FillLineOpaque(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  PatternX, PatternY, X: Integer;
  OpaqueAlpha: TColor32;
  Src: PColor32;
  BlendMemEx: TBlendMemEx;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  if Assigned(AlphaValues) then
  begin
    OpaqueAlpha := TColor32($FF shl 24);
    BlendMemEx := BLEND_MEM_EX[FPattern.CombineMode]^;
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMemEx(Src^ and $00FFFFFF or OpaqueAlpha, Dst^, AlphaValues^);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
      Inc(AlphaValues);
    end
  end
  else
    for X := DstX to DstX + Length - 1 do
    begin
      Dst^ := Src^;
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
    end;
end;

procedure TBitmapPolygonFiller.FillLineBlend(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  PatternX, PatternY, X: Integer;
  Src: PColor32;
  BlendMemEx: TBlendMemEx;
  BlendMem: TBlendMem;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  if Assigned(AlphaValues) then
  begin
    BlendMemEx := BLEND_MEM_EX[FPattern.CombineMode]^;
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMemEx(Src^, Dst^, AlphaValues^);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
      Inc(AlphaValues);
    end
  end
  else
  begin
    BlendMem := BLEND_MEM[FPattern.CombineMode]^;
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMem(Src^, Dst^);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
    end;
  end;
end;

procedure TBitmapPolygonFiller.FillLineBlendMasterAlpha(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  PatternX, PatternY, X: Integer;
  Src: PColor32;
  BlendMemEx: TBlendMemEx;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  BlendMemEx := BLEND_MEM_EX[FPattern.CombineMode]^;

  if Assigned(AlphaValues) then
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMemEx(Src^, Dst^, Div255(AlphaValues^ * FPattern.MasterAlpha));
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
      Inc(AlphaValues);
    end
  else
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMemEx(Src^, Dst^, FPattern.MasterAlpha);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
    end;
end;

procedure TBitmapPolygonFiller.FillLineCustomCombine(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  PatternX, PatternY, X: Integer;
  Src: PColor32;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  if Assigned(AlphaValues) then
    for X := DstX to DstX + Length - 1 do
    begin
      FPattern.OnPixelCombine(Src^, Dst^, Div255(AlphaValues^ * FPattern.MasterAlpha));
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
      Inc(AlphaValues);
    end
  else
    for X := DstX to DstX + Length - 1 do
    begin
      FPattern.OnPixelCombine(Src^, Dst^, FPattern.MasterAlpha);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
    end;
end;

function TBitmapPolygonFiller.GetFillLine: TFillLineEvent;
begin
  if not Assigned(FPattern) then
  begin
    Result := nil;
  end
  else if FPattern.DrawMode = dmOpaque then
    Result := FillLineOpaque
  else if FPattern.DrawMode = dmBlend then
  begin
    if FPattern.MasterAlpha = 255 then
      Result := FillLineBlend
    else
      Result := FillLineBlendMasterAlpha;
  end
  else if (FPattern.DrawMode = dmCustom) and Assigned(FPattern.OnPixelCombine) then
  begin
    Result := FillLineCustomCombine;
  end
  else
    Result := nil;
end;

{ TSamplerFiller }

constructor TSamplerFiller.Create(Sampler: TCustomSampler = nil);
begin
  inherited Create;
  FSampler := Sampler;
  SamplerChanged;
end;

procedure TSamplerFiller.EndRendering;
begin
  if Assigned(FSampler) then
    FSampler.FinalizeSampling
  else
    raise Exception.Create(RCStrNoSamplerSpecified);
end;

procedure TSamplerFiller.SampleLineOpaque(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: Integer;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(FGetSample(X, DstY) and $00FFFFFF or $FF000000, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TSamplerFiller.SamplerChanged;
begin
  if Assigned(FSampler) then
    FGetSample := FSampler.GetSampleInt;
end;

procedure TSamplerFiller.BeginRendering;
begin
  if Assigned(FSampler) then
    FSampler.PrepareSampling
  else
    raise Exception.Create(RCStrNoSamplerSpecified);
end;

function TSamplerFiller.GetFillLine: TFillLineEvent;
begin
  Result := SampleLineOpaque;
end;

procedure TSamplerFiller.SetSampler(const Value: TCustomSampler);
begin
  if FSampler <> Value then
  begin
    FSampler := Value;
    SamplerChanged;
  end;
end;


{ TCustomPolygonRenderer }

procedure TCustomPolygonRenderer.PolygonFS(
  const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect;
  Transformation: TTransformation);
begin
  PolyPolygonFS(PolyPolygon(Points), ClipRect, Transformation);
end;

procedure TCustomPolygonRenderer.PolygonFS(
  const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect);
begin
  PolyPolygonFS(PolyPolygon(Points), ClipRect);
end;

procedure TCustomPolygonRenderer.PolyPolygonFS(
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
begin
  // implemented by descendants
end;

procedure TCustomPolygonRenderer.PolyPolygonFS(
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect;
  Transformation: TTransformation);
var
  APoints: TArrayOfArrayOfFloatPoint;
begin
  if Assigned(Transformation) then
    APoints := TransformPolyPolygon(Points, Transformation)
  else
    APoints := Points;
  PolyPolygonFS(APoints, ClipRect);
end;

{ TPolygonRenderer32 }

constructor TPolygonRenderer32.Create(Bitmap: TBitmap32;
  Fillmode: TPolyFillMode);
begin
  inherited Create;
  FBitmap := Bitmap;
  FFillMode := Fillmode;
end;

procedure TPolygonRenderer32.PolygonFS(const Points: TArrayOfFloatPoint);
begin
  PolyPolygonFS(PolyPolygon(Points), FloatRect(FBitmap.ClipRect));
end;

procedure TPolygonRenderer32.PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint);
begin
  PolyPolygonFS(Points, FloatRect(FBitmap.ClipRect));
end;

procedure TPolygonRenderer32.SetBitmap(const Value: TBitmap32);
begin
  if FBitmap <> Value then
  begin
    FBitmap := Value;
    Changed;
  end;
end;

procedure TPolygonRenderer32.SetColor(const Value: TColor32);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TPolygonRenderer32.SetFiller(const Value: TCustomPolygonFiller);
begin
  if FFiller <> Value then
  begin
    FFiller := Value;
    Changed;
  end;
end;

procedure TPolygonRenderer32.SetFillMode(const Value: TPolyFillMode);
begin
  if FFillMode <> Value then
  begin
    FFillMode := Value;
    Changed;
  end;
end;

{ TPolygonRenderer32VPR }

{$IFDEF USESTACKALLOC}
{$W+}
{$ENDIF}
procedure TPolygonRenderer32VPR.FillSpan(const Span: TValueSpan; DstY: Integer);
var
  AlphaValues: PColor32Array;
  Count: Integer;
begin
  Count := Span.X2 - Span.X1 + 1;
  {$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc(Count * SizeOf(TColor32));
  {$ELSE}
  GetMem(AlphaValues, Count * SizeOf(TColor32));
  {$ENDIF}
  FFillProc(Span.Values, AlphaValues, Count, FColor);
  FFiller.FillLine(@Bitmap.ScanLine[DstY][Span.X1], Span.X1, DstY, Count,
    PColor32(AlphaValues), Bitmap.CombineMode);
  EMMS;
  {$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
  {$ELSE}
  FreeMem(AlphaValues);
  {$ENDIF}
end;
{$IFDEF USESTACKALLOC}
{$W-}
{$ENDIF}

function TPolygonRenderer32VPR.GetRenderSpan: TRenderSpanEvent;
begin
  if Assigned(FFiller) then
    Result := FillSpan
  else
    Result := RenderSpan;
end;

procedure TPolygonRenderer32VPR.PolyPolygonFS(
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
{$IFDEF CHANGENOTIFICATIONS}
var
  I: Integer;
{$ENDIF}
begin
  UpdateFillProcs;
  if Assigned(FFiller) then
  begin
    FFiller.BeginRendering;
    RenderPolyPolygon(Points, ClipRect, GetRenderSpan());
    FFiller.EndRendering;
  end
  else
    RenderPolyPolygon(Points, ClipRect, GetRenderSpan());

{$IFDEF CHANGENOTIFICATIONS}
  if TBitmap32Access(Bitmap).UpdateCount = 0 then
    for I := 0 to High(Points) do
      if Length(Points[I]) > 0 then
        Bitmap.Changed(MakeRect(PolygonBounds(Points[I])));
{$ENDIF}
end;

{$W+}
procedure TPolygonRenderer32VPR.RenderSpan(const Span: TValueSpan;
  DstY: Integer);
var
  AlphaValues: PColor32Array;
  Count: Integer;
begin
  Count := Span.X2 - Span.X1 + 1;
  {$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc(Count * SizeOf(TColor32));
  {$ELSE}
  GetMem(AlphaValues, Count * SizeOf(TColor32));
  {$ENDIF}
  FFillProc(Span.Values, AlphaValues, Count, FColor);
  if Bitmap.CombineMode = cmMerge then
    MergeLine(@AlphaValues[0], @Bitmap.ScanLine[DstY][Span.X1], Count)
  else
    BlendLine(@AlphaValues[0], @Bitmap.ScanLine[DstY][Span.X1], Count);
  EMMS;
  {$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
  {$ELSE}
  FreeMem(AlphaValues);
  {$ENDIF}
end;
{$W-}

procedure TPolygonRenderer32VPR.UpdateFillProcs;
const
  FillProcs: array [Boolean, TPolyFillMode] of TFillProc = (
    (MakeAlphaEvenOddUP, MakeAlphaNonZeroUP),
    (MakeAlphaEvenOddUPF, MakeAlphaNonZeroUPF)
  );
begin
  FFillProc := FillProcs[Assigned(FFiller), FillMode];
end;

{ TPolygonRenderer32LCD }

procedure TPolygonRenderer32LCD.PolyPolygonFS(
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
var
  R: TFloatRect;
  APoints: TArrayOfArrayOfFloatPoint;
{$IFDEF CHANGENOTIFICATIONS}
  I: Integer;
{$ENDIF}
begin
  APoints := ScalePolyPolygon(Points, 3, 1);
  R.Top := ClipRect.Top;
  R.Bottom := ClipRect.Bottom;
  R.Left := ClipRect.Left * 3;
  R.Right := ClipRect.Right * 3;
  RenderPolyPolygon(APoints, R, RenderSpan);
{$IFDEF CHANGENOTIFICATIONS}
  if TBitmap32Access(Bitmap).UpdateCount = 0 then
    for I := 0 to High(Points) do
      if length(Points[I]) > 0 then
        Bitmap.Changed(MakeRect(PolygonBounds(Points[I])));
{$ENDIF}
end;

{$W+}
procedure TPolygonRenderer32LCD.RenderSpan(const Span: TValueSpan;
  DstY: Integer);
const
  PADDING = 5;
var
  AlphaValues: SysUtils.PByteArray;
  Count: Integer;
  X1, Offset: Integer;
const
  MakeAlpha: array [TPolyFillMode] of TMakeAlphaProcLCD = (MakeAlphaEvenOddLCD, MakeAlphaNonZeroLCD);
begin
  Count := Span.X2 - Span.X1 + 1;
  X1 := DivMod(Span.X1, 3, Offset);

  // Left Padding + Right Padding + Filter Width = 2 + 2 + 2 = 6
  {$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc((Count + 6 + PADDING) * SizeOf(Byte));
  {$ELSE}
  GetMem(AlphaValues, (Count + 6 + PADDING) * SizeOf(Byte));
  {$ENDIF}
  AlphaValues[0] := 0;
  AlphaValues[1] := 0;
  if (X1 > 0) then
  begin
    Dec(X1);
    Inc(Offset, 3);
    AlphaValues[2] := 0;
    AlphaValues[3] := 0;
    AlphaValues[4] := 0;
  end;

  MakeAlpha[FFillMode](Span.Values, PByteArray(@AlphaValues[PADDING]), Count, FColor);
  CombineLineLCD(@AlphaValues[PADDING - Offset], PColor32Array(@Bitmap.ScanLine[DstY][X1]), FColor, (Count + Offset + 2) div 3);

  {$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
  {$ELSE}
  FreeMem(AlphaValues);
  {$ENDIF}
end;
{$W-}


{ TPolygonRenderer32LCD2 }

{$W+}
procedure TPolygonRenderer32LCD2.RenderSpan(const Span: TValueSpan;
  DstY: Integer);
const
  PADDING = 5;
var
  AlphaValues: SysUtils.PByteArray;
  Count: Integer;
  X1, Offset: Integer;
const
  MakeAlpha: array [TPolyFillMode] of TMakeAlphaProcLCD = (MakeAlphaEvenOddLCD2, MakeAlphaNonZeroLCD2);
begin
  Count := Span.X2 - Span.X1 + 1;
  X1 := DivMod(Span.X1, 3, Offset);

  // Left Padding + Right Padding + Filter Width = 2 + 2 + 2 = 6
  {$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc((Count + 6 + PADDING) * SizeOf(Byte));
  {$ELSE}
  GetMem(AlphaValues, (Count + 6 + PADDING) * SizeOf(Byte));
  {$ENDIF}
  AlphaValues[0] := 0;
  AlphaValues[1] := 0;
  if (X1 > 0) then
  begin
    Dec(X1);
    Inc(Offset, 3);
    AlphaValues[2] := 0;
    AlphaValues[3] := 0;
    AlphaValues[4] := 0;
  end;

  Dec(Offset, 1);
  MakeAlpha[FFillMode](Span.Values, PByteArray(@AlphaValues[PADDING]), Count, FColor);
  Inc(Count);
  CombineLineLCD(@AlphaValues[PADDING - Offset], PColor32Array(@Bitmap.ScanLine[DstY][X1]), FColor, (Count + Offset + 2) div 3);

  {$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
  {$ELSE}
  FreeMem(AlphaValues);
  {$ENDIF}
end;
{$W-}

//=== ct9999 ========================================

type
  TCustomBitmap32Access = class(TCustomBitmap32);
  TShiftFunc = function(Value: Integer): Integer;  // needed for antialiasing to speed things up
// These are for edge scan info. Note, that the most significant bit of the
// edge in a scan line is used for winding (edge direction) info.

  TEdgePoint = Integer;

  PEdgePoints = ^TEdgePoints;
  TEdgePoints = array [0..MaxListSize-1] of TEdgePoint;

  PScanLine = ^TScanLine;
  TScanLine = record
    Count: Integer;
    EdgePoints: PEdgePoints;
    EdgePointsLength: Integer;
  end;

  TScanLines = array of TScanLine;

const
  AA_LINES: Array[TAntialiasMode] of Integer = (32, 16, 8, 4, 2, 1);
  AA_SHIFT: Array[TAntialiasMode] of Integer = (5, 4, 3, 2, 1, 0);
  AA_MULTI: Array[TAntialiasMode] of Integer = (65, 273, 1167, 5460, 32662, 0);

{ POLYLINES }

procedure PolylineTS(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfFixedPoint;
  Color: TColor32;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I, Count: Integer;
  DoAlpha: Boolean;
begin
  Count := Length(Points);

  if (Count = 1) and Closed then
    if Assigned(Transformation) then
      with Transformation.Transform(Points[0]) do
        Bitmap.SetPixelTS(FixedRound(X), FixedRound(Y), Color)
    else
      with Points[0] do
        Bitmap.SetPixelTS(FixedRound(X), FixedRound(Y), Color);

  if Count < 2 then Exit;
  DoAlpha := Color and $FF000000 <> $FF000000;
  Bitmap.BeginUpdate;
  Bitmap.PenColor := Color;

  if Assigned(Transformation) then
  begin
    with Transformation.Transform(Points[0]) do Bitmap.MoveTo(FixedRound(X), FixedRound(Y));
    if DoAlpha then
      for I := 1 to Count - 1 do
        with Transformation.Transform(Points[I]) do
          Bitmap.LineToTS(FixedRound(X), FixedRound(Y))
    else
      for I := 1 to Count - 1 do
        with Transformation.Transform(Points[I]) do
          Bitmap.LineToS(FixedRound(X), FixedRound(Y));

    if Closed then with Transformation.Transform(Points[0]) do
      if DoAlpha then
        Bitmap.LineToTS(FixedRound(X), FixedRound(Y))
      else
        Bitmap.LineToS(FixedRound(X), FixedRound(Y));
  end
  else
  begin
    with Points[0] do Bitmap.MoveTo(FixedRound(X), FixedRound(Y));
    if DoAlpha then
      for I := 1 to Count - 1 do
        with Points[I] do
          Bitmap.LineToTS(FixedRound(X), FixedRound(Y))
    else
      for I := 1 to Count - 1 do
        with Points[I] do
          Bitmap.LineToS(FixedRound(X), FixedRound(Y));

    if Closed then with Points[0] do
      if DoAlpha then
        Bitmap.LineToTS(FixedRound(X), FixedRound(Y))
      else
        Bitmap.LineToS(FixedRound(X), FixedRound(Y));
  end;

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure PolylineAS(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfFixedPoint;
  Color: TColor32;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I, Count: Integer;
begin
  Count := Length(Points);
  if (Count = 1) and Closed then
    if Assigned(Transformation) then
      with Transformation.Transform(Points[0]) do
        Bitmap.SetPixelTS(FixedRound(X), FixedRound(Y), Color)
    else
      with Points[0] do
        Bitmap.SetPixelTS(FixedRound(X), FixedRound(Y), Color);

  if Count < 2 then Exit;
  Bitmap.BeginUpdate;
  Bitmap.PenColor := Color;

  if Assigned(Transformation) then
  begin
    with Transformation.Transform(Points[0]) do Bitmap.MoveTo(FixedRound(X), FixedRound(Y));
    for I := 1 to Count - 1 do
      with Transformation.Transform(Points[I]) do
        Bitmap.LineToAS(FixedRound(X), FixedRound(Y));
    if Closed then with Transformation.Transform(Points[0]) do Bitmap.LineToAS(FixedRound(X), FixedRound(Y));
  end
  else
  begin
    with Points[0] do Bitmap.MoveTo(FixedRound(X), FixedRound(Y));
    for I := 1 to Count - 1 do
      with Points[I] do
        Bitmap.LineToAS(FixedRound(X), FixedRound(Y));
    if Closed then with Points[0] do Bitmap.LineToAS(FixedRound(X), FixedRound(Y));
  end;

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure PolylineXS2(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfFixedPoint;
  Color: TColor32;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I, Count: Integer;
begin
  Count := Length(Points);
  if (Count = 1) and Closed then
    if Assigned(Transformation) then
      with Transformation.Transform(Points[0]) do Bitmap.PixelXS[X, Y] := Color
    else
      with Points[0] do Bitmap.PixelXS[X, Y] := Color;

  if Count < 2 then Exit;
  Bitmap.BeginUpdate;
  Bitmap.PenColor := Color;

  if Assigned(Transformation) then
  begin
    with Transformation.Transform(Points[0]) do Bitmap.MoveToX(X, Y);
    for I := 1 to Count - 1 do with Transformation.Transform(Points[I]) do Bitmap.LineToXS(X, Y);
    if Closed then with Transformation.Transform(Points[0]) do Bitmap.LineToXS(X, Y);
  end
  else
  begin
    with Points[0] do Bitmap.MoveToX(X, Y);
    for I := 1 to Count - 1 do with Points[I] do Bitmap.LineToXS(X, Y);
    if Closed then with Points[0] do Bitmap.LineToXS(X, Y);
  end;

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure PolylineXSP(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfFixedPoint;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I, Count: Integer;
begin
  Count := Length(Points);
  if Count < 2 then Exit;
  Bitmap.BeginUpdate;
  if Assigned(Transformation) then
  begin
    with Transformation.Transform(Points[0]) do Bitmap.MoveToX(X, Y);
    for I := 1 to Count - 1 do with Transformation.Transform(Points[I]) do Bitmap.LineToXSP(X, Y);
    if Closed then with Transformation.Transform(Points[0]) do Bitmap.LineToXSP(X, Y);
  end
  else
  begin
    with Points[0] do Bitmap.MoveToX(X, Y);
    for I := 1 to Count - 1 do with Points[I] do Bitmap.LineToXSP(X, Y);
    if Closed then with Points[0] do Bitmap.LineToXSP(X, Y);
  end;

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure PolyPolylineTS(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I: Integer;
begin
  for I := 0 to High(Points) do PolylineTS(Bitmap, Points[I], Color, Closed, Transformation);
end;

procedure PolyPolylineAS(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I: Integer;
begin
  for I := 0 to High(Points) do PolylineAS(Bitmap, Points[I], Color, Closed, Transformation);
end;

procedure PolyPolylineXS2(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I: Integer;
begin
  for I := 0 to High(Points) do PolylineXS2(Bitmap, Points[I], Color, Closed, Transformation);
end;

procedure PolyPolylineXSP(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I: Integer;
begin
  for I := 0 to High(Points) do PolylineXSP(Bitmap, Points[I], Closed, Transformation);
end;


{ General routines for drawing polygons }

procedure ScanLinesCreate(var ScanLines: TScanLines; Length: Integer);
begin
  SetLength(ScanLines, Length);
end;

procedure ScanLinesDestroy(var ScanLines: TScanLines);
var
  I: Integer;
begin
  for I := 0 to High(ScanLines) do
    FreeMem(ScanLines[I].EdgePoints);

  SetLength(ScanLines, 0);
end;


{ Routines for sorting edge points in scanlines }

const
  SortThreshold = 10;
  ReallocationThreshold = 64;

procedure InsertionSort(LPtr, RPtr: PInteger);
var
  IPtr, JPtr: PInteger;
  Temp: PInteger;
  P, C, T: Integer;
begin
  IPtr := LPtr;
  Inc(IPtr);
  repeat
    C := IPtr^;
    P := C and $7FFFFFFF;
    JPtr := IPtr;

{$IFDEF HAS_NATIVEINT}
    if NativeUInt(JPtr) > NativeUInt(LPtr) then
{$ELSE}
    if Cardinal(JPtr) > Cardinal(LPtr) then
{$ENDIF}
    repeat
      Temp := JPtr;
      Dec(Temp);
      T := Temp^;
      if T and $7FFFFFFF > P then
      begin
        JPtr^ := T;
        JPtr := Temp;
      end
      else
        Break;
{$IFDEF HAS_NATIVEINT}
    until NativeUInt(JPtr) <= NativeUInt(LPtr);
{$ELSE}
    until Cardinal(JPtr) <= Cardinal(LPtr);
{$ENDIF}

    JPtr^ := C;
    Inc(IPtr);
{$IFDEF HAS_NATIVEINT}
  until NativeUInt(IPtr) > NativeUInt(RPtr);
{$ELSE}
  until Cardinal(IPtr) > Cardinal(RPtr);
{$ENDIF}
end;

procedure QuickSort(LPtr, RPtr: PInteger);
var
{$IFDEF HAS_NATIVEINT}
  P: NativeUInt;
{$ELSE}
  P: Cardinal;
{$ENDIF}
  TempVal: Integer;
  IPtr, JPtr: PInteger;
  Temp: Integer;
const
  OddMask = SizeOf(Integer) and not(SizeOf(Integer) - 1);
begin
  {$IFDEF HAS_NATIVEINT}
  if NativeUInt(RPtr) - NativeUInt(LPtr) > SortThreshold shl 2 then
  {$ELSE}
  if Cardinal(RPtr) - Cardinal(LPtr) > SortThreshold shl 2 then
  {$ENDIF}
  repeat
    {$IFDEF HAS_NATIVEINT}
    P := NativeUInt(RPtr) - NativeUInt(LPtr);
    if (P and OddMask > 0) then Dec(P, SizeOf(Integer));
    TempVal := PInteger(NativeUInt(LPtr) + P shr 1)^ and $7FFFFFFF;
    {$ELSE}
    P := Cardinal(RPtr) - Cardinal(LPtr);
    if (P and OddMask > 0) then Dec(P, SizeOf(Integer));
    TempVal := PInteger(Cardinal(LPtr) + P shr 1)^ and $7FFFFFFF;
    {$ENDIF}

    IPtr := LPtr;
    JPtr := RPtr;
    repeat
      while (IPtr^ and $7FFFFFFF) < TempVal do Inc(IPtr);
      while (JPtr^ and $7FFFFFFF) > TempVal do Dec(JPtr);
      {$IFDEF HAS_NATIVEINT}
      if NativeUInt(IPtr) <= NativeUInt(JPtr) then
      {$ELSE}
      if Cardinal(IPtr) <= Cardinal(JPtr) then
      {$ENDIF}
      begin
        Temp := IPtr^;
        IPtr^ := JPtr^;
        JPtr^ := Temp;
//        Swap(IPtr^, JPtr^);
        Inc(IPtr);
        Dec(JPtr);
      end;
    {$IFDEF HAS_NATIVEINT}
    until NativeUInt(IPtr) > NativeUInt(JPtr);
    if NativeUInt(LPtr) < NativeUInt(JPtr) then
    {$ELSE}
    until Integer(IPtr) > Integer(JPtr);
    if Cardinal(LPtr) < Cardinal(JPtr) then
    {$ENDIF}
      QuickSort(LPtr, JPtr);
    LPtr := IPtr;
  {$IFDEF HAS_NATIVEINT}
  until NativeUInt(IPtr) >= NativeUInt(RPtr)
  {$ELSE}
  until Cardinal(IPtr) >= Cardinal(RPtr)
  {$ENDIF}
  else
    InsertionSort(LPtr, RPtr);
end;

procedure SortLine(const ALine: TScanLine);
var
  L, T: Integer;
begin
  L := ALine.Count;
  Assert(not Odd(L));
  if L = 2 then
  begin
    if (ALine.EdgePoints[0] and $7FFFFFFF) > (ALine.EdgePoints[1] and $7FFFFFFF) then
    begin
      T := ALine.EdgePoints[0];
      ALine.EdgePoints[0] := ALine.EdgePoints[1];
      ALine.EdgePoints[1] := T;
    end;
  end
  else if L > SortThreshold then
    QuickSort(@ALine.EdgePoints[0], @ALine.EdgePoints[L - 1])
  else if L > 2 then
    InsertionSort(@ALine.EdgePoints[0], @ALine.EdgePoints[L - 1]);
end;

procedure SortLines(const ScanLines: TScanLines);
var
  I: Integer;
begin
  for I := 0 to High(ScanLines) do SortLine(ScanLines[I]);
end;


{ Routines for rendering polygon edges to scanlines }

procedure AddEdgePoint(X: Integer; const Y: Integer; const ClipRect: TFixedRect; const ScanLines: TScanLines; const Direction: Integer);
var
  L: Integer;
  ScanLine: PScanLine;
begin
  if (Y < ClipRect.Top) or (Y > ClipRect.Bottom) then Exit;

  if X < ClipRect.Left then
    X := ClipRect.Left
  else if X > ClipRect.Right then
    X := ClipRect.Right;

  // positive direction (+1) is down
  if Direction < 0 then
    X := Integer(Longword(X) or $80000000); // set the highest bit if the winding is up

  ScanLine := @ScanLines[Y - ClipRect.Top];

  L := ScanLine.Count;
  Inc(ScanLine.Count);
  if ScanLine.Count > ScanLine.EdgePointsLength then
  begin
    ScanLine.EdgePointsLength := L + ReallocationThreshold;
    ReallocMem(ScanLine.EdgePoints, ScanLine.EdgePointsLength * SizeOf(TEdgePoint));
  end;
  ScanLine.EdgePoints[L] := X;
end;

function DrawEdge(const P1, P2: TFixedPoint; const ClipRect: TFixedRect; const ScanLines: TScanLines): Integer;
var
  X, Y: Integer;
  I, K: Integer;
  Dx, Dy, Sx, Sy: Integer;
  Delta: Integer;
begin
  // this function 'renders' a line into the edge point (ScanLines) buffer
  // and returns the line direction (1 - down, -1 - up, 0 - horizontal)
  Result := 0;
  if P2.Y = P1.Y then Exit;
  Dx := P2.X - P1.X;
  Dy := P2.Y - P1.Y;

  if Dy > 0 then Sy := 1
  else
  begin
    Sy := -1;
    Dy := -Dy;
  end;

  Result := Sy;

  if Dx > 0 then Sx := 1
  else
  begin
    Sx := -1;
    Dx := -Dx;
  end;

  Delta := (Dx mod Dy) shr 1;
  X := P1.X; Y := P1.Y;

  for I := 0 to Dy - 1 do
  begin
    AddEdgePoint(X, Y, ClipRect, ScanLines, Result);
    Inc(Y, Sy);
    Inc(Delta, Dx);

    // try it two times and if anything else left, use div and mod
    if Delta > Dy then
    begin
      Inc(X, Sx);
      Dec(Delta, Dy);

      if Delta > Dy then  // segment is tilted more than 45 degrees?
      begin
        Inc(X, Sx);
        Dec(Delta, Dy);

        if Delta > Dy then // are we still here?
        begin
          K := (Delta + Dy - 1) div Dy;
          Inc(X, Sx * K);
          Dec(Delta, Dy * K);
        end;
      end;
    end;
  end;
end;


procedure RoundShift1(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); forward; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure RoundShift2(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); forward; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure RoundShift4(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); forward; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure RoundShift8(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); forward; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure RoundShift16(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); forward; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure RoundShift32(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); forward; {$IFDEF USEINLINING} inline; {$ENDIF}

type
  TTransformProc = procedure(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
  TTransformationAccess = class(TTransformation);

procedure Transform1(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
begin
  TTransformationAccess(T).TransformFixed(SrcPoint.X, SrcPoint.Y, DstPoint.X, DstPoint.Y);
  RoundShift1(DstPoint, DstPoint, nil);
end;

procedure RoundShift1(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
{$IFDEF PUREPASCAL}
begin
  DstPoint.X := (SrcPoint.X + $7F) div 256;
  DstPoint.Y := (SrcPoint.Y + $7FFF) div 65536;
{$ELSE}
asm
{$IFDEF TARGET_x64}
    MOV EAX, [SrcPoint]
    ADD EAX, $0000007F
    SAR EAX, 8 // sub-sampled
    MOV [DstPoint], EAX
    MOV EDX, [SrcPoint + $4]
    ADD EDX, $00007FFF
    SAR EDX, 16
    MOV [DstPoint + $4], EDX
{$ENDIF}
{$IFDEF TARGET_x86}
    MOV ECX, [SrcPoint.X]
    ADD ECX, $0000007F
    SAR ECX, 8 // sub-sampled
    MOV [DstPoint.X], ECX
    MOV EDX, [SrcPoint.Y]
    ADD EDX, $00007FFF
    SAR EDX, 16
    MOV [DstPoint.Y], EDX
{$ENDIF}
{$ENDIF}
end;

procedure Transform2(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
begin
  TTransformationAccess(T).TransformFixed(SrcPoint.X, SrcPoint.Y, DstPoint.X, DstPoint.Y);
  RoundShift2(DstPoint, DstPoint, nil);
end;

procedure RoundShift2(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
{$IFDEF PUREPASCAL}
begin
  DstPoint.X := (SrcPoint.X + $3FFF) div 32768;
  DstPoint.Y := (SrcPoint.Y + $3FFF) div 32768;
{$ELSE}
asm
{$IFDEF TARGET_x64}
    MOV EAX, [SrcPoint]
    ADD EAX, $00003FFF
    SAR EAX, 15
    MOV [DstPoint], EAX
    MOV EDX, [SrcPoint + $4]
    ADD EDX, $00003FFF
    SAR EDX, 15
    MOV [DstPoint + $4], EDX
{$ENDIF}
{$IFDEF TARGET_x86}
    MOV ECX, [SrcPoint.X]
    ADD ECX, $00003FFF
    SAR ECX, 15
    MOV [DstPoint.X], ECX
    MOV EDX, [SrcPoint.Y]
    ADD EDX, $00003FFF
    SAR EDX, 15
    MOV [DstPoint.Y], EDX
{$ENDIF}
{$ENDIF}
end;

procedure Transform4(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
begin
  TTransformationAccess(T).TransformFixed(SrcPoint.X, SrcPoint.Y, DstPoint.X, DstPoint.Y);
  RoundShift4(DstPoint, DstPoint, nil);
end;

procedure RoundShift4(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
{$IFDEF PUREPASCAL}
begin
  DstPoint.X := (SrcPoint.X + $1FFF) div 16384;
  DstPoint.Y := (SrcPoint.Y + $1FFF) div 16384;
{$ELSE}
asm
{$IFDEF TARGET_x64}
    MOV EAX, [SrcPoint]
    ADD EAX, $00001FFF
    SAR EAX, 14
    MOV [DstPoint], EAX
    MOV EDX, [SrcPoint + $4]
    ADD EDX, $00001FFF
    SAR EDX, 14
    MOV [DstPoint + $4], EDX
{$ENDIF}
{$IFDEF TARGET_x86}
    MOV ECX, [SrcPoint.X]
    ADD ECX, $00001FFF
    SAR ECX, 14
    MOV [DstPoint.X], ECX
    MOV EDX, [SrcPoint.Y]
    ADD EDX, $00001FFF
    SAR EDX, 14
    MOV [DstPoint.Y], EDX
{$ENDIF}
{$ENDIF}
end;

procedure Transform8(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
begin
  TTransformationAccess(T).TransformFixed(SrcPoint.X, SrcPoint.Y, DstPoint.X, DstPoint.Y);
  RoundShift8(DstPoint, DstPoint, nil);
end;

procedure RoundShift8(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
{$IFDEF PUREPASCAL}
begin
  DstPoint.X := (SrcPoint.X + $FFF) div 8192;
  DstPoint.Y := (SrcPoint.Y + $FFF) div 8192;
{$ELSE}
asm
{$IFDEF TARGET_x64}
    MOV EAX, [SrcPoint]
    ADD EAX, $00000FFF
    SAR EAX, 13
    MOV [DstPoint], EAX
    MOV EDX, [SrcPoint + $4]
    ADD EDX, $00000FFF
    SAR EDX, 13
    MOV [DstPoint + $4], EDX
{$ENDIF}
{$IFDEF TARGET_x86}
    MOV ECX, [SrcPoint.X]
    ADD ECX, $00000FFF
    SAR ECX, 13
    MOV [DstPoint.X], ECX
    MOV EDX, [SrcPoint.Y]
    ADD EDX, $00000FFF
    SAR EDX, 13
    MOV [DstPoint.Y], EDX
{$ENDIF}
{$ENDIF}
end;

procedure Transform16(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
begin
  TTransformationAccess(T).TransformFixed(SrcPoint.X, SrcPoint.Y, DstPoint.X, DstPoint.Y);
  RoundShift16(DstPoint, DstPoint, nil);
end;

procedure RoundShift16(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
{$IFDEF PUREPASCAL}
begin
  DstPoint.X := (SrcPoint.X + $7FF) div 4096;
  DstPoint.Y := (SrcPoint.Y + $7FF) div 4096;
{$ELSE}
asm
{$IFDEF TARGET_x64}
    MOV EAX, [SrcPoint]
    ADD EAX, $000007FF
    SAR EAX, 12
    MOV [DstPoint], EAX
    MOV EDX, [SrcPoint + $4]
    ADD EDX, $000007FF
    SAR EDX, 12
    MOV [DstPoint + $4], EDX
{$ENDIF}
{$IFDEF TARGET_x86}
    MOV ECX, [SrcPoint.X]
    ADD ECX, $000007FF
    SAR ECX, 12
    MOV [DstPoint.X], ECX
    MOV EDX, [SrcPoint.Y]
    ADD EDX, $000007FF
    SAR EDX, 12
    MOV [DstPoint.Y], EDX
{$ENDIF}
{$ENDIF}
end;

procedure Transform32(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
begin
  TTransformationAccess(T).TransformFixed(SrcPoint.X, SrcPoint.Y, DstPoint.X, DstPoint.Y);
  RoundShift32(DstPoint, DstPoint, nil);
end;

procedure RoundShift32(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
{$IFDEF PUREPASCAL}
begin
  DstPoint.X := (SrcPoint.X + $3FF) div 2048;
  DstPoint.Y := (SrcPoint.Y + $3FF) div 2048;
{$ELSE}
asm
{$IFDEF TARGET_x64}
    MOV EAX, [SrcPoint]
    ADD EAX, $000003FF
    SAR EAX, 11
    MOV [DstPoint], EAX
    MOV EDX, [SrcPoint + $4]
    ADD EDX, $000003FF
    SAR EDX, 11
    MOV [DstPoint + $4], EDX
{$ENDIF}
{$IFDEF TARGET_x86}
    MOV ECX, [SrcPoint.X]
    ADD ECX, $000003FF
    SAR ECX, 11
    MOV [DstPoint.X], ECX
    MOV EDX, [SrcPoint.Y]
    ADD EDX, $000003FF
    SAR EDX, 11
    MOV [DstPoint.Y], EDX
{$ENDIF}
{$ENDIF}
end;

const
  RoundShiftProcs: array[TAntialiasMode] of TTransformProc = (RoundShift32, RoundShift16, RoundShift8, RoundShift4, RoundShift2, RoundShift1);
  TransformProcs:  array[TAntialiasMode] of TTransformProc = (Transform32, Transform16, Transform8, Transform4, Transform2, Transform1);

procedure AddPolygon(const Points: TArrayOfFixedPoint; const ClipRect: TFixedRect;
  var ScanLines: TScanLines; AAMode: TAntialiasMode; Transformation: TTransformation);
var
  P1, P2: TFixedPoint;
  I: Integer;
  PPtr: PFixedPoint;
  Transform: TTransformProc;
  Direction, PrevDirection: Integer; // up = 1 or down = -1
begin
  if Length(Points) < 3 then Exit;

  if Assigned(Transformation) then
    Transform := TransformProcs[AAMode]
  else
    Transform := RoundShiftProcs[AAMode];

  Transform(P1, Points[0], Transformation);

  // find the last Y different from Y1 and get direction
  PrevDirection := 0;
  I := High(Points);
  PPtr := @Points[I];

  while (I > 0) and (PrevDirection = 0) do
  begin
    Dec(I);
    Transform(P2, PPtr^, Transformation); { TODO : optimize minor inefficiency... }
    PrevDirection := P1.Y - P2.Y;
    Dec(PPtr);
  end;

  if PrevDirection > 0 then
    PrevDirection := 1
  else if PrevDirection < 0 then
    PrevDirection := -1
  else
    PrevDirection := 0;

  PPtr := @Points[1];
  for I := 1 to High(Points) do
  begin
    Transform(P2, PPtr^, Transformation);

    if P1.Y <> P2.Y then
    begin
      Direction := DrawEdge(P1, P2, ClipRect, ScanLines);
      if Direction <> PrevDirection then
      begin
        AddEdgePoint(P1.X, P1.Y, ClipRect, ScanLines, -Direction);
        PrevDirection := Direction;
      end;
    end;

    P1 := P2;
    Inc(PPtr);
  end;

  Transform(P2, Points[0], Transformation);

  if P1.Y <> P2.Y then
  begin
    Direction := DrawEdge(P1, P2, ClipRect, ScanLines);
    if Direction <> PrevDirection then AddEdgePoint(P1.X, P1.Y, ClipRect, ScanLines, -Direction);
  end;
end;


{ FillLines routines }
{ These routines rasterize the sorted edge points in the scanlines to
  the bitmap buffer }

procedure ColorFillLines(Bitmap: TCustomBitmap32; BaseY: Integer;
  const ScanLines: TScanLines; Color: TColor32; Mode: TPolyFillMode);
var
  I, J, L: Integer;
  Top, Left, Right, OldRight, LP, RP, Cx: Integer;
  Winding, NextWinding: Integer;
  HorzLine: procedure(X1, Y, X2: Integer; Value: TColor32) of Object;
begin
  if Color and $FF000000 <> $FF000000 then
    HorzLine := Bitmap.HorzLineT
  else
    HorzLine := Bitmap.HorzLine;

  Cx := Bitmap.ClipRect.Right - 1;
  Top := BaseY - 1;

  if Mode = pfAlternate then
    for J := 0 to High(ScanLines) do
    begin
      Inc(Top);
      L := ScanLines[J].Count; // assuming length is even
      if L = 0 then Continue;
      I := 0;
      OldRight := -1;

      while I < L do
      begin
        Left := ScanLines[J].EdgePoints[I] and $7FFFFFFF;
        Inc(I);
        Right := ScanLines[J].EdgePoints[I] and $7FFFFFFF - 1;
        if Right > Left then
        begin
          if (Left and $FF) < $80 then Left := Left shr 8
          else Left := Left shr 8 + 1;

          if (Right and $FF) < $80 then Right := Right shr 8
          else Right := Right shr 8 + 1;

          if Right >= Cx then Right := Cx;

          if Left <= OldRight then Left := OldRight + 1;
          OldRight := Right;
          if Right >= Left then HorzLine(Left, Top, Right, Color);
        end;
        Inc(I);
      end
    end
  else // Mode = pfWinding
    for J := 0 to High(ScanLines) do
    begin
      Inc(Top);
      L := ScanLines[J].Count; // assuming length is even
      if L = 0 then Continue;
      I := 0;

      Winding := 0;
      Left := ScanLines[J].EdgePoints[0];
      if (Left and $80000000) <> 0 then Inc(Winding) else Dec(Winding);
      Left := Left and $7FFFFFFF;
      Inc(I);

      while I < L do
      begin
        Right := ScanLines[J].EdgePoints[I];
        if (Right and $80000000) <> 0 then NextWinding := 1 else NextWinding := -1;
        Right := Right and $7FFFFFFF;
        Inc(I);

        if Winding <> 0 then
        begin
          if (Left and $FF) < $80 then LP := Left shr 8
          else LP := Left shr 8 + 1;
          if (Right and $FF) < $80 then RP := Right shr 8
          else RP := Right shr 8 + 1;

          if RP >= Cx then RP := Cx;

          if RP >= LP then HorzLine(LP, Top, RP, Color);
        end;

        Inc(Winding, NextWinding);
        Left := Right;
      end;
    end;
end;

procedure ColorFillLines2(Bitmap: TCustomBitmap32; BaseY: Integer;
  const ScanLines: TScanLines; Color: TColor32; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode = DefaultAAMode);
var
  I, J, L, N: Integer;
  MinY, MaxY, Y, Top, Bottom: Integer;
  MinX, MaxX, X, Dx: Integer;
  Left, Right: Integer;
  Buffer: array of Integer;
  ColorBuffer: array of TColor32;
  BufferSize: Integer;
  C, A: TColor32;
  ScanLine: PIntegerArray;
  Winding, NextWinding: Integer;
  AAShift, AALines, AAMultiplier: Integer;
  BlendLineEx: TBlendLineEx;
begin
  A := Color shr 24;

  AAShift := AA_SHIFT[AAMode];
  AALines := AA_LINES[AAMode] - 1; // we do the -1 here for optimization.
  AAMultiplier := AA_MULTI[AAMode];

  BlendLineEx := BLEND_LINE_EX[Bitmap.CombineMode]^;

  // find the range of Y screen coordinates
  MinY := BaseY shr AAShift;
  MaxY := (BaseY + Length(ScanLines) + AALines) shr AAShift;

  Y := MinY;
  while Y < MaxY do
  begin
    Top := Y shl AAShift - BaseY;
    Bottom := Top + AALines;
    if Top < 0 then Top := 0;
    if Bottom >= Length(ScanLines) then Bottom := High(ScanLines);

    // find left and right edges of the screen scanline
    MinX := $7F000000; MaxX := -$7F000000;
    for J := Top to Bottom do
    begin
      L := ScanLines[J].Count - 1;
      if L > 0 then
      begin
        Left := (ScanLines[J].EdgePoints[0] and $7FFFFFFF);
        Right := (ScanLines[J].EdgePoints[L] and $7FFFFFFF + AALines);
        if Left < MinX then MinX := Left;
        if Right > MaxX then MaxX := Right;
      end
    end;

    if MaxX >= MinX then
    begin
      MinX := MinX shr AAShift;
      MaxX := MaxX shr AAShift;
      // allocate buffer for a single scanline
      BufferSize := MaxX - MinX + 2;
      if Length(Buffer) < BufferSize then
      begin
        SetLength(Buffer, BufferSize + 64);
        SetLength(ColorBuffer, BufferSize + 64);
      end;
      FillLongword(Buffer[0], BufferSize, 0);

      // ...and fill it
      if Mode = pfAlternate then
        for J := Top to Bottom do
        begin
          I := 0;
          L := ScanLines[J].Count;
          ScanLine := @ScanLines[J].EdgePoints[0];
          while I < L do
          begin
            // Left edge
            X := ScanLine[I] and $7FFFFFFF;
            Dx := X and AALines;
            X := X shr AAShift - MinX;
            Inc(Buffer[X], Dx xor AALines);
            Inc(Buffer[X + 1], Dx);
            Inc(I);

            // Right edge
            X := ScanLine[I] and $7FFFFFFF;
            Dx := X and AALines;
            X := X shr AAShift - MinX;
            Dec(Buffer[X], Dx xor AALines);
            Dec(Buffer[X + 1], Dx);
            Inc(I);
          end
        end
      else // mode = pfWinding
        for J := Top to Bottom do
        begin
          I := 0;
          L := ScanLines[J].Count;
          ScanLine := @ScanLines[J].EdgePoints[0];
          Winding := 0;
          while I < L do
          begin
            X := ScanLine[I];
            Inc(I);
            if (X and $80000000) <> 0 then NextWinding := 1 else NextWinding := -1;
            X := X and $7FFFFFFF;
            if Winding = 0 then
            begin
              Dx := X and AALines;
              X := X shr AAShift - MinX;
              Inc(Buffer[X], Dx xor AALines);
              Inc(Buffer[X + 1], Dx);
            end;
            Inc(Winding, NextWinding);
            if Winding = 0 then
            begin
              Dx := X and AALines;
              X := X shr AAShift - MinX;
              Dec(Buffer[X], Dx xor AALines);
              Dec(Buffer[X + 1], Dx);
            end;
          end;
        end;

      // integrate the buffer
      N := 0;
      C := Color and $00FFFFFF;
      for I := 0 to BufferSize - 1 do
      begin
        Inc(N, Buffer[I]);
        ColorBuffer[I] := TColor32(N * AAMultiplier and $FF00) shl 16 or C;
      end;

      // draw it to the screen
      BlendLineEx(@ColorBuffer[0], Pointer(Bitmap.PixelPtr[MinX, Y]),
        Min(BufferSize, Bitmap.Width - MinX), A);
      EMMS;
    end;

    Inc(Y);
  end;
end;

procedure CustomFillLines(Bitmap: TCustomBitmap32; BaseY: Integer;
  const ScanLines: TScanLines; FillLineCallback: TFillLineEvent; Mode: TPolyFillMode);
var
  I, J, L: Integer;
  Top, Left, Right, OldRight, LP, RP, Cx: Integer;
  Winding, NextWinding: Integer;
begin
  Top := BaseY - 1;
  Cx := Bitmap.ClipRect.Right - 1;

  if Mode = pfAlternate then
    for J := 0 to High(ScanLines) do
    begin
      Inc(Top);
      L := ScanLines[J].Count; // assuming length is even
      if L = 0 then Continue;
      I := 0;
      OldRight := -1;

      while I < L do
      begin
        Left := ScanLines[J].EdgePoints[I] and $7FFFFFFF;
        Inc(I);
        Right := ScanLines[J].EdgePoints[I] and $7FFFFFFF - 1;
        if Right > Left then
        begin
          if (Left and $FF) < $80 then Left := Left shr 8
          else Left := Left shr 8 + 1;
          if (Right and $FF) < $80 then Right := Right shr 8
          else Right := Right shr 8 + 1;

          if Right >= Cx then Right := Cx;

          if Left <= OldRight then Left := OldRight + 1;
          OldRight := Right;
          if Right >= Left then
            FillLineCallback(Bitmap.PixelPtr[Left, Top], Left, Top, Right - Left, nil, Bitmap.CombineMode);
        end;
        Inc(I);
      end
    end
  else // Mode = pfWinding
    for J := 0 to High(ScanLines) do
    begin
      Inc(Top);
      L := ScanLines[J].Count; // assuming length is even
      if L = 0 then Continue;
      I := 0;

      Winding := 0;
      Left := ScanLines[J].EdgePoints[0];
      if (Left and $80000000) <> 0 then Inc(Winding) else Dec(Winding);
      Left := Left and $7FFFFFFF;
      Inc(I);
      while I < L do
      begin
        Right := ScanLines[J].EdgePoints[I];
        if (Right and $80000000) <> 0 then NextWinding := 1 else NextWinding := -1;
        Right := Right and $7FFFFFFF;
        Inc(I);

        if Winding <> 0 then
        begin
          if (Left and $FF) < $80 then LP := Left shr 8
          else LP := Left shr 8 + 1;
          if (Right and $FF) < $80 then RP := Right shr 8
          else RP := Right shr 8 + 1;

          if RP >= Cx then RP := Cx;

          if RP >= LP then
            FillLineCallback(Bitmap.PixelPtr[LP, Top], LP, Top, RP - LP, nil, Bitmap.CombineMode);
        end;

        Inc(Winding, NextWinding);
        Left := Right;
      end;
    end;
  EMMS;
end;

procedure CustomFillLines2(Bitmap: TCustomBitmap32; BaseY: Integer;
  const ScanLines: TScanLines; FillLineCallback: TFillLineEvent; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode = DefaultAAMode);
var
  I, J, L, N: Integer;
  MinY, MaxY, Y, Top, Bottom: Integer;
  MinX, MaxX, X, Dx: Integer;
  Left, Right: Integer;
  Buffer: array of Integer;
  AlphaBuffer: array of TColor32;
  BufferSize: Integer;
  ScanLine: PIntegerArray;
  Winding, NextWinding: Integer;
  AAShift, AALines, AAMultiplier: Integer;
begin
  AAShift := AA_SHIFT[AAMode];
  AALines := AA_LINES[AAMode] - 1; // we do the -1 here for optimization.
  AAMultiplier := AA_MULTI[AAMode];

  // find the range of Y screen coordinates
  MinY := BaseY shr AAShift;
  MaxY := (BaseY + Length(ScanLines) + AALines) shr AAShift;

  Y := MinY;
  while Y < MaxY do
  begin
    Top := Y shl AAShift - BaseY;
    Bottom := Top + AALines;
    if Top < 0 then Top := 0;
    if Bottom >= Length(ScanLines) then Bottom := High(ScanLines);

    // find left and right edges of the screen scanline
    MinX := $7F000000; MaxX := -$7F000000;
    for J := Top to Bottom do
    begin
      L := ScanLines[J].Count - 1;
      if L > 0 then
      begin
        Left := (ScanLines[J].EdgePoints[0] and $7FFFFFFF);
        Right := (ScanLines[J].EdgePoints[L] and $7FFFFFFF + AALines);
        if Left < MinX then MinX := Left;
        if Right > MaxX then MaxX := Right;
      end
    end;

    if MaxX >= MinX then
    begin
      MinX := MinX shr AAShift;
      MaxX := MaxX shr AAShift;
      // allocate buffer for a single scanline
      BufferSize := MaxX - MinX + 2;
      if Length(Buffer) < BufferSize then
      begin
        SetLength(Buffer, BufferSize + 64);
        SetLength(AlphaBuffer, BufferSize + 64);
      end;
      FillLongword(Buffer[0], BufferSize, 0);

      // ...and fill it
      if Mode = pfAlternate then
        for J := Top to Bottom do
        begin
          I := 0;
          L := ScanLines[J].Count;
          ScanLine := @ScanLines[J].EdgePoints[0];
          while I < L do
          begin
            // Left edge
            X := ScanLine[I] and $7FFFFFFF;
            Dx := X and AALines;
            X := X shr AAShift - MinX;
            Inc(Buffer[X], Dx xor AALines);
            Inc(Buffer[X + 1], Dx);
            Inc(I);

            // Right edge
            X := ScanLine[I] and $7FFFFFFF;
            Dx := X and AALines;
            X := X shr AAShift - MinX;
            Dec(Buffer[X], Dx xor AALines);
            Dec(Buffer[X + 1], Dx);
            Inc(I);
          end
        end
      else // mode = pfWinding
        for J := Top to Bottom do
        begin
          I := 0;
          L := ScanLines[J].Count;
          ScanLine := @ScanLines[J].EdgePoints[0];
          Winding := 0;
          while I < L do
          begin
            X := ScanLine[I];
            Inc(I);
            if (X and $80000000) <> 0 then NextWinding := 1 else NextWinding := -1;
            X := X and $7FFFFFFF;
            if Winding = 0 then
            begin
              Dx := X and AALines;
              X := X shr AAShift - MinX;
              Inc(Buffer[X], Dx xor AALines);
              Inc(Buffer[X + 1], Dx);
            end;
            Inc(Winding, NextWinding);
            if Winding = 0 then
            begin
              Dx := X and AALines;
              X := X shr AAShift - MinX;
              Dec(Buffer[X], Dx xor AALines);
              Dec(Buffer[X + 1], Dx);
            end;
          end;
        end;

      // integrate the buffer
      N := 0;
      for I := 0 to BufferSize - 1 do
      begin
        Inc(N, Buffer[I]);
        AlphaBuffer[I] := (N * AAMultiplier) shr 8;
      end;

      // draw it to the screen
      FillLineCallback(Pointer(Bitmap.PixelPtr[MinX, Y]), MinX, Y, BufferSize, @AlphaBuffer[0], Bitmap.CombineMode);  ;
      EMMS;
    end;

    Inc(Y);
  end;
end;


{ Helper routines for drawing Polygons and PolyPolygons }

procedure RenderPolyPolygon(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint; Color: TColor32;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode; Transformation: TTransformation);
var
  ChangedRect, DstRect: TFixedRect;
  P: TFixedPoint;
  AAShift: Integer;
  I: Integer;
  ScanLines: TScanLines;
begin
  if not Bitmap.MeasuringMode then
  begin
    ChangedRect := PolyPolygonBounds(Points, Transformation);

    with DstRect do
    if AAMode <> amNone then
    begin
      AAShift := AA_SHIFT[AAMode];
      Left := Bitmap.ClipRect.Left shl AAShift;
      Right := Bitmap.ClipRect.Right shl AAShift - 1;
      Top := Bitmap.ClipRect.Top shl AAShift;
      Bottom := Bitmap.ClipRect.Bottom shl AAShift - 1;

      P.X := ChangedRect.Top;
      P.Y := ChangedRect.Bottom;
      RoundShiftProcs[AAMode](P, P, nil);
      Top := Constrain(P.X, Top, Bottom);
      Bottom := Constrain(P.Y, Top, Bottom);
    end
    else
    begin
      Left := Bitmap.ClipRect.Left shl 8;
      Right := Bitmap.ClipRect.Right shl 8 - 1;
      Top := Constrain(SAR_16(ChangedRect.Top + $00007FFF),
        Bitmap.ClipRect.Top, Bitmap.ClipRect.Bottom - 1);
      Bottom := Constrain(SAR_16(ChangedRect.Bottom + $00007FFF),
        Bitmap.ClipRect.Top, Bitmap.ClipRect.Bottom - 1);
    end;

    if DstRect.Top >= DstRect.Bottom then Exit;

    ScanLinesCreate(ScanLines, DstRect.Bottom - DstRect.Top + 1);
    for I := 0 to High(Points) do
      AddPolygon(Points[I], DstRect, ScanLines, AAMode, Transformation);

    SortLines(ScanLines);
    Bitmap.BeginUpdate;
    try
      if AAMode <> amNone then
        if Assigned(FillLineCallback) then
          CustomFillLines2(Bitmap, DstRect.Top, ScanLines, FillLineCallback, Mode, AAMode)
        else
          ColorFillLines2(Bitmap, DstRect.Top, ScanLines, Color, Mode, AAMode)
      else
        if Assigned(FillLineCallback) then
          CustomFillLines(Bitmap, DstRect.Top, ScanLines, FillLineCallback, Mode)
        else
          ColorFillLines(Bitmap, DstRect.Top, ScanLines, Color, Mode);
    finally
      Bitmap.EndUpdate;
      ScanLinesDestroy(ScanLines);
    end;
    Bitmap.Changed(MakeRect(ChangedRect, rrOutside));
  end
  else
    Bitmap.Changed(MakeRect(PolyPolygonBounds(Points, Transformation), rrOutside));
end;

procedure RenderPolygon(Bitmap: TCustomBitmap32;
  const Points: TArrayOfFixedPoint; Color: TColor32;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode; Transformation: TTransformation);
var
  H: TArrayOfArrayOfFixedPoint;
begin
  SetLength(H, 1);
  H[0] := Points;
  RenderPolyPolygon(Bitmap, H, Color, FillLineCallback, Mode, AAMode, Transformation);
  H[0] := nil;
end;


{ Polygons }

procedure PolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Mode: TPolyFillMode; Transformation: TTransformation);
begin
  RenderPolygon(Bitmap, Points, Color, nil, Mode, amNone, Transformation);
end;

procedure PolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode;
  Transformation: TTransformation);
begin
  RenderPolygon(Bitmap, Points, 0, FillLineCallback, Mode, amNone, Transformation);
end;

procedure PolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Mode: TPolyFillMode;
  Transformation: TTransformation);
begin
  RenderPolygon(Bitmap, Points, 0, Filler.FillLine, Mode, amNone, Transformation);
end;

procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode; Transformation: TTransformation);
begin
  RenderPolygon(Bitmap, Points, Color, nil, Mode, AAMode, Transformation);
end;

procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode; Transformation: TTransformation);
begin
  RenderPolygon(Bitmap, Points, 0, FillLineCallback, Mode, AAMode, Transformation);
end;

procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode; Transformation: TTransformation);
begin
  RenderPolygon(Bitmap, Points, 0, Filler.FillLine, Mode, AAMode, Transformation);
end;


{ PolyPolygons }

procedure PolyPolygonTS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; Mode: TPolyFillMode;
  Transformation: TTransformation);
begin
  RenderPolyPolygon(Bitmap, Points, Color, nil, Mode, amNone, Transformation);
end;

procedure PolyPolygonTS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint; FillLineCallback: TFillLineEvent;
  Mode: TPolyFillMode; Transformation: TTransformation);
begin
  RenderPolyPolygon(Bitmap, Points, 0, FillLineCallback, Mode, amNone, Transformation);
end;

procedure PolyPolygonTS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint; Filler: TCustomPolygonFiller;
  Mode: TPolyFillMode; Transformation: TTransformation);
begin
  RenderPolyPolygon(Bitmap, Points, 0, Filler.FillLine, Mode, amNone, Transformation);
end;

procedure PolyPolygonXS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode; Transformation: TTransformation);
begin
  RenderPolyPolygon(Bitmap, Points, Color, nil, Mode, AAMode, Transformation);
end;

procedure PolyPolygonXS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint; FillLineCallback: TFillLineEvent;
  Mode: TPolyFillMode; const AAMode: TAntialiasMode;
  Transformation: TTransformation);
begin
  RenderPolyPolygon(Bitmap, Points, 0, FillLineCallback, Mode, AAMode, Transformation);
end;

procedure PolyPolygonXS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint; Filler: TCustomPolygonFiller;
  Mode: TPolyFillMode; const AAMode: TAntialiasMode;
  Transformation: TTransformation);
begin
  RenderPolyPolygon(Bitmap, Points, 0, Filler.FillLine, Mode, AAMode, Transformation);
end;


{ Helper routines }

function PolygonBounds(const Points: TArrayOfFixedPoint;
  Transformation: TTransformation): TFixedRect;
var
  I: Integer;
begin
  with Result do
  begin
    Left := $7FFFFFFF;
    Right := -$7FFFFFFF;
    Top := $7FFFFFFF;
    Bottom := -$7FFFFFFF;

    if Assigned(Transformation) then
    begin
      for I := 0 to High(Points) do
      with Transformation.Transform(Points[I]) do
      begin
        if X < Left   then Left := X;
        if X > Right  then Right := X;
        if Y < Top    then Top := Y;
        if Y > Bottom then Bottom := Y;
      end
    end
    else
      for I := 0 to High(Points) do
      with Points[I] do
      begin
        if X < Left   then Left := X;
        if X > Right  then Right := X;
        if Y < Top    then Top := Y;
        if Y > Bottom then Bottom := Y;
      end;
  end;
end;

function PolyPolygonBounds(const Points: TArrayOfArrayOfFixedPoint;
  Transformation: TTransformation): TFixedRect;
var
  I, J: Integer;
begin
  with Result do
  begin
    Left := $7FFFFFFF;
    Right := -$7FFFFFFF;
    Top := $7FFFFFFF;
    Bottom := -$7FFFFFFF;

    if Assigned(Transformation) then
      for I := 0 to High(Points) do
        for J := 0 to High(Points[I]) do
        with Transformation.Transform(Points[I, J]) do
        begin
          if X < Left   then Left := X;
          if X > Right  then Right := X;
          if Y < Top    then Top := Y;
          if Y > Bottom then Bottom := Y;
        end
    else
      for I := 0 to High(Points) do
        for J := 0 to High(Points[I]) do
        with Points[I, J] do
        begin
          if X < Left   then Left := X;
          if X > Right  then Right := X;
          if Y < Top    then Top := Y;
          if Y > Bottom then Bottom := Y;
        end;
  end;
end;

function PtInPolygon(const Pt: TFixedPoint; const Points: TArrayOfFixedPoint): Boolean;
var
  I: Integer;
  iPt, jPt: PFixedPoint;
begin
  Result := False;
  iPt := @Points[0];
  jPt := @Points[High(Points)];
  for I := 0 to High(Points) do
  begin
    Result := Result xor (((Pt.Y >= iPt.Y) xor (Pt.Y >= jPt.Y)) and
      (Pt.X - iPt.X < MulDiv(jPt.X - iPt.X, Pt.Y - iPt.Y, jPt.Y - iPt.Y)));
    jPt := iPt;
    Inc(iPt);
  end;
end;

{ TPolygon32 }

procedure TPolygon32.Add(const P: TFixedPoint);
var
  H, L: Integer;
begin
  H := High(Points);
  L := Length(Points[H]);
  SetLength(Points[H], L + 1);
  Points[H][L] := P;
  Normals := nil;
end;

procedure TPolygon32.AddPoints(var First: TFixedPoint; Count: Integer);
var
  H, L, I: Integer;
begin
  H := High(Points);
  L := Length(Points[H]);
  SetLength(Points[H], L + Count);
  for I := 0 to Count - 1 do
    Points[H, L + I] := PFixedPointArray(@First)[I];
  Normals := nil;
end;

procedure TPolygon32.CopyPropertiesTo(Dst: TPolygon32);
begin
  Dst.Antialiased := Antialiased;
  Dst.AntialiasMode := AntialiasMode;
  Dst.Closed := Closed;
  Dst.FillMode := FillMode;
end;

procedure TPolygon32.AssignTo(Dst: TPersistent);
var
  DstPolygon: TPolygon32;
  Index: Integer;
begin
  if Dst is TPolygon32 then
  begin
    DstPolygon := TPolygon32(Dst);
    CopyPropertiesTo(DstPolygon);
    SetLength(DstPolygon.FNormals, Length(Normals));
    for Index := 0 to Length(Normals) - 1 do
    begin
      DstPolygon.Normals[Index] := Copy(Normals[Index]);
    end;

    SetLength(DstPolygon.FPoints, Length(Points));
    for Index := 0 to Length(Points) - 1 do
    begin
      DstPolygon.Points[Index] := Copy(Points[Index]);
    end;
  end
  else
    inherited;
end;

function TPolygon32.GetBoundingRect: TFixedRect;
begin
  Result := PolyPolygonBounds(Points);
end;

procedure TPolygon32.BuildNormals;
var
  I, J, Count, NextI: Integer;
  dx, dy, f: Single;
begin
  if Length(Normals) <> 0 then Exit;
  SetLength(FNormals, Length(Points));

  for J := 0 to High(Points) do
  begin
    Count := Length(Points[J]);
    SetLength(Normals[J], Count);

    if Count = 0 then Continue;
    if Count = 1 then
    begin
      FillChar(Normals[J][0], SizeOf(TFixedPoint), 0);
      Continue;
    end;

    I := 0;
    NextI := 1;
    dx := 0;
    dy := 0;

    while I < Count do
    begin
      if Closed and (NextI >= Count) then NextI := 0;
      if NextI < Count then
      begin
        dx := (Points[J][NextI].X - Points[J][I].X) / $10000;
        dy := (Points[J][NextI].Y - Points[J][I].Y) / $10000;
      end;
      if (dx <> 0) or (dy <> 0) then
      begin
        f := 1 / GR32_Math.Hypot(dx, dy);
        dx := dx * f;
        dy := dy * f;
      end;
      with Normals[J][I] do
      begin
        X := Fixed(dy);
        Y := Fixed(-dx);
      end;
      Inc(I);
      Inc(NextI);
    end;
  end;
end;

procedure TPolygon32.Clear;
begin
  Points := nil;
  Normals := nil;
  NewLine;
end;

function TPolygon32.ContainsPoint(const P: TFixedPoint): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(FPoints) do
    if PtInPolygon(P, FPoints[I]) then
    begin
      Result := True;
      Exit;
    end;
end;

constructor TPolygon32.Create;
begin
  inherited;
  FClosed := True;
  FAntialiasMode := DefaultAAMode;
  NewLine; // initiate a new contour
end;

destructor TPolygon32.Destroy;
begin
  Clear;
  inherited;
end;

procedure TPolygon32.Draw(Bitmap: TCustomBitmap32; OutlineColor, FillColor: TColor32; Transformation: TTransformation);
begin
  Bitmap.BeginUpdate;

  if Antialiased then
  begin
    if (FillColor and $FF000000) <> 0 then
      PolyPolygonXS(Bitmap, Points, FillColor, FillMode, AntialiasMode, Transformation);
    if (OutlineColor and $FF000000) <> 0 then
      PolyPolylineXS2(Bitmap, Points, OutlineColor, Closed, Transformation);
  end
  else
  begin
    if (FillColor and $FF000000) <> 0 then
      PolyPolygonTS(Bitmap, Points, FillColor, FillMode, Transformation);
    if (OutlineColor and $FF000000) <> 0 then
      PolyPolylineTS(Bitmap, Points, OutlineColor, Closed, Transformation);
  end;

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure TPolygon32.Draw(Bitmap: TCustomBitmap32; OutlineColor: TColor32;
  FillCallback: TFillLineEvent; Transformation: TTransformation);
begin
  Bitmap.BeginUpdate;

  if Antialiased then
  begin
    RenderPolyPolygon(Bitmap, Points, 0, FillCallback, FillMode, AntialiasMode, Transformation);
    if (OutlineColor and $FF000000) <> 0 then
      PolyPolylineXS2(Bitmap, Points, OutlineColor, Closed, Transformation);
  end
  else
  begin
    RenderPolyPolygon(Bitmap, Points, 0, FillCallback, FillMode, amNone, Transformation);
    if (OutlineColor and $FF000000) <> 0 then
      PolyPolylineTS(Bitmap, Points, OutlineColor, Closed, Transformation);
  end;

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure TPolygon32.Draw(Bitmap: TCustomBitmap32; OutlineColor: TColor32;
  Filler: TCustomPolygonFiller; Transformation: TTransformation);
begin
  Bitmap.BeginUpdate;

  if Antialiased then
  begin
    RenderPolyPolygon(Bitmap, Points, 0, Filler.FillLine, FillMode, AntialiasMode, Transformation);
    if (OutlineColor and $FF000000) <> 0 then
      PolyPolylineXS2(Bitmap, Points, OutlineColor, Closed, Transformation);
  end
  else
  begin
    RenderPolyPolygon(Bitmap, Points, 0, Filler.FillLine, FillMode, amNone, Transformation);
    if (OutlineColor and $FF000000) <> 0 then
      PolyPolylineTS(Bitmap, Points, OutlineColor, Closed, Transformation);
  end;

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure TPolygon32.DrawEdge(Bitmap: TCustomBitmap32; Color: TColor32; Transformation: TTransformation);
begin
  Bitmap.BeginUpdate;

  if Antialiased then
    PolyPolylineXS2(Bitmap, Points, Color, Closed, Transformation)
  else
    PolyPolylineTS(Bitmap, Points, Color, Closed, Transformation);

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure TPolygon32.DrawFill(Bitmap: TCustomBitmap32; Color: TColor32; Transformation: TTransformation);
begin
  Bitmap.BeginUpdate;

  if Antialiased then
    PolyPolygonXS(Bitmap, Points, Color, FillMode, AntialiasMode, Transformation)
  else
    PolyPolygonTS(Bitmap, Points, Color, FillMode, Transformation);

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure TPolygon32.DrawFill(Bitmap: TCustomBitmap32; FillCallback: TFillLineEvent;
  Transformation: TTransformation);
begin
  Bitmap.BeginUpdate;

  if Antialiased then
    RenderPolyPolygon(Bitmap, Points, 0, FillCallback, FillMode, AntialiasMode, Transformation)
  else
    RenderPolyPolygon(Bitmap, Points, 0, FillCallback, FillMode, amNone, Transformation);

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure TPolygon32.DrawFill(Bitmap: TCustomBitmap32; Filler: TCustomPolygonFiller;
  Transformation: TTransformation);
begin
  Bitmap.BeginUpdate;
  if Antialiased then
    RenderPolyPolygon(Bitmap, Points, 0, Filler.FillLine, FillMode, AntialiasMode, Transformation)
  else
    RenderPolyPolygon(Bitmap, Points, 0, Filler.FillLine, FillMode, amNone, Transformation);

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

function TPolygon32.Grow(const Delta: TFixed; EdgeSharpness: Single = 0): TPolygon32;
var
  J, I, PrevI: Integer;
  PX, PY, AX, AY, BX, BY, CX, CY, R, D, E: Integer;

  procedure AddPoint(LongDeltaX, LongDeltaY: Integer);
  var
    N, L: Integer;
  begin
    with Result do
    begin
      N := High(Points);
      L := Length(Points[N]);
      SetLength(Points[N], L + 1);
    end;
    with Result.Points[N][L] do
    begin
      X := PX + LongDeltaX;
      Y := PY + LongDeltaY;
    end;
  end;

begin
  BuildNormals;

  if EdgeSharpness > 0.99 then
    EdgeSharpness := 0.99
  else if EdgeSharpness < 0 then
    EdgeSharpness := 0;

  D := Delta;
  E := Round(D * (1 - EdgeSharpness));

  Result := TPolygon32.Create;
  CopyPropertiesTo(Result);

  if Delta = 0 then
  begin
    // simply copy the data
    SetLength(Result.FPoints, Length(Points));
    for J := 0 to High(Points) do
      Result.Points[J] := Copy(Points[J], 0, Length(Points[J]));
    Exit;
  end;

  Result.Points := nil;

  for J := 0 to High(Points) do
  begin
    if Length(Points[J]) < 2 then Continue;

    Result.NewLine;

    for I := 0 to High(Points[J]) do
    begin
      with Points[J][I] do
      begin
        PX := X;
        PY := Y;
      end;

      with Normals[J][I] do
      begin
        BX := MulDiv(X, D, $10000);
        BY := MulDiv(Y, D, $10000);
      end;

      if (I > 0) or Closed then
      begin
        PrevI := I - 1;
        if PrevI < 0 then PrevI := High(Points[J]);
        with Normals[J][PrevI] do
        begin
          AX := MulDiv(X, D, $10000);
          AY := MulDiv(Y, D, $10000);
        end;

        if (I = High(Points[J])) and (not Closed) then AddPoint(AX, AY)
        else
        begin
          CX := AX + BX;
          CY := AY + BY;
          R := MulDiv(AX, CX, D) + MulDiv(AY, CY, D);
          if R > E then AddPoint(MulDiv(CX, D, R), MulDiv(CY, D, R))
          else
          begin
            AddPoint(AX, AY);
            AddPoint(BX, BY);
          end;
        end;
      end
      else AddPoint(BX, BY);
    end;
  end;
end;

procedure TPolygon32.NewLine;
begin
  SetLength(FPoints, Length(Points) + 1);
  Normals := nil;
end;

procedure TPolygon32.Offset(const Dx, Dy: TFixed);
var
  J, I: Integer;
begin
  for J := 0 to High(Points) do
    for I := 0 to High(Points[J]) do
      with Points[J][I] do
      begin
        Inc(X, Dx);
        Inc(Y, Dy);
      end;
end;

function TPolygon32.Outline: TPolygon32;
var
  J, I, L, H: Integer;
begin
  BuildNormals;

  Result := TPolygon32.Create;
  CopyPropertiesTo(Result);

  Result.Points := nil;

  for J := 0 to High(Points) do
  begin
    if Length(Points[J]) < 2 then Continue;

    if Closed then
    begin
      Result.NewLine;
      for I := 0 to High(Points[J]) do Result.Add(Points[J][I]);
      Result.NewLine;
      for I := High(Points[J]) downto 0 do Result.Add(Points[J][I]);
    end
    else // not closed
    begin
      // unrolled...
      SetLength(Result.FPoints, Length(Result.FPoints) + 1);
      Result.FNormals:= nil;

      L:= Length(Points[J]);
      H:= High(Result.FPoints);
      SetLength(Result.FPoints[H], L * 2);
      for I := 0 to High(Points[J]) do
        Result.FPoints[H][I]:= (Points[J][I]);
      for I := High(Points[J]) downto 0 do
        Result.FPoints[H][2 * L - (I + 1)]:= (Points[J][I]);
    end;
  end;
end;

procedure TPolygon32.Transform(Transformation: TTransformation);
begin
  Points := TransformPoints(Points, Transformation);
end;



//==================================================

initialization
  RegisterPolygonRenderer(TPolygonRenderer32VPR);
  RegisterPolygonRenderer(TPolygonRenderer32LCD);
  RegisterPolygonRenderer(TPolygonRenderer32LCD2);

finalization
  PolygonRendererList.Free;

end.
