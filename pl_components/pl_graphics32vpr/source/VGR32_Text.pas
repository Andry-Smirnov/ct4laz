
{**********************************************************************
 Package pl_Graphics32VPR
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

unit VGR32_Text;

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
 * The Original Code is Graphics32
 * The Initial Developer of the Original Code is Alex A. Denisov
 * Portions created by the Initial Developer are Copyright (C) 2000-2007
 * the Initial Developer. All rights reserved
 *
 * The Initial Developer of the code in GR32_Text.pas is Angus Johnson
 * <angus@angusj.com>. GR32_Text.pas code is Copyright (C) 2009-2012.
 * All rights reserved
 *
 * Acknowledgements:
 * Sub-pixel rendering based on code provided by Akaiten <akaiten@mail.ru>
 * TLCDDistributionLut class provided by Akaiten and is based on code from
 * Maxim Shemanarev.
 *
 * Version 4.0 (Last updated 5-Aug-2012)
 *
 *  The TText32 class renders text using the Windows API function
 *  GetGlyphOutline(). This requires TrueType fonts.
 *
 * More info on fonts -
 *   http://www.w3.org/TR/CSS2/fonts.html#emsq
 *   http://msdn.microsoft.com/en-us/library/dd162755(VS.85).aspx
 *
 * END LICENSE BLOCK **********************************************************)

interface

uses
  Windows,
  LCLIntf, LCLType,
  Classes, SysUtils, Math, Graphics, Types,
  GR32, GR32_Blend, GR32_Filters,
  GR32_LowLevel, GR32_Math, GR32_MicroTiles,
  GR32_Polygons, GR32_Rasterizers,
  GR32_Resamplers, GR32_System, GR32_VPR,
  VGR32_PolygonsEx, GR32_Transforms, GR32_VectorUtils,
  VGR32_Misc, VGR32_Lines;

type
  TAlignH = (aLeft, aRight, aCenter, aJustify);
  TAlignV = (aBottom, aMiddle, aTop);

  TFloatSize = record
    sx: single;
    sy: single;
  end;

  //Often glyph outlines require more than one line to define them, so each
  //glyph is defined by a two dimentional array - TArrayOfArrayOfFixedPoint.
  //Therefore outlines of multiple glyphs require three dimentional arrays ...
  //TArrayOfArrayOfArrayOfFixedPoint = array of TArrayOfArrayOfFixedPoint;

{$IFNDEF UNICODE}
  UnicodeString = WideString;
{$ENDIF}

  TGlyphInfo = record
    cached: boolean;
    metrics: TGlyphMetrics;
    pts: TArrayOfArrayOfFixedPoint;
  end;

  TTrueTypeFontClass = class of TTrueTypeFont;

  //*TTrueTypeFont: This class provides access to true-type font data,
  //*primarily glyph outlines (encapsulating the Windows API function
  //*GetGlyphOutline), but also retrieves TTextMetric and TOutlineTextmetric
  //*information. Descendant classes also perform caching of glyph outline data.
  TTrueTypeFont = class(TPersistent)
  private
    fFont: TFont;
    fMemDC: HDC;
    fOldFntHdl: HFont;
    fHinted: boolean;
    fFlushed: boolean;
    function GetFontName: TFontName;
    procedure SetFontName(const aFontName: TFontName);
    function GetHeight: integer;
    procedure SetHeight(aHeight: integer);
    function GetSize: integer;
    procedure SetSize(aSize: integer);
    function GetStyle: TFontStyles;
    procedure SetStyle(aStyle: TFontStyles);
    function GetCharSet: TFontCharset;
    procedure SetCharSet(aCharSet: TFontCharset);
    procedure SetHinted(Value: boolean);
  protected
    property MemDC: HDC read fMemDC;
  public
    constructor Create; overload; virtual;
    constructor Create(aFont: TFont); overload; virtual;
    constructor Create(const aFontName: TFontName; aHeight: integer = 14; aStyle: TFontStyles = []; aCharSet: TFontCharset = 0);
      overload; virtual;
    destructor Destroy; override;
    class function IsValidTTFont(font: TFont): boolean;
    function Lock: boolean;
    function IsLocked: boolean;
    procedure Unlock;
    procedure Assign(Source: TPersistent); override;
    function GetGlyphInfo(wc: widechar; out gm: TGlyphMetrics; out polyPts: TArrayOfArrayOfFixedPoint): boolean; virtual;
    function GetTextMetrics(out tm: TTextMetric): boolean;
    function GetOutlineTextMetrics(out otm: TOutlineTextmetric): boolean;
    procedure Flush;

    property Flushed: boolean read fFlushed write fFlushed;
    property FontName: TFontName read GetFontName write SetFontName;
    property Height: integer read GetHeight write SetHeight;
    property Size: integer read GetSize write SetSize;
    property CharSet: TFontCharset read GetCharSet write SetCharSet;
    property Style: TFontStyles read GetStyle write SetStyle;
    //*'Hinting' can marginally improve the drawing clarity of unrotated,
    //*unskewed text when sub-pixel font smoothing is applied. Default = disabled
    property Hinted: boolean read fHinted write SetHinted;
  end;

  //*TTrueTypeFontAnsiCache: Descendant class of TTrueTypeFont that caches glyph
  //*information for ANSI characters between 32 to 127 providing a small
  //*improvement in performance.
  TTrueTypeFontAnsiCache = class(TTrueTypeFont)
  private
    fGlyphInfo: array [32 ..127] of TGlyphInfo;
  public
    function GetGlyphInfo(wc: widechar; out gm: TGlyphMetrics; out polyPts: TArrayOfArrayOfFixedPoint): boolean; override;
  end;

  //*TText32: Class that greatly simplifies the drawing of true-type text, and
  //*the application of various transformations (scaling, skewing and rotating)
  //*and text alignments.
  TText32 = class
  private
    fTmpBmp: TBitmap32;
    fLCDDraw: boolean;
    fAngle: integer;
    fSkew: TFloatSize;
    fScale: TFloatSize;
    fTranslate: TFloatSize;
    fPadding: single;
    fInverted: boolean;
    fMirrored: boolean;
    fCurrentPos: TFixedPoint;
    fGlyphMatrix: TMat2;
    fCurrentPosMatrix: TMat2;
    fUnrotatedMatrix: TMat2;
    procedure SetInverted(Value: boolean);
    procedure SetMirrored(Value: boolean);
    procedure SetAngle(Value: integer);
    procedure SetScaleX(Value: single);
    procedure SetScaleY(Value: single);
    procedure SetSkewX(Value: single);
    procedure SetSkewY(Value: single);
    procedure SetPadding(Value: single);
    procedure PrepareMatrices;
    function GetCurrentPos: TFixedPoint;
    procedure SetCurrentPos(newPos: TFixedPoint);
    procedure SetLCDDraw(Value: boolean);
  protected
    procedure GetTextMetrics(const Text: UnicodeString; ttFont: TTrueTypeFont; const InsertionPt: TFixedPoint;
      out NextInsertionPt: TFixedPoint; out BoundsRect: TFixedRect);
    procedure GetDrawInfo(const Text: UnicodeString; ttFont: TTrueTypeFont; const InsertionPt: TFloatPoint;
      out NextInsertionPt: TFloatPoint; out polyPolyPts: TArrayOfArrayOfArrayOfFixedPoint);
  public
    constructor Create;
    destructor Destroy; override;

    //*GetTextFixedRect: bounding rectangle with all transformations applied
    function GetTextFixedRect(X, Y: single; const Text: UnicodeString; ttFont: TTrueTypeFont): TFixedRect;
    //*GetTextHeight: text height with all transformations applied
    function GetTextHeight(const Text: UnicodeString; ttFont: TTrueTypeFont): single;
    //*GetTextWidth: text width with all transformations applied
    function GetTextWidth(const Text: UnicodeString; ttFont: TTrueTypeFont): single;
    function CountCharsThatFit(X, Y: single; const Text: UnicodeString; ttFont: TTrueTypeFont; const boundsRect: TFloatRect;
      forceWordBreak: boolean = False): integer;

    procedure Draw(bitmap: TBitmap32; X, Y: single; const Text: UnicodeString; ttFont: TTrueTypeFont; color: TColor32); overload;
    //*Draw: without X & Y parameters specified draws 'text' at CurrentPos ...
    procedure Draw(bitmap: TBitmap32; const Text: UnicodeString; ttFont: TTrueTypeFont; color: TColor32); overload;

    //*Draw: text along a path. Text is always drawn on the 'left' side of the
    //*path relative to the path's direction. So if the path is constructed from
    //*left to right, the text will be drawn above the path.
    procedure Draw(bitmap: TBitmap32; const path: array of TFixedPoint; const Text: UnicodeString; ttFont: TTrueTypeFont;
      color: TColor32; alignH: TAlignH = aCenter; alignV: TAlignV = aMiddle; offsetFromLine: single = 0); overload;

    //*Draw: text that wraps within a bounding rectangle ...
    procedure Draw(bitmap: TBitmap32; const boundsRect: TFloatRect; const Text: UnicodeString; ttFont: TTrueTypeFont;
      color: TColor32; alignH: TAlignH; alignV: TAlignV; forceWordBreak: boolean = False); overload;

    //*GetEx: Similar to 'Get' but returns each glyph individually using a
    //*multi-dimentional array (TArrayOfArrayOfArrayOfFixedPoint) which
    //*allows the user to draw each glyph individually.
    function GetEx(const boundsRect: TFloatRect; const Text: UnicodeString; ttFont: TTrueTypeFont; alignH: TAlignH;
      alignV: TAlignV; forceWordBreak: boolean = False): TArrayOfArrayOfArrayOfFixedPoint; overload;

    //*DrawAndOutline: method to outline text with pen widths greater than 1px.
    procedure DrawAndOutline(bitmap: TBitmap32; X, Y: single; const Text: UnicodeString; ttFont: TTrueTypeFont;
      outlinePenWidth: single; outlineColor, fillColor: TColor32);

    function Get(X, Y: single; const Text: UnicodeString; ttFont: TTrueTypeFont; out NextInsertionPt: TFloatPoint): TArrayOfArrayOfFixedPoint;
      overload;

    function GetEx(X, Y: single; const Text: UnicodeString; ttFont: TTrueTypeFont;
      out NextInsertionPt: TFloatPoint): TArrayOfArrayOfArrayOfFixedPoint; overload;

    //*Text is subtly distorted to fit the specified path.
    //*If rotateTextToPath = true, the tops of glyphs are stretched at path
    //*convexes and compressed at path concaves. This avoids unsightly spreading
    //*and bunching of glyphs at path convexes and concaves respectively that
    //*occurs when glyphs are simply rotated along the path.
    //*If rotateTextToPath = false, then each glyph is distorted (without
    //*rotation) to follow the path.
    function Get(const path: array of TFixedPoint; const Text: UnicodeString; ttFont: TTrueTypeFont; alignH: TAlignH;
      alignV: TAlignV; rotateTextToPath: boolean; offsetFromLine: single = 0): TArrayOfArrayOfFixedPoint; overload;

    //*Text is subtly distorted to fit the specified path.
    //*If rotateTextToPath = true, the tops of glyphs are stretched at path
    //*convexes and compressed at path concaves. This avoids unsightly spreading
    //*and bunching of glyphs at path convexes and concaves respectively that
    //*occurs when glyphs are simply rotated along the path.
    //*If rotateTextToPath = false, then each glyph is distorted (without
    //*rotation) to follow the path.
    function GetEx(const path: array of TFixedPoint; const Text: UnicodeString; ttFont: TTrueTypeFont; alignH: TAlignH;
      alignV: TAlignV; rotateTextToPath: boolean; offsetFromLine: single = 0): TArrayOfArrayOfArrayOfFixedPoint; overload;

    //*This method assumes that the font adhers to convention(#) by drawing
    //*outer glyph outlines clockwise and inner outlines (or 'holes' like the
    //*middle of an 'O') anti-clockwise.
    //*(#)see http://www.microsoft.com/typography/ProductionGuidelines.mspx
    function GetInflated(X, Y, delta: single; const Text: UnicodeString; ttFont: TTrueTypeFont;
      out NextInsertionPt: TFloatPoint): TArrayOfArrayOfFixedPoint;

    //*This method assumes that the font adhers to convention(#) by drawing
    //*outer glyph outlines clockwise and inner outlines (or 'holes' like the
    //*middle of an 'O') anti-clockwise.
    //*(#)see http://www.microsoft.com/typography/ProductionGuidelines.mspx
    function GetInflatedEx(X, Y, delta: single; const Text: UnicodeString; ttFont: TTrueTypeFont;
      out NextInsertionPt: TFloatPoint): TArrayOfArrayOfArrayOfFixedPoint;

    function GetBetweenPaths(const bottomCurve, topCurve: TArrayOfFixedPoint; const Text: UnicodeString;
      ttFont: TTrueTypeFont): TArrayOfArrayOfFixedPoint;

    function GetBetweenPathsEx(const bottomCurve, topCurve: TArrayOfFixedPoint; const Text: UnicodeString;
      ttFont: TTrueTypeFont): TArrayOfArrayOfArrayOfFixedPoint;

    procedure Scale(sx, sy: single);
    procedure Skew(sx, sy: single);
    procedure Translate(dx, dy: TFloat);
    procedure ClearTransformations; //including angle

    property Angle: integer read fAngle write SetAngle;
    property ScaleX: single read fScale.sx write SetScaleX;
    property ScaleY: single read fScale.sy write SetScaleY;
    property SkewX: single read fSkew.sx write SetSkewX;
    property SkewY: single read fSkew.sy write SetSkewY;
    property TranslateX: single read fTranslate.sx write fTranslate.sx;
    property TranslateY: single read fTranslate.sy write fTranslate.sy;

    //CurrentPos: internally updates after every Draw method
    property CurrentPos: TFixedPoint read GetCurrentPos write SetCurrentPos;
    //LCDDraw: subpixel antialiasing for much smoother text on LCD displays
    //(see http://www.grc.com/cttech.htm for more info on this)
    property LCDDraw: boolean read fLCDDraw write SetLCDDraw;
    //Inverted - flips text vertically
    property Inverted: boolean read fInverted write SetInverted;
    //Mirrored - flips text horizontally
    property Mirrored: boolean read fMirrored write SetMirrored;
    //Spacing - extra space between each character
    property Spacing: single read fPadding write SetPadding;
  end;

  //------------------------------------------------------------------------------
  //------------------------------------------------------------------------------

  TPolyPolygonFunc = procedure(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint; Color: TColor32;
    FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil);

var
  TrueTypeFontClass: TTrueTypeFontClass = TTrueTypeFontAnsiCache;

  //*Text32LCDDrawDefault: when set to true LCD sub-pixel antialiasing
  //*will be enabled by default whenever a Text32 object is created.
  //*See initialization section below.
  Text32LCDDrawDefault: boolean = False;

  PolyPolygonFunc: TPolyPolygonFunc = @GR32_Polygons.PolyPolygonFS;
  PolyPolygonFuncLCD: TPolyPolygonFunc = @GR32_Polygons.PolyPolygonFS_LCD;

const
{$T-}
  PolyPolygonFunction: array[boolean] of ^TPolyPolygonFunc = (
    (@PolyPolygonFunc), (@PolyPolygonFuncLCD));
{$T+}

  identity_mat2: TMat2 =
    (eM11: (fract: 0; Value: 1); eM12: (fract: 0; Value: 0);
    eM21: (fract: 0; Value: 0); eM22: (fract: 0; Value: 1));

  vert_flip_mat2: TMat2 =
    (eM11: (fract: 0; Value: 1); eM12: (fract: 0; Value: 0);
    eM21: (fract: 0; Value: 0); eM22: (fract: 0; Value: -1));

  horz_flip_mat2: TMat2 =
    (eM11: (fract: 0; Value: -1); eM12: (fract: 0; Value: 0);
    eM21: (fract: 0; Value: 0); eM22: (fract: 0; Value: -1));

//*ScaleMat2: Matrix scale transformation using Windows TMat2 record structure
procedure ScaleMat2(var mat: TMat2; const sx, sy: single);
//*SkewMat2: Matrix skew transformation using Windows TMat2 record structure
procedure SkewMat2(var mat: TMat2; const fx, fy: single);
//*RotateMat2: Matrix rotate transformation using Windows TMat2 record structure
procedure RotateMat2(var mat: TMat2; const angle_radians: single);

//*GetTTFontCharInfo: calls the Windows API GetGlyphOutlineW and processes the
//*raw info into a TArrayOfArrayOfFixedPoint that contains the glyph outline.
function GetTTFontCharInfo(MemDC: HDC; wc: widechar; dx, dy: single; mat2: TMat2; Hinted: boolean; out gm: TGlyphMetrics;
  out polyPts: TArrayOfArrayOfFixedPoint): boolean;

//*GetTTFontCharMetrics: calls the Windows API GetGlyphOutlineW and simply
//*returns a TGlyphMetrics record without the glyph outline.
function GetTTFontCharMetrics(MemDC: HDC; wc: widechar; dx, dy: single; mat2: TMat2; out gm: TGlyphMetrics): boolean;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function FloatSize(sx, sy: single): TFloatSize;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//*SimpleText: This procedure is ideal for text that doesn't require any
//*special formatting, and where DrawMode = dmBlend (ie where canvas.textout()
//*can't be used).
procedure SimpleText(bmp: TBitmap32; font: TFont; X, Y: integer; const widetext: WideString; color: TColor32);

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


implementation


{$IFNDEF UNICODE}
type
  PByte = PAnsiChar;
{$ENDIF}

const
  GGO_UNHINTED = $100;
  GGO_BEZIER = $3;
  TT_PRIM_CSPLINE = $3;

  SPACE: widechar = #32;
  TAB: widechar = #9;
  LF: widechar = #10;
  CR: widechar = #13;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TransformPolyPoints(mat: TMat2; translate: TFixedPoint; var pts: TArrayOfArrayOfFixedPoint);
var
  i, j: integer;
  tmp: TFixed;
begin
  for i := 0 to high(pts) do
    for j := 0 to high(pts[i]) do
      with pts[i][j] do
      begin
        tmp := FixedMul(X, TFixed(mat.eM11)) + FixedMul(Y, TFixed(mat.eM21)) + translate.X;
        Y := FixedMul(X, TFixed(mat.eM12)) + FixedMul(Y, TFixed(mat.eM22)) + translate.Y;
        pts[i][j].X := tmp;
      end;
end;
//------------------------------------------------------------------------------

procedure TransformPoint(mat: TMat2; var pt: TFloatPoint);
var
  tmp: single;
begin
  with pt do
  begin
    tmp := X * TFixed(mat.eM11) * FixedToFloat + Y * TFixed(mat.eM21) * FixedToFloat;
    Y := X * TFixed(mat.eM12) * FixedToFloat + Y * TFixed(mat.eM22) * FixedToFloat;
    pt.X := tmp;
  end;
end;
//------------------------------------------------------------------------------

procedure SkewPolyPoints(var pts: TArrayOfArrayOfFixedPoint; originY: TFixed; sx, sy: single);
var
  i, j: integer;
begin
  for i := 0 to length(pts) - 1 do
    for j := 0 to length(pts[i]) - 1 do
      with pts[i][j] do
      begin
        X := round(X + (Y - originY) * sx);
        Y := round(Y + X * sy);
      end;
end;
//------------------------------------------------------------------------------

function FloatSize(sx, sy: single): TFloatSize;
begin
  Result.sx := sx;
  Result.sy := sy;
end;
//------------------------------------------------------------------------------

function MultMat2(const M1, M2: tMat2): tmat2;
var
  m1em11, m1em12, m1em21, m1em22, m2em11, m2em12, m2em21, m2em22: single;
begin
  m1em11 := GR32.TFixed(m1.eM11) * FixedToFloat;
  m1em12 := GR32.TFixed(m1.eM12) * FixedToFloat;
  m1em21 := GR32.TFixed(m1.eM21) * FixedToFloat;
  m1em22 := GR32.TFixed(m1.eM22) * FixedToFloat;

  m2em11 := GR32.TFixed(m2.eM11) * FixedToFloat;
  m2em12 := GR32.TFixed(m2.eM12) * FixedToFloat;
  m2em21 := GR32.TFixed(m2.eM21) * FixedToFloat;
  m2em22 := GR32.TFixed(m2.eM22) * FixedToFloat;

  GR32.TFixed(Result.eM11) := trunc((m1em11 * m2em11 + m1em21 * m2em12) * FixedOne);
  GR32.TFixed(Result.eM12) := trunc((m1em12 * m2em11 + m1em22 * m2em12) * FixedOne);
  GR32.TFixed(Result.eM21) := trunc((m1em11 * m2em21 + m1em21 * m2em22) * FixedOne);
  GR32.TFixed(Result.eM22) := trunc((m1em12 * m2em21 + m1em22 * m2em22) * FixedOne);
end;
//------------------------------------------------------------------------------

procedure ScaleMat2(var mat: TMat2; const sx, sy: single);
var
  tmp: TMat2;
begin
  tmp := identity_mat2;
  GR32.TFixed(tmp.eM11) := Fixed(sx);
  GR32.TFixed(tmp.eM22) := Fixed(sy);
  mat := MultMat2(tmp, mat);
end;
//------------------------------------------------------------------------------

procedure SkewMat2(var mat: TMat2; const fx, fy: single);
var
  tmp: TMat2;
begin
  tmp := identity_mat2;
  GR32.TFixed(tmp.eM21) := Fixed(fx);
  GR32.TFixed(tmp.eM12) := Fixed(fy);
  mat := MultMat2(tmp, mat);
end;
//------------------------------------------------------------------------------

procedure RotateMat2(var mat: TMat2; const angle_radians: single);
var
  cosAng, sinAng: single;
  tmp: TMat2;
begin
  GR32_Math.sincos(angle_radians, sinAng, cosAng);
  GR32.TFixed(tmp.eM11) := trunc(cosAng * FixedOne);
  GR32.TFixed(tmp.eM21) := trunc(sinAng * FixedOne);
  GR32.TFixed(tmp.eM12) := trunc(-sinAng * FixedOne);
  GR32.TFixed(tmp.eM22) := trunc(cosAng * FixedOne);
  mat := MultMat2(tmp, mat);
end;
//------------------------------------------------------------------------------

function OffsetPoint(const pt: Windows.TPointfx; const dx, dy: single): GR32.TFixedPoint;
begin
  Result := TFixedPoint(pt);
  Result.X := Result.X + Fixed(dx);
  Result.Y := Result.Y + Fixed(dy);
end;
//------------------------------------------------------------------------------

function StripCRs(const widetext: WideString): WideString;
var
  i: integer;
begin
  Result := widetext;
  i := pos(#13, Result);
  while i <> 0 do
  begin
    Delete(Result, i, 1);
    i := pos(#13, Result);
  end;
end;
//------------------------------------------------------------------------------

procedure ParseFontCharInfo(info, endInfo: PByte; dx, dy: single; out polyPts: TArrayOfArrayOfFixedPoint);
var
  tmpCurvePts: TArrayOfFixedPoint;
  i, polyCnt, ptCnt, ptCntTot: integer;
  endContour: PByte;
begin
  polyCnt := -1;
  while Info < endInfo do
  begin
    with PTTPolygonHeader(info)^ do
    begin
      if dwType <> TT_POLYGON_TYPE then
        raise Exception.Create('GetGlyphOutline() error - wrong header type');
      endContour := info + cb;
      Inc(info, SizeOf(TTTPolygonHeader));
      Inc(polyCnt);
      setLength(polyPts, polyCnt + 1);
      setlength(polyPts[polyCnt], 1);
      polyPts[polyCnt][0] := OffsetPoint(pfxStart, dx, dy);
      ptCntTot := 1;
    end;
    while info < endContour do
    begin
      with PTTPolyCurve(info)^ do
      begin
        ptCnt := cpfx;
        Inc(info, SizeOf(word) * 2);
        case wType of
          TT_PRIM_LINE:
          begin
            setlength(polyPts[polyCnt], ptCntTot + ptCnt);
            for i := 0 to ptCnt - 1 do
            begin
              polyPts[polyCnt][ptCntTot + i] :=
                OffsetPoint(PPointfx(info)^, dx, dy);
              Inc(info, sizeOf(TPointfx));
            end;
          end;
          TT_PRIM_QSPLINE: //http://support.microsoft.com/kb/q87115/
          begin
            //tmpCurvePts is used for the QSPLINE control points ...
            setLength(tmpCurvePts, ptCnt + 1);
            //must include the previous point in the QSPLINE ...
            tmpCurvePts[0] := polyPts[polyCnt][ptCntTot - 1];
            for i := 1 to ptCnt do
            begin
              tmpCurvePts[i] := OffsetPoint(PPointfx(info)^, dx, dy);
              Inc(info, sizeOf(TPointfx));
            end;
            tmpCurvePts := GetQSplinePoints(tmpCurvePts);
            ptCnt := length(tmpCurvePts) - 1;//nb: first point already added
            setlength(polyPts[polyCnt], ptCntTot + ptCnt);
            move(tmpCurvePts[1],
              polyPts[polyCnt][ptCntTot], ptCnt * sizeOf(TFixedPoint));
          end;
          TT_PRIM_CSPLINE:
          begin
            //tmpCurvePts is used for the CSPLINE control points ...
            setLength(tmpCurvePts, ptCnt + 1);
            //must include the previous point in the CSPLINE ...
            tmpCurvePts[0] := polyPts[polyCnt][ptCntTot - 1];
            for i := 1 to ptCnt do
            begin
              tmpCurvePts[i] := OffsetPoint(PPointfx(info)^, dx, dy);
              Inc(info, sizeOf(TPointfx));
            end;
            tmpCurvePts := GetCBezierPoints(tmpCurvePts);
            ptCnt := length(tmpCurvePts) - 1;//nb: first point already added
            setlength(polyPts[polyCnt], ptCntTot + ptCnt);
            move(tmpCurvePts[1],
              polyPts[polyCnt][ptCntTot], ptCnt * sizeOf(TFixedPoint));
          end;
          else
            raise
            Exception.Create('GetGlyphOutline() error - wrong curve type');
        end;
        Inc(ptCntTot, ptCnt);
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function GetMemoryDeviceContext(font: TFont; out MemDC: HDC; out oldFntHdl: HFont): boolean;
begin
  Result := False;
  if not assigned(font) then
    exit;
  memDC := Windows.CreateCompatibleDC(0);
  if memDC = 0 then
    exit;
  oldFntHdl := Windows.SelectObject(memDC, font.Handle);
  Result := oldFntHdl <> 0;
  if not Result then
    DeleteDC(memDC);
end;
//------------------------------------------------------------------------------

procedure DeleteMemoryDeviceContext(MemDC: HDC; oldFntHdl: HFont);
begin
  Windows.SelectObject(memDC, oldFntHdl);
  DeleteDC(memDC);
end;
//------------------------------------------------------------------------------

function GetTTFontCharInfo(MemDC: HDC; wc: widechar; dx, dy: single; mat2: TMat2; Hinted: boolean; out gm: TGlyphMetrics;
  out polyPts: TArrayOfArrayOfFixedPoint): boolean;
var
  size: DWord;
  info, startInfo: PByte;
const
  hintBool: array[boolean] of UINT = (GGO_UNHINTED, 0);
begin
  startInfo := nil;
  size := Windows.GetGlyphOutlineW(memDC, cardinal(wc), GGO_NATIVE or hintBool[Hinted], gm, 0, nil, mat2);
  Result := (size <> GDI_ERROR);
  if not Result or (size = 0) then
    exit;
  GetMem(info, size);
  try
    startInfo := info;
    if Windows.GetGlyphOutlineW(memDC, cardinal(wc), GGO_NATIVE or hintBool[Hinted], gm, size, info, mat2) <> GDI_ERROR then
      ParseFontCharInfo(info, info + size, dx, dy, polyPts)
    else
      Result := False;
  finally
    FreeMem(startInfo);
  end;
end;
//------------------------------------------------------------------------------

function GetTTFontCharMetrics(MemDC: HDC; wc: widechar; dx, dy: single; mat2: TMat2; out gm: TGlyphMetrics): boolean;
begin
  Result := Windows.GetGlyphOutlineW(MemDC, cardinal(wc), GGO_METRICS, gm, 0, nil, mat2) <> GDI_ERROR;
end;

//------------------------------------------------------------------------------
// TTrueTypeFont methods
//------------------------------------------------------------------------------

constructor TTrueTypeFont.Create;
begin
  fFont := TFont.Create;
  fFont.Name := 'Arial';
  fFont.Height := 18;
  fFont.Style := [];
  fFont.Charset := 0;
end;
//------------------------------------------------------------------------------


constructor TTrueTypeFont.Create(aFont: TFont);
begin
  Create;
  if not assigned(aFont) then
    exit;
  if IsValidTTFont(aFont) then
    fFont.Name := aFont.Name;
  fFont.Height := aFont.Height;
  fFont.Style := aFont.Style;
  fFont.Charset := aFont.Charset;
end;
//------------------------------------------------------------------------------

constructor TTrueTypeFont.Create(const aFontName: TFontName; aHeight: integer; aStyle: TFontStyles; aCharSet: TFontCharset);
begin
  Create;
  fFont.Name := aFontName;
  if not IsValidTTFont(fFont) then
    fFont.Name := 'Arial';
  fFont.Height := aHeight;
  fFont.Style := aStyle;
  fFont.Charset := aCharSet;
end;
//------------------------------------------------------------------------------

destructor TTrueTypeFont.Destroy;
begin
  if IsLocked then
    DeleteMemoryDeviceContext(fMemDC, fOldFntHdl); //just in case
  fFont.Free;
end;
//------------------------------------------------------------------------------

class function TTrueTypeFont.IsValidTTFont(font: TFont): boolean;
var
  xmemDC: HDC;
  oldFontHdl: HFont;
  gm: TGlyphMetrics;
  mat2: TMat2;
begin
  Result := False;
  if not assigned(font) then
    exit;
  xmemDC := Windows.CreateCompatibleDC(0);
  oldFontHdl := Windows.SelectObject(xmemDC, Font.Handle);
  if oldFontHdl <> 0 then
  begin
    mat2 := identity_mat2;
    Result := Windows.GetGlyphOutlineW(xmemDC, $21, GGO_METRICS, gm, 0, nil, mat2) <> GDI_ERROR;
    Windows.SelectObject(xmemDC, oldFontHdl);
  end;
  Windows.DeleteDC(xmemDC);
end;
//------------------------------------------------------------------------------

function TTrueTypeFont.IsLocked: boolean;
begin
  Result := fOldFntHdl <> 0;
end;
//------------------------------------------------------------------------------

function TTrueTypeFont.Lock: boolean;
begin
  Result := not IsLocked;
  if not Result then
    exit;
  Result := GetMemoryDeviceContext(fFont, fMemDC, fOldFntHdl);
end;
//------------------------------------------------------------------------------

procedure TTrueTypeFont.Unlock;
begin
  if IsLocked then
    DeleteMemoryDeviceContext(fMemDC, fOldFntHdl);
  fMemDC := 0;
  fOldFntHdl := 0;
end;
//------------------------------------------------------------------------------

procedure TTrueTypeFont.Flush;
begin
  fFlushed := True;
end;
//------------------------------------------------------------------------------

function TTrueTypeFont.GetFontName: TFontName;
begin
  Result := fFont.Name;
end;
//------------------------------------------------------------------------------

procedure TTrueTypeFont.SetFontName(const aFontName: TFontName);
var
  oldFontName: TFontName;
begin
  oldFontName := fFont.Name;
  if SameText(aFontName, oldFontName) then
    exit;
  fFont.Name := aFontName;
  if not IsValidTTFont(fFont) then
    fFont.Name := oldFontName;
  Flush;
end;
//------------------------------------------------------------------------------

function TTrueTypeFont.GetHeight: integer;
begin
  Result := fFont.Height;
end;
//------------------------------------------------------------------------------

procedure TTrueTypeFont.SetHeight(aHeight: integer);
begin
  if fFont.Height = aHeight then
    exit;
  fFont.Height := aHeight;
  Flush;
end;
//------------------------------------------------------------------------------

function TTrueTypeFont.GetSize: integer;
begin
  Result := fFont.Size;
end;
//------------------------------------------------------------------------------

procedure TTrueTypeFont.SetSize(aSize: integer);
begin
  if fFont.Size = aSize then
    exit;
  fFont.Size := aSize;
  Flush;
end;
//------------------------------------------------------------------------------

function TTrueTypeFont.GetStyle: TFontStyles;
begin
  Result := fFont.Style;
end;
//------------------------------------------------------------------------------

procedure TTrueTypeFont.SetStyle(aStyle: TFontStyles);
begin
  if fFont.Style = aStyle then
    exit;
  fFont.Style := aStyle;
  Flush;
end;
//------------------------------------------------------------------------------

function TTrueTypeFont.GetCharSet: TFontCharset;
begin
  Result := fFont.Charset;
end;
//------------------------------------------------------------------------------

procedure TTrueTypeFont.SetCharSet(aCharSet: TFontCharset);
begin
  if fFont.Charset = aCharset then
    exit;
  fFont.Charset := aCharset;
  Flush;
end;
//------------------------------------------------------------------------------

procedure TTrueTypeFont.SetHinted(Value: boolean);
begin
  if fHinted = Value then
    exit;
  fHinted := Value;
  Flush;
end;
//------------------------------------------------------------------------------

procedure TTrueTypeFont.Assign(Source: TPersistent);
begin
  if IsLocked then
    exit;
  if (Source is TFont) and IsValidTTFont(TFont(Source)) then
    with TFont(Source) do
    begin
      fFlushed := True;
      fFont.Name := Name;
      fFont.Height := Height;
      fFont.Style := Style;
      fFont.Charset := CharSet;
    end
  else if Source is TTrueTypeFont then
    with TTrueTypeFont(Source) do
    begin
      self.fFlushed := True;
      self.fFont.Name := FontName;
      self.fFont.Height := Height;
      self.fFont.Style := Style;
      self.fFont.Charset := CharSet;
    end
  else
    inherited;
end;
//------------------------------------------------------------------------------

function TTrueTypeFont.GetGlyphInfo(wc: widechar; out gm: TGlyphMetrics; out polyPts: TArrayOfArrayOfFixedPoint): boolean;
begin
  Result := IsLocked;
  if not Result then
    exit;
  Result := GetTTFontCharInfo(fMemDC, wc, 0, 0, vert_flip_mat2, fHinted, gm, polyPts);
end;
//------------------------------------------------------------------------------

function TTrueTypeFont.GetTextMetrics(out tm: TTextMetric): boolean;
var
  wasUnlocked: boolean;
begin
  wasUnlocked := Lock;
  try
    Result := Windows.GetTextMetrics(fMemDC, Windows.TEXTMETRIC(tm)); // ct9999
  finally
    if wasUnlocked then
      UnLock;
  end;
end;
//------------------------------------------------------------------------------

function TTrueTypeFont.GetOutlineTextMetrics(out otm: TOutlineTextmetric): boolean;
var
  wasUnlocked: boolean;
begin
  wasUnlocked := Lock;
  try
    otm.otmSize := sizeof(TOutlineTextmetric);
    Result :=
      Windows.GetOutlineTextMetrics(fMemDC, sizeof(TOutlineTextmetric), @otm) <> 0;
  finally
    if wasUnlocked then
      UnLock;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function TTrueTypeFontAnsiCache.GetGlyphInfo(wc: widechar; out gm: TGlyphMetrics; out polyPts: TArrayOfArrayOfFixedPoint): boolean;
var
  i: integer;
begin
  Result := IsLocked;
  if not Result then
    exit;

  if Flushed then
  begin
    for i := low(fGlyphInfo) to high(fGlyphInfo) do
      fGlyphInfo[i].cached := False;
    Flushed := False;
  end;

  if Ord(wc) in [32..127] then
    with fGlyphInfo[Ord(wc)] do
    begin
      if cached then
      begin
        gm := metrics;
        polyPts := CopyPolyPoints(pts);
        Result := True;
      end
      else
      begin
        cached :=
          GetTTFontCharInfo(MemDC, wc, 0, 0, vert_flip_mat2, Hinted, metrics, pts);
        Result := cached;
        if not Result then
          exit;
        gm := metrics;
        polyPts := CopyPolyPoints(pts);
      end;
    end
  else
    Result := GetTTFontCharInfo(MemDC, wc, 0, 0, vert_flip_mat2, Hinted, gm, polyPts);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

constructor TText32.Create;
begin
  fTmpBmp := TBitmap32.Create;
  fScale.sx := 1;
  fScale.sy := 1;
  fLCDDraw := Text32LCDDrawDefault;
end;
//------------------------------------------------------------------------------

destructor TText32.Destroy;
begin
  fTmpBmp.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TText32.SetInverted(Value: boolean);
begin
  if fInverted = Value then
    exit;
  fInverted := Value;
end;
//------------------------------------------------------------------------------

procedure TText32.SetMirrored(Value: boolean);
begin
  if fMirrored = Value then
    exit;
  fMirrored := Value;
end;
//------------------------------------------------------------------------------

procedure TText32.SetAngle(Value: integer);
begin
  Value := Value mod 360;
  if Value > 180 then
    Value := Value - 360
  else if Value < -180 then
    Value := Value + 360;
  fAngle := Value;
end;
//------------------------------------------------------------------------------

procedure TText32.Skew(sx, sy: single);
begin
  SetSkewX(sx);
  SetSkewY(sy);
end;
//------------------------------------------------------------------------------

procedure TText32.SetSkewX(Value: single);
begin
  if Value < -2 then
    Value := -2
  else if Value > 2 then
    Value := 2;
  fSkew.sx := Value;
end;
//------------------------------------------------------------------------------

procedure TText32.SetSkewY(Value: single);
begin
  if Value < -20 then
    Value := -20
  else if Value > 20 then
    Value := 20;
  fSkew.sy := Value;
end;
//------------------------------------------------------------------------------

procedure TText32.Scale(sx, sy: single);
begin
  SetScaleX(sx);
  SetScaleY(sy);
end;
//------------------------------------------------------------------------------

procedure TText32.SetScaleX(Value: single);
begin
  if Value < 0.001 then
    Value := 0.001
  else if Value > 200 then
    Value := 200;
  fScale.sx := Value;
end;
//------------------------------------------------------------------------------

procedure TText32.SetScaleY(Value: single);
begin
  if Value < 0.001 then
    Value := 0.001
  else if Value > 200 then
    Value := 200;
  fScale.sy := Value;
end;
//------------------------------------------------------------------------------

procedure TText32.Translate(dx, dy: TFloat);
begin
  fTranslate.sx := dx;
  fTranslate.sy := dy;
end;
//------------------------------------------------------------------------------

procedure TText32.ClearTransformations;
begin
  fTranslate.sx := 0;
  fTranslate.sy := 0;
  fSkew.sx := 0;
  fSkew.sy := 0;
  fScale.sx := 1;
  fScale.sy := 1;
  fMirrored := False;
  fInverted := False;
  fAngle := 0;
end;
//------------------------------------------------------------------------------

procedure TText32.SetPadding(Value: single);
begin
  if Value < -5 then
    Value := -5
  else if Value > 20 then
    Value := 20;
  fPadding := Value;
end;
//------------------------------------------------------------------------------

function TText32.GetTextFixedRect(X, Y: single; const Text: UnicodeString; ttFont: TTrueTypeFont): TFixedRect;
var
  pt: TFixedPoint;
begin
  pt := FixedPoint(0, 0);
  GetTextMetrics(Text, ttFont, FixedPoint(X, Y), pt, Result);
end;
//------------------------------------------------------------------------------

function TText32.GetTextHeight(const Text: UnicodeString; ttFont: TTrueTypeFont): single;
begin
  with GetTextFixedRect(0, 0, Text, ttFont) do
    Result := (Bottom - Top) * FixedToFloat;
end;
//------------------------------------------------------------------------------

function TText32.GetTextWidth(const Text: UnicodeString; ttFont: TTrueTypeFont): single;
begin
  with GetTextFixedRect(0, 0, Text, ttFont) do
    Result := (Right - Left) * FixedToFloat;
end;
//------------------------------------------------------------------------------

procedure TText32.Draw(bitmap: TBitmap32; const boundsRect: TFloatRect; const Text: UnicodeString; ttFont: TTrueTypeFont;
  color: TColor32; alignH: TAlignH; alignV: TAlignV; forceWordBreak: boolean = False);
var
  i: integer;
  ppts: TArrayOfArrayOfArrayOfFixedPoint;
  ppts2: TArrayOfArrayOfFloatPoint;
  PolyPolygonFn: TPolyPolygonFunc;
begin
  ppts := GetEx(boundsRect, Text, ttFont, alignH, alignV, forceWordBreak);
  PolyPolygonFn := PolyPolygonFunction[fLCDDraw]^;
  for i := 0 to high(ppts) do
  begin
    ppts2 := MakeArrayOfArrayOfFloatPoints(ppts[i]);
    PolyPolygonFn(bitmap, ppts2, color, pfWinding);
  end;
end;
//------------------------------------------------------------------------------

{$WARNINGS OFF}//prevents a warning about a potentially unassigned result.
function TText32.GetEx(const boundsRect: TFloatRect; const Text: UnicodeString; ttFont: TTrueTypeFont; alignH: TAlignH;
  alignV: TAlignV; forceWordBreak: boolean = False): TArrayOfArrayOfArrayOfFixedPoint;
var
  i, j, k, len, cntChrsThatFit, spcCnt, lineCnt, lineHeight, maxLines, savedAngle: integer;
  theText, theLine: UnicodeString;
  lineWidth, descenderSpace, yPos, savedSkewY: single;
  spaceWidth, spaceChrPadding, lastSpaceChrPadding: single;
  origin: TFixedPoint;
  pt: TFloatPoint;
  a, adjustedLeft: single;
  textBeforeLineFeed: boolean;
  cp: TFixedPoint;
  tm: TTextMetric;

  procedure GetAdjustedLeft;
  begin
    //adjust Left to compensate for skewing etc ...
    adjustedLeft :=
      GetTextFixedRect(0, 0, theText[1], ttFont).Left * FixedToFloat;
    ;
    if adjustedLeft < 0 then
      adjustedLeft := boundsRect.Left - adjustedLeft
    else
      adjustedLeft := boundsRect.Left;
    //and to help text to look just a tiny bit better ...
    adjustedLeft := Round(adjustedLeft);
  end;

begin
  //nb: this method requires horizontal text ...
  savedAngle := fAngle;
  savedSkewY := fSkew.sy;
  fAngle := 0;
  fSkew.sy := 0;
  try
    ttFont.GetTextMetrics(tm);
    lineHeight := round((tm.tmHeight + tm.tmExternalLeading) * fScale.sy);
    descenderSpace := tm.tmDescent * fScale.sy;
    maxLines :=
      trunc((boundsRect.Bottom - boundsRect.Top - descenderSpace) / lineHeight);

    //text is *much* clearer when the baseline is set to an integer ...
    yPos := round(boundsRect.Top + lineHeight - descenderSpace);
    //for vertical alignment, calculate the number of lines to be displayed ...
    if alignV <> aTop then
    begin
      theText := StripCRs(trim(Text));
      lineCnt := 0;
      textBeforeLineFeed := False;
      while length(theText) > 0 do
      begin
        if (lineCnt > maxLines) then
          break;
        if (yPos + descenderSpace > boundsRect.Bottom) then
        begin
          lineCnt := maxLines + 1;
          break;
        end;
        GetAdjustedLeft;
        cntChrsThatFit := CountCharsThatFit(adjustedLeft, yPos, theText, ttFont, boundsRect, forceWordBreak);
        if cntChrsThatFit = 0 then
        begin
          lineCnt := maxLines + 1;
          break;
        end;
        if (cntChrsThatFit = 1) and (theText[1] = LF) then
        begin
          if textBeforeLineFeed then
            yPos := yPos - lineHeight
          else
            Inc(lineCnt);
          textBeforeLineFeed := False;
        end
        else
        begin
          textBeforeLineFeed := True;
          Inc(lineCnt);
        end;
        while theText[cntChrsThatFit + 1] <= SPACE do
          Inc(cntChrsThatFit);
        Delete(theText, 1, cntChrsThatFit);
        yPos := yPos + lineHeight;
      end;
      //now adjust starting the yPos based on the number of displayed lines ...
      yPos := boundsRect.Top + lineHeight - descenderSpace;
      if lineCnt <= maxLines then
        case alignV of
          aMiddle: yPos := boundsRect.Top + (lineHeight - descenderSpace) + (boundsRect.Bottom - boundsRect.Top - lineCnt * lineHeight) / 2;
          aBottom:
            yPos := boundsRect.Bottom - 1 - descenderSpace - (lineCnt - 1) * lineHeight;
        end;
      //text is *much* clearer when the baseline is set to an integer ...
      yPos := round(yPos);
    end;

    textBeforeLineFeed := False;
    theText := StripCRs(trim(Text));
    len := length(theText);
    spaceWidth := GetTextWidth(' ', ttFont);
    while len > 0 do
    begin
      if yPos + descenderSpace > boundsRect.Bottom then
        break;
      GetAdjustedLeft;
      cntChrsThatFit := CountCharsThatFit(adjustedLeft, yPos, theText, ttFont, boundsRect, forceWordBreak);
      if cntChrsThatFit = 0 then
        break
      else if (cntChrsThatFit = 1) and (theText[1] = LF) then
      begin
        Delete(theText, 1, 1);
        Dec(len);
        if not textBeforeLineFeed then
          yPos := yPos + lineHeight;
        textBeforeLineFeed := False;
        continue;
      end;
      textBeforeLineFeed := True;

      //ie trim any trailing spaces in 'theLine' ...
      while theText[cntChrsThatFit] <= SPACE do
        Dec(cntChrsThatFit);
      theLine := copy(theText, 1, cntChrsThatFit);

      case alignH of
        aLeft:
        begin
          i := length(Result);
          setlength(Result, i + 1);
          Result[i] := Get(adjustedLeft, yPos, theLine, ttFont, pt);
        end;
        aRight:
        begin
          lineWidth := GetTextWidth(theLine, ttFont);
          i := length(Result);
          setlength(Result, i + 1);
          Result[i] := Get(boundsRect.Right - lineWidth, yPos, theLine, ttFont, pt);
        end;
        aCenter:
        begin
          lineWidth := GetTextWidth(theLine, ttFont);
          i := length(Result);
          setlength(Result, i + 1);
          with boundsRect do
            Result[i] := Get(adjustedLeft + (Right - Left - lineWidth) / 2, yPos, theLine, ttFont, pt);
        end;
        aJustify:
        begin
          lineWidth := GetTextWidth(theLine, ttFont);
          //count no. spaces in theLine ...
          spcCnt := 0;
          for i := 1 to length(theLine) do
            if theLine[i] <= SPACE then
              Inc(spcCnt);
          j := cntChrsThatFit + 1;
          while (j < len) and (theText[j] <= SPACE) do
            Inc(j);
          if (spcCnt = 0) or (cntChrsThatFit = len) or (theText[j] = LF) then
          begin
            i := length(Result);
            setlength(Result, i + 1);
            Result[i] := Get(adjustedLeft, yPos, theLine, ttFont, pt);
          end
          else
          begin
            with boundsRect do
            begin
              spaceChrPadding := round((Right - Left - lineWidth) / spcCnt);
              lastSpaceChrPadding := spaceChrPadding - (spaceChrPadding * spcCnt - (Right - Left - lineWidth));
            end;
            pt := FloatPoint(adjustedLeft, yPos);

            //break up 'theLine' into words so there's less work for Draw ...
            j := length(theLine);
            while j > 0 do
            begin
              i := 1;
              while (i < j) and (theLine[i + 1] <> SPACE) do
                Inc(i);
              k := length(Result);
              setlength(Result, k + 1);
              Result[k] := Get(pt.X, pt.Y, copy(theLine, 1, i), ttFont, pt);
              Inc(i);
              while (i <= j) and (theLine[i] <= SPACE) do
              begin
                Dec(spcCnt);
                if spcCnt = 1 then
                  pt.X := pt.X + spaceWidth + lastSpaceChrPadding
                else
                  pt.X := pt.X + spaceWidth + spaceChrPadding;
                Inc(i);
              end;
              Delete(theLine, 1, i - 1);
              j := length(theLine);
            end;
          end;
        end;
      end;
      while theText[cntChrsThatFit + 1] <= SPACE do
        Inc(cntChrsThatFit);
      Delete(theText, 1, cntChrsThatFit);
      Dec(len, cntChrsThatFit);
      yPos := yPos + lineHeight;
    end;
  finally
    fAngle := savedAngle;
    fSkew.sy := savedSkewY;
  end;

  if (fAngle <> 0) and assigned(Result) then
  begin
    a := Angle * DegToRad;
    with boundsRect do
      origin := FixedPoint((Left + right) / 2, (top + bottom) / 2);
    for i := 0 to high(Result) do
      for j := 0 to high(Result[i]) do
        Result[i][j] := RotatePoints(Result[i][j], origin, a);
  end;
end;
//------------------------------------------------------------------------------
{$WARNINGS ON}

function TText32.CountCharsThatFit(X, Y: single; const Text: UnicodeString; ttFont: TTrueTypeFont; const boundsRect: TFloatRect;
  forceWordBreak: boolean = False): integer;
var
  i, j, len: integer;
  thisWord: UnicodeString;
  ip, nextIp: TFixedPoint;
  rec, boundsRec: TFixedRect;
begin
  Result := 0;
  len := length(Text);
  if len = 0 then
    exit;
  j := 1;
  //nb: Character outlines can sometimes (and regularly with skewing)
  //begin a fraction of a pixel before X. If the first glyph's outline in
  //'text' does overlap X when X = boundRect.left then this method could
  //return 0 because the glyph doesn't strictly fit within boundRect. Hence ...
  if X - 1 <= boundsRect.Left then
    X := X + 1;
  ip := FixedPoint(X, Y);
  boundsRec := FixedRect(boundsRect);
  if not FixedPtInRect(boundsRec, ip) then
    exit;

  if Text[1] = LF then
  begin
    Result := 1;
    exit;
  end;

  while j <= len do
  begin
    i := j;
    //try to break the text on a word break ...
    //(nb: to avoid CharInSet, we check for TAB & SPACE separately)
    while (j <= len) and ((Text[j] = TAB) or (Text[j] = SPACE)) do
      Inc(j); //ignore leading spaces
    if (j > len) then
      break
    else if (Text[j] = LF) then
      break
    else
      while (j <= len) and (Text[j] > SPACE) do
        Inc(j); //find trailing space
    thisWord := copy(Text, i, j - i);
    GetTextMetrics(thisWord, ttFont, ip, nextIp, rec);
    if (rec.Left + 1 < boundsRec.Left) or (rec.Right > boundsRec.Right) or (rec.Top < boundsRec.Top) or (rec.Bottom > boundsRec.Bottom) then
    begin
      //if at least one word fits then quit ...
      if (Result > 0) or ForceWordBreak then
        exit;
      //since a whole word doesn't fit, find the max no. chars that do ...
      j := 1;
      while j <= len do
      begin
        thisWord := copy(Text, 1, j);
        GetTextMetrics(thisWord, ttFont, ip, nextIp, rec);
        if (rec.Left < boundsRec.Left) or (rec.Right > boundsRec.Right) or (rec.Top < boundsRec.Top) or (rec.Bottom > boundsRec.Bottom) then

          exit;
        Result := j;
        Inc(j);
      end;
      exit;
    end;
    Result := j - 1;
    ip := nextIp;
  end;
end;
//------------------------------------------------------------------------------

procedure TText32.PrepareMatrices;
begin
  fGlyphMatrix := identity_mat2;
  if mirrored then
    fGlyphMatrix.eM11.Value := -1;
  if inverted then
    fGlyphMatrix.eM22.Value := -1;
  fCurrentPosMatrix := fGlyphMatrix;
  if (fScale.sx <> 1) or (fScale.sy <> 1) then
    ScaleMat2(fGlyphMatrix, fScale.sx, fScale.sy);
  if (fScale.sx <> 1) then
    ScaleMat2(fCurrentPosMatrix, fScale.sx, 1);
  fUnrotatedMatrix := fGlyphMatrix;
  if (fSkew.sx <> 0) or (fSkew.sy <> 0) then
  begin
    SkewMat2(fGlyphMatrix, fSkew.sx, fSkew.sy);
    if (fSkew.sx <> 0) then
      SkewMat2(fUnrotatedMatrix, fSkew.sx, 0);
    if (fSkew.sy <> 0) then
      SkewMat2(fCurrentPosMatrix, 0, -fSkew.sy);
  end;
  if (fAngle <> 0) then
  begin
    RotateMat2(fGlyphMatrix, fAngle * degToRad);
    RotateMat2(fCurrentPosMatrix, -fAngle * degToRad);
  end;
end;
//------------------------------------------------------------------------------

procedure TText32.GetTextMetrics(const Text: UnicodeString; ttFont: TTrueTypeFont; const InsertionPt: TFixedPoint;
  out NextInsertionPt: TFixedPoint; out BoundsRect: TFixedRect);
var
  i, len: integer;
  p: TFloatPoint;
  gm: TGlyphMetrics;
  fixedRec2: TFixedRect;
  tmpPolyPts: TArrayOfArrayOfFixedPoint;
begin
  NextInsertionPt.X := InsertionPt.X + Fixed(fTranslate.sx);
  NextInsertionPt.Y := InsertionPt.Y + Fixed(fTranslate.sy);
  with NextInsertionPt do
    BoundsRect := FixedRect(X, Y, X, Y);
  len := length(Text);
  if (len = 0) or not assigned(ttFont) then
    exit;
  PrepareMatrices;

  if not ttFont.Lock then
    exit;
  try
    for i := 1 to len do
    begin
      //get each untransformed glyph ...
      if not ttFont.GetGlyphInfo(Text[i], gm, tmpPolyPts) then
        break;

      if length(tmpPolyPts) = 0 then //ie if a space
      begin
        setlength(tmpPolyPts, 1);
        setlength(tmpPolyPts[0], 2);
        tmpPolyPts[0][0] := FixedPoint(0, 0);
        tmpPolyPts[0][1] := FixedPoint(gm.gmCellIncX, 0);
      end;

      //do matrix transformations ...
      TransformPolyPoints(fGlyphMatrix, NextInsertionPt, tmpPolyPts);

      //enlarge BoundsRect to fit the glyph ...
      fixedRec2 := GetBoundsFixedRect(tmpPolyPts);
      BoundsRect := GetRectUnion(BoundsRect, fixedRec2);

      //finally update the current position ...
      p := FloatPoint(gm.gmCellIncX + fPadding, gm.gmCellIncY);
      TransformPoint(fCurrentPosMatrix, p);
      NextInsertionPt.X := NextInsertionPt.X + Fixed(p.X);
      NextInsertionPt.Y := NextInsertionPt.Y - Fixed(p.Y);
    end;
  finally
    ttFont.Unlock;
  end;
end;
//------------------------------------------------------------------------------

procedure TText32.GetDrawInfo(const Text: UnicodeString; ttFont: TTrueTypeFont; const InsertionPt: TFloatPoint;
  out NextInsertionPt: TFloatPoint; out polyPolyPts: TArrayOfArrayOfArrayOfFixedPoint);
var
  i, len: integer;
  gm: TGlyphMetrics;
  cp, p: TFloatPoint;
begin
  if (Text = '') or not assigned(ttFont) then
    exit;
  PrepareMatrices;

  //cp := FloatPoint(InsertionPt.X+ fTranslate.sx, InsertionPt.Y + fTranslate.sy);

  //note: text is clearest when derived using integer coordinates...
  cp := FloatPoint(round(InsertionPt.X + fTranslate.sx), round(InsertionPt.Y + fTranslate.sy));

  if not ttFont.Lock then
    exit;
  try
    len := length(Text);
    setlength(polyPolyPts, len);
    for i := 0 to len - 1 do
    begin
      //first, get each untransformed glyph ...
      if not ttFont.GetGlyphInfo(Text[i + 1], gm, polyPolyPts[i]) then
        break;

      //do matrix transformations ...
      TransformPolyPoints(fGlyphMatrix, FixedPoint(cp), polyPolyPts[i]);

      //update the current position ...
      p := FloatPoint(gm.gmCellIncX + fPadding, gm.gmCellIncY);
      TransformPoint(fCurrentPosMatrix, p);

      cp.X := cp.X + p.X;
      cp.Y := cp.Y - p.Y;
    end;
  finally
    ttFont.UnLock;
  end;
  NextInsertionPt.X := cp.X - fTranslate.sx;
  NextInsertionPt.Y := cp.Y - fTranslate.sy;
end;
//------------------------------------------------------------------------------

procedure TText32.Draw(bitmap: TBitmap32; const Text: UnicodeString; ttFont: TTrueTypeFont; color: TColor32);
var
  i: integer;
  fontCacheAssigned: boolean;
  ppts: TArrayOfArrayOfArrayOfFixedPoint;
  pos: TFloatPoint;
  ppts2: TArrayOfArrayOfFloatPoint;
  PolyPolygonFn: TPolyPolygonFunc;
begin
  if Text = '' then
    exit;
  PolyPolygonFn := PolyPolygonFunction[fLCDDraw]^;
  fontCacheAssigned := assigned(ttFont);
  if not fontCacheAssigned then
    ttFont := TTrueTypeFont.Create(bitmap.Font);
  try
    GetDrawInfo(Text, ttFont, FloatPoint(fCurrentPos), pos, ppts);
    for i := 0 to high(ppts) do
    begin
      ppts2 := MakeArrayOfArrayOfFloatPoints(ppts[i]);
      PolyPolygonFn(bitmap, ppts2, color, pfWinding);
    end;
    fCurrentPos := FixedPoint(pos);
  finally
    if not fontCacheAssigned then
      ttFont.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TText32.Draw(bitmap: TBitmap32; X, Y: single; const Text: UnicodeString; ttFont: TTrueTypeFont; color: TColor32);
begin
  SetCurrentPos(FixedPoint(X, Y));
  Draw(bitmap, Text, ttFont, color);
end;
//------------------------------------------------------------------------------

procedure TText32.Draw(bitmap: TBitmap32; const path: array of TFixedPoint; const Text: UnicodeString; ttFont: TTrueTypeFont;
  color: TColor32; alignH: TAlignH = aCenter; alignV: TAlignV = aMiddle; offsetFromLine: single = 0);
var
  i: integer;
  glyphPts: TArrayOfArrayOfArrayOfFixedPoint;
  ppts2: TArrayOfArrayOfFloatPoint;
  PolyPolygonFn: TPolyPolygonFunc;
begin
  if Text = '' then
    exit;
  glyphPts := GetEx(path, Text, ttFont, alignH, alignV, True, offsetFromLine);
  PolyPolygonFn := PolyPolygonFunction[fLCDDraw]^;
  for i := 0 to high(glyphPts) do
    ppts2 := MakeArrayOfArrayOfFloatPoints(glyphPts[i]);
  PolyPolygonFn(bitmap, ppts2, color, pfWinding);
end;
//------------------------------------------------------------------------------

procedure TText32.DrawAndOutline(bitmap: TBitmap32; X, Y: single; const Text: UnicodeString; ttFont: TTrueTypeFont;
  outlinePenWidth: single; outlineColor, fillColor: TColor32);
var
  i: integer;
  ppts: TArrayOfArrayOfArrayOfFixedPoint;
  NextInsertionPt: TFloatPoint;
  ppts2: TArrayOfArrayOfFloatPoint;
  PolyPolygonFn: TPolyPolygonFunc;
begin
  if Text = '' then
    exit;
  ppts := GetEx(X, Y, Text, ttFont, NextInsertionPt);

  PolyPolygonFn := PolyPolygonFunction[fLCDDraw]^;
  for i := 0 to high(ppts) do
  begin
    ppts2 := MakeArrayOfArrayOfFloatPoints(ppts[i]);
    if AlphaComponent(fillColor) > 0 then
      PolyPolygonFn(bitmap, ppts2, fillColor, pfWinding);
    PolyPolylineFS(bitmap, ppts2, outlineColor, True, outlinePenWidth);
  end;
end;
//------------------------------------------------------------------------------

function TText32.Get(const path: array of TFixedPoint; const Text: UnicodeString; ttFont: TTrueTypeFont; alignH: TAlignH;
  alignV: TAlignV; rotateTextToPath: boolean; offsetFromLine: single = 0): TArrayOfArrayOfFixedPoint;
var
  i: integer;
  tmp: TArrayOfArrayOfArrayOfFixedPoint;
begin
  Result := nil;
  tmp := GetEx(path, Text, ttFont, alignH, alignV, rotateTextToPath, offsetFromLine);
  for i := 0 to high(tmp) do
    ConcatenatePolyPoints(Result, tmp[i]);
end;
//------------------------------------------------------------------------------

function TText32.GetEx(const path: array of TFixedPoint; const Text: UnicodeString; ttFont: TTrueTypeFont;
  alignH: TAlignH; alignV: TAlignV; rotateTextToPath: boolean; offsetFromLine: single = 0): TArrayOfArrayOfArrayOfFixedPoint;
var
  i, j, k, chrCnt, startChrIdx, endChrIdx: integer;
  rec, tmpRec: TFixedRect;
  pathLen, dy: single;
  cumulDists: array of single;
  normals: array of TFloatPoint;
  nextOffset, xOffset: single;
  gm: TGlyphMetrics;
  thePath: TArrayOfFixedPoint;

  procedure TransformPt(var pt: TFixedPoint);
  var
    i, highI: integer;
    X, Y, dx, dist: single;
    pathPt: TFixedPoint;
  begin
    if alignH = aJustify then
      X := pt.X * FixedToFloat * pathLen / (rec.Right * FixedToFloat)
    else
      X := (pt.X + xOffset) * FixedToFloat;
    i := 0;
    highI := high(thePath);
    while (i < highI) and (X > cumulDists[i + 1]) do
      Inc(i);
    dx := (X - cumulDists[i]) / (cumulDists[i + 1] - cumulDists[i]);
    pathPt.X := thePath[i].X + Round(dx * (thePath[i + 1].X - thePath[i].X));
    pathPt.Y := thePath[i].Y + Round(dx * (thePath[i + 1].Y - thePath[i].Y));
    if rotateTextToPath then
    begin
      dist := -pt.Y;
      //the strictly 'correct' positions for rotated points is to use
      //normal[i], but this can badly distort text passing over line joins.
      //To lessen this distortion it's worth graduating angle changes for
      //points close to joins (ie within a pixel or two of a join) ...
      if ((1 - dx) * (cumulDists[i + 1] - cumulDists[i]) < 2) then
      begin
        X := (1 - dx) * normals[i].X + dx * normals[i + 1].X;
        Y := (1 - dx) * normals[i].Y + dx * normals[i + 1].Y;
        pt.X := pathPt.X + round(X * dist);
        pt.Y := pathPt.Y + round(Y * dist);
      end
      else
      begin
        pt.X := pathPt.X + round(normals[i].X * dist);
        pt.Y := pathPt.Y + round(normals[i].Y * dist);
      end;
    end
    else
    begin
      pt.X := pathPt.X;
      pt.Y := pathPt.Y + pt.Y;
    end;
  end;

begin
  Result := nil;
  chrCnt := length(Text);
  //it's important to strip duplicate points otherwise normals are buggy ...
  thePath := StripDuplicatePoints(path);
  if (chrCnt = 0) or (length(thePath) < 2) then
    exit;

  setLength(cumulDists, length(thePath));
  setLength(normals, length(thePath));
  cumulDists[0] := 0;
  pathLen := 0;
  for i := 1 to high(thePath) do
  begin
    pathLen := pathLen + DistBetweenPoints(thePath[i - 1], thePath[i]);
    cumulDists[i] := pathLen;
    normals[i - 1] := GetUnitNormal(thePath[i - 1], thePath[i]);
  end;
  i := high(thePath);
  normals[i] := normals[i - 1];

  nextOffset := 0;
  PrepareMatrices;

  case alignV of
    aBottom: dy := GetTextFixedRect(0, 0, Text, ttFont).bottom * FixedToFloat;
    aTop: dy := GetTextFixedRect(0, 0, Text, ttFont).top * FixedToFloat - 4;
    else
      dy := 0;
  end;
  dy := dy + offsetFromLine + 2;

  if not ttFont.Lock then
    exit;
  try
    setlength(Result, chrCnt);
    for i := 0 to chrCnt - 1 do
    begin
      //get each untransformed glyph ...
      if not ttFont.GetGlyphInfo(Text[i + 1], gm, Result[i]) then
        break;

      //do matrix transformations ...
      TransformPolyPoints(fUnrotatedMatrix, FixedPoint(0, -dy), Result[i]);

      //1. horizontally offset each set of glyph points and also
      //2. get the height and width of the text ...
      if i = 0 then
      begin
        rec := GetBoundsFixedRect(Result[i]);
        nextOffset := gm.gmCellIncX * fScale.sx + fPadding;
      end
      else
      begin
        OffsetPolyPoints(Result[i], nextOffset, 0);
        tmpRec := GetBoundsFixedRect(Result[i]);
        rec := GetRectUnion(rec, tmpRec);
        nextOffset := nextOffset + gm.gmCellIncX * fScale.sx + fPadding;
        if (i = chrCnt - 1) and (Text[chrCnt] = #32) then
          rec.Right := Fixed(nextOffset);
      end;

    end;
  finally
    ttFont.UnLock;
  end;

  if (rec.Left = rec.Right) or (rec.Top = rec.Bottom) then
    exit;

  case alignH of
    aCenter: xOffset := Fixed(pathLen / 2) - (rec.Right - rec.Left) / 2;
    aRight: xOffset := Fixed(pathLen) - (rec.Right - rec.Left);
    else
      xOffset := -rec.Left;
  end;

  //now trim any characters from result that won't fit on the path ...
  startChrIdx := 0;
  endChrIdx := chrCnt - 1;
  if alignH <> aJustify then
  begin
    while (endChrIdx >= 0) and (xOffset + GetBoundsFixedRect(Result[endChrIdx]).Right > Fixed(pathLen)) do
      Dec(endChrIdx);
    while (startChrIdx <= endChrIdx) and (xOffset + GetBoundsFixedRect(Result[startChrIdx]).Left < 0) do
      Inc(startChrIdx);
  end;
  if startChrIdx > 0 then
  begin
    for i := startChrIdx to endChrIdx do
      Result[i - startChrIdx] := CopyPolyPoints(Result[i]);
    setlength(Result, endChrIdx - startChrIdx + 1);
  end
  else if endChrIdx < chrCnt - 1 then
    setlength(Result, endChrIdx + 1);

  //finally, transform the points ...
  for i := 0 to high(Result) do
    for j := 0 to high(Result[i]) do
      for k := 0 to high(Result[i][j]) do
        TransformPt(Result[i][j][k]);
end;
//------------------------------------------------------------------------------

function TText32.GetBetweenPaths(const bottomCurve, topCurve: TArrayOfFixedPoint; const Text: UnicodeString;
  ttFont: TTrueTypeFont): TArrayOfArrayOfFixedPoint;
var
  i: integer;
  tmp: TArrayOfArrayOfArrayOfFixedPoint;
begin
  Result := nil;
  tmp := GetBetweenPathsEx(bottomCurve, topCurve, Text, ttFont);
  for i := 0 to high(tmp) do
    ConcatenatePolyPoints(Result, tmp[i]);
end;
//------------------------------------------------------------------------------

function TText32.GetBetweenPathsEx(const bottomCurve, topCurve: TArrayOfFixedPoint; const Text: UnicodeString;
  ttFont: TTrueTypeFont): TArrayOfArrayOfArrayOfFixedPoint;
var
  i, j, k, chrCnt: integer;
  rec, tmpRec: TFixedRect;
  textRec: TFloatRect;
  bCurve, tCurve: TArrayOfFloatPoint;
  bottomCurveLen, topCurveLen: single;
  bottomDistances, topDistances: array of single;
  nextOffset: single;
  gm: TGlyphMetrics;

  procedure TransformPt(var pt: TFixedPoint);
  var
    I, H: integer;
    X, Y, fx, dx, dy, r: TFloat;
    topPt, bottomPt: TFloatPoint;
  begin

    X := pt.X * FixedToFloat / textRec.Right;
    Y := (pt.Y * FixedToFloat - textRec.Top) / (textRec.Bottom - textRec.Top);

    fx := X * topCurveLen;
    I := 1;
    H := High(topDistances);
    while (topDistances[I] < fx) and (I < H) do
      Inc(I);
    if abs(topDistances[I] - topDistances[I - 1]) < 0.01 then
      r := 0
    else
      r := (topDistances[I] - fx) / (topDistances[I] - topDistances[I - 1]);
    dx := (tCurve[I - 1].X - tCurve[I].X);
    dy := (tCurve[I - 1].Y - tCurve[I].Y);
    topPt.X := tCurve[I].X + r * dx;
    topPt.Y := tCurve[I].Y + r * dy;

    fx := X * bottomCurveLen;
    I := 1;
    H := High(bottomDistances);
    while (bottomDistances[I] < fx) and (I < H) do
      Inc(I);
    if abs(bottomDistances[I] - bottomDistances[I - 1]) < 0.01 then
      r := 0
    else
      r := (bottomDistances[I] - fx) / (bottomDistances[I] - bottomDistances[I - 1]);
    dx := (bCurve[I - 1].X - bCurve[I].X);
    dy := (bCurve[I - 1].Y - bCurve[I].Y);
    bottomPt.X := bCurve[I].X + r * dx;
    bottomPt.Y := bCurve[I].Y + r * dy;

    pt.X := Fixed(topPt.X + Y * (bottomPt.X - topPt.X));
    pt.Y := Fixed(topPt.Y + Y * (bottomPt.Y - topPt.Y));
  end;

begin
  Result := nil;
  if (length(bottomCurve) < 2) or (length(topCurve) < 2) then
    exit;

  //get the lengths of both the bottom and top curves ...
  setLength(bottomDistances, length(bottomCurve));
  setLength(bCurve, length(bottomCurve));
  bottomDistances[0] := 0;
  bCurve[0] := FloatPoint(bottomCurve[0]);
  bottomCurveLen := 0;
  for i := 1 to high(bottomCurve) do
  begin
    bottomCurveLen := bottomCurveLen + DistBetweenPoints(bottomCurve[i - 1], bottomCurve[i]);
    bottomDistances[i] := bottomCurveLen;
    bCurve[i] := FloatPoint(bottomCurve[i]);
  end;
  setLength(topDistances, length(topCurve));
  setLength(tCurve, length(topCurve));
  topDistances[0] := 0;
  tCurve[0] := FloatPoint(topCurve[0]);
  topCurveLen := 0;
  for i := 1 to high(topCurve) do
  begin
    topCurveLen := topCurveLen + DistBetweenPoints(topCurve[i - 1], topCurve[i]);
    topDistances[i] := topCurveLen;
    tCurve[i] := FloatPoint(topCurve[i]);
  end;

  chrCnt := length(Text);
  if chrCnt = 0 then
    exit;

  nextOffset := 0;
  PrepareMatrices;

  if not ttFont.Lock then
    exit;
  try
    setlength(Result, chrCnt);
    for i := 0 to chrCnt - 1 do
    begin
      //get each untransformed glyph ...
      if not ttFont.GetGlyphInfo(Text[i + 1], gm, Result[i]) then
        break;

      //do matrix transformations ...
      TransformPolyPoints(fUnrotatedMatrix, FixedPoint(0, 0), Result[i]);

      //1. horizontally offset each set of glyph points and also
      //2. get the height and width of the text ...
      if i = 0 then
      begin
        rec := GetBoundsFixedRect(Result[i]);
        nextOffset := gm.gmCellIncX * fScale.sx + fPadding;
      end
      else
      begin
        OffsetPolyPoints(Result[i], nextOffset, 0);
        tmpRec := GetBoundsFixedRect(Result[i]);
        rec := GetRectUnion(rec, tmpRec);
        nextOffset := nextOffset + gm.gmCellIncX * fScale.sx + fPadding;
        if (i = chrCnt - 1) and (Text[chrCnt] = #32) then
          rec.Right := Fixed(nextOffset);
      end;

    end;
  finally
    ttFont.UnLock;
  end;

  if (rec.Left = rec.Right) or (rec.Top = rec.Bottom) then
    exit;
  textRec := FloatRect(rec);

  for i := 0 to high(Result) do
    for j := 0 to high(Result[i]) do
      for k := 0 to high(Result[i][j]) do
        TransformPt(Result[i][j][k]);
end;
//------------------------------------------------------------------------------

procedure TText32.SetLCDDraw(Value: boolean);
begin
  fLCDDraw := Value;
end;
//------------------------------------------------------------------------------

function TText32.GetCurrentPos: TFixedPoint;
begin
  Result := fCurrentPos;
end;
//------------------------------------------------------------------------------

procedure TText32.SetCurrentPos(newPos: TFixedPoint);
begin
  fCurrentPos := newPos;
end;
//------------------------------------------------------------------------------

function TText32.Get(X, Y: single; const Text: UnicodeString; ttFont: TTrueTypeFont; out NextInsertionPt: TFloatPoint): TArrayOfArrayOfFixedPoint;
var
  i: integer;
  tmp: TArrayOfArrayOfArrayOfFixedPoint;
begin
  Result := nil;
  tmp := GetEx(X, Y, Text, ttFont, NextInsertionPt);
  for i := 0 to high(tmp) do
    ConcatenatePolyPoints(Result, tmp[i]);
end;
//------------------------------------------------------------------------------

function TText32.GetEx(X, Y: single; const Text: UnicodeString; ttFont: TTrueTypeFont;
  out NextInsertionPt: TFloatPoint): TArrayOfArrayOfArrayOfFixedPoint;
begin
  GetDrawInfo(Text, ttFont, FloatPoint(X, Y), NextInsertionPt, Result);
end;
//------------------------------------------------------------------------------

function TText32.GetInflated(X, Y, delta: single; const Text: UnicodeString; ttFont: TTrueTypeFont;
  out NextInsertionPt: TFloatPoint): TArrayOfArrayOfFixedPoint;
var
  i: integer;
  tmp: TArrayOfArrayOfArrayOfFixedPoint;
begin
  Result := nil;
  tmp := GetInflatedEx(X, Y, delta, Text, ttFont, NextInsertionPt);
  for i := 0 to high(tmp) do
    ConcatenatePolyPoints(Result, tmp[i]);
end;
//------------------------------------------------------------------------------

function TText32.GetInflatedEx(X, Y, delta: single; const Text: UnicodeString; ttFont: TTrueTypeFont;
  out NextInsertionPt: TFloatPoint): TArrayOfArrayOfArrayOfFixedPoint;
var
  i, j, highI, highJ: integer;
  pppts: TArrayOfArrayOfArrayOfFloatPoint;
begin
  Result := GetEx(X, Y, Text, ttFont, NextInsertionPt);
  highI := high(Result);
  if (highI < 0) or (delta = 0) then
    exit;

  delta := delta / 2;
  setlength(pppts, highI + 1);
  for i := 0 to highI do
  begin
    highJ := high(Result[i]);
    pppts[i] := MakeArrayOfArrayOfFloatPoints(Result[i]);
    for j := 0 to highJ do
      pppts[i][j] := Grow(pppts[i][j], delta);
    Result[i] := MakeArrayOfArrayOfFixedPoints(pppts[i]);
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure SimpleText(bmp: TBitmap32; font: TFont; X, Y: integer; const widetext: WideString; color: TColor32);
var
  a: byte;
  sz: TSize;
  Src, Dst: PColor32;
  i: integer;
  b: TBitmap32;
begin
  b := TBitmap32.Create;
  try
    b.Width := bmp.Width;
    b.Height := bmp.Height;
    b.Clear($FFFFFFFF);
    b.DrawMode := dmOpaque;
    if assigned(font) then
      b.Canvas.Font.Assign(font)
    else
      b.Canvas.Font.Assign(bmp.font);
    b.Canvas.Font.Color := clBlack;

    with b.Canvas do
    begin
     { if CanvasOrientation = coRightToLeft then
      begin
        GetTextExtentPoint32W(Handle, PWideChar(widetext), Length(widetext), sz);
        Inc(X, sz.cx + 1);
      end;
      ExtTextOutW(Handle, X, Y, TextFlags,
        nil, PWideChar(widetext), Length(widetext), nil);    }
    end;

    Src := PColor32(b.Bits);
    Dst := PColor32(bmp.Bits);
    for i := 0 to bmp.Width * bmp.Height - 1 do
    begin
      if Src^ <> $FFFFFFFF then
      begin
        //a := Src^ and $FF;     //this isn't great with white text on black
        //a := Intensity(Src^);  //nor is this
        a := (((Src^ shr 16) and $FF) + ((Src^ shr 8) and $FF) + (Src^ and $FF)) div 3;
        if a < $F8 then
          BlendMemEx(Color, Dst^, 255 - a);
      end;
      Inc(Src);
      Inc(Dst);
    end;
    EMMS;
  finally
    b.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure SetLcdDrawDefault;
var
  fontSmoothingEnabled: longbool;
begin
  SystemParametersInfo(SPI_GETFONTSMOOTHING, 0, @fontSmoothingEnabled, 0);
  Text32LCDDrawDefault := fontSmoothingEnabled;
end;
//------------------------------------------------------------------------------

initialization
  SetLcdDrawDefault;

end.
