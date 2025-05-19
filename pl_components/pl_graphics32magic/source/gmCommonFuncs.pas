{*************************************************************************
  Package pl_Graphics32Magic
  This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)  

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/
 
  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the
  License.
 
  Initial Developers:
    Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com > 
    x2nie - Fathony Luthfillah  <x2nie@yahoo.com> 
  
**************************************************************************}

unit gmCommonFuncs;

interface

uses
  LCLIntf, LCLType, LMessages,types, Graphics,
  SysUtils, Math,
  GR32;

  // Make sure StartPoint is at the upper left and EndPoint is at the lower right.
  procedure PointStandardizeOrder(var StartPoint, EndPoint: TPoint);

  procedure GetCanvasProperties(ACanvas: TCanvas;
    var PenColor, BrushColor: TColor; var PenWidth: Integer;
    var PenStyle: TPenStyle; var PenMode: TPenMode;
    var BrushStyle: TBrushStyle);

  procedure SetCanvasProperties(ACanvas: TCanvas;
    const PenColor, BrushColor: TColor; const PenWidth: Integer;
    const PenStyle: TPenStyle; const PenMode: TPenMode;
    const BrushStyle: TBrushStyle);

  function GetPixelFormatString(const PixelFormat: TPixelFormat): string;
  function GetBitmapDimensionString(const ABitmap: TBitmap32): string;
  function GetPenStyleString(const PenStyle: TPenStyle): string;
  function GetBrushStyleString(const BrushStyle: TBrushStyle): string;

implementation

// Make sure StartPoint is at the upper left and EndPoint is at the lower right.
procedure PointStandardizeOrder(var StartPoint, EndPoint: TPoint);
var
  TempA, TempB: TPoint;
begin
  TempA := StartPoint;
  TempB := EndPoint;

  // StartPoint is at the upper left.
  StartPoint.X := MinIntValue([TempA.X, TempB.X]);
  StartPoint.Y := MinIntValue([TempA.Y, TempB.Y]);
  
  // EndPoint is at the lower right.
  EndPoint.X := MaxIntValue([TempA.X, TempB.X]);
  EndPoint.Y := MaxIntValue([TempA.Y, TempB.Y]);
end;

procedure GetCanvasProperties(ACanvas: TCanvas; var PenColor, BrushColor: TColor;
  var PenWidth: Integer; var PenStyle: TPenStyle; var PenMode: TPenMode;
  var BrushStyle: TBrushStyle);
begin
  with ACanvas do
  begin
    PenColor   := Pen.Color;
    PenWidth   := Pen.Width;
    PenStyle   := Pen.Style;
    PenMode    := Pen.Mode;
    BrushColor := Brush.Color;
    BrushStyle := Brush.Style;
  end;
end;

procedure SetCanvasProperties(ACanvas: TCanvas;
  const PenColor, BrushColor: TColor; const PenWidth: Integer;
  const PenStyle: TPenStyle; const PenMode: TPenMode;
  const BrushStyle: TBrushStyle);
begin
  with ACanvas do
  begin
    Pen.Color   := PenColor;
    Pen.Width   := PenWidth;
    Pen.Style   := PenStyle;
    Pen.Mode    := PenMode;
    Brush.Color := BrushColor;
    Brush.Style := BrushStyle;
  end;
end;

function GetPixelFormatString(const PixelFormat: TPixelFormat): string;
var
  Format: string;
begin
  case PixelFormat of
    pfDevice: Format := 'Device';
    pf1bit  : Format := '1 bit';
    pf4bit  : Format := '4 bit';
    pf8bit  : Format := '8 bit';
    pf15bit : Format := '15 bit';
    pf16bit : Format := '16 bit';
    pf24bit : Format := '24 bit';
    pf32bit : Format := '32 bit'
    else      Format := 'Unknown';
  end;
  
  Result := Format;
end;

function GetBitmapDimensionString(const ABitmap: TBitmap32): string;
begin
  Result := IntToStr(ABitmap.Width) + ' x ' + IntToStr(ABitmap.Height);
end;

function GetPenStyleString(const PenStyle: TPenStyle): string;
var
  s: string;
begin
  case PenStyle of
    psSolid      : s := 'Solid';
    psDash       : s := 'Dash';
    psDot        : s := 'Dot';
    psDashDot    : s := 'Dash Dot';
    psDashDotDot : s := 'Dash Double Dot';
    psClear      : s := 'Clear';
    psInsideFrame: s := 'Inside Frame';
  end;
  
  Result := s;
end;

function GetBrushStyleString(const BrushStyle: TBrushStyle): String;
var
  s: string;
begin
  case BrushStyle of
    bsSolid     : s := 'Solid';
    bsClear     : s := 'Clear';
    bsBDiagonal : s := 'Backward Diagonal';
    bsFDiagonal : s := 'Foreward Diagonal';
    bsCross     : s := 'Cross';
    bsDiagCross : s := 'Diagonal Cross';
    bsHorizontal: s := 'Horizontal';
    bsVertical  : s := 'Vertical';
  end;
  
  Result := s;
end; 

end.
