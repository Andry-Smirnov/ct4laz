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

unit gmUtilsRealColorLibrary;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, LMessages, types,
  Graphics, Math,
  gmUtilsIEEE754,
  SysUtils,
  gmUtilsImgProcPrimitives;

type
  TRealToRGBConversion = procedure(const x: TReal; var R, G, B: byte);

  TRealColorMatrix =   // pf32bit Bitmap, or matrix of single float values
    class(TBitmap)
  private
    function GetReal(i, j: integer): TReal;
    procedure SetReal(i, j: integer; Value: TReal);

  public
    constructor Create; override;
    procedure ConvertRealMatrixToRGBImage(ConversionFunction: TRealToRGBConversion);
    property Element[i, j: integer]: TReal read GetReal write SetReal; default;

  end;


// Color Conversions

// HLS
procedure HLStoRGB(const H, L, S: TReal; var R, G, B: TReal);
procedure RGBToHLS(const R, G, B: TReal; var H, L, S: TReal);

// HSV
procedure HSVtoRGB(const H, S, V: TReal; var R, G, B: TReal);
procedure RGBToHSV(const R, G, B: TReal; var H, S, V: TReal);

// CMY
procedure CMYtoRGB(const C, M, Y: TReal; var R, G, B: TReal);
procedure RGBToCMY(const R, G, B: TReal; var C, M, Y: TReal);

// CMYK
procedure CMYKtoRGB(const C, M, Y, K: TReal; var R, G, B: TReal);
procedure RGBToCMYK(const R, G, B: TReal; var C, M, Y, K: TReal);


// Color Matrices:  All Bitmap parameters are pf24bit bitmaps
procedure BitmapToRGBMatrices(const Bitmap: TBitmap; var Red, Green, Blue: TRealColorMatrix);
function RGBMatricesToBitmap(const Red, Green, Blue: TRealColorMatrix): TBitmap;

procedure RGBMatricesToHSVMatrices(const Red, Green, Blue: TRealColorMatrix;
  var Hue, Saturation, Value: TRealColorMatrix);
procedure HSVMatricesToRGBMatrices(const Hue, Saturation, Value: TRealColorMatrix;
  var Red, Green, Blue: TRealColorMatrix);


implementation

type
  EColorError = class(Exception);

// == TColorMatrix ====================================================



constructor TRealColorMatrix.Create;
begin
  ASSERT(SizeOf(TRGBQuad) = SizeOf(TReal));  // to be sure

  inherited Create;
  PixelFormat := pf32bit;   // Each "pixel" is a "single"
end {Create};


procedure TRealColorMatrix.ConvertRealMatrixToRGBImage(ConversionFunction: TRealToRGBConversion);
var
  FloatRow: pSingleArray;
  i: integer;
  j: integer;
  PixelRow: pRGBQuadArray;
  R, G, B: byte;
begin
  for j := 0 to Height - 1 do
  begin
    PixelRow := Scanline[j];
    FloatRow := Scanline[j];
    for i := 0 to Width - 1 do
    begin
      ConversionFunction(FloatRow[i], R, G, B);
      with  PixelRow[i] do
      begin
        rgbRed := R;
        rgbGreen := G;
        rgbBlue := B;
        rgbReserved := 0;
      end;
    end;
  end;
end {ConvertSingleToRGB};


function TRealColorMatrix.GetReal(i, j: integer): TReal;
begin
  Result := pSingleArray(Scanline[j])[i];
end {GetSingle};


procedure TRealColorMatrix.SetReal(i, j: integer; Value: TReal);
begin
  pSingleArray(Scanline[j])[i] := Value;
end {SetSingle};


// == HLS / RGB =======================================================

// Based on C Code in "Computer Graphics -- Principles and Practice,"
// Foley et al, 1996, p. 596.

procedure HLStoRGB(const H, L, S: TReal; var R, G, B: TReal);
var
  m1: TReal;
  m2: TReal;

  function Value(const n1, n2: TReal; hue: TReal): TReal;
  begin
    if hue > 360.0 then
      hue := hue - 360.0
    else
    if hue < 0.0 then
      hue := hue + 360.0;

    if hue < 60.0 then
      Result := n1 + (n2 - n1) * hue / 60.0
    else
    if hue < 180 then
      Result := n2
    else
    if hue < 240.0 then
      Result := n1 + (n2 - n1) * (240.0 - hue) / 60.0
    else
      Result := n1;
  end {Value};

begin

  // There is an error in Computer Graphics Principles and Practice,
  // Foley, et al, 1996, pp. 592-596.  The formula uses a lower case
  // "el", "l", and defines in C:

  //      m2 = (l < 0.5) ? (l * (l+s)):(l+s-l*s)

  // This is a perfect example of why "l" (a lower case "el") should
  // NEVER be used as a variable name, and why a programming convention --
  // to use lower case letters in variable names -- should not override
  // the problem definition, which defines the color space using an "L".
  // The 1982 version of the book, in Pascal, shows the correct formula
  // (but alas, also used a lower case "l"):

  //      if   l <= 0.5
  //      then m2 := l*(1+s)    //  NOTE the one, in "1+s", here
  //      else m2 := l + s - l*s 

  // [Thanks to Gary Freestone, IBM Global Services Australia, for
  // bringing this to my attention.  efg, Sept. 2001]

  if L <= 0.5 then
    m2 := L * (1 + S)
  else
    m2 := L + S - L * S;

  m1 := 2.0 * L - m2;

  if S = 0.0 then
  begin      // achromatic -- no hue
    if IsNAN(H) then
    begin
      R := L;
      G := L;
      B := L;
    end
    else
      raise EColorError.Create('HLStoRGB:  S = 0 and H has a value');
  end
  else
  begin
    // Chromatic case -- there is a hue
    R := Value(m1, m2, H + 120.0);
    G := Value(m1, m2, H);
    B := Value(m1, m2, H - 120.0);
  end;
end {HLStoRGB};


// H = 0 to 360 (corresponding to 0..360 degrees around hexcone)
// L = 0.0 (shade of gray) to 1.0 (pure color)
// S = 0.0 (black)         to 1.0 {white)

// R, G, B each in [0,1]

// Based on C Code in "Computer Graphics -- Principles and Practice,"
// Foley et al, 1996, p. 595.

procedure RGBToHLS(const R, G, B: TReal; var H, L, S: TReal);
var
  Delta: TReal;
  Max: TReal;
  Min: TReal;
begin
  Max := MaxValue([R, G, B]);
  Min := MinValue([R, G, B]);

  L := (Max + Min) / 2.0;   // Lightness

  if Max = Min            // Achromatic case since r = g = b
  then
  begin
    S := 0.0;
    H := NAN;               // Undefined
  end
  else
  begin
    Delta := Max - Min;

    if L <= 0.5 then
      S := Delta / (Max + Min)
    else
      S := Delta / (2.0 - (Max + Min));

    if R = Max then // degrees between yellow and magenta
      H := (60.0 * (G - B)) / Delta
    else
    if G = Max then // degrees between cyan and yellow
      H := 120.0 + (60.0 * (B - R)) / Delta
    else
    if B = Max then // degrees between magenta and cyan
      H := 240.0 + (60.0 * (R - G)) / Delta;

    if H < 0 then
      H := H + 360.0;  // Keep in interval [0, 360)

  end;
end {RGBtoHLS};


// == HSV / RGB =======================================================

// Based on C Code in "Computer Graphics -- Principles and Practice,"
// Foley et al, 1996, p. 593.

//  H = 0.0 to 360.0 (corresponding to 0..360 degrees around hexcone)
//      NaN (undefined) for S = 0
//  S = 0.0 (shade of gray) to 1.0 (pure color)
//  V = 0.0 (black)         to 1.0 (white)

procedure HSVtoRGB(const H, S, V: TReal; var R, G, B: TReal);
var
  f: TReal;
  i: integer;
  hTemp: TReal;              // since H is CONST parameter
  p, q, t: TReal;
begin
  if S = 0.0                  // color is on black-and-white center line
  then
  begin
    if IsNaN(H) then
    begin
      R := V;                   // achromatic:  shades of gray
      G := V;
      B := V;
    end
    else
      raise EColorError.Create('HSVtoRGB:  S = 0 and H has a value');
  end

  else
  begin                    // chromatic color
    if H = 360.0              // 360 degrees same as 0 degrees
    then
      hTemp := 0.0
    else
      hTemp := H;

    hTemp := hTemp / 60;        // h is now IN [0,6)
    i := TRUNC(hTemp);          // largest integer <= h
    f := hTemp - i;             // fractional part of h

    p := V * (1.0 - S);
    q := V * (1.0 - (S * f));
    t := V * (1.0 - (S * (1.0 - f)));

    case i of
      0:
      begin
        R := V;
        G := t;
        B := p;
      end;
      1:
      begin
        R := q;
        G := V;
        B := p;
      end;
      2:
      begin
        R := p;
        G := V;
        B := t;
      end;
      3:
      begin
        R := p;
        G := q;
        B := V;
      end;
      4:
      begin
        R := t;
        G := p;
        B := V;
      end;
      5:
      begin
        R := V;
        G := p;
        B := q;
      end
    end;
  end;
end {HSVtoRGB};


// RGB, each 0 to 255, to HSV.
//   H = 0.0 to 360.0 (corresponding to 0..360.0 degrees around hexcone)
//   S = 0.0 (shade of gray) to 1.0 (pure color)
//   V = 0.0 (black) to 1.0 {white)

// Based on C Code in "Computer Graphics -- Principles and Practice,"
// Foley et al, 1996, p. 592.  Floating point fractions, 0..1, replaced with
// integer values, 0..255.

procedure RGBToHSV(const R, G, B: TReal; var H, S, V: TReal);
var
  Delta: TReal;
  Min: TReal;
begin
  Min := MinValue([R, G, B]);
  V := MaxValue([R, G, B]);

  Delta := V - Min;

  // Calculate saturation:  saturation is 0 if r, g and b are all 0
  if V = 0.0 then
    S := 0
  else
    S := Delta / V;

  if S = 0.0 then
    H := NAN // Achromatic:  When s = 0, h is undefined
  else
  begin    // Chromatic
    if R = V then  // between yellow and magenta [degrees]
      H := 60.0 * (G - B) / Delta
    else
    if G = V then // between cyan and yellow
      H := 120.0 + 60.0 * (B - R) / Delta
    else
    if B = V then // between magenta and cyan
      H := 240.0 + 60.0 * (R - G) / Delta;

    if H < 0.0 then
      H := H + 360.0;
  end;
end {RGBtoHSV};


// == CMY / RGB =======================================================

// Based on C Code in "Computer Graphics -- Principles and Practice,"
// Foley et al, 1996, p. 588

// R, G, B, C, M, Y each IN [0.0 .. 1.0]
procedure CMYtoRGB(const C, M, Y: TReal; var R, G, B: TReal);
begin
  R := 1.0 - C;
  G := 1.0 - M;
  B := 1.0 - Y;
end {CMYtoRGB};


// R, G, B, C, M, Y each IN [0.0 .. 1.0]
procedure RGBtoCMY(const R, G, B: TReal; var C, M, Y: TReal);
begin
  C := 1.0 - R;
  M := 1.0 - G;
  Y := 1.0 - B;
end {RGBtoCMY};


// == CMYK / RGB ======================================================

// Based on C Code in "Computer Graphics -- Principles and Practice,"
// Foley et al, 1996, p. 589

// R, G, B, C, M, Y, K each IN [0.0 .. 1.0]
procedure CMYKtoRGB(const C, M, Y, K: TReal; var R, G, B: TReal);
begin
  R := 1.0 - (C + K);
  G := 1.0 - (M + K);
  B := 1.0 - (Y + K);
end {CMYtoRGB};


// R, G, B, C, M, Y each IN [0.0 .. 1.0]
procedure RGBToCMYK(const R, G, B: TReal; var C, M, Y, K: TReal);
begin
  RGBtoCMY(R, G, B, C, M, Y);
  K := MinValue([C, M, Y]);
  C := C - K;
  M := M - K;
  Y := Y - K;
end {RGBtoCMYK};


// == Color Matrices  =================================================


procedure BitmapToRGBMatrices(const Bitmap: TBitmap; var Red, Green, Blue: TRealColorMatrix);
var
  i, j: integer;
  row: pRGBTripleArray;
begin
  ASSERT(Bitmap.PixelFormat = pf24bit);

  Red := TRealColorMatrix.Create;
  Red.Width := Bitmap.Width;
  Red.Height := Bitmap.Height;

  Green := TRealColorMatrix.Create;
  Green.Width := Bitmap.Width;
  Green.Height := Bitmap.Height;

  Blue := TRealColorMatrix.Create;
  Blue.Width := Bitmap.Width;
  Blue.Height := Bitmap.Height;

  for j := 0 to Bitmap.Height - 1 do
  begin
    row := Bitmap.Scanline[j];
    for i := 0 to Bitmap.Width - 1 do
    begin
      Red[i, j] := row[i].rgbtRed / 255;   // 0.0 to 1.0
      Green[i, j] := row[i].rgbtGreen / 255;
      Blue[i, j] := row[i].rgbtBlue / 255;
    end;
  end;

end {BitmapToRGBMatrices};




function RGBMatricesToBitmap(const Red, Green, Blue: TRealColorMatrix): TBitmap;
var
  i, j: integer;
  row: pRGBTripleArray;
begin
  ASSERT((Red.Width = Green.Width) and (Red.Width = Blue.Width) and (Red.Height = Green.Height) and
    (Red.Height = Blue.Height));

  Result := TBitmap.Create;
  Result.Width := Red.Width;
  Result.Height := Red.Height;
  Result.PixelFormat := pf24bit;

  for j := 0 to Red.Height - 1 do
  begin
    row := Result.Scanline[j];
    for i := 0 to Red.Width - 1 do
    begin
      with row[i] do
      begin
        rgbtRed := TRUNC(Red[i, j] * 255 + 0.5);
        rgbtGreen := TRUNC(Green[i, j] * 255 + 0.5);
        rgbtBlue := TRUNC(Blue[i, j] * 255 + 0.5);
      end;
    end;
  end;

end {RGBMatricesToBitmap};


procedure RGBMatricesToHSVMatrices(const Red, Green, Blue: TRealColorMatrix;
  var Hue, Saturation, Value: TRealColorMatrix);
var
  i, j: integer;
  H, S, V: TReal;
begin
  ASSERT((Red.Width = Green.Width) and (Red.Width = Blue.Width) and (Red.Height = Green.Height) and
    (Red.Height = Blue.Height) and (Hue.Width = Saturation.Width) and (Hue.Width = Value.Width) and
    (Hue.Height = Saturation.Height) and (Hue.Height = Value.Height));

  for j := 0 to Red.Height - 1 do
  begin
    for i := 0 to Red.Width - 1 do
    begin
      RGBToHSV(Red[i, j], Green[i, j], Blue[i, j], H, S, V);
      Hue[i, j] := H;
      Saturation[i, j] := S;
      Value[i, j] := V;
    end;
  end;
end {RGBMatricesToHSVMatrices};


procedure HSVMatricesToRGBMatrices(const Hue, Saturation, Value: TRealColorMatrix;
  var Red, Green, Blue: TRealColorMatrix);
var
  i, j: integer;
  R, G, B: TReal;
begin
  ASSERT((Red.Width = Green.Width) and (Red.Width = Blue.Width) and (Red.Height = Green.Height) and
    (Red.Height = Blue.Height) and (Hue.Width = Saturation.Width) and (Hue.Width = Value.Width) and
    (Hue.Height = Saturation.Height) and (Hue.Height = Value.Height));

  for j := 0 to Red.Height - 1 do
  begin
    for i := 0 to Red.Width - 1 do
    begin
      HSVToRGB(Hue[i, j],
        Saturation[i, j],
        Value[i, j],
        R, G, B);
      Red[i, j] := R;
      Green[i, j] := G;
      Blue[i, j] := B;
    end;
  end;

end {HSVMatriceesToRGBMatrices};

end.
