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

unit gmUtilsColorLibrary;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, LMessages, types,
  Graphics,
  Dialogs,
  Math,
  gmUtilsIEEE754,
  gmUtilsImgProcPrimitives,
  gmUtilsRealColorLibrary,
  SysUtils;

const
  // 16 of the 20 System Palette Colors are defined in Graphics.PAS.
  // The additional 4 colors that NEVER dither, even in 256-color mode,
  // are as follows:  (See Microsoft Systems Journal, Sept. 91,
  // some of the "standard" colors weren't always the same!)
  clMoneyGreen = TColor($C0DCC0);   // Color   "8"  RGB:  192 220 192
  clSkyBlue = TColor($F0CAA6);   // Color   "9"  RGB:  166 202 240
  clCream = TColor($F0FBFF);   // Color "246"  RGB:  255 251 240
  clMediumGray = TColor($A4A0A0);   // Color "247"  RGB:  160 160 164

  NonDitherColors: array[0..19] of TColor =
    (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clSilver,
    clMoneyGreen, clSkyblue, clCream, clMediumGray,
    clGray, clRed, clGreen, clYellow, clBlue, clFuchsia, clAqua, clWhite);

  // Only the first 8 colors display/print the same in 256 color, 16-bit
  // and 24-bit color modes.
  MaskBlack = 0;   {RGB =   0   0   0}
  MaskDarkRed = 1;         {128   0   0}
  MaskDarkGreen = 2;         {  0 128   0}
  MaskPeaGreen = 3;         {128 128   0}
  MaskDarkBlue = 4;         {  0   0 128}
  MaskLavender = 5;         {128   0 128}
  MaskSlate = 6;         {  0 128 128}
  MaskLightGray = 7;         {192 192 192}

type
  TColorSpace = (csRGB, csHSV, csHLS, csCMYK);
  TColorPlane = (cpRGB,     {really a composite of csRed, csGreen, csBlue}
    cpRed, cpGreen, cpBlue,
    cpHueHSV, cpSaturationHSV, cpValue,       // HSV
    cpHueHLS, cpLightness, cpSaturationHLS,   // HLS
    cpCyan, cpMagenta, cpYellow, cpBlack, // CMYK
    cpIntensity,
    cpY);         // Y in YIQ coordinates

// Miscellaneous
function GetColorPlaneString(const ColorPlane: TColorPlane): string;

function ExtractImagePlane(const ColorPlane: TColorPlane; const ColorOutput: boolean;
  const Invert: boolean;
  const OriginalBitmap: TBitmap): TBitmap;

// Color Conversions

// HLS
function HLSToRGBTriple(const H, L, S: integer): TRGBTriple;
procedure RGBTripleToHLS(const RGBTriple: TRGBTriple; var H, L, S: integer);

// HSV
function HSVToRGBTriple(const H, S, V: integer): TRGBTriple;
procedure RGBTripleToHSV(const RGBTriple: TRGBTriple; var H, S, V: integer);

// CMY
function CMYToRGBTriple(const C, M, Y: integer): TRGBTriple;
procedure RGBTripleToCMY(const RGB: TRGBTriple;    // r, g and b IN [0..255]
  var C, M, Y: integer);    // c, m and y IN [0..255]


// CMYK
function CMYKToRGBTriple(const C, M, Y, K: integer): TRGBTriple;
procedure RGBTripleToCMYK(const RGB: TRGBTriple; var C, M, Y, K: integer);

implementation

type
  EColorError = class(Exception);


// ==  Miscellaneous  =================================================

function GetColorPlaneString(const ColorPlane: TColorPlane): string;
begin
  case ColorPlane of
    cpRGB: Result := 'RGB Composite';

    cpRed: Result := 'Red';
    cpGreen: Result := 'Green';
    cpBlue: Result := 'Blue';

    cpHueHSV: Result := 'Hue (HSV)';
    cpSaturationHSV: Result := 'Saturation (HSV)';
    cpValue: Result := 'Value (HSV)';

    cpHueHLS: Result := 'Hue (HLS)';
    cpLightness: Result := 'Lightness';
    cpSaturationHLS: Result := 'Saturation (HLS)';

    cpIntensity: Result := 'Intensity';
  end;

end {GetColorPlaneString};


function ExtractImagePlane(const ColorPlane: TColorPlane; const ColorOutput: boolean;
  const Invert: boolean; const OriginalBitmap: TBitmap): TBitmap;
var
  C, M, Y, K: integer;
  H, S, V: integer;      // color coordinates
  i: integer;
  Intensity: integer;
  j: integer;
  L: integer;
  RowOriginal: pRGBTripleArray;
  RowProcessed: pRGBTripleArray;
begin
  if OriginalBitmap.PixelFormat <> pf24bit then
    raise EColorError.Create('GetImageSpace:  ' + 'Bitmap must be 24-bit color.');

  Result := TBitmap.Create;
  Result.Width := OriginalBitmap.Width;
  Result.Height := OriginalBitmap.Height;
  Result.PixelFormat := OriginalBitmap.PixelFormat;

  // Step through each row of image.
  for j := OriginalBitmap.Height - 1 downto 0 do
  begin
    RowOriginal := OriginalBitmap.Scanline[j];
    RowProcessed := Result.Scanline[j];

    for i := OriginalBitmap.Width - 1 downto 0 do
    begin
      case ColorPlane of
        // ===============================================================
        cpRGB:
          if ColorOutput then
            RowProcessed[i] := RowOriginal[i]
          else
          begin
            Intensity := RGBTripleToIntensity(RowOriginal[i]);
            RowProcessed[i] :=
              RGBtoRGBTriple(Intensity, Intensity, Intensity);
          end;

        cpRed:
          if ColorOutput then
            RowProcessed[i] :=
              RGBtoRGBTriple(RowOriginal[i].rgbtRed, 0, 0)
          else
          begin
            Intensity := RowOriginal[i].rgbtRed;
            RowProcessed[i] :=
              RGBtoRGBTriple(Intensity, Intensity, Intensity);
          end;

        cpGreen:
          if ColorOutput then
            RowProcessed[i] :=
              RGBtoRGBTriple(0, RowOriginal[i].rgbtGreen, 0)
          else
          begin
            Intensity := RowOriginal[i].rgbtGreen;
            RowProcessed[i] :=
              RGBtoRGBTriple(Intensity, Intensity, Intensity);
          end;

        cpBlue:
          if ColorOutput then
            RowProcessed[i] :=
              RGBtoRGBTriple(0, 0, RowOriginal[i].rgbtBlue)
          else
          begin
            Intensity := RowOriginal[i].rgbtBlue;
            RowProcessed[i] :=
              RGBtoRGBTriple(Intensity, Intensity, Intensity);
          end;

        // ===============================================================
        cpHueHSV:
        begin
          RGBTripleToHSV(RowOriginal[i], H, S, V);
          // "Shades" of Hue with full saturation and value.
          RowProcessed[i] := HSVtoRGBTriple(H, 255, 255);

          if not ColorOutput then
          begin
            Intensity := RGBTripleToIntensity(RowProcessed[i]);
            RowProcessed[i] :=
              RGBtoRGBTriple(Intensity, Intensity, Intensity);
          end;
        end;

        cpSaturationHSV:
        begin
          RGBTripletoHSV(RowOriginal[i], H, S, V);
          // "Shades" of Saturation
          RowProcessed[i] := RGBtoRGBTriple(S, S, S);
        end;

        cpValue:
        begin
          RGBTripleToHSV(RowOriginal[i], H, S, V);
          // "Shades" of Value
          RowProcessed[i] := RGBtoRGBTriple(V, V, V);
        end;

        // ===============================================================
        cpHueHLS:
        begin
          RGBTripleToHLS(RowOriginal[i], H, L, S);
          // "Shades" of Hue with half lightness and full saturation.
          RowProcessed[i] := HLStoRGBTriple(H, 128, 255);

          if not ColorOutput then
          begin
            Intensity := RGBTripleToIntensity(RowProcessed[i]);
            RowProcessed[i] :=
              RGBtoRGBTriple(Intensity, Intensity, Intensity);
          end;
        end;

        cpLightness:
        begin
          RGBTripleToHLS(RowOriginal[i], H, L, S);
          // "Shades" of Lightness
          RowProcessed[i] := RGBtoRGBTriple(L, L, L);
        end;

        cpSaturationHLS:
        begin
          RGBTripleToHLS(RowOriginal[i], H, L, S);
          // Shades of Saturation
          RowProcessed[i] := RGBtoRGBTriple(S, S, S);
        end;
        // ===============================================================
        cpCyan:
        begin
          RGBTripleToCMYK(RowOriginal[i], C, M, Y, K);
          // Shades of Cyan
          RowProcessed[i] := RGBtoRGBTriple(C, C, C);
        end;

        cpMagenta:
        begin
          RGBTripleToCMYK(RowOriginal[i], C, M, Y, K);
          // Shades of Magenta
          RowProcessed[i] := RGBtoRGBTriple(M, M, M);
        end;

        cpYellow:
        begin
          RGBTripleToCMYK(RowOriginal[i], C, M, Y, K);
          // Shades of Yellow
          RowProcessed[i] := RGBtoRGBTriple(Y, Y, Y);
        end;

        cpBlack:
        begin
          RGBTripleToCMYK(RowOriginal[i], C, M, Y, K);
          // Shades of "Black"
          RowProcessed[i] := RGBtoRGBTriple(K, K, K);
        end;

        // ===============================================================
        cpIntensity:
        begin
          Intensity := RGBTripleToIntensity(RowOriginal[i]);
          // Shades of Intensity
          RowProcessed[i] :=
            RGBtoRGBTriple(Intensity, Intensity, Intensity);
        end

      end;  {Case}

      if Invert then
        RowProcessed[i] := RGBTripleInvert(RowProcessed[i]);

    end;

  end;
end {ExtractImagePlane};


// == HLS / RGB =======================================================

// Based on C Code in "Computer Graphics -- Principles and Practice,"
// Foley et al, 1996, p. 596.

function HLStoRGBTriple(const H, L, S: integer): TRGBTriple;
var
  R, G, B: TReal;
begin
  HLStoRGB(H, L / 255, S / 255, R, G, B);
  Result := ColorToRGBTriple(RGB(ROUND(255 * R), ROUND(255 * G), ROUND(255 * B)));
end {HLStoRGBTriple};


// RGB to HLS
// H = 0 to 360 (corresponding to 0..360 degrees around hexcone)
// L = 0 (shade of gray) to 255 (pure color)
// S = 0 (black) to 255 {white)

// R, G, B each in [0, 255]

procedure RGBTripleToHLS(const RGBTriple: TRGBTriple; var H, L, S: integer);
var
  Hue: TReal;
  Lightness: TReal;
  Saturation: TReal;
begin
  with RGBTriple do
    RGBToHLS(rgbtRed / 255, rgbtGreen / 255, rgbtBlue / 255,
      Hue, Lightness, Saturation);

  if IsNan(Hue) then
    H := 0
  else
    H := ROUND(Hue);         // 0..360
  L := ROUND(255 * Lightness);    // 0..255
  S := ROUND(255 * Saturation);   // 0..255
end {RGBTripleToHLS};


// Floating point fractions, 0..1, replaced with integer values, 0..255.
// Use integer conversion ONLY for one-way, or a single final conversions.
// Use floating-point for converting reversibly (see HSVtoRGB above).

//  H = 0 to 360 (corresponding to 0..360 degrees around hexcone)
//      0 (undefined) for S = 0
//  S = 0 (shade of gray) to 255 (pure color)
//  V = 0 (black) to 255 (white)

function HSVtoRGBTriple(const H, S, V: integer): TRGBTriple;
const
  divisor: integer = 255 * 60;
var
  f: integer;
  hTemp: integer;
  p, q, t: integer;
  VS: integer;
begin
  if S = 0 then
    Result := RGBtoRGBTriple(V, V, V)  // achromatic:  shades of gray
  else
  begin                              // chromatic color
    if H = 360 then
      hTemp := 0
    else
      hTemp := H;

    f := hTemp mod 60;     // f is IN [0, 59]
    hTemp := hTemp div 60;     // h is now IN [0,6)

    VS := V * S;
    p := V - VS div 255;                 // p = v * (1 - s)
    q := V - (VS * f) div divisor;         // q = v * (1 - s*f)
    t := V - (VS * (60 - f)) div divisor;  // t = v * (1 - s * (1 - f))

    case hTemp of
      0: Result := RGBtoRGBTriple(V, t, p);
      1: Result := RGBtoRGBTriple(q, V, p);
      2: Result := RGBtoRGBTriple(p, V, t);
      3: Result := RGBtoRGBTriple(p, q, V);
      4: Result := RGBtoRGBTriple(t, p, V);
      5: Result := RGBtoRGBTriple(V, p, q);
      else
        Result := RGBtoRGBTriple(0, 0, 0)  // should never happen;
      // avoid compiler warning
    end;
  end;
end {HSVtoRGBTriple};


// RGB, each 0 to 255, to HSV.
//   H = 0 to 360 (corresponding to 0..360 degrees around hexcone)
//   S = 0 (shade of gray) to 255 (pure color)
//   V = 0 (black) to 255 {white)

// Based on C Code in "Computer Graphics -- Principles and Practice,"
// Foley et al, 1996, p. 592.  Floating point fractions, 0..1, replaced with
// integer values, 0..255.

procedure RGBTripleToHSV(const RGBTriple: TRGBTriple;  {r, g and b IN [0..255]}
  var H, S, V: integer);    {h IN 0..359; s,v IN 0..255}
var
  Delta: integer;
  Min: integer;
begin
  with RGBTriple do
  begin
    Min := MinIntValue([rgbtRed, rgbtGreen, rgbtBlue]);
    V := MaxIntValue([rgbtRed, rgbtGreen, rgbtBlue]);
  end;

  Delta := V - Min;

  // Calculate saturation:  saturation is 0 if r, g and b are all 0
  if V = 0 then
    S := 0
  else
    S := MulDiv(Delta, 255, V);

  if S = 0 then
    H := 0   // Achromatic:  When s = 0, h is undefined but assigned the value 0
  else
  begin    // Chromatic

    with RGBTriple do
    begin
      if rgbtRed = V then  // degrees -- between yellow and magenta
        H := MulDiv(rgbtGreen - rgbtBlue, 60, Delta)
      else
      if rgbtGreen = V then // between cyan and yellow
        H := 120 + MulDiv(rgbtBlue - rgbtRed, 60, Delta)
      else
      if rgbtBlue = V then // between magenta and cyan
        H := 240 + MulDiv(rgbtRed - rgbtGreen, 60, Delta);
    end;

    if H < 0 then
      H := H + 360;

  end;
end {RGBTripleToHSV};


// == CMY / RGB =======================================================

// Based on C Code in "Computer Graphics -- Principles and Practice,"
// Foley et al, 1996, p. 588


// R, G, B, C, M, Y each IN [0..255]
function CMYtoRGBTriple(const C, M, Y: integer): TRGBTriple;
begin
  with Result do
  begin
    rgbtRed := 255 - C;
    rgbtGreen := 255 - M;
    rgbtBlue := 255 - Y;
  end;
end {CMYtoRGBTriple};


// R, G, B, C, M, Y each IN [0..255]
procedure RGBTripleToCMY(const RGB: TRGBTriple; var C, M, Y: integer);
begin
  with RGB do
  begin
    C := 255 - rgbtRed;
    M := 255 - rgbtGreen;
    Y := 255 - rgbtBlue;
  end;
end {RGBtoCMY};


// == CMYK / RGB ======================================================

// Based on C Code in "Computer Graphics -- Principles and Practice,"
// Foley et al, 1996, p. 589


// R, G, B, C, M, Y,K each IN [0..255]
function CMYKtoRGBTriple(const C, M, Y, K: integer): TRGBTriple;
begin
  with Result do
  begin
    rgbtRed := 255 - (C + K);
    rgbtGreen := 255 - (M + K);
    rgbtBlue := 255 - (Y + K);
  end;
end {CMYtoRGBTriple};


// R, G, B, C, M, Y each IN [0..255]
procedure RGBTripleToCMYK(const RGB: TRGBTriple; var C, M, Y, K: integer);
begin
  RGBTripleToCMY(RGB, C, M, Y);
  K := MinIntValue([C, M, Y]);
  C := C - K;
  M := M - K;
  Y := Y - K;
end {RGBtoCMYK};


end.



