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

unit gmUtilsImgProcPrimitives;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, LMessages, types,
  Graphics,
  Classes,
  Math,
  SysUtils;

const
  MaxPixelCount = 65536;

type
  TReal = single;

  // use SysUtils.pByteArray for pf8bit Scanlines

  // For pf24bit Scanlines
  pRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..MaxPixelCount - 1] of TRGBTriple;

  // for pf32bit Scanlines
  pRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array[0..MaxPixelCount - 1] of TRGBQuad;
  pSingleArray = ^TSingleArray;
  TSingleArray = array[0..MaxPixelCount - 1] of TReal;


// General info
function GetBitmapDimensionsString(const Bitmap: TBitmap): string;
function GetPixelFormatString(const PixelFormat: TPixelFormat): string;

// Bitmap manipulations
procedure PrintBitmap(Canvas: TCanvas; DestRect: TRect; Bitmap: TBitmap);

// TRGBTriple
function AreRGBTriplesEqual(const Triple1, Triple2: TRGBTriple): boolean;
function ColorToRGBTriple(const Color: TColor): TRGBTriple;
function RGBtoRGBTriple(const red, green, blue: byte): TRGBTriple;
function RGBTripleToColor(const RGBTriple: TRGBTriple): TColor;
function RGBTripleInvert(const RGBTriple: TRGBTriple): TRGBTriple;

// RGBTriple Array manipulations
function RGBTripleAverage(RGB: array of TRGBTriple): TRGBTriple;
function RGBTripleBrightest(RGB: array of TRGBTriple): TRGBTriple;
function RGBTripleMaximum(RGB: array of TRGBTriple): TRGBTriple;
function RGBTripleMedian(RGB: array of TRGBTriple): TRGBTRiple;
function RGBTripleMinimum(RGB: array of TRGBTriple): TRGBTriple;

// TRGBTriple Brightness:  Intensity, Lightness, Value, Y; Saturation
function RGBTripleToIntensity(const RGB: TRGBTriple): integer;
function RGBTripleToLightness(const RGB: TRGBTriple): integer;
function RGBTripleToSaturation(const RGB: TRGBTriple): integer;
function RGBTripleToValue(const RGB: TRGBTriple): integer;
function RGBTripleToY(const RGB: TRGBTriple): integer;

function CountColors(const Bitmap: TBitmap): integer;

implementation


//==  General Info  ======================================================

function GetBitmapDimensionsString(const Bitmap: TBitmap): string;
begin
  Result := IntToStr(Bitmap.Width) + ' by ' + IntToStr(Bitmap.Height) + ' pixels by ' +
    GetPixelFormatString(Bitmap.PixelFormat) + ' color';
  if Bitmap.HandleType = bmDDB then
    Result := Result + '(DDB)';
end ;


function GetPixelFormatString(const PixelFormat: TPixelFormat): string;
begin
  case PixelFormat of
    pfDevice: Result := 'Device';
    pf1bit: Result := '1 bit';
    pf4bit: Result := '4 bit';
    pf8bit: Result := '8 bit';
    pf15bit: Result := '15 bit';
    pf16bit: Result := '16 bit';
    pf24bit: Result := '24 bit';
    pf32bit: Result := '32 bit'
    else
      Result := 'Unknown'
  end;
end ;


//==  Bitmap Manipulations  ==============================================

procedure PrintBitmap(Canvas: TCanvas; DestRect: TRect; Bitmap: TBitmap);
var
  BitmapHeader: pBitmapInfo;
  BitmapImage: POINTER;
  HeaderSize: DWORD;    // Use DWORD for compatibility with D3-D5
  ImageSize: DWORD;
begin  {
    GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
    GetMem(BitmapHeader, HeaderSize);
    GetMem(BitmapImage,  ImageSize);
    TRY
      GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
      StretchDIBits(Canvas.Handle,
                    DestRect.Left, DestRect.Top,     // Destination Origin
                    DestRect.Right  - DestRect.Left, // Destination Width
                    DestRect.Bottom - DestRect.Top,  // Destination Height
                    0, 0,                            // Source Origin
                    Bitmap.Width, Bitmap.Height,     // Source Width & Height
                    BitmapImage,
                    TBitmapInfo(BitmapHeader^),
                    DIB_RGB_COLORS,
                    SRCCOPY)
    FINALLY
      FreeMem(BitmapHeader);
      FreeMem(BitmapImage)
    END   }
end ;

//==  TRGBTriple   =====================================================


function AreRGBTriplesEqual(const Triple1, Triple2: TRGBTriple): boolean;
begin
  Result := (Triple1.rgbtRed = Triple2.rgbtRed) and (Triple1.rgbtGreen = Triple2.rgbtGreen) and
    (Triple1.rgbtBlue = Triple2.rgbtBlue);
end ;


function ColorToRGBTriple(const Color: TColor): TRGBTriple;
begin
  with Result do
  begin
    rgbtRed := GetRValue(Color);
    rgbtGreen := GetGValue(Color);
    rgbtBlue := GetBValue(Color);
  end;
end ;


function RGBtoRGBTriple(const red, green, blue: byte): TRGBTriple;
begin
  with Result do
  begin
    rgbtRed := red;
    rgbtGreen := green;
    rgbtBlue := blue;
  end;
end ;


function RGBTripleToColor(const RGBTriple: TRGBTriple): TColor;
begin
  with RGBTriple do
    Result := RGB(rgbtRed, rgbtGreen, rgbtBlue);
end ;


function RGBTripleInvert(const RGBTriple: TRGBTriple): TRGBTriple;
begin
  with Result do
  begin
    rgbtRed := 255 - RGBTriple.rgbtRed;
    rgbtGreen := 255 - RGBTriple.rgbtGreen;
    rgbtBlue := 255 - RGBTriple.rgbtBlue;
  end;
end ;


//==  RGBTriple manipulations  =========================================

// Calculate "average" RGBTriple given array of RGBTriples
function RGBTripleAverage(RGB: array of TRGBTriple): TRGBTriple;
var
  Count: integer;
  i: integer;
  SumRed: integer;
  SumGreen: integer;
  SumBlue: integer;
begin
  SumRed := 0;
  SumGreen := 0;
  SumBlue := 0;
  for i := Low(RGB) to High(RGB) do
  begin
    Inc(SumRed, RGB[i].rgbtRed);
    Inc(SumGreen, RGB[i].rgbtGreen);
    Inc(SumBlue, RGB[i].rgbtBlue);
  end;

  Count := High(RGB) - Low(RGB) + 1;

  Result := RGBtoRGBTriple(sumRed div Count, sumGreen div Count, sumBlue div Count);
end ;


// Find brightest RGBTriple in an array
function RGBTripleBrightest(RGB: array of TRGBTriple): TRGBTriple;
var
  MaxIndex: integer;
  MaxIntensity: integer;
  i: integer;
  Intensity: integer;
begin
  MaxIndex := Low(RGB);
  MaxIntensity := RGBTripletoIntensity(RGB[Low(RGB)]);

  for i := Low(RGB) + 1 to High(RGB) do
  begin
    Intensity := RGBTripletoIntensity(RGB[i]);
    if Intensity > MaxIntensity then
    begin
      MaxIntensity := Intensity;
      MaxIndex := i;
    end;
  end;

  Result := RGB[MaxIndex];
end ;


// Returned TRGBTriple has the max of R, G, and B components for input array
function RGBTripleMaximum(RGB: array of TRGBTriple): TRGBTriple;
var
  i: integer;
  MaxRed: byte;
  MaxGreen: byte;
  MaxBlue: byte;
begin
  MaxRed := RGB[0].rgbtRed;
  MaxGreen := RGB[0].rgbtGreen;
  MaxBlue := RGB[0].rgbtBlue;

  for i := 1 to High(RGB) do
  begin
    if RGB[i].rgbtRed > MaxRed then
      MaxRed := RGB[i].rgbtRed;

    if RGB[i].rgbtGreen > MaxGreen then
      MaxGreen := RGB[i].rgbtRed;

    if RGB[i].rgbtBlue > MaxBlue then
      MaxBlue := RGB[i].rgbtBlue;
  end;

  Result := RGBtoRGBTriple(MaxRed, MaxGreen, MaxBlue);
end ;


// Use copy of MedianInteger here instead of using StatisticsLibrary, so
// "low-level" image processing primitives aren't dependent on
// "high-level" statistics library.
function MedianInteger(x: array of integer): integer;
var
  i: integer;
  j: integer;
  Middle: integer;
  Temporary: integer;
begin
  // Use truncated selection sort to find median

  Middle := (High(x) + 1) div 2;

  for i := 0 to Middle do
  begin
    for j := 1 to High(x) - i do
    begin
      if x[j] > x[j - 1] then
      begin
        Temporary := x[j];
        x[j] := x[j - 1];
        x[j - 1] := Temporary;
      end;
    end;

  end;

  if Odd(High(x)) then
  begin
    // When High(x) is Odd, there are an even number of elements in array.
    // Define median as average of two middle values.
    Result := (x[middle] + x[middle - 1]) div 2;
  end
  else
  begin
    // When High(x) is Even, there are an odd number of elements in array.
    // Median is the middle value.
    Result := x[middle];
  end;
end;


function RGBTripleMedian(RGB: array of TRGBTriple): TRGBTriple;
type
  pIntegerArray = ^TIntegerArray;
  TIntegerArray = array[0..MaxPixelCount - 1] of integer;
var
  Count: integer;
  i: integer;
  MedianIndex: integer;
  MedianValue: integer;
  pIntensity: pIntegerArray;
begin
  Count := High(RGB) - Low(RGB) + 1;

  // Allocate temporary integer array for intensity values
  // Use this technique until D4 dynamic arrays can be used.
  GetMem(pIntensity, Count * SizeOf(integer));

  try
    for i := Low(RGB) to High(RGB) do
    begin
      pIntensity^[i] := RGBTripletoIntensity(RGB[i]);
    end;

    MedianValue := MedianInteger(Slice(pIntensity^, Count));

    MedianIndex := Low(RGB);
    while (MedianIndex < High(RGB)) and (pIntensity^[MedianIndex] <> MedianValue) do
    begin
      Inc(MedianIndex);
    end

  finally
    FreeMem(pIntensity)
  end;

  Result := RGB[MedianIndex];
end;


// Returned TRGBTriple has the min of R, G, and B components for input array
function RGBTripleMinimum(RGB: array of TRGBTriple): TRGBTriple;
var
  i: integer;
  MinRed: byte;
  MinGreen: byte;
  MinBlue: byte;
begin
  MinRed := RGB[0].rgbtRed;
  MinGreen := RGB[0].rgbtGreen;
  MinBlue := RGB[0].rgbtBlue;

  for i := 1 to High(RGB) do
  begin
    if RGB[i].rgbtRed < MinRed then
      MinRed := RGB[i].rgbtRed;

    if RGB[i].rgbtGreen < MinGreen then
      MinGreen := RGB[i].rgbtGreen;

    if RGB[i].rgbtBlue < MinBlue then
      MinBlue := RGB[i].rgbtBlue;
  end;

  Result := RGBtoRGBTriple(MinRed, MinGreen, MinBlue);
end;


//======================================================================

// See [Russ95, p. 41]
function RGBTripleToIntensity(const RGB: TRGBTriple): integer;
begin
  with RGB do
    Result := (rgbtRed + rgbtGreen + rgbtBlue) div 3;
end;


// See [Foley96, p. 595]
function RGBTripleToLightness(const RGB: TRGBTriple): integer;
begin
  // Use DIV here since histogram looks "odd" when IEEE rounding is used.
  with RGB do
    Result := (MinIntValue([rgbtRed, rgbtGreen, rgbtBlue]) + MaxIntValue([rgbtRed, rgbtGreen, rgbtBlue])) div 2;
end;


// See [Foley96, p. 592]
function RGBTripleToSaturation(const RGB: TRGBTriple): integer;
var
  MaxValue: integer;
  MinValue: integer;
begin
  with RGB do
  begin
    MinValue := MinIntValue([rgbtRed, rgbtGReen, rgbtBlue]);
    MaxValue := MaxIntValue([rgbtRed, rgbtGReen, rgbtBlue]);
  end;

  // Calculate saturation:  saturation is 0 if r, g and b are all 0
  if MaxValue = 0 then
    Result := 0
  else
    Result := MulDiv(MaxValue - MinValue, 255, MaxValue);
end;


// See [Foley96, p. 592]
function RGBTripleToValue(const RGB: TRGBTriple): integer;
begin
  with RGB do
    Result := MaxIntValue([rgbtRed, rgbtGreen, rgbtBlue]);
end ;


// Y (in YIQ) = 0.299R + 0.587G + 0.114B, which can be performed in
// integer arithmetic as Y = (77R + 150G + 29B) DIV 256
// See [Foley96, pp. 589-590]
function RGBTripleToY(const RGB: TRGBTriple): integer;
begin
  with RGB do
    Result := integer(77 * rgbtRed + 150 * rgbtGreen + 29 * rgbtBlue) shr 8;
end;


//==  CountColors  =====================================================

// Count number of unique R-G-B triples in a pf24bit Bitmap.

// Use 2D array of TBits objects -- when (R,G) combination occurs
// for the first time, create 256-bit array of bits in blue dimension.
// So, overall this is a fairly sparse matrix for most pictures.
// Tested with pictures created with a known number of colors, including
// a specially constructed image with 1024*1024 = 1,048,576 colors.

// efg, October 1998.
function CountColors(const Bitmap: TBitmap): integer;
var
  Flags: array[byte, byte] of TBits;
  i: integer;
  j: integer;
  k: integer;
  rowIn: pRGBTripleArray;
begin
  // Be sure bitmap is 24-bits/pixel
  ASSERT(Bitmap.PixelFormat = pf24Bit);

  // Clear 2D array of TBits objects
  for j := 0 to 255 do
    for i := 0 to 255 do
      Flags[i, j] := nil;

  // Step through each scanline of image
  for j := 0 to Bitmap.Height - 1 do
  begin
    rowIn := Bitmap.Scanline[j];
    for i := 0 to Bitmap.Width - 1 do
    begin
      with rowIn[i] do
      begin

        if not Assigned(Flags[rgbtRed, rgbtGreen]) then
        begin
          // Create 3D column when needed
          Flags[rgbtRed, rgbtGreen] := TBits.Create;
          Flags[rgbtRed, rgbtGreen].Size := 256;
        end;

        // Mark this R-G-B triple
        Flags[rgbtRed, rgbtGreen].Bits[rgbtBlue] := True;
      end;
    end;
  end;

  Result := 0;
  // Count and Free TBits objects
  for j := 0 to 255 do
  begin
    for i := 0 to 255 do
    begin

      if Assigned(Flags[i, j]) then
      begin
        for k := 0 to 255 do
          if Flags[i, j].Bits[k] then
            Inc(Result);
        Flags[i, j].Free;
      end;

    end;
  end;

end ;


end.



