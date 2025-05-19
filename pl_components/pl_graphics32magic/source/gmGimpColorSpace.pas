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


unit gmGimpColorSpace;

interface

procedure GimpRGBToHSL(var ARed, AGreen, ABlue: Integer);
procedure GimpHSLToRGB(var AHue, ASaturation, ALightness: Integer);

function GimpRGBToLightness(const ARed, AGreen, ABlue: Byte): Byte;

implementation

uses

  Math;

function GimpHSLValue(An1, An2, AHue: Double): Integer;
var
  LValue: Double;
begin
  if AHue > 255 then
  begin
    AHue := AHue - 255;
  end
  else if AHue < 0 then
  begin
    AHue := AHue + 255;
  end;

  if AHue < 42.5 then
  begin
    LValue := An1 + (An2 - An1) * (AHue / 42.5);
  end
  else if AHue < 127.5 then
  begin
    LValue := An2;
  end
  else if AHue < 170 then
  begin
    LValue := An1 + (An2 - An1) * ( (170 - AHue) / 42.5 );
  end
  else
  begin
    LValue := An1;
  end;

  Result := Round(LValue * 255.0);
end;

{
  Based on function gimp_rgb_to_hsl_int():
  @red: Red channel, returns Hue channel
  @green: Green channel, returns Lightness channel
  @blue: Blue channel, returns Saturation channel

  The arguments are pointers to int representing channel values in the
  RGB colorspace, and the values pointed to are all in the range [0, 255].

  The function changes the arguments to point to the corresponding HLS
  value with the values pointed to in the following ranges:  H [0, 360],
  L [0, 255], S [0, 255].
 }
procedure GimpRGBToHSL(var ARed, AGreen, ABlue: Integer);
var
  R, G, B   : Integer;
  H, S, L   : Double;
  LMin, LMax: Integer;
  LDelta    : Integer;
begin
  R := ARed;
  G := AGreen;
  B := ABlue;

  if (R > G) then
  begin
    LMax := Max(R, B);
    LMin := Min(G, B);
  end
  else
  begin
    LMax := Max(G, B);
    LMin := Min(R, B);
  end;

  L := (LMax + LMin) / 2.0;

  if (LMax = LMin) then
  begin
    S := 0.0;
    H := 0.0;
  end
  else
  begin
    LDelta := LMax - LMin;

    if L < 128 then
    begin
      S := 255 * LDelta / (LMax + LMin);
    end
    else
    begin
      S := 255 * LDelta / (511 - LMax - LMin);
    end;

    if R = LMax then
    begin
      H := (G - B) / LDelta;
    end
    else if G = LMax then
    begin
      H := 2 + (B - R) / LDelta;
    end
    else
    begin
      H := 4 + (R - G) / LDelta;
    end;

    H := H * 42.5;

    if H < 0 then
    begin
      H := H + 255;
    end
    else if H > 255 then
    begin
      H := H - 255;
    end;
  end;

  ARed   := Round(H);
  AGreen := Round(S);
  ABlue  := Round(L);
end;

{
  Based on function gimp_hsl_to_rgb_int():
  @hue: Hue channel, returns Red channel
  @saturation: Saturation channel, returns Green channel
  @lightness: Lightness channel, returns Blue channel

  The arguments are pointers to int, with the values pointed to in the
  following ranges:  H [0, 360], L [0, 255], S [0, 255].

  The function changes the arguments to point to the RGB value
  corresponding, with the returned values all in the range [0, 255].
 }
procedure GimpHSLToRGB(var AHue, ASaturation, ALightness: Integer);
var
  H, S, L: Double;
  m1, m2 : Double;
begin
  H := AHue;
  S := ASaturation;
  L := ALightness;

  if S = 0 then
  begin
    // achromatic case
    AHue        := Round(L);
    ALightness  := Round(L);
    ASaturation := Round(L);
  end
  else
  begin
    if L < 128 then
    begin
      m2 := ( L * (255 + S) ) / 65025.0;
    end
    else
    begin
      m2 := ( L + S - (L * S) / 255.0 ) / 255.0;
    end;

    m1 := (L / 127.5) - m2;

    // chromatic case
    AHue        := GimpHSLValue(m1, m2, H + 85);
    ASaturation := GimpHSLValue(m1, m2, H);
    ALightness  := GimpHSLValue(m1, m2, H - 85);
  end;
end;

{
  Based on function gimp_rgb_to_l_int():
  @red: Red channel
  @green: Green channel
  @blue: Blue channel

  Calculates the lightness value of an RGB triplet with the formula
  L = (max(R, G, B) + min (R, G, B)) / 2

  Return value: Luminance vaue corresponding to the input RGB value
 }
function GimpRGBToLightness(const ARed, AGreen, ABlue: Byte): Byte;
var
  LMin, LMax: Byte;
begin
  if ARed > AGreen then
  begin
    LMax := Max(ARed, ABlue);
    LMin := Min(AGreen, ABlue);
  end
  else
  begin
    LMax := Max(AGreen, ABlue);
    LMin := Min(ARed, ABlue);
  end;

  Result := Round( (LMax + LMin) / 2 );
end;

end.

