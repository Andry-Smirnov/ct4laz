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


unit gmColorRange;

interface

uses
  GR32,
  GR32_LowLevel,
  gmColorSpace;

type
  TgmSelectedColorMode = (scmRGB, scmHLS, scmHSV);

  // HLS Color Range
  procedure HLSColorRange(const AColor: TColor32; const AFuzziness: Integer;
    var AMaxH, AMinH: Word; var AMaxL, AMinL, AMaxS, AMinS: Byte);

  // HSV Color Range
  procedure HSVColorRange(const AColor: TColor32; const AFuzziness: Integer;
    var AMaxH, AMinH: Word; var AMaxS, AMinS, AMaxV, AMinV: Byte);

  // RGB Color Range
  procedure RGBColorRange32(const AColor: TColor32; const ARange: Integer;
    var AMaxR, AMinR, AMaxG, AMinG, AMaxB, AMinB: Byte);

  procedure LightnessRange(const AColor: TColor32; const AFuzziness: Integer;
    var AMinLight, AMaxLight: Byte);

implementation

// HLS Color Range
procedure HLSColorRange(const AColor: TColor32; const AFuzziness: Integer;
  var AMaxH, AMinH: Word; var AMaxL, AMinL, AMaxS, AMinS: Byte);
var
  H, L, S: Integer;
begin
  RGBToHLS32(AColor, H, L, S);

  AMinH := Clamp(H - AFuzziness, 0, 360);
  AMaxH := Clamp(H + AFuzziness, 0, 360);
  AMinL := Clamp(L - AFuzziness, 0, 255);
  AMaxL := Clamp(L + AFuzziness, 0, 255);
  AMinS := Clamp(S - AFuzziness, 1, 255);
  AMaxS := Clamp(S + AFuzziness, 1, 255);
end; 

// HSV Color Range
procedure HSVColorRange(const AColor: TColor32; const AFuzziness: Integer;
  var AMaxH, AMinH: Word; var AMaxS, AMinS, AMaxV, AMinV: Byte);
var
  H, S, V: Integer;
begin
  RGBToHSV32(AColor, H, S, V);

  AMinH := Clamp(H - AFuzziness, 0, 360);
  AMaxH := Clamp(H + AFuzziness, 0, 360);
  AMinS := Clamp(S - AFuzziness, 0, 255);
  AMaxS := Clamp(S + AFuzziness, 0, 255);
  AMinV := Clamp(V - AFuzziness, 0, 255);
  AMaxV := Clamp(V + AFuzziness, 0, 255);
end; 

// RGB Color Range
procedure RGBColorRange32(const AColor: TColor32; const ARange: Integer;
  var AMaxR, AMinR, AMaxG, AMinG, AMaxB, AMinB: Byte);
var
  R, G, B: Integer;
begin
  R := AColor shr 16 and $FF;
  G := AColor shr 8  and $FF;
  B := AColor        and $FF;

  AMinR := Clamp(R - ARange, 0, 255);
  AMaxR := Clamp(R + ARange, 0, 255);
  AMinG := Clamp(G - ARange, 0, 255);
  AMaxG := Clamp(G + ARange, 0, 255);
  AMinB := Clamp(B - ARange, 0, 255);
  AMaxB := Clamp(B + ARange, 0, 255);
end; 

procedure LightnessRange(const AColor: TColor32; const AFuzziness: Integer;
  var AMinLight, AMaxLight: Byte);
var
  LLightValue: Integer;
begin
  LLightValue := RGBToLightness32(AColor);
  AMinLight   := Clamp(LLightValue - AFuzziness, 0, 255);
  AMaxLight   := Clamp(LLightValue + AFuzziness, 0, 255);
end; 

end.
