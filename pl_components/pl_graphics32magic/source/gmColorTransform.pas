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


unit gmColorTransform;

interface

uses
  GR32, GR32_LowLevel;

  procedure ColorTransform32(const ASourceBmp, ADestBmp: TBitmap32;
    const ARedToGreenPercent, ARedToBluePercent: Double;
    const AGreenToRedPercent, AGreenToBluePercent: Double;
    const ABlueToRedPercent, ABlueToGreenPercent: Double);

implementation

{$WARN UNSAFE_CODE OFF}

procedure ColorTransform32(const ASourceBmp, ADestBmp: TBitmap32;
  const ARedToGreenPercent, ARedToBluePercent: Double;
  const AGreenToRedPercent, AGreenToBluePercent: Double;
  const ABlueToRedPercent, ABlueToGreenPercent: Double);
var
  r, g, b               : Cardinal;
  i                     : Integer;
  LSourceBits, LDestBits: PColor32;
begin
  if (ADestBmp.Width = ASourceBmp.Width) and
     (ADestBmp.Height = ASourceBmp.Height) then
  begin
    LSourceBits := @ASourceBmp.Bits[0];
    LDestBits   := @ADestBmp.Bits[0];
    
    for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
    begin
      r := LSourceBits^ shr 16 and $FF;
      g := LSourceBits^ shr 8  and $FF;
      b := LSourceBits^        and $FF;

      r := Round( r * (1 - AGreenToRedPercent)  + g * AGreenToRedPercent );
      r := Round( r * (1 - ABlueToRedPercent)   + b * ABlueToRedPercent );

      g := Round( g * (1 - ARedToGreenPercent)  + r * ARedToGreenPercent );
      g := Round( g * (1 - ABlueToGreenPercent) + b * ABlueToGreenPercent );
      
      b := Round( b * (1 - ARedToBluePercent)   + r * ARedToBluePercent );
      b := Round( b * (1 - AGreenToBluePercent) + g * AGreenToBluePercent );

      r := Clamp(r, 255);
      g := Clamp(g, 255);
      b := Clamp(b, 255);

      LDestBits^ := (LSourceBits^ and $FF000000) or (r shl 16) or (g shl 8) or b;

      Inc(LSourceBits);
      Inc(LDestBits);
    end;
  end;
end; 

end.
