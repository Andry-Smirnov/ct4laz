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


unit gmSolarize;

{$WARN UNSAFE_CODE OFF}

interface

uses
  SysUtils, GR32;

  procedure Solarize32(const ASourceBmp, ADestBmp: TBitmap32;
    const AAmount: Integer);

implementation

procedure Solarize32(const ASourceBmp, ADestBmp: TBitmap32;
  const AAmount: Integer);
var
  i         : Integer;
  LIntensity: Byte;
  a, r, g, b: Byte;
  LSrcBit   : PColor32;
  LDstBit   : PColor32;
begin
  if (AAmount >= 0) and (AAmount <= 255) then
  begin
    if (ADestBmp.Width  <> ASourceBmp.Width) or
       (ADestBmp.Height <> ASourceBmp.Height) then
    begin
      raise Exception.Create('The dimension of the source and dest parameters are different! -- Solarize32');
      Exit;
    end;

    LSrcBit := @ASourceBmp.Bits[0];
    LDstBit := @ADestBmp.Bits[0];

    for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
    begin
      a := LSrcBit^ shr 24 and $FF;
      r := LSrcBit^ shr 16 and $FF;
      g := LSrcBit^ shr  8 and $FF;
      b := LSrcBit^        and $FF;

      LIntensity := (r + g + b) div 3;

      if LIntensity > AAmount then
      begin
        if a > 0 then
        begin
          r := 255 - r;
          g := 255 - g;
          b := 255 - b;
        end;
      end;
      
      LDstBit^ := (a shl 24) or (r shl 16) or (g shl 8) or b;
      
      Inc(LSrcBit);
      Inc(LDstBit);
    end;
  end;
end; 

end.
