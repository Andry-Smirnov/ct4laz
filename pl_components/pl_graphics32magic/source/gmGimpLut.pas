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


unit gmGimpLut;

interface

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

uses
  GR32, gmTypes;

type
  TgmGimpLut = record
    LUTs     : array of array of Byte;
    NChannels: Integer;
  end;

  function gimp_lut_new: TgmGimpLut;

  procedure gimp_lut_process(ALUT: TgmGimpLut; ASourceBmp, ADestBmp: TBitmap32;
    const AChannelSet: TgmChannelSet);

implementation

function gimp_lut_new: TgmGimpLut;
begin
  Result.LUTs      := nil;
  Result.NChannels := 0;
end; 

procedure gimp_lut_process(ALUT: TgmGimpLut; ASourceBmp, ADestBmp: TBitmap32;
  const AChannelSet: TgmChannelSet);
var
  i                : Integer;
  LSrcBit, LDestBit: PColor32;
  a, r, g, b       : Cardinal;
begin
  ADestBmp.SetSize(ASourceBmp.Width, ASourceBmp.Height);

  LSrcBit  := @ASourceBmp.Bits[0];
  LDestBit := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    a := LSrcBit^ and $FF000000;

    if csGrayscale in AChannelSet then
    begin
      b         := LSrcBit^ and $FF;
      b         := ALUT.LUTs[2, b];
      LDestBit^ := a or (b shl 16) or (b shl 8) or b;
    end
    else
    begin
      r := LSrcBit^ shr 16 and $FF;
      g := LSrcBit^ shr  8 and $FF;
      b := LSrcBit^        and $FF;

      if csRed in AChannelSet then
      begin
        r := ALUT.LUTs[0, r];
      end;

      if csGreen in AChannelSet then
      begin
        g := ALUT.LUTs[1, g];
      end;

      if csBlue in AChannelSet then
      begin
        b := ALUT.LUTs[2, b];
      end;

      LDestBit^ := a or (r shl 16) or (g shl 8) or b;
    end;

    Inc(LSrcBit);
    Inc(LDestBit);
  end;
end; 

end.
