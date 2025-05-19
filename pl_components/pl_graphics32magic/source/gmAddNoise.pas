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

unit gmAddNoise;

interface

uses
  GR32, GR32_LowLevel;

type
  TgmNoiseMode = (nmColor, nmMono);

  procedure AddColorNoise32(const ADestBmp: TBitmap32; const AAmount: Integer);
  procedure AddMonoNoise32(const ADestBmp: TBitmap32; const AAmount: Integer);

implementation

procedure AddColorNoise32(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  i, rr, gg, bb, LAmount: Integer;
  a, r, g, b            : Cardinal;
  p                     : PColor32;
begin
  LAmount := AAmount;

  if LAmount < 0 then
  begin
    LAmount := 0;
  end;

  p := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    a := p^ and $FF000000;

    if a > 0 then
    begin
      rr  := p^ shr 16 and $FF;
      gg  := p^ shr  8 and $FF;
      bb  := p^        and $FF;

      rr := rr + ( Random(LAmount) - (LAmount shr 1) );
      gg := gg + ( Random(LAmount) - (LAmount shr 1) );
      bb := bb + ( Random(LAmount) - (LAmount shr 1) );
      
      r  := Clamp(rr, 255);
      g  := Clamp(gg, 255);
      b  := Clamp(bb, 255);
      p^ := a or (r shl 16) or (g shl 8) or b;
    end;

    Inc(p);
  end;
end; 

procedure AddMonoNoise32(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  i, LDelta, LAmount: Integer;
  rr, gg, bb        : Integer;
  a, r, g, b        : Cardinal;
  p                 : PColor32;
begin
  LAmount := AAmount;

  if LAmount < 0 then
  begin
    LAmount := 0;
  end;

  p := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    a := p^ and $FF000000;

    if a > 0 then
    begin
      LDelta := Random(LAmount) - (LAmount shr 1);

      rr := p^ shr 16 and $FF;
      gg := p^ shr  8 and $FF;
      bb := p^        and $FF;

      rr := rr + LDelta;
      gg := gg + LDelta;
      bb := bb + LDelta;

      r  := Clamp(rr, 255);
      g  := Clamp(gg, 255);
      b  := Clamp(bb, 255);
      p^ := a or (r shl 16) or (g shl 8) or b;
    end;

    Inc(p);
  end;
end; 

end.
