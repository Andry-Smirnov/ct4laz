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


unit gmSplitColor;

{$WARN UNSAFE_CODE OFF}

interface

uses
  GR32;

type
  TgmSplitColorChannel = (sccRed,
                          sccGreen,
                          sccBlue,
                          sccCyan,
                          sccMagenta,
                          sccYellow);

  procedure SplitColor32(const ADestBmp: TBitmap32; const AAmount: Integer;
    const ASplitColorChannel: TgmSplitColorChannel);

implementation

uses
  GR32_LowLevel;

procedure SplitColor32(const ADestBmp: TBitmap32; const AAmount: Integer;
  const ASplitColorChannel: TgmSplitColorChannel);
var
  i, ir, ig, ib: Integer;
  r, g, b      : Byte;
  p            : PColor32;
begin
  ir := 0;
  ig := 0;
  ib := 0;
  p  := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    case ASplitColorChannel of
      sccRed:
        begin
          ir := p^ shr 16 and $FF;
          ir := ir + AAmount;
        end;

      sccGreen:
        begin
          ig := p^ shr 8 and $FF;
          ig := ig + AAmount;
        end;

      sccBlue:
        begin
          ib := p^ and $FF;
          ib := ib + AAmount;
        end;

      sccCyan:
        begin
          ig := p^ shr 8 and $FF;
          ig := ig + AAmount;

          ib := p^ and $FF;
          ib := ib + AAmount;
        end;

      sccMagenta:
        begin
          ir := p^ shr 16 and $FF;
          ir := ir + AAmount;

          ib := p^ and $FF;
          ib := ib + AAmount;
        end;

      sccYellow:
        begin
          ir := p^ shr 16 and $FF;
          ir := ir + AAmount;

          ig := p^ shr 8 and $FF;
          ig := ig + AAmount;
        end;
    end;

    r  := Clamp(ir, 0, 255);
    g  := Clamp(ig, 0, 255);
    b  := Clamp(ib, 0, 255);
    p^ := (p^ and $FF000000) or (r shl 16) or (g shl 8) or b;

    Inc(p);
  end;
end; 

end.
