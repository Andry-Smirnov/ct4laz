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


unit gmWave;

interface

uses
  GR32;

type
  TgmWaveOption = (woStandard, woExtra);

  procedure Wave32(const ASourceBmp, ADestBmp: TBitmap32;
    const AAmount: Integer; const AWaveOption: TgmWaveOption;
    const ABKColor: TColor32);

implementation

procedure Wave32(const ASourceBmp, ADestBmp: TBitmap32; const AAmount: Integer;
  const AWaveOption: TgmWaveOption; const ABKColor: TColor32);
var
  LAngle          : Double;
  i, j, LDelta    : Integer;
  LSrcRow, LDstRow: PColor32Array;
begin
{$RANGECHECKS OFF}

  if AAmount <> 0 then
  begin
    if ADestBmp.Width <> ASourceBmp.Width then
    begin
      ADestBmp.Width := ASourceBmp.Width;
    end;

    if ADestBmp.Height <> ASourceBmp.Height then
    begin
      ADestBmp.Height := ASourceBmp.Height;
    end;

    ADestBmp.FillRect(0, 0, ADestBmp.Width, ADestBmp.Height, ABKColor);
    LAngle := PI / 2 / AAmount;

    { original code is:
      for j := (SourceBmp.Height - 1) - (Amount * 2) downto AAmount do }
      
    for j := ((ASourceBmp.Height - 1) - (AAmount * 2)) downto 0 do
    begin
      LSrcRow := ASourceBmp.ScanLine[j];
      LDelta  := 0;

      for i := 0 to (ASourceBmp.Width - 1) do
      begin
        LDstRow    := ADestBmp.ScanLine[j + AAmount + LDelta];
        LDstRow[i] := LSrcRow[i];

        case AWaveOption of
          woStandard:
            begin
              LDelta := AAmount * Variant( Sin(LAngle * i) );
            end;

          woExtra:
            begin
              LDelta := AAmount * Variant( Sin(LAngle * i) * Cos(LAngle * i) );
            end;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

end.
 
