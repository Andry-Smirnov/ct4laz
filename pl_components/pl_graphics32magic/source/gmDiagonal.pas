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


unit gmDiagonal;

interface

uses
  GR32;

type
  TgmDiagonalDirection = (ddLeftDiag, ddRightDiag);

  procedure Diagonal32(const ASourceBmp, ADestBmp: TBitmap32;
    const ABackColor: TColor32; const AAmount: Integer;
    const ADiagDirection: TgmDiagonalDirection);

implementation

procedure Diagonal32(const ASourceBmp, ADestBmp: TBitmap32;
  const ABackColor: TColor32; const AAmount: Integer;
  const ADiagDirection: TgmDiagonalDirection);
var
  LSourceRow, LDestRow         : PColor32Array;
  i, j, LDelta, LHeight, LWidth: Integer;
  LSourceColumn, LDestColumn   : Integer;
  LFactor                      : Extended;
begin
{$RANGECHECKS OFF}

  LSourceColumn := 0;
  LDestColumn   := 0;

  if (AAmount >= 0) and (AAmount <= 100) then
  begin
    LHeight := ASourceBmp.Height;
    LWidth  := ASourceBmp.Width;

    ADestBmp.Width  := LWidth;
    ADestBmp.Height := LHeight;

    ADestBmp.FillRect(0, 0, ADestBmp.Width, ADestBmp.Height, ABackColor);

    LFactor := LWidth / ( LWidth + (AAmount / 100) * LHeight );

    for j := 0 to (LHeight - 1) do
    begin
      LSourceRow := ASourceBmp.ScanLine[j];
      LDestRow   := ADestBmp.Scanline[j];
      LDelta     := Round(AAmount / 100 * j);

      for i := 0 to (LWidth - 1) do
      begin
        case ADiagDirection of
          ddLeftDiag:
            begin
              LSourceColumn := i;
              LDestColumn   := Round( LFactor * (i + LDelta) );
            end;

          ddRightDiag:
            begin
              LSourceColumn := (LWidth - 1 - i);
              LDestColumn   := Round( LWidth - 1 - LFactor * (i + LDelta) );
            end;
        end;
        
        LDestRow[LDestColumn] := LSourceRow[LSourceColumn];
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

end.
