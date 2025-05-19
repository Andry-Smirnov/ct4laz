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


unit gmRandomRipple;

interface

uses
  Math, GR32;

  procedure RandomRipple32(const ADestBmp: TBitmap32; const AAmount: Integer);

implementation

procedure RandomRipple32(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  i, j,  LMinAmount        : Integer;
  LHalfHeight, LRandomValue: Integer;
  LRow1, LRow2             : PColor32Array;
begin
{$RANGECHECKS OFF}

  LHalfHeight := (ADestBmp.Height - 1) div 2;
  LMinAmount  := MinIntValue([LHalfHeight, AAmount]);

  Randomize;

  for j := (ADestBmp.Height - 1 - AAmount) downto 0 do
  begin
    LRow1        := ADestBmp.ScanLine[j];
    LRandomValue := 0;
    
    for i := 0 to (ADestBmp.Width - 1) do
    begin
      LRow2        := ADestBmp.ScanLine[j + LRandomValue];
      LRow2[i]     := LRow1[i];
      LRandomValue := Random(LMinAmount);
    end;
  end;

{$RANGECHECKS ON}
end; 

end.
