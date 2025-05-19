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


unit gmMosaic;

{$MODE DELPHI}

interface

uses
  GR32;

  procedure Mosaic32(const ADestBmp: TBitmap32; const ASize: Integer);

implementation

procedure Mosaic32(const ADestBmp: TBitmap32; const ASize: Integer);
var
  x, y, i, j    : Integer;
  a1, r1, g1, b1: Cardinal;
  a2, r2, g2, b2: Cardinal;
  LRow1, LRow2  : PColor32Array;
begin
{$RANGECHECKS OFF}

  y := 0;
  repeat
    LRow1 := ADestBmp.ScanLine[y];

    repeat
      j := 1;

      repeat
        LRow2 := ADestBmp.ScanLine[y];
        x     := 0;

        repeat

          a1 := LRow1[x] and $FF000000;
          r1 := LRow1[x] and $FF0000;
          g1 := LRow1[x] and $FF00;
          b1 := LRow1[x] and $FF;
          i  := 1;

          repeat
            a2 := a1;
            r2 := r1;
            g2 := g1;
            b2 := b1;

            LRow2[x] := a2 or r2 or g2 or b2;

            Inc(x);
            Inc(i);
          until (x >= ADestBmp.Width) or (i > ASize);

        until x >= ADestBmp.Width;

        Inc(j);
        Inc(y);
        
      until (y >= ADestBmp.Height) or (j > ASize);
    until (y >= ADestBmp.Height) or (x >= ADestBmp.Width);
  until y >= ADestBmp.Height;

{$RANGECHECKS ON} 
end;

end.
