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


unit gmSepia;

{$MODE DELPHI}

interface

uses
  GR32;

  procedure Sepia32(const ABmp: TBitmap32; const ADepth: Byte);

implementation

//*********************************************************
// Colorize given bitmap with sepia colors.
// Original author is Daniel Lopes from Graphics32 newsgroup.
// About 20% accelerated by Gerd Platl
// input:   bmp32  source and destination bitmap
//          depth  expedient values 10 .. 60
//                 default = 34
//---------------------------------------------------------
procedure Sepia32(const ABmp: TBitmap32; const ADepth: Byte);
var
  LDepth2, i: Integer;
  LPixel    : PColor32Entry;
begin
{$RANGECHECKS OFF}

  LDepth2 := ADepth * 2;
  LPixel  := @ABmp.Bits[0];

  for i := 0 to (ABmp.Width * ABmp.Height - 1) do
  begin
    // blue component = gray scaled color
    LPixel.B := (LPixel.R + LPixel.G + LPixel.B) div 3;

    // set red component of sepia color
    LPixel.R := LPixel.B + LDepth2;

    if LPixel.R < LDepth2 then
    begin
      LPixel.R := 255;
    end;

    // set green component of sepia color
    LPixel.G := LPixel.B + ADepth;

    if LPixel.G < ADepth then
    begin
      LPixel.G := 255;
    end;

    Inc(LPixel);
  end;

{$RANGECHECKS ON}
end; 

end.
