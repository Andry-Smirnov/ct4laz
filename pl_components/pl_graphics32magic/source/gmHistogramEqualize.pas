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
unit gmHistogramEqualize;

interface

uses

  GR32;

procedure HistogramEqualize(ABitmap: TBitmap32);

implementation

// This routine is based on the article written by Peng Jia Le (������).
// The article is at here:
// http://www.cnblogs.com/Imageshop/p/3139209.html
//
// The effect of this routine is same as the Equalize command in Photoshop.
procedure HistogramEqualize(ABitmap: TBitmap32);
var
  LHistogram  : array [0..255] of Cardinal;
  LMap        : array [0..255] of Byte;
  i           : Integer;
  LPixelCount : Integer;
  r, g, b     : Byte;
  LSum        : Cardinal;
  p           : PColor32;
begin
  if (not Assigned(ABitmap)) or
     (ABitmap.Width <= 0) or
     (ABitmap.Height <= 0) then
  begin
    Exit;
  end;

  for i := 0 to 255 do
  begin
    LHistogram[i] := 0;
    LHistogram[i] := 0;
    LHistogram[i] := 0;
  end;

  LPixelCount := ABitmap.Width * ABitmap.Height;

  // calculating histogram
  p := @ABitmap.Bits[0];

  for i := 1 to LPixelCount do
  begin
    r := p^ shr 16 and $FF;
    g := p^ shr  8 and $FF;
    b := p^        and $FF;

    LHistogram[r] := LHistogram[r] + 1;
    LHistogram[g] := LHistogram[g] + 1;
    LHistogram[b] := LHistogram[b] + 1;
    
    Inc(p);
  end;

  // calculating the map
  LSum := 0;

  for i := 0 to 255 do
  begin
    LSum    := LSum + LHistogram[i];
    LMap[i] := Round( LSum / (ABitmap.Width * ABitmap.Height * 3) * 255 );
  end;

  // doing map
  p := @ABitmap.Bits[0];

  for i := 1 to LPixelCount do
  begin
    r := p^ shr 16 and $FF;
    g := p^ shr  8 and $FF;
    b := p^        and $FF;

    r := LMap[r];
    g := LMap[g];
    b := LMap[b];

    p^ := (p^ and $FF000000) or (r shl 16) or (g shl 8) or b;
    
    Inc(p);
  end;
end;

end.
