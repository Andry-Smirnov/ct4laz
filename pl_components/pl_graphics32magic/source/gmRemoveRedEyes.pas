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


unit gmRemoveRedEyes;

interface

uses
  Math, GR32;

type
  TARGB = record // color splitting type
    b: Byte;
    g: Byte;
    r: Byte;
    a: Byte;
  end;

  PARGB      = ^TARGB;
  TArrayARGB = array [0..0] of TARGB;
  PArrayARGB = ^TArrayARGB;

  procedure RemoveRedEyes(bm: TBitmap32);

implementation

procedure RemoveRedEyes(bm: TBitmap32);
var
  x, y, w, h      : Integer;
  pixptr          : PARGB;
  nrv, bluf, redq : Single;
  powr, powb, powg: Single;
begin
{$RANGECHECKS OFF}

  w := bm.Width;
  h := bm.Height;

  for y := 0 to (h - 1) do
  begin
    for x := 0 to (w - 1) do
    begin
      pixptr := PARGB(bm.PixelPtr[x, y]);
      nrv    := pixptr^.g + pixptr^.b;

      if nrv < 1 then
      begin
        nrv := 1;
      end;

      if pixptr^.g > 1 then
      begin
        bluf := pixptr^.b / pixptr^.g;
      end
      else
      begin
        bluf := pixptr^.b;
      end;

      bluf := Max ( 0.5, Min ( 1.5, Sqrt(bluf) ) );
      redq := (pixptr^.r / nrv) * bluf;

      if redq > 0.7 then
      begin
        powr := 1.775 - (redq * 0.75 + 0.25);

        if powr < 0 then
        begin
          powr := 0;
        end;

        powr := powr * powr;
        powb := 1 - (1 - powr) / 2;
        powg := 1 - (1 - powr) / 4;

        pixptr^.r := Round (powr * pixptr^.r);
        pixptr^.b := Round (powb * pixptr^.b);
        pixptr^.g := Round (powg * pixptr^.g);
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

end.
