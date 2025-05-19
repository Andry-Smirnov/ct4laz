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


unit gmImageTile;

interface

uses
  LCLIntf, LCLType, LMessages,types, GR32;

type
  TgmImageTileMode = (itmTile, itmOverlap);

  procedure OverlapTexturize32(const ADestBmp: TBitmap32; const AAmount: Integer);
  procedure TileTexturize32(const ADestBmp: TBitmap32; const AAmount: Integer);

implementation

uses
  gmImageProcessFuncs;

procedure OverlapTexturize32(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  X, Y                : Integer;
  LOverlapX, LOverlapY: Integer;
  LWidth, LHeight     : Integer;
  LBitmap             : TBitmap32;
begin
  if (AAmount >= 0) and (AAmount <= 100) then
  begin
    LWidth    := MulDiv(ADestBmp.Width,  AAmount, 100);
    LHeight   := MulDiv(ADestBmp.Height, AAmount, 100);
    LOverlapX := MulDiv(LWidth,  2, 3);
    LOverlapY := MulDiv(LHeight, 2, 3);

    LBitmap := TBitmap32.Create;
    try
      LBitmap.Assign(ADestBmp);
      SmoothResize32(LBitmap, LWidth, LHeight);

      Y := 0;
      repeat
        X := 0;

        repeat
          ADestBmp.Draw(X, Y, LBitmap);
          X := X + LOverlapX;
        until X >= ADestBmp.Width;

        Y := Y + LOverlapY;
      until Y >= ADestBmp.Height;

    finally
      LBitmap.Free;
    end;
  end;
end;

procedure TileTexturize32(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  X, Y, LWidth, LHeight: Integer;
  LBitmap              : TBitmap32;
begin
  if (AAmount >= 0) and (AAmount <= 100) then
  begin
    LWidth  := MulDiv(ADestBmp.Width,  AAmount, 100);
    LHeight := MulDiv(ADestBmp.Height, AAmount, 100);
    
    LBitmap := TBitmap32.Create;
    try
      LBitmap.Assign(ADestBmp);
      SmoothResize32(LBitmap, LWidth, LHeight);

      Y := 0;
      repeat

        X := 0;
        repeat
          ADestBmp.Draw(X, Y, LBitmap);
          X := X + LWidth;
        until X >= ADestBmp.Width;

        Y := Y + LHeight;
      until Y >= ADestBmp.Height;
      
    finally
      LBitmap.Free;
    end;
  end;
end; 

end.
