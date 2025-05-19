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


unit gmGimpGlassTile;

interface

uses
  GR32;

  procedure GimpGlassTile32(const ASourceBmp, ADestBmp: TBitmap32;
    const ATileWidth, ATileHeight: Integer);

implementation

procedure GimpGlassTile32(const ASourceBmp, ADestBmp: TBitmap32;
  const ATileWidth, ATileHeight: Integer);
var
  i, j                              : Integer;
  LHalfTileW, LXPlus, LXMitt, LXOffs: Integer;
  LHalfTileH, LYPlus, LYMitt, LYOffs: Integer;
  dx, sx, sy                        : Integer;
  LSrcRows, LDstRows                : array of PColor32Array;
begin
{$RANGECHECKS OFF}

  if ADestBmp.Width <> ASourceBmp.Width then
  begin
    ADestBmp.Width := ASourceBmp.Width;
  end;

  if ADestBmp.Height <> ASourceBmp.Height then
  begin
    ADestBmp.Height := ASourceBmp.Height;
  end;

  SetLength(LSrcRows, ASourceBmp.Height);
  SetLength(LDstRows, ASourceBmp.Height);

  for j := 0 to (ASourceBmp.Height - 1) do
  begin
    LSrcRows[j] := ASourceBmp.ScanLine[j];
    LDstRows[j] := ADestBmp.ScanLine[j];
  end;

  LHalfTileW := ATileWidth  div 2;
  LHalfTileH := ATileHeight div 2;
  LXPlus     := ATileWidth  mod 2;
  LYPlus     := ATileHeight mod 2;

  LYMitt := 0;
  LYOffs := 0;
  
  for j := 0 to (ASourceBmp.Height - 1) do
  begin
    sy := LYMitt + LYOffs * 2;
    Inc(LYOffs);

    if sy < 0 then
    begin
      sy := 0;
    end;

    if sy > (ASourceBmp.Height - 1) then
    begin
      sy := ASourceBmp.Height - 1;
    end;

    if LYOffs = LHalfTileH then
    begin
      LYMitt := LYMitt + ATileHeight;
      LYOffs := -LHalfTileH;
      LYOffs := LYOffs - LYPlus;
    end;

    LXMitt := 0;
    LXOffs := 0;
    
    for i := 0 to (ASourceBmp.Width - 1) do
    begin
      dx := LXMitt + LXOffs;
      sx := LXMitt + LXOffs * 2;

      if sx < 0 then
      begin
        sx := 0;
      end;

      if sx < (ASourceBmp.Width - 1) then
      begin
        LDstRows[j, dx] := LSrcRows[sy, sx];
      end
      else
      begin
        LDstRows[j, dx] := LSrcRows[sy, dx];
      end;

      Inc(LXOffs);
      
      if LXOffs = LHalfTileW then
      begin
        LXMitt := LXMitt + ATileWidth;
        LXOffs := -LHalfTileW;
        LXOffs := LXOffs - LXPlus;
      end;
    end;
  end;

  SetLength(LSrcRows, 0);
  SetLength(LDstRows, 0);

{$RANGECHECKS ON}
end; 

end.
