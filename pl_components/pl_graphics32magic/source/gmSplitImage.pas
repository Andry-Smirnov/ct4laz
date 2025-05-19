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


unit gmSplitImage;

interface

uses
  GR32;

type
  TgmSplitImageType = (sitRound, sitWaste);

  procedure SplitImage32(const ASourceBmp, ADestBmp: TBitmap32;
    const AAmount: Integer; const ASplitType: TgmSplitImageType;
    const ABKColor: TColor32);

implementation

uses
  Classes;

procedure SplitImage32(const ASourceBmp, ADestBmp: TBitmap32;
  const AAmount: Integer; const ASplitType: TgmSplitImageType;
  const ABKColor: TColor32);
var
  i, j, LCenterX, LDelta: Integer;
  LCorrectAmount        : Integer;
  LRect1, LRect2        : TRect;
  LBitmap1, LBitmap2    : TBitmap32;
  LRow1, LRow2, LSrcRow : PColor32Array;
begin
{$RANGECHECKS OFF}

  LDelta         := 0;
  LCorrectAmount := AAmount;

  if LCorrectAmount = 0 then
  begin
    ADestBmp.Assign(ASourceBmp);
    Exit;
  end;

  if ADestBmp.Width <> ASourceBmp.Width then
  begin
    ADestBmp.Width := ASourceBmp.Width;
  end;
  
  if ADestBmp.Height <> ASourceBmp.Height then
  begin
    ADestBmp.Height := ASourceBmp.Height;
  end;

  ADestBmp.FillRect(0, 0, ADestBmp.Width, ADestBmp.Height, ABKColor);

  LCenterX := ASourceBmp.Width div 2;

  if LCorrectAmount > LCenterX then
  begin
    LCorrectAmount := LCenterX;
  end;

  LBitmap1 := TBitmap32.Create;
  LBitmap2 := TBitmap32.Create;
  try
    LBitmap1.Height := 1;
    LBitmap1.Width  := LCenterX;
    LBitmap2.Height := 1;
    LBitmap2.Width  := LCenterX;
    LRow1           := LBitmap1.ScanLine[0];
    LRow2           := LBitmap2.ScanLine[0];

    for j := 0 to (ASourceBmp.Height - 1) do
    begin
      LSrcRow := ASourceBmp.ScanLine[j];

      for i := 0 to (LCenterX - 1) do
      begin
        LRow1[i] := LSrcRow[i];
        LRow2[i] := LSrcRow[i + LCenterX];
      end;

      case ASplitType of
        sitRound:
          begin
            LDelta := Round(   LCorrectAmount * Abs(  Sin( j / (ASourceBmp.Height - 1) * Pi )  )   );
          end;

        sitWaste:
          begin
            LDelta := Round(   LCorrectAmount * Abs(  Cos( j / (ASourceBmp.Height - 1) * Pi )  )   );
          end;
      end;

      LRect1 := Rect(0, j, LDelta, j + 1);
      ADestBmp.Draw(LRect1, LBitmap1.Canvas.ClipRect, LBitmap1);
      
      LRect2 := Rect(ASourceBmp.Width - 1 - LDelta, j, ASourceBmp.Width - 1, j + 1);
      ADestBmp.Draw(LRect2, LBitmap2.Canvas.ClipRect, LBitmap2);
    end;
    
  finally
    LBitmap1.Free;
    LBitmap2.Free;
  end;

{$RANGECHECKS ON}
end;

end.
