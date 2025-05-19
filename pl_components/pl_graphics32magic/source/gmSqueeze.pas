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


unit gmSqueeze;

interface

uses
  Classes, GR32;

type
  TgmSqueezeStyle = (ssHorizontal,
                     ssTop,
                     ssBottom,
                     ssDiamond,
                     ssWaste,
                     ssRound,
                     ssDoubleRound);

  function GetSqueezeStyles: TStringList;

  procedure Squeeze32(const ASourceBmp, ADestBmp: TBitmap32;
    const AAmount: Integer; const ASqueezeStyle: TgmSqueezeStyle;
    const ABKColor: TColor32);

implementation

function GetSqueezeStyles: TStringList;
begin
  Result := TStringList.Create;

  with Result do
  begin
    Add('Horizontal');
    Add('Top');
    Add('Bottom');
    Add('Diamond');
    Add('Waste');
    Add('Round');
    Add('Double Round');
  end;
end;

procedure Squeeze32(const ASourceBmp, ADestBmp: TBitmap32;
  const AAmount: Integer; const ASqueezeStyle: TgmSqueezeStyle;
  const ABKColor: TColor32);
var
  LBitmap               : TBitmap32;
  i, j, LDelta, LCenterX: Integer;
  LCorrectAmount        : Integer;
  LRect                 : TRect;
  LRow1, LRow2          : PColor32Array;
begin
{$RANGECHECKS OFF}

  LCorrectAmount := AAmount;

  if ADestBmp.Width <> ASourceBmp.Width then
  begin
    ADestBmp.Width := ASourceBmp.Width;
  end;

  if ADestBmp.Height <> ASourceBmp.Height then
  begin
    ADestBmp.Height := ASourceBmp.Height;
  end;

  ADestBmp.FillRect(0, 0, ADestBmp.Width, ADestBmp.Height, ABKColor);

  if LCorrectAmount > (ASourceBmp.Width div 2) then
  begin
    LCorrectAmount := ASourceBmp.Width div 2;
  end;

  LCenterX := ASourceBmp.Width div 2;
  
  LBitmap := TBitmap32.Create;
  try
    LBitmap.Width  := ASourceBmp.Width;
    LBitmap.Height := 1;
    LRow1          := LBitmap.ScanLine[0];

    for j := 0 to (ASourceBmp.Height - 1) do
    begin
      LRow2 := ASourceBmp.ScanLine[j];

      for i := 0 to (ASourceBmp.Width - 1) do
      begin
        LRow1[i] := LRow2[i];
      end;

      case ASqueezeStyle of
        ssHorizontal:
          begin
            LDelta := LCorrectAmount;
            LRect  := Rect(LDelta, j, ASourceBmp.Width - LDelta, j + 1);
          end;

        ssTop:
          begin
            LDelta := Round( (ASourceBmp.Height - 1 - j) / ASourceBmp.Height * LCorrectAmount );
            LRect  := Rect(LDelta, j, ASourceBmp.Width - LDelta, j + 1);
          end;

        ssBottom:
          begin
            LDelta := Round(j / ASourceBmp.Height * LCorrectAmount);
            LRect  := Rect(LDelta, j, ASourceBmp.Width - LDelta, j + 1);
          end;

        ssDiamond:
          begin
            LDelta := Round(   LCorrectAmount * Abs(  Cos( j / (ASourceBmp.Height - 1) * Pi )  )   );
            LRect  := Rect(LDelta, j, ASourceBmp.Width - LDelta, j + 1);
          end;

        ssWaste:
          begin
            LDelta := Round(   LCorrectAmount * Abs(  Sin( j / (ASourceBmp.Height - 1) * Pi )  )   );
            LRect  := Rect(LDelta, j, ASourceBmp.Width - LDelta, j + 1);
          end;

        ssRound:
          begin
            LDelta := Round(   LCorrectAmount * Abs(  Sin( j / (ASourceBmp.Height - 1) * Pi )  )   );
            LRect  := Rect(LCenterX - LDelta, j, LCenterX + LDelta, j + 1);
          end;

        ssDoubleRound:
          begin
            LDelta := Round(   LCorrectAmount * Abs(  Sin( j / (ASourceBmp.Height - 1) * Pi * 2 )  )   );
            LRect  := Rect(LCenterX - LDelta, j, LCenterX + LDelta, j + 1);
          end;
      end;
      
      ADestBmp.Draw(LRect, LBitmap.Canvas.ClipRect, LBitmap);
    end;
    
  finally
    LBitmap.Free;
  end;

{$RANGECHECKS ON}
end; 

end.
