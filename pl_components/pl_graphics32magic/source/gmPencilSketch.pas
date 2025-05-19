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


unit gmPencilSketch;

interface

uses
  GR32;

  procedure PencilSketch(const ASourceBmp, ADestBmp: TBitmap32;
    const ARadius: Integer);

implementation

uses
  gmTypes,
  gmGimpGaussianBlur,
  gmLevelsTool,
  gmGimpCommonFuncs,
  gmImageProcessFuncs,
  gmBlendModes;

const
  MIN_BLUR_RADIUS = 1;
  MAX_BLUR_RADIUS = 20;

procedure PencilSketch(const ASourceBmp, ADestBmp: TBitmap32;
  const ARadius: Integer);
var
  LForeBmp   : TBitmap32;
  LBackBmp   : TBitmap32;
  i, LRadius : Integer;
  LForeBits  : PColor32;
  LBackBits  : PColor32;
  LLevelsTool: TgmLevelsTool;
  LBlurFilter: TgmGimpGaussianBlur;
begin
  if ( not Assigned(ASourceBmp) ) or
     ( not Assigned(ADestBmp) ) then
  begin
    Exit;
  end;

  LRadius := Clamp(ARadius, MIN_BLUR_RADIUS, MAX_BLUR_RADIUS);

  LForeBmp := TBitmap32.Create;
  LBackBmp := TBitmap32.Create;
  try
    // step 1 -- desaturate both foreground and background bitmap
    LForeBmp.Assign(ASourceBmp);
    Desaturate32(LForeBmp);
    LBackBmp.Assign(LForeBmp);

    // step 2 -- invert the foreground bitmap
    InvertBitmap32(LForeBmp, [csRed, csGreen, csBlue]);

    // step 3 -- bluring the foreground bitmap
    LBlurFilter := TgmGimpGaussianBlur.Create;
    try
      // the radius should be above 3 times than original GBlur32()
      LBlurFilter.HorizontalRadius := LRadius * 3.2;
      LBlurFilter.VerticalRadius   := LRadius * 3.2;
      LBlurFilter.Execute(LForeBmp);
    finally
      LBlurFilter.Free;
    end;

    // step 4 -- blend foreground and background in Color Dodage blend mode
    LForeBits := @LForeBmp.Bits[0];
    LBackBits := @LBackBmp.Bits[0];

    for i := 0 to (LBackBmp.Width * LBackBmp.Height - 1) do
    begin
      LBackBits^ := ColorDodgeBlend32(LForeBits^, LBackBits^);

      Inc(LForeBits);
      Inc(LBackBits);
    end;

    // step 5 -- make the result darker a little with Levels adjustment
    LLevelsTool := TgmLevelsTool.Create(LBackBmp);
    try
      ADestBmp.Assign(LBackBmp);

      LLevelsTool.LevelsHighOutput := 245;
      LLevelsTool.Map(ADestBmp, [csRed, csGreen, csBlue]);
    finally
      LLevelsTool.Free;
    end;

  finally
    LForeBmp.Free;
    LBackBmp.Free;
  end;
end; 

end.
