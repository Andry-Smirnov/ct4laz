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


unit gmReversalFilm;

interface

uses
  GR32,  GR32_LowLevel;

type
  // RFND stands for "Reversal Film Negative Developing"
  TgmRFNDFilter = class(TObject)
  private
    FGreenWeight: Byte;
    FBlueWeight : Byte;

    procedure ChannelAdjustment(const ASourceBmp, ADestBmp: TBitmap32);
  public
    constructor Create;

    procedure ExecuteFilter(const ASourceBmp, ADestBmp: TBitmap32);
  end;

implementation

uses
  gmTypes,
  gmLevelsTool,
  gmGimpBaseEnums,
  gmColorSpace,
  gmImageProcessFuncs;

const
  GREEN_OPACITY = 0.2;
  BLUE_OPACITY  = 0.5;

procedure LevelsAdjustment(const ADestBmp: TBitmap32);
var
  LLevelsTool: TgmLevelsTool;
begin
  if (ADestBmp.Width <= 0) or (ADestBmp.Height <= 0) then
  begin
    Exit;
  end;

  LLevelsTool := TgmLevelsTool.Create(ADestBmp);
  try
    // red channel
    LLevelsTool.Channel         := GIMP_HISTOGRAM_RED;
    LLevelsTool.LevelsLowInput  := 50;
    LLevelsTool.LevelsGamma     := 1.3;
    LLevelsTool.LevelsHighInput := 255;

    // green channel
    LLevelsTool.Channel         := GIMP_HISTOGRAM_GREEN;
    LLevelsTool.LevelsLowInput  := 40;
    LLevelsTool.LevelsGamma     := 1.2;
    LLevelsTool.LevelsHighInput := 220;

    // green channel
    LLevelsTool.Channel         := GIMP_HISTOGRAM_BLUE;
    LLevelsTool.LevelsLowInput  := 25;
    LLevelsTool.LevelsGamma     := 0.75;
    LLevelsTool.LevelsHighInput := 150;

    LLevelsTool.Map(ADestBmp, [csRed, csGreen, csBlue]);
  finally
    LLevelsTool.Free;
  end;
end; 

constructor TgmRFNDFilter.Create;
begin
  inherited Create;

  FGreenWeight := Round(255 * GREEN_OPACITY);
  FBlueWeight  := Round(255 * BLUE_OPACITY);
end; 

procedure TgmRFNDFilter.ChannelAdjustment(
  const ASourceBmp, ADestBmp: TBitmap32);
var
  i         : Integer;
  sr, sg, sb: Integer;
  dr, dg, db: Integer;
  r, g, b   : Cardinal;
  LSrcBits  : PColor32;
  LDstBits  : PColor32;
begin
  if (ASourceBmp.Width  <> ADestBmp.Width) or
     (ASourceBmp.Height <> ADestBmp.Height) then
  begin
    Exit;
  end;

  LSrcBits := @ASourceBmp.Bits[0];
  LDstBits := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    sr := LSrcBits^ shr 16 and $FF;
    sg := LSrcBits^ shr  8 and $FF;
    sb := LSrcBits^        and $FF;

    { For red channel...

      Blend the red channels from source pixel and destination pixel with ColorBurn blending mode.
    }
    if sr > 0 then
    begin
      dr := LDstBits^ shr 16 and $FF;
      dr := 255 - ( (255 - dr) * 255 div sr );

      if dr < 0 then
      begin
        dr := 0;
      end
    end
    else
    begin
      dr := 0;
    end;

    { For green channel...

      1. invert green channel of source pixel
      2. blend the green channels from source pixel and destination pixel with Mutiply blending mode
      3. opacity blend
    }

    dg := 255 - sg;
    dg := dg * sg div 255;
    dg := (dg * FGreenWeight + sg * (255 - FGreenWeight)) div 255;

    { For blue channel...

      1. invert blue channel of source pixel
      2. blend the blue channels from source pixel and destination pixel with Mutiply blending mode
      3. opacity blend
    }
    db := 255 - sb;
    db := db * sb div 255;
    db := (db * FBlueWeight + sb * (255 - FBlueWeight)) div 255;

    r := dr shl 16;
    g := dg shl 8;
    b := db;

    LDstBits^ := (LDstBits^ and $FF000000) or r or g or b;

    Inc(LSrcBits);
    Inc(LDstBits);
  end;
end;

procedure TgmRFNDFilter.ExecuteFilter(const ASourceBmp, ADestBmp: TBitmap32);
begin
  if not Assigned(ASourceBmp) then
  begin
    Exit;
  end;

  if not Assigned(ADestBmp) then
  begin
    Exit;
  end;

  if (ASourceBmp.Width  <= 0 ) or
     (ASourceBmp.Height <= 0) then
  begin
    Exit;
  end;
  
  ADestBmp.Assign(ASourceBmp);

  // 1. color channels adjustment
  ChannelAdjustment(ASourceBmp, ADestBmp);

  // 2. levels ajustment
  LevelsAdjustment(ADestBmp);

  // 3. brightness/contrast adjustment
  BrightnessAdjustment(ADestBmp, -5);
  Contrast32(ADestBmp, 20);

  // 4. saturation adjustment
  SaturationAdjustment(ADestBmp, 15); 
end; 

end.
