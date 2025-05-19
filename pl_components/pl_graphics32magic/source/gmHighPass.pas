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
unit gmHighPass;

interface

uses

  GR32,
{ GraphicsMagic }
  gmGimpGaussianBlur;

type
  // This class is based on the tutorial at:
  // http://www.gimp.org/tutorials/Sketch_Effect/
  TgmHighPass = class(TObject)
  private
    FBlurredBitmap: TBitmap32;            // using internally
    FBlurFilter   : TgmGimpGaussianBlur;
    FRadius       : Single;               // range from 0.1 to 250

    procedure SetRadius(AValue: Single);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(ABitmap: TBitmap32);

    property Radius: Single read FRadius write SetRadius;
  end;

// If you don't like to use the class above, what following method could do
// the same thing for you.
procedure HighPass(ABitmap: TBitmap32; ARadius: Single = 10.0);

implementation

uses
{ Graphics32 Third-Party }
  XGR32_Blendmodes,
{ GraphicsMagic }
  gmTypes,
  gmImageProcessFuncs,
  gmLevelsTool;

// Based on the tutorial at: http://www.gimp.org/tutorials/Sketch_Effect/
procedure HighPass(ABitmap: TBitmap32; ARadius: Single = 10.0);
var
  LForeBmp   : TBitmap32;
  LLevelsTool: TgmLevelsTool;
  LBlurFilter: TgmGimpGaussianBlur;
  i          : Integer;
  LSrcBits   : PColor32;
  LDstBits   : PColor32;
begin
  Assert( Assigned(ABitmap), 'Error: The parameter is nil.' );

  if ARadius < 0.1 then
  begin
    ARadius := 0.1;
  end
  else
  if ARadius > 250 then
  begin
    ARadius := 250;
  end;

  ARadius := ARadius * 3;

  LForeBmp := TBitmap32.Create;
  try
    LForeBmp.Assign(ABitmap);

    LBlurFilter := TgmGimpGaussianBlur.Create;
    try
      LBlurFilter.HorizontalRadius := ARadius;
      LBlurFilter.VerticalRadius   := ARadius;
      LBlurFilter.Execute(LForeBmp);

      InvertBitmap32(LForeBmp, [csRed, csGreen, csBlue]);

      LSrcBits := @LForeBmp.Bits[0];
      LDstBits := @ABitmap.Bits[0];

      for i := 1 to (ABitmap.Width * ABitmap.Height) do
      begin
        BlendMode.NormalBlend(LSrcBits^, LDstBits^, 127);

        Inc(LSrcBits);
        Inc(LDstBits);
      end;
    finally
      LBlurFilter.Free;
    end;

    LLevelsTool := TgmLevelsTool.Create(ABitmap);
    try
      with LLevelsTool do
      begin
        // composite channel...
        LLevelsTool.LevelsLowInput  := 63;
        LLevelsTool.LevelsHighInput := 192;

        Map(ABitmap, [csRed, csGreen, csBlue]);
      end;
    finally
      LLevelsTool.Free;
    end;

  finally
    LForeBmp.Free;
  end;
end;

{ TgmHighPass }

constructor TgmHighPass.Create;
begin
  inherited Create;

  FBlurredBitmap := TBitmap32.Create;
  FBlurFilter    := TgmGimpGaussianBLur.Create;
  FRadius        := 10.0;
end;

destructor TgmHighPass.Destroy;
begin
  FBlurFilter.Free;
  FBlurredBitmap.Free;
  
  inherited Destroy;
end;

procedure TgmHighPass.SetRadius(AValue: Single);
begin
  FRadius := AValue;
  
  if FRadius < 0.1 then
  begin
    FRadius := 0.1;
  end
  else
  if FRadius > 250 then
  begin
    FRadius := 250;
  end;
end;

procedure TgmHighPass.Execute(ABitmap: TBitmap32);
var
  i          : Integer;
  LRadius    : Single;
  LSrcBits   : PColor32;
  LDstBits   : PColor32;
  LLevelsTool: TgmLevelsTool;
begin
  Assert( Assigned(ABitmap), 'Error: TgmHighPass.Execute()-- The parameter is nil.' );

  // To make the radius three times than the setting is because this will
  // make the result is very similar to the filter that was in Photoshop. 
  LRadius := FRadius * 3;

  // Step 1 -- Get blurred bitmap of the passed one.
  FBlurredBitmap.Assign(ABitmap);
  FBlurFilter.HorizontalRadius := LRadius;
  FBlurFilter.VerticalRadius   := LRadius;
  FBlurFilter.Execute(FBlurredBitmap);

  // Step 2 -- Invert the blurred bitmap and then blend it with the passed one.
  InvertBitmap32(FBlurredBitmap, [csRed, csGreen, csBlue]);

  LSrcBits := @FBlurredBitmap.Bits[0];
  LDstBits := @ABitmap.Bits[0];

  for i := 1 to (ABitmap.Width * ABitmap.Height) do
  begin
    // transparency blending, opacity = 50%
    BlendMode.NormalBlend(LSrcBits^, LDstBits^, 127);

    Inc(LSrcBits);
    Inc(LDstBits);
  end;

  // Step 3 -- Levels Adjustment
  LLevelsTool := TgmLevelsTool.Create(ABitmap);
  try
    with LLevelsTool do
    begin
      // in composite channel ...
      LLevelsTool.LevelsLowInput  := 63;
      LLevelsTool.LevelsHighInput := 192;

      Map(ABitmap, [csRed, csGreen, csBlue]);
    end;
  finally
    LLevelsTool.Free;
  end;
end;

end.
