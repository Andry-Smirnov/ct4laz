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
unit gmHueSaturationLayer;

interface

uses

  GR32,

  gmLayers,
  gmTypes;

type
  { TgmHueSaturationLayer }

  TgmHueSaturationLayer = class(TgmNonPixelizedLayer)
  private
    FAdjustMode : TgmHueSaturationAdjustMode;
    FHue        : Integer;
    FSaturation : Integer;
    FLightValue : Integer;
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    function GetCopy: TgmCustomLayer; override;

    property AdjustMode : TgmHueSaturationAdjustMode read FAdjustMode write FAdjustMode;
    property Hue        : Integer                    read FHue        write FHue;
    property LightValue : Integer                    read FLightValue write FLightValue;
    property Saturation : Integer                    read FSaturation write FSaturation;
  end;

implementation

uses

  GR32_LowLevel,

  gmColorSpace;           // RGBToHLS32(), HLSToRGB32()

{$R gmHueSaturationLayerIcons.res}

function LayerHLS(const AColor: TColor32;
  const AHueAmount, ALightnessAmount, ASaturationAmount: Integer): TColor32;
var
  A       : Cardinal;
  H, L, S : Integer;
begin
  A := AColor shr 24 and $FF;
  RGBToHLS32(AColor, H, L, S);
  
  H      := Clamp(H + AHueAmount, 0, 360);
  L      := Clamp(L + ALightnessAmount, 0, 255);
  S      := Clamp(S + ASaturationAmount, 1, 255);
  Result := HLSToRGB32(A, H, L, S);
end; 

function LayerHSV(const AColor: TColor32;
  const AHueAmount, ASaturationAmount, AValueAmount: Integer): TColor32;
var
  a       : Cardinal;
  h, s, v : Integer;
begin
  a := AColor shr 24 and $FF;
  RGBToHSV32(AColor, h, s, v);
  
  h      := Clamp(h + AHueAmount, 0, 360);
  s      := Clamp(s + ASaturationAmount, 0, 255);
  v      := Clamp(v + AValueAmount, 0, 255);
  Result := HSVToRGB32(a, h, s, v);
end; 

{ TgmHueSaturationLayer }

constructor TgmHueSaturationLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FDefaultLayerName := 'Hue/Saturation';
  FLogoThumbEnabled := True;
  FAdjustMode       := hsamHSL;
  FHue              := 0;
  FSaturation       := 0;
  FLightValue       := 0;

  FLogoBitmap := TBitmap32.Create();
  FLogoBitmap.LoadFromResourceName(HInstance, 'HUESATURATIONLAYERLOGO');

  FLogoThumb := TBitmap32.Create();
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  UpdateLogoThumbnail();
end;

function TgmHueSaturationLayer.GetCopy: TgmCustomLayer;
var
  LLayer : TgmHueSaturationLayer;
begin
  LLayer := TgmHueSaturationLayer.Create(Self.FOwner,
    Self.FLayerBitmap.Width, Self.FLayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);

  LLayer.Hue        := Self.Hue;
  LLayer.Saturation := Self.Saturation;
  LLayer.LightValue := Self.LightValue;
  LLayer.AdjustMode := Self.AdjustMode;

  Result := LLayer;
end;

procedure TgmHueSaturationLayer.LayerBlend(F: TColor32;
  var B: TColor32; M: TColor32);
var
  LForeColor : TColor32;
  LAlpha     : Byte;
begin
  // avoiding compiler warnings
  LForeColor := $0;

  LAlpha := B shr 24 and $FF;

  // only process when the background pixel is not transparent
  if LAlpha > 0 then
  begin
    case FAdjustMode of
      hsamHSL:
        begin
          LForeColor := LayerHLS(B, FHue, FLightValue, FSaturation);
        end;
        
      hsamHSV:
        begin
          LForeColor := LayerHSV(B, FHue, FSaturation, FLightValue);
        end;
    end;

    LForeColor := (B and $FF000000) or (LForeColor and $FFFFFF);

    // blending
    FLayerBlendEvent(LForeColor, B, M);
  end;
end;

end.
