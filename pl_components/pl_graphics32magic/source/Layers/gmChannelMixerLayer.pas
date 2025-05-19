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
unit gmChannelMixerLayer;

interface

uses

  GR32,

  gmChannelMixer,
  gmLayers;

type
  { TgmChannelMixerLayer }

  TgmChannelMixerLayer = class(TgmNonPixelizedLayer)
  private
    FChannelMixer : TgmChannelMixer;
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    destructor Destroy; override;

    function GetCopy: TgmCustomLayer; override;

    property ChannelMixer : TgmChannelMixer read FChannelMixer;
  end;


implementation

{$R gmChannelMixerLayerIcons.res}

{ TgmChannelMixerLayer }

constructor TgmChannelMixerLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FDefaultLayerName := 'Channel Mixer';
  FLogoThumbEnabled := True;

  FChannelMixer := TgmChannelMixer.Create();

  FLogoBitmap := TBitmap32.Create();
  FLogoBitmap.LoadFromResourceName(HInstance, 'CHANNELMIXERLAYERLOGO');

  FLogoThumb := TBitmap32.Create();
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  UpdateLogoThumbnail();
end;

destructor TgmChannelMixerLayer.Destroy;
begin
  FChannelMixer.Free();
  inherited;
end;

function TgmChannelMixerLayer.GetCopy: TgmCustomLayer;
var
  LLayer : TgmChannelMixerLayer;
begin
  LLayer := TgmChannelMixerLayer.Create(Self.FOwner,
    Self.FLayerBitmap.Width, Self.FLayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);
  LLayer.ChannelMixer.AssignData(Self.FChannelMixer);

  Result := LLayer;
end;

procedure TgmChannelMixerLayer.LayerBlend(F: TColor32;
  var B: TColor32; M: TColor32);
var
  LForeColor : TColor32;
  LAlpha     : Byte;
begin
  LAlpha := B shr 24 and $FF;

  // only process when the background pixel is not transparent
  if LAlpha > 0 then
  begin
    FChannelMixer.InputColor := B;

    // only need RGB components of the color returned by the Channel Mixer
    LForeColor := (LAlpha shl 24) or (FChannelMixer.OutputColor and $FFFFFF);

    // blending
    FLayerBlendEvent(LForeColor, B, M);
  end;
end;

end.
