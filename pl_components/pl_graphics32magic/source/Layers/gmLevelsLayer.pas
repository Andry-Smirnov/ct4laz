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
unit gmLevelsLayer;

interface

uses

  GR32,

  gmGimpHistogram,
  gmLayers,
  gmLevelsTool;

type
  { TgmLevelsLayer }

  TgmLevelsLayer = class(TgmNonPixelizedLayer)
  private
    FHistogramScale : TgmGimpHistogramScale;
    FLevelsTool     : TgmLevelsTool;
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    destructor Destroy; override;

    function GetCopy: TgmCustomLayer; override;

    property HistogramScale : TgmGimpHistogramScale read FHistogramScale write FHistogramScale;
    property LevelsTool     : TgmLevelsTool         read FLevelsTool;
  end;

implementation

{$R gmLevelsLayerIcons.res}

{ TgmLevelsLayer }

constructor TgmLevelsLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FDefaultLayerName := 'Levels';
  FLogoThumbEnabled := True;

  FLevelsTool := TgmLevelsTool.Create(FLayerBitmap);
  FLevelsTool.LUTSetup(3);

  FLogoBitmap := TBitmap32.Create();
  FLogoBitmap.LoadFromResourceName(HInstance, 'LEVELSLAYERLOGO');

  FLogoThumb := TBitmap32.Create();
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  UpdateLogoThumbnail();
end;

destructor TgmLevelsLayer.Destroy;
begin
  FLevelsTool.Free();
  inherited;
end;

function TgmLevelsLayer.GetCopy: TgmCustomLayer;
var
  i, j   : Integer;
  LLayer : TgmLevelsLayer;
begin
  LLayer := TgmLevelsLayer.Create(Self.FOwner,
    Self.FLayerBitmap.Width, Self.FLayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);

  for j := Low(FLevelsTool.LUT.luts) to High(FLevelsTool.LUT.luts) do
  begin
    for i := Low(FLevelsTool.LUT.luts[j]) to High(FLevelsTool.LUT.luts[j]) do
    begin
      LLayer.LevelsTool.LUT.luts[j, i] := Self.FLevelsTool.LUT.luts[j, i];
    end;
  end;

  for i := 0 to 4 do
  begin
    LLayer.LevelsTool.SliderPos[i]         := Self.FLevelsTool.SliderPos[i];
    LLayer.LevelsTool.Levels.Gamma[i]      := Self.FLevelsTool.Levels.Gamma[i];
    LLayer.LevelsTool.Levels.LowInput[i]   := Self.FLevelsTool.Levels.LowInput[i];
    LLayer.LevelsTool.Levels.HighInput[i]  := Self.FLevelsTool.Levels.HighInput[i];
    LLayer.LevelsTool.Levels.LowOutput[i]  := Self.FLevelsTool.Levels.LowOutput[i];
    LLayer.LevelsTool.Levels.HighOutput[i] := Self.FLevelsTool.Levels.HighOutput[i];

    for j := 0 to 255 do
    begin
      LLayer.LevelsTool.Levels.FInput[i, j] := Self.FLevelsTool.Levels.FInput[i, j];
    end;
  end;

  LLayer.LevelsTool.Channel := Self.FLevelsTool.Channel;

  Result := LLayer;
end;

procedure TgmLevelsLayer.LayerBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  LForeColor : TColor32;
  LAlpha     : Byte;
  rr, gg, bb : Byte;
begin
  LAlpha := B shr 24 and $FF;

  // only process when the background pixel is not transparent
  if LAlpha > 0 then
  begin
    rr := B shr 16 and $FF;
    gg := B shr  8 and $FF;
    bb := B        and $FF;

    rr := FLevelsTool.LUT.luts[0, rr];
    gg := FLevelsTool.LUT.luts[1, gg];
    bb := FLevelsTool.LUT.luts[2, bb];

    LForeColor := (LAlpha shl 24) or (rr shl 16) or (gg shl 8) or bb;

    // blending
    FLayerBlendEvent(LForeColor, B, M);
  end;
end;


end.
