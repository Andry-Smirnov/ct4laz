{*************************************************************************
  Package pl_Graphics32Magic
  This unit is part of CodeTyphon Studio (https://www.pilotlogic.com/)  

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


unit gmGammaTuner;

interface

uses
  SysUtils,
  GR32,
  gmTypes;

type
  TgmGammaTuner = class(TObject)
  private
    FSourceBitmap   : TBitmap32;
    FChannel        : TgmWorkingChannelType;
    FRGBGammaValue  : Single;
    FRedGammaValue  : Single;
    FGreenGammaValue: Single;
    FBlueGammaValue : Single;

    function GetRGBGammaValue: Single;
    function GetRedGammaValue: Single;
    function GetGreenGammaValue: Single;
    function GetBlueGammaValue: Single;

    procedure SetRGBGammaValue(AValue: Single);
    procedure SetRedGammaValue(AValue: Single);
    procedure SetGreenGammaValue(AValue: Single);
    procedure SetBlueGammaValue(AValue: Single);
  public
    constructor Create(ASrcImage: TBitmap32);
    destructor Destroy; override;

    procedure Execute(ADestImage: TBitmap32);

    property Channel        : TgmWorkingChannelType read FChannel           write FChannel;
    property RGBGammaValue  : Single                read GetRGBGammaValue   write SetRGBGammaValue;
    property RedGammaValue  : Single                read GetRedGammaValue   write SetRedGammaValue;
    property GreenGammaValue: Single                read GetGreenGammaValue write SetGreenGammaValue;
    property BlueGammaValue : Single                read GetBlueGammaValue  write SetBlueGammaValue;
  end;

var
  GAMMA_TABLE: array [Byte] of Byte;

implementation

constructor TgmGammaTuner.Create(ASrcImage: TBitmap32);
begin
  if not Assigned(ASrcImage) then
  begin
    raise Exception.Create('TgmGammaToner.Create() -- Source image is nil.' );
  end;

  inherited Create;

  FSourceBitmap := TBitmap32.Create;
  FSourceBitmap.Assign(ASrcImage);

  FChannel         := wctRGB;
  FRGBGammaValue   := 1.0;
  FRedGammaValue   := 1.0;
  FGreenGammaValue := 1.0;
  FBlueGammaValue  := 1.0;
end;

destructor TgmGammaTuner.Destroy;
begin
  FSourceBitmap.Free;

  inherited Destroy;
end;

function TgmGammaTuner.GetRGBGammaValue: Single;
begin
  Result := FRGBGammaValue;
end;

procedure TgmGammaTuner.SetRGBGammaValue(AValue: Single);
begin
  if FRGBGammaValue <> AValue then
  begin
    FRGBGammaValue := AValue;
  end;
end;

function TgmGammaTuner.GetRedGammaValue: Single;
begin
  Result := FRedGammaValue;
end;

procedure TgmGammaTuner.SetRedGammaValue(AValue: Single);
begin
  if FRedGammaValue <> AValue then
  begin
    FRedGammaValue := AValue;
  end;
end;

function TgmGammaTuner.GetGreenGammaValue: Single;
begin
  Result := FGreenGammaValue;
end;

procedure TgmGammaTuner.SetGreenGammaValue(AValue: Single);
begin
  if FGreenGammaValue <> AValue then
  begin
    FGreenGammaValue := AValue;
  end;
end;

function TgmGammaTuner.GetBlueGammaValue: Single;
begin
  Result := FBlueGammaValue;
end;

procedure TgmGammaTuner.SetBlueGammaValue(AValue: Single);
begin
  if FBlueGammaValue <> AValue then
  begin
    FBlueGammaValue := AValue;
  end;
end;

procedure TgmGammaTuner.Execute(ADestImage: TBitmap32);
var
  i        : Integer;
  r, g, b  : Byte;
  LDestBit : PColor32;
begin
  if not Assigned(ADestImage) then
  begin
    raise Exception.Create('TgmGammaToner.Execute() -- Destination image is nil.');
  end;

  ADestImage.Assign(FSourceBitmap);

{ Tone Adjustment }

  // Red channel gamma correction...
  if FRedGammaValue <> 1.0 then
  begin
    SetGamma(FRedGammaValue);

    LDestBit := @ADestImage.Bits[0];

    for i := 0 to (ADestImage.Width * ADestImage.Height - 1) do
    begin
      r := LDestBit^ shr 16 and $FF;
      r := GAMMA_TABLE[r];

      LDestBit^ := (LDestBit^ and $FF00FFFF) or (r shl 16);
  
      Inc(LDestBit);
    end;
  end;

  // Green channel gamma correction...
  if FGreenGammaValue <> 1.0 then
  begin
    SetGamma(FGreenGammaValue);

    LDestBit := @ADestImage.Bits[0];

    for i := 0 to (ADestImage.Width * ADestImage.Height - 1) do
    begin
      g := LDestBit^ shr 8 and $FF;
      g := GAMMA_TABLE[g];

      LDestBit^ := (LDestBit^ and $FFFF00FF) or (g shl 8);

      Inc(LDestBit);
    end;
  end;

  // Blue channel gamma correction...
  if FBlueGammaValue <> 1.0 then
  begin
    SetGamma(FBlueGammaValue);

    LDestBit := @ADestImage.Bits[0];

    for i := 0 to (ADestImage.Width * ADestImage.Height - 1) do
    begin
      b := LDestBit^ and $FF;
      b := GAMMA_TABLE[b];

      LDestBit^ := (LDestBit^ and $FFFFFF00) or b;

      Inc(LDestBit);
    end;
  end;

{ Brightness Adjustment }

  // RGB channel gamma correction...
  if FRGBGammaValue <> 1.0 then
  begin
    SetGamma(FRGBGammaValue);

    LDestBit := @ADestImage.Bits[0];

    for i := 0 to (ADestImage.Width * ADestImage.Height - 1) do
    begin
      r := LDestBit^ shr 16 and$FF;
      r := GAMMA_TABLE[r];

      g := LDestBit^ shr 8 and $FF;
      g := GAMMA_TABLE[g];

      b := LDestBit^ and $FF;
      b := GAMMA_TABLE[b];

      LDestBit^ := (LDestBit^ and $FF000000) or (r shl 16) or (g shl 8) or b;

      Inc(LDestBit);
    end;
  end;
end;

end.
