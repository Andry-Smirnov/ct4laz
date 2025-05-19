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

unit gmApplyImage;

{$MODE DELPHI}

interface

uses
  GR32,
  XGR32_Blendmodes,
  gmBlendModes, gmImageProcessFuncs,
  gmTypes;

type


  TgmSourceChannelSelector = (gmscsRGB,
                              gmscsRed,
                              gmscsGreen,
                              gmscsBlue,
                              gmscsTransparency,
                              gmscsAlphaChannel);

  TgmApplyImage = class(TObject)
  private
    FSourceChannel       : TgmSourceChannelSelector;
    FMaskChannel         : TgmSourceChannelSelector;
    FSourceChannelInverse: Boolean;
    FPreserveTransparency: Boolean;
    FMaskChannelInverse  : Boolean;
    FBlendModeIndex      : Integer;
    FBlendOpacity        : Byte;
    FBlendMode           : TBlendMode32;

    procedure SetBlendModeIndex(const Value: Integer);
  public
    constructor Create;
    
    procedure Execute(const Src, Dst: TBitmap32; const ChannelSet: TgmChannelSet); overload;
    procedure Execute(const Src, Dst, Mask: TBitmap32; const ChannelSet: TgmChannelSet); overload;

    property SourceChannel         : TgmSourceChannelSelector read FSourceChannel        write FSourceChannel;
    property MaskChannel           : TgmSourceChannelSelector read FMaskChannel          write FMaskChannel;
    property IsSourceChannelInverse: Boolean                  read FSourceChannelInverse write FSourceChannelInverse;
    property IsPreserveTransparency: Boolean                  read FPreserveTransparency write FPreserveTransparency;
    property IsMaskChannelInverse  : Boolean                  read FMaskChannelInverse   write FMaskChannelInverse;
    property BlendModeIndex        : Integer                  read FBlendModeIndex       write SetBlendModeIndex;
    property BlendOpacity          : Byte                     read FBlendOpacity         write FBlendOpacity;
  end;

implementation

function RGBToSingleChannelGrayscale(const AColor: TColor32;
  const ChannelSelection: TgmSourceChannelSelector): TColor32;
var
  cc: Cardinal;
begin
  case ChannelSelection of
    gmscsRed:
      begin
        cc     := AColor and $FF0000;
        Result := (AColor and $FF000000) or cc or (cc shr 8) or (cc shr 16);
      end;

    gmscsGreen:
      begin
        cc     := AColor and $FF00;
        Result := (AColor and $FF000000) or (cc shl 8) or cc or (cc shr 8);
      end;

    gmscsBlue:
      begin
        cc     := AColor and $FF;
        Result := (AColor and $FF000000) or (cc shl 16) or (cc shl 8) or cc;
      end;

    gmscsTransparency:
      begin
        cc     := AColor and $FF000000;
        Result := cc or (cc shr 8) or (cc shr 16) or (cc shr 24);
      end;

    else
      Result := AColor;
  end;
end;

//-- TgmApplyImage -------------------------------------------------------------

constructor TgmApplyImage.Create;
begin
  inherited Create;

  FSourceChannel        := gmscsRGB;
  FMaskChannel          := gmscsRGB;
  FSourceChannelInverse := False;
  FPreserveTransparency := False;
  FMaskChannelInverse   := False;
  FBlendModeIndex       := 1;
  FBlendOpacity         := 255;
  FBlendMode            := TBlendMode32(FBlendModeIndex);
end;

procedure TgmApplyImage.SetBlendModeIndex(const Value: Integer);
begin
  FBlendModeIndex := Value;
  FBlendMode      := TBlendMode32(FBlendModeIndex);
end;

procedure TgmApplyImage.Execute(const Src, Dst: TBitmap32;
  const ChannelSet: TgmChannelSet);
var
  x, y, LRGBChannelCount : Integer;
  fa, LAdjustedOpacity   : Byte;
  r, g, b                : Cardinal;
  LSrcRow, LDstRow       : PColor32Array;
  LSrcColor, LResultColor: TColor32;
begin
{$RANGECHECKS OFF}

  if not Assigned(Src) then
  begin
    Exit;
  end;

  if not Assigned(Dst) then
  begin
    Exit;
  end;

  if (Src.Width  <= 0) or (Src.Height <= 0) then
  begin
    Exit;
  end;

  if (Src.Width  <> Dst.Width) or (Src.Height <> Dst.Height) then
  begin
    Exit;
  end;

  LRGBChannelCount := 0;

  if csRed in ChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  if csGreen in ChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  if csBlue in ChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  for y := 0 to (Dst.Height - 1) do
  begin
    LSrcRow := Src.ScanLine[y];
    LDstRow := Dst.ScanLine[y];

    for x := 0 to (Dst.Width - 1) do
    begin
      LSrcColor := LSrcRow[x];

      { Extract color channel from source pixel and save it as a grayscale pixel.

        Note, the function will only process Alpha, Red, Green, Blue component,
        repectively. If you specify other channels, such as RGB composite,
        the function will return the original value of the first parameter. }

      if FSourceChannel in [gmscsRed, gmscsGreen, gmscsBlue, gmscsTransparency] then
      begin
        LSrcColor := RGBToSingleChannelGrayscale(LSrcColor, FSourceChannel);
      end;

      // invert the color
      if FSourceChannelInverse then
      begin
        LSrcColor := InvertRGB(LSrcColor);
      end;

      if csGrayscale in ChannelSet then
      begin
        // blend to grayscale bitmap...
        if FSourceChannel in [gmscsTransparency, gmscsAlphaChannel] then
        begin
          LSrcColor    := LSrcColor or $FF000000;
          LResultColor := RGBBlendByMode(LSrcColor, LDstRow[x], FBlendOpacity, FBlendMode);
        end
        else
        begin
          fa               := LSrcColor shr 24 and $FF;
          fa               := fa * Src.MasterAlpha div 255;
          LAdjustedOpacity := fa * FBlendOpacity div 255;
          LResultColor     := RGBBlendByMode(LSrcColor, LDstRow[x], LAdjustedOpacity, FBlendMode);
        end;

        b          := Intensity(LResultColor);
        LDstRow[x] := (LDstRow[x] and $FF000000) or (b shl 16) or (b shl 8) or b;
      end
      else
      begin
        if FPreserveTransparency or (LRGBChannelCount < 3) then
        begin
          if FSourceChannel in [gmscsTransparency, gmscsAlphaChannel] then
          begin
            LSrcColor    := LSrcColor or $FF000000;
            LResultColor := RGBBlendByMode(LSrcColor, LDstRow[x], FBlendOpacity, FBlendMode);
          end
          else
          begin
            fa               := LSrcColor shr 24 and $FF;
            fa               := fa * Src.MasterAlpha div 255;
            LAdjustedOpacity := fa * FBlendOpacity div 255;
            LResultColor     := RGBBlendByMode(LSrcColor, LDstRow[x], LAdjustedOpacity, FBlendMode);
          end;
        end
        else // full RGB components are selected and not preserve transparency... 
        begin
          if FSourceChannel in [gmscsTransparency, gmscsAlphaChannel] then
          begin
            LSrcColor    := LSrcColor or $FF000000;
            LResultColor := ARGBBlendByMode(LSrcColor, LDstRow[x], FBlendOpacity, FBlendMode);
          end
          else
          begin
            LAdjustedOpacity := Src.MasterAlpha * FBlendOpacity div 255;
            LResultColor     := ARGBBlendByMode(LSrcColor, LDstRow[x], LAdjustedOpacity, FBlendMode);
          end;
        end;

        r := LDstRow[x] and $FF0000;
        g := LDstRow[x] and $FF00;
        b := LDstRow[x] and $FF;

        if csRed in ChannelSet then
        begin
          r := LResultColor and $FF0000;
        end;

        if csGreen in ChannelSet then
        begin
          g := LResultColor and $FF00;
        end;

        if csBlue in ChannelSet then
        begin
          b := LResultColor and $FF;
        end;

        if FPreserveTransparency or (LRGBChannelCount < 3) then
        begin
          LDstRow[x] := (LDstRow[x] and $FF000000) or r or g or b;
        end
        else
        begin
          LDstRow[x] := (LResultColor and $FF000000) or r or g or b;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

procedure TgmApplyImage.Execute(const Src, Dst, Mask: TBitmap32;
  const ChannelSet: TgmChannelSet);
var
  x, y, LRGBChannelCount             : Integer;
  r, g, b                            : Cardinal;
  fa, LMaskWeight, LAdjustedOpacity  : Byte;
  LSrcRow, LDstRow, LMaskRow         : PColor32Array;
  LSrcColor, LResultColor, LMaskColor: TColor32;
begin
{$RANGECHECKS OFF}

  if not Assigned(Src) then
  begin
    Exit;
  end;

  if not Assigned(Dst) then
  begin
    Exit;
  end;

  if (Src.Width  <= 0) or (Src.Height <= 0) then
  begin
    Exit;
  end;

  if (Src.Width <> Dst.Width) or (Src.Height <> Dst.Height) then
  begin
    Exit;
  end;

  if (Src.Width <> Mask.Width) or (Src.Height <> Mask.Height) then
  begin
    Exit;
  end;

  LRGBChannelCount := 0;

  if csRed in ChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  if csGreen in ChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  if csBlue in ChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  for y := 0 to (Dst.Height - 1) do
  begin
    LSrcRow  := Src.ScanLine[y];
    LDstRow  := Dst.ScanLine[y];
    LMaskRow := Mask.ScanLine[y];

    for x := 0 to (Dst.Width - 1) do
    begin
      LSrcColor := LSrcRow[x];

      { Extract color channel from source pixel and save it as a grayscale pixel.

        Note, the function will only process Alpha, Red, Green, Blue component,
        repectively. If you specify other channels, such as RGB composite,
        the function will return the original value of the first parameter. }

      if FSourceChannel in [gmscsRed, gmscsGreen, gmscsBlue, gmscsTransparency] then
      begin
        LSrcColor := RGBToSingleChannelGrayscale(LSrcColor, FSourceChannel);
      end;

      // invert the color
      if FSourceChannelInverse then
      begin
        LSrcColor := InvertRGB(LSrcColor);
      end;

      LMaskColor := LMaskRow[x];

      // invert the mask
      if FMaskChannelInverse then
      begin
        LMaskColor := InvertRGB(LMaskColor);
      end;

      case FMaskChannel of
        gmscsRGB:
          begin
            LMaskWeight := Intensity(LMaskColor);
          end;

        gmscsRed:
          begin
            LMaskWeight := LMaskColor shr 16 and $FF;
          end;

        gmscsGreen:
          begin
            LMaskWeight := LMaskColor shr 8 and $FF;
          end;

        gmscsBlue:
          begin
            LMaskWeight := LMaskColor and $FF;
          end;

        gmscsTransparency:
          begin
            LMaskWeight := LMaskColor shr 24 and $FF;
          end;

        gmscsAlphaChannel:
          begin
            LMaskWeight := LMaskColor and $FF;
          end;

      else
        LMaskWeight := 0;
      end;

      if csGrayscale in ChannelSet then
      begin
        // blend to grayscale bitmap...
        if FSourceChannel in [gmscsTransparency, gmscsAlphaChannel] then
        begin
          LSrcColor        := LSrcColor or $FF000000;
          LAdjustedOpacity := FBlendOpacity * LMaskWeight div 255;
          LResultColor     := RGBBlendByMode(LSrcColor, LDstRow[x], LAdjustedOpacity, FBlendMode);
        end
        else
        begin
          fa               := LSrcColor shr 24 and $FF;
          fa               := fa * Src.MasterAlpha div 255;
          fa               := fa * FBlendOpacity div 255;
          LAdjustedOpacity := fa * LMaskWeight div 255;
          LResultColor     := RGBBlendByMode(LSrcColor, LDstRow[x], LAdjustedOpacity, FBlendMode);
        end;

        b          := Intensity(LResultColor);
        LDstRow[x] := (LDstRow[x] and $FF000000) or (b shl 16) or (b shl 8) or b;
      end
      else
      begin
        if FPreserveTransparency or (LRGBChannelCount < 3) then
        begin
          if FSourceChannel in [gmscsTransparency, gmscsAlphaChannel] then
          begin
            LSrcColor        := LSrcColor or $FF000000;
            LAdjustedOpacity := FBlendOpacity * LMaskWeight div 255;
            LResultColor     := RGBBlendByMode(LSrcColor, LDstRow[x], LAdjustedOpacity, FBlendMode);
          end
          else
          begin
            fa               := LSrcColor shr 24 and $FF;
            fa               := fa * Src.MasterAlpha div 255;
            fa               := fa * FBlendOpacity div 255;
            LAdjustedOpacity := fa * LMaskWeight div 255;
            LResultColor     := RGBBlendByMode(LSrcColor, LDstRow[x], LAdjustedOpacity, FBlendMode);
          end;
        end
        else // full RGB components are selected and not preserve transparency... 
        begin
          if FSourceChannel in [gmscsTransparency, gmscsAlphaChannel] then
          begin
            LSrcColor        := LSrcColor or $FF000000;
            LAdjustedOpacity := FBlendOpacity * LMaskWeight div 255;
            LResultColor     := ARGBBlendByMode(LSrcColor, LDstRow[x], LAdjustedOpacity, FBlendMode);
          end
          else
          begin
            LAdjustedOpacity := Src.MasterAlpha * FBlendOpacity div 255;
            LAdjustedOpacity := LAdjustedOpacity * LMaskWeight div 255;
            LResultColor     := ARGBBlendByMode(LSrcColor, LDstRow[x], LAdjustedOpacity, FBlendMode);
          end;
        end;

        r := LDstRow[x] and $FF0000;
        g := LDstRow[x] and $FF00;
        b := LDstRow[x] and $FF;

        if csRed in ChannelSet then
        begin
          r := LResultColor and $FF0000;
        end;

        if csGreen in ChannelSet then
        begin
          g := LResultColor and $FF00;
        end;

        if csBlue in ChannelSet then
        begin
          b := LResultColor and $FF;
        end;

        if FPreserveTransparency or (LRGBChannelCount < 3) then
        begin
          LDstRow[x] := (LDstRow[x] and $FF000000) or r or g or b;
        end
        else
        begin
          LDstRow[x] := (LResultColor and $FF000000) or r or g or b;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

end.
