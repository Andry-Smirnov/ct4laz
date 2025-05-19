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
unit gmJHSmartBlurFilter;

interface

uses

  GR32,

  gmKernel;

type
  TgmJHSmartBlurFilter = class(TObject)
  private
    FRadius    : Single;
    FThreshold : Byte;

    procedure SetRadius(AValue : Single);

    {**
     * Convolve with a kernel consisting of one row
     *}
    procedure ThresholdBlur(AKernel: TgmKernel;
      AInPixels: TArrayOfColor32; var AOutPixels: TArrayOfColor32;
      AWidth: Integer; AHeight: Integer; AAlpha: Boolean);

  public
    constructor Create;

    procedure Execute(ASourceBitmap, ADestBitmap: TBitmap32);

    property Radius    : Single read FRadius    write SetRadius;
    property Threshold : Byte   read FThreshold write FThreshold;
  end;


implementation

uses

  GR32_LowLevel,

  gmGaussianBlurFilter,
  gmImageProcessFuncs;


const
  MIN_RADIUS = 0.1;
  MAX_RADIUS = 100.0;


{ TgmJHSmartBlurFilter }

constructor TgmJHSmartBlurFilter.Create;
begin
  inherited;

  FRadius    := 5.0;
  FThreshold := 10;
end;

procedure TgmJHSmartBlurFilter.Execute(ASourceBitmap, ADestBitmap: TBitmap32);
var
  LWidth, LHeight : Integer;
  LPixelCount     : Integer;
  LInPixels       : TArrayOfColor32;
  LOutPixels      : TArrayOfColor32;
  LKernel         : TgmKernel;
begin
  if ( not Assigned(ASourceBitmap) ) or
     ( not Assigned(ADestBitmap) ) then
  begin
    Exit;
  end;

  if (ASourceBitmap.Width <= 0) or
     (ASourceBitmap.Height <= 0) then
  begin
    Exit;
  end;

  if (ASourceBitmap.Width <> ADestBitmap.Width) or
     (ASourceBitmap.Height <> ADestBitmap.Height) then
  begin
    Exit;
  end;

  LKernel     := nil;
  LWidth      := ASourceBitmap.Width;
  LHeight     := ASourceBitmap.Height;
  LPixelCount := LWidth * LHeight;

  SetLength(LInPixels, LPixelCount);
  SetLength(LOutPixels, LPixelCount);
  try
    GetRGB(ASourceBitmap, LInPixels);
    LKernel := gmGaussianBlurFilter.MakeKernel(FRadius);

    if Assigned(LKernel) then
    begin
      ThresholdBlur(LKernel, LInPixels, LOutPixels, LWidth, LHeight, True);
      ThresholdBlur(LKernel, LOutPixels, LInPixels, LHeight, LWidth, True);
    end;

    SetRGB(ADestBitmap, LWidth, LHeight, LInPixels);

  finally
    SetLength(LInPixels, 0);
    SetLength(LOutPixels, 0);

    LInPixels  := nil;
    LOutPixels := nil;

    if Assigned(LKernel) then
    begin
      LKernel.Free();
    end;
  end;
end;

procedure TgmJHSmartBlurFilter.SetRadius(AValue : Single);
begin
  if AValue > MAX_RADIUS then
  begin
    FRadius := MAX_RADIUS;
  end
  else if AValue < MIN_RADIUS then
  begin
    FRadius := MIN_RADIUS;
  end
  else
  begin
    FRadius := AValue;
  end;
end;

{**
 * Convolve with a kernel consisting of one row
 *}
procedure TgmJHSmartBlurFilter.ThresholdBlur(AKernel: TgmKernel;
  AInPixels: TArrayOfColor32; var AOutPixels: TArrayOfColor32;
  AWidth: Integer; AHeight: Integer; AAlpha: Boolean);
var
  x, y, ix, d    : Integer;
  Lcol           : Integer;
  Lcols, Lcols2  : Integer;
  Lioffset       : Integer;
  Lmoffset       : Integer;
  LoutIndex      : Integer;
  r, g, b, a     : Single;
  Lrgb1, Lrgb2   : Cardinal;
  a1, r1, g1, b1 : Byte;
  a2, r2, g2, b2 : Byte;
  ia, ir, ig, ib : Cardinal;
  af, rf, gf, bf : Single;
  f              : Single;
begin
  if not Assigned(AKernel) then
  begin
    Exit;
  end;

  Lcols  := AKernel.Width;
  Lcols2 := Lcols div 2;

  for y := 0 to (AHeight - 1) do
  begin
    Lioffset  := y * AWidth;
    LoutIndex := y;

    for x := 0 to (AWidth - 1) do
    begin
      r := 0;
      g := 0;
      b := 0;
      a := 0;

      Lmoffset := Lcols2;

      Lrgb1 := AInPixels[Lioffset + x];
      a1    := Lrgb1 shr 24 and $FF;
      r1    := Lrgb1 shr 16 and $FF;
      g1    := Lrgb1 shr  8 and $FF;
      b1    := Lrgb1        and $FF;

      af := 0;
      rf := 0;
      gf := 0;
      bf := 0;

      for Lcol := -Lcols2 to Lcols2 do
      begin
        f := AKernel.Data[Lmoffset + Lcol];

        if (f <> 0) then
        begin
          ix := x + Lcol;

          if not ( (ix >= 0) and (ix < AWidth) ) then
          begin
            ix := x;
          end;

          Lrgb2 := AInPixels[Lioffset + ix];
          a2    := Lrgb2 shr 24 and $FF;
          r2    := Lrgb2 shr 16 and $FF;
          g2    := Lrgb2 shr  8 and $FF;
          b2    := Lrgb2        and $FF;

          d := a1 - a2;
          if (d >= (-FThreshold)) and (d <= Threshold) then
          begin
            a  := a + f * a2;
            af := af + f;
          end;

          d := r1 - r2;
          if (d >= (-FThreshold)) and (d <= Threshold) then
          begin
            r  := r + f * r2;
            rf := rf + f;
          end;

          d := g1 - g2;
          if (d >= (-FThreshold)) and (d <= Threshold) then
          begin
            g  := g + f * g2;
            gf := gf + f;
          end;

          d := b1 - b2;
          if (d >= (-FThreshold)) and (d <= Threshold) then
          begin
            b  := b + f * b2;
            bf := bf + f;
          end;
        end;
      end;

      if af = 0 then
      begin
        a := a1;
      end
      else
      begin
        a := a / af;
      end;

      if rf = 0 then
      begin
        r := r1;
      end
      else
      begin
        r := r / rf;
      end;

      if gf = 0 then
      begin
        g := g1;
      end
      else
      begin
        g := g / gf;
      end;

      if bf = 0 then
      begin
        b := b1;
      end
      else
      begin
        b := b / bf;
      end;

      if AAlpha then
      begin
        ia := Clamp( Round(a), 255 );
      end
      else
      begin
        ia := $FF;
      end;

      ir := Clamp( Round(r), 255 );
      ig := Clamp( Round(g), 255 );
      ib := Clamp( Round(b), 255 );

      AOutPixels[LoutIndex] := (ia shl 24) or (ir shl 16) or (ig shl 8) or ib;

      LOutIndex := LOutIndex + AHeight;
    end;
  end;
end;


end.
