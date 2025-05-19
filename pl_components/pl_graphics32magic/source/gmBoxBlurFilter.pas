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

unit gmBoxBlurFilter;

interface

uses
  GR32,
  GR32_LowLevel,
  gmImageProcessFuncs;

type
  TgmBoxBlurFilter = class(TObject)
  private
    FHorzRadius: Integer;
    FVertRadius: Integer;
    FIterations: Integer;

    procedure SetRadius(const AValue: Integer);

    procedure Blur(const AInPixels, AOutPixels: TArrayOfColor32;
      const AWidth, AHeight, ARadius: Integer);
  public
    constructor Create;

    function Execute(const ASourceBmp, ADestBmp: TBitmap32): Boolean;
    function ToString: string;

    property Radius          : Integer read FHorzRadius write SetRadius;
    property HorizontalRadius: Integer read FHorzRadius write FHorzRadius;
    property VerticalRadius  : Integer read FVertRadius write FVertRadius;
    property Iterations      : Integer read FIterations write FIterations;
  end;

implementation

constructor TgmBoxBlurFilter.Create;
begin
  inherited Create;
  FHorzRadius := 0;
  FVertRadius := 0;
  FIterations := 1;
end;

procedure TgmBoxBlurFilter.SetRadius(const AValue: Integer);
begin
  FHorzRadius := AValue;
  FVertRadius := AValue;
end;

procedure TgmBoxBlurFilter.Blur(const AInPixels, AOutPixels: TArrayOfColor32;
  const AWidth, AHeight, ARadius: Integer);
var
  i, x, y, i1, i2: Integer;
  LWidthMinusOne : Integer;
  LTableSize     : Integer;
  LItemCount     : Integer;
  LInIndex       : Integer;
  LOutIndex      : Integer;
  ta, tr, tg, tb : Integer;
  a1, r1, g1, b1 : Byte;
  a2, r2, g2, b2 : Byte;
  LInColor       : TColor32;
  rgb1, rgb2     : TColor32;
  LDivideArray   : array of Integer;
begin
{$RANGECHECKS OFF}
  LWidthMinusOne := AWidth - 1;
  LTableSize     := 2 * ARadius + 1;
  LItemCount     := 256 * LTableSize;

  SetLength(LDivideArray, LItemCount);
  try
    for i := 0 to (LItemCount - 1) do
    begin
      LDivideArray[i] := Trunc(i / LTableSize);
    end;

    LInIndex := 0;

    for y := 0 to (AHeight - 1) do
    begin
      LOutIndex := y;
      ta        := 0;
      tr        := 0;
      tg        := 0;
      tb        := 0;

      for i := -ARadius to ARadius do
      begin
        LInColor := AInPixels[LInIndex + Clamp(i, 0, LWidthMinusOne)];

        a1 := LInColor shr 24 and $FF;
        r1 := LInColor shr 16 and $FF;
        g1 := LInColor shr  8 and $FF;
        b1 := LInColor        and $FF;

        ta := ta + a1;
        tr := tr + r1;
        tg := tg + g1;
        tb := tb + b1;
      end;

      for x := 0 to LWidthMinusOne do
      begin
        a1 := LDivideArray[ta];
        r1 := LDivideArray[tr];
        g1 := LDivideArray[tg];
        b1 := LDivideArray[tb];

        AOutPixels[LOutIndex] := (a1 shl 24) or (r1 shl 16) or (g1 shl  8) or b1;

        i1 := x + ARadius + 1;

        if i1 > LWidthMinusOne then
        begin
          i1 := LWidthMinusOne;
        end;

        i2 := x - ARadius;

        if i2 < 0 then
        begin
          i2 := 0;
        end;

        rgb1 := AInPixels[LInIndex + i1];
        rgb2 := AInPixels[LInIndex + i2];

        a1 := rgb1 shr 24 and $FF;
        r1 := rgb1 shr 16 and $FF;
        g1 := rgb1 shr  8 and $FF;
        b1 := rgb1        and $FF;

        a2 := rgb2 shr 24 and $FF;
        r2 := rgb2 shr 16 and $FF;
        g2 := rgb2 shr  8 and $FF;
        b2 := rgb2        and $FF;

        ta := ta + (a1 - a2);
        tr := tr + (r1 - r2);
        tg := tg + (g1 - g2);
        tb := tb + (b1 - b2);

        LOutIndex := LOutIndex + AHeight;
      end;

      LInIndex := LInIndex + AWidth;
    end;
  finally
    SetLength(LDivideArray, 0);
    LDivideArray := nil;
  end;
{$RANGECHECKS ON}
end;

function TgmBoxBlurFilter.Execute(
  const ASourceBmp, ADestBmp: TBitmap32): Boolean;
var
  LWidth, LHeight: Integer;
  LPixelCount, i : Integer;
  LInPixels      : TArrayOfColor32;
  LOutPixels     : TArrayOfColor32;
begin
  Result := False;
  
  if (not Assigned(ASourceBmp)) or
     (not Assigned(ADestBmp)) or
     (ASourceBmp.Width <= 0) or
     (ASourceBmp.Height <= 0) then
  begin
    Exit;
  end;

  LWidth      := ASourceBmp.Width;
  LHeight     := ASourceBmp.Height;
  LPixelCount := LWidth * LHeight;

  SetLength(LInPixels, LPixelCount);
  SetLength(LOutPixels, LPixelCount);
  try
    GetRGB(ASourceBmp, LInPixels);

    for i := 0 to (FIterations - 1) do
    begin
      Blur(LInPixels, LOutPixels, LWidth, LHeight, FHorzRadius);
      Blur(LOutPixels, LInPixels, LHeight, LWidth, FVertRadius);
    end;

    Result := SetRGB(ADestBmp, LWidth, LHeight, LInPixels);
  finally
    SetLength(LInPixels,  0);
    SetLength(LOutPixels, 0);

    LInPixels  := nil;
    LOutPixels := nil;
  end;
end; 

function TgmBoxBlurFilter.ToString: string;
begin
  Result := 'Blur/Box Blur...';
end;

end.
