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


unit gmIllusion;

{$MODE DELPHI}

interface

uses
  Math, GR32, GR32_LowLevel;

type
  TgmIllusionTypes = (itType1, itType2);

  TgmGimpIllusion = class(TObject)
  private
    FSourceBitmap: TBitmap32;
    FDivision    : Integer;
    FType        : TgmIllusionTypes;
  public
    constructor Create(const ASourceBmp: TBitmap32);
    destructor Destroy; override;

    procedure Execute(ADestBmp: TBitmap32);

    property IllusionDivision: Integer          read FDivision write FDivision;
    property IllusionType    : TgmIllusionTypes read FType     write FType;
  end;

implementation

const
  G_PI   = 3.14159265358979323846;
  G_PI_2 = 1.57079632679489661923;

constructor TgmGimpIllusion.Create(const ASourceBmp: TBitmap32);
begin
  inherited Create;

  FSourceBitmap := TBitmap32.Create;
  FSourceBitmap.Assign(ASourceBmp);

  FDivision := 8;
  FType     := itType1;
end; 

destructor TgmGimpIllusion.Destroy;
begin
  FSourceBitmap.Free;
  inherited Destroy;
end; 

procedure TgmGimpIllusion.Execute(ADestBmp: TBitmap32);
var
  x, y, xx, yy           : Integer;
  LWidth, LHeight, LTemp : Integer;
  LCenterX, LCenterY     : Double;
  LScale, LOffset, cx, cy: Double;
  LAngle, LRadius        : Double;
  a, r, g, b             : Byte;
  LSrcRows, LDstRows     : array of PColor32Array;
begin
{$RANGECHECKS OFF}

  if ADestBmp.Width <> FSourceBitmap.Width then
  begin
    ADestBmp.Width := FSourceBitmap.Width;
  end;

  if ADestBmp.Height <> FSourceBitmap.Height then
  begin
    ADestBmp.Height := FSourceBitmap.Height;
  end;

  SetLength(LSrcRows, FSourceBitmap.Height);
  SetLength(LDstRows, FSourceBitmap.Height);

  for y := 0 to (FSourceBitmap.Height - 1) do
  begin
    LSrcRows[y] := FSourceBitmap.ScanLine[y];
    LDstRows[y] := ADestBmp.ScanLine[y];
  end;

  xx       := 0;
  yy       := 0;
  LWidth   := FSourceBitmap.Width;
  LHeight  := FSourceBitmap.Height;
  LCenterX := LWidth  / 2;
  LCenterY := LHeight / 2;

  LScale  := Sqrt(LWidth * LWidth + LHeight * LHeight) / 2;
  LOffset := Round(LScale / 2);

  for y := 0 to (LHeight - 1) do
  begin
    cy := (y - LCenterY) / LScale;

    for x := 0 to (LWidth - 1) do
    begin
      cx      := (x - LCenterX) / LScale;
      LRadius := Sqrt(cx * cx + cy * cy);

      if FDivision <> 0 then
      begin
        LAngle := Floor( ArcTan2(cy, cx) * FDivision / G_PI_2 ) * G_PI_2 / FDivision + (G_PI / FDivision);

        case FType of
          itType1:
            begin
	            xx := Round( x - LOffset * Cos(LAngle) );
	            yy := Round( y - LOffset * Sin(LAngle) );
            end;

          itType2:
            begin
              xx := Round( x - LOffset * Sin(LAngle) );
	            yy := Round( y - LOffset * Cos(LAngle) );
            end;
        end;

        if xx < 0 then
        begin
          xx := 0;
        end
        else
        if LWidth <= xx then
        begin
          xx := LWidth - 1;
        end;

        if yy < 0 then
        begin
          yy := 0;
        end
        else
        if LHeight <= yy then
        begin
          yy := LHeight - 1;
        end;
      end
      else // if FDivision equals to 0 ...
      begin
        xx := 0;
        yy := 0;
      end;

      LTemp := Round( (1 - LRadius) * (LSrcRows[y, x] shr 24 and $FF) + LRadius * (LSrcRows[yy, xx] shr 24 and $FF) );
      a     := Clamp(LTemp, 255);

      LTemp := Round( (1 - LRadius) * (LSrcRows[y, x] shr 16 and $FF) + LRadius * (LSrcRows[yy, xx] shr 16 and $FF) );
      r     := Clamp(LTemp, 255);

      LTemp := Round( (1 - LRadius) * (LSrcRows[y, x] shr 8 and $FF) + LRadius * (LSrcRows[yy, xx] shr 8 and $FF) );
      g     := Clamp(LTemp, 255);

      LTemp := Round( (1 - LRadius) * (LSrcRows[y, x] and $FF) + LRadius * (LSrcRows[yy, xx] and $FF) );
      b     := Clamp(LTemp, 255);

      LDstRows[y, x] := (a shl 24) or (r shl 16) or (g shl 8) or b;
    end;
  end;

  LSrcRows := nil;
  LDstRows := nil;

{$RANGECHECKS ON}
end; 

end.
