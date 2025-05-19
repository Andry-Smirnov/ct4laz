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


unit gmMotionBlurFilter;

interface

uses

  GR32;

type
  TgmMotionBlurFilter = class(TObject)
  private
    FAngle    : Single;   // in radians
    FFalloff  : Single;
    FDistance : Single;
    FZoom     : Single;
    FRotation : Single;   // in radians
    FWrapEdges: Boolean;
  public
    constructor Create;

    function Execute(const ASourceBmp, ADestBmp: TBitmap32): Boolean;
    function ToString: string;

    property Angle      : Single  read FAngle     write FAngle;
    property Distance   : Single  read FDistance  write FDistance;
    property Rotation   : Single  read FRotation  write FRotation;
    property Zoom       : Single  read FZoom      write FZoom;
    property IsWrapEdges: Boolean read FWrapEdges write FWrapEdges;
  end;

implementation

uses

  Math,

  GR32_Transforms, GR32_LowLevel,
{ own }
  gmImageMath, gmImageProcessFuncs;

constructor TgmMotionBlurFilter.Create;
begin
  inherited Create;

  FAngle     := 0.0;
  FFalloff   := 1.0;
  FDistance  := 1.0;
  FZoom      := 0.0;
  FRotation  := 0.0;
  FWrapEdges := False;
end;

function TgmMotionBlurFilter.Execute(
  const ASourceBmp, ADestBmp: TBitmap32): Boolean;
var
  LWidth         : Integer;
  LHeight        : Integer;
  LPixelCount    : Integer;
  i, x, y        : Integer;
  cx, cy         : Integer;
  LNewX, LNewY   : Integer;
  LCount         : Integer;
  LIndex         : Integer;
  LRepetitions   : Integer;
  a, r, g, b     : Integer;
  aa, rr, gg, bb : Byte;
  LSinAngle      : Single;
  LCosAngle      : Single;
  LImageRadius   : Single;
  LTranslateX    : Single;
  LTranslateY    : Single;
  LMaxDistance   : Single;
  f, s           : Single;
  tx, ty         : Single;
  AT             : TAffineTransformation;
  p              : TFloatPoint;
  LColor         : TColor32;
  LInPixels      : TArrayOfColor32;
  LOutPixels     : TArrayOfColor32;
begin
{$RANGECHECKS OFF}
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
  AT := TAffineTransformation.Create;
  try
    GetRGB(ASourceBmp, LInPixels);

    LSinAngle := Sin(FAngle);
    LCosAngle := Cos(FAngle);

    cx     := LWidth div 2;
    cy     := LHeight div 2;
    LIndex := 0;

    LImageRadius := Sqrt(cx * cx + cy * cy);
    LTranslateX  := FDistance * LCosAngle;
    LTranslateY  := FDistance * (-LSinAngle);
    LMaxDistance := FDistance + Abs(FRotation * LImageRadius) + (FZoom * LImageRadius);
    LRepetitions := Trunc(LMaxDistance);

    for y := 0 to (LHeight - 1) do
    begin
      for x := 0 to (LWidth - 1) do
      begin
        a := 0;
        r := 0;
        g := 0;
        b := 0;

        LCount := 0;

        for i := 0 to (LRepetitions - 1) do
        begin
          f := i / LRepetitions;

          p.X := x;
          p.Y := y;

          AT.Clear;  // set to identity matrix

          { NOTE:

              In the original Java code, the transform matrix concatenation
              is Translation->Scaling->Rotation, but in our translated code,
              the matrix concatenation is in opposite order. We think that
              this is due to the order of matrix concatenation in Java is
              different from GR32.
           }

          // Step 1 -- Rotation
          if FRotation <> 0 then
          begin
            AT.Rotate(cx, cy, -RadToDeg(FRotation * f));
          end;

          // Step 2 -- Scale
          s := 1 - FZoom * f;

          { Special notice, the order of the following two line in the
            original Java code is:

              AT.Scale(s, s);
              AT.Translate(-cx, -cy);

            We think that this is because the matrix concatenation in Java is
            different from GR32.
            }
          AT.Translate(-cx, -cy);
          AT.Scale(s, s);

          // Step 3 -- Tranlation
          tx := cx + f * LTranslateX;
          ty := cy + f * LTranslateY;
          AT.Translate(tx, ty);

          p := AT.Transform(p);

          LNewX := Trunc(p.X);
          LNewY := Trunc(p.Y);

          if (LNewX < 0) or (LNewX >= LWidth) then
          begin
            if FWrapEdges then
            begin
              LNewX := ImageMath.Modul(LNewX, LWidth);
            end
            else
            begin
              Break;
            end;
          end;

          if (LNewY < 0) or (LNewY >= LHeight) then
          begin
            if FWrapEdges then
            begin
              LNewY := ImageMath.Modul(LNewY, LHeight);
            end
            else
            begin
              Break;
            end;
          end;

          Inc(LCount);

          LColor := LInPixels[LNewY * LWidth + LNewX];

          Inc(a, LColor shr 24 and $FF);
          Inc(r, LColor shr 16 and $FF);
          Inc(g, LColor shr 8 and $FF);
          Inc(b, LColor and $FF);
        end;

        if LCount = 0 then
        begin
          LOutPixels[LIndex] := LInPixels[LIndex];
        end
        else
        begin
          aa := Clamp(a div LCount);
          rr := Clamp(r div LCount);
          gg := Clamp(g div LCount);
          bb := Clamp(b div LCount);

          LOutPixels[LIndex] := (aa shl 24) or (rr shl 16) or (gg shl 8) or bb;
        end;

        Inc(LIndex);
      end;
    end;

    SetRGB(ADestBmp, LWidth, LHeight, LOutPixels);

  finally
    SetLength(LInPixels, 0);
    SetLength(LOutPixels, 0);

    LInPixels  := nil;
    LOutPixels := nil;

    AT.Free;
  end;
{$RANGECHECKS ON}
end;

function TgmMotionBlurFilter.ToString: string;
begin
  Result := 'Blur/Motion Blur...';
end; 

end.
