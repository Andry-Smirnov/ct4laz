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
unit gmMiscFuncs;

interface

uses

  Graphics,types,

  GR32;

procedure OffsetRect(var ARect: TRect; const ADeltaX, ADeltaY: Integer);
procedure OffsetPoint(var APoint: TPoint; const ADeltaX, ADeltaY: Integer);

procedure DrawCheckerboard(RampImage: TBitmap32; CheckersSize: Integer = 4;
  TransColor: TColor =clLtGray); overload;

procedure DrawCheckerboard(RampImage: TBitmap32; ARect: TRect;
  CheckersSize: Integer = 4; TransColor: TColor = clLtGray); overload;

procedure FrameRectS(const ABmp: TBitmap32; const ARect: TRect;
  const AValue: TColor32);

function MakeArrayOfFixedPoints(const APointArray: TArrayOfPoint): TArrayOfFixedPoint;

implementation

uses

  Classes,

  GR32_LowLevel;

procedure OffsetRect(var ARect: TRect; const ADeltaX, ADeltaY: Integer);
begin
  ARect := Rect(ARect.Left   + ADeltaX,
                ARect.Top    + ADeltaY,
                ARect.Right  + ADeltaX,
                ARect.Bottom + ADeltaY);
end; { OffsetRect }

procedure OffsetPoint(var APoint: TPoint; const ADeltaX, ADeltaY: Integer);
begin
  APoint.X := APoint.X + ADeltaX;
  APoint.Y := APoint.Y + ADeltaY;
end; { OffsetPoint }

procedure DrawCheckerboard(RampImage: TBitmap32; CheckersSize: Integer = 4;
  TransColor: TColor = clLtGray) ;
// original code : GradientEditor.pas ver 1.1 (procedure TGradientEditor.GenRamp;)
// by Alex Rabochy, rablex@ukr.net
begin
  DrawCheckerboard(RampImage, RampImage.BoundsRect, CheckersSize, TransColor);
end;

procedure DrawCheckerboard(RampImage: TBitmap32; ARect: TRect;
  CheckersSize: Integer = 4; TransColor:TColor = clLtGray) ;
// Modified by x2nie 30/11/2011 20:30:47 to use a TRect
var
  x, y        : Integer;
  LTransColor : TColor32;
  LRect       : TRect;
begin
  IntersectRect(LRect, ARect, RampImage.BoundsRect);

  LRect.Top    := Clamp(LRect.Top,    0, RampImage.Height - 1);
  LRect.Left   := Clamp(LRect.Left,   0, RampImage.Width - 1);
  LRect.Bottom := Clamp(LRect.Bottom, 0, RampImage.Height - 1);
  LRect.Right  := Clamp(LRect.Right,  0, RampImage.Width - 1);

  LTransColor := Color32(TransColor);
  
  for Y := LRect.Top to LRect.Bottom do
    for X := LRect.Left to LRect.Right do
    begin
      if Odd(X div CheckersSize) = Odd(Y div CheckersSize) then
        RampImage[X, Y] := LTransColor
      else
        RampImage[X, Y] := clWhite32;
    end;
end;

procedure FrameRectS(const ABmp: TBitmap32; const ARect: TRect;
  const AValue: TColor32);
// we can't use Bitmap32.FrameRectS(), because they decrase width & height
// we use our own DrawRect that is meet our precission.
begin
  with ARect do
  begin
    ABmp.HorzLineS(Left, Top, Right, AValue);      //top
    ABmp.VertLineS(Left, Top, Bottom, AValue);     //left
    ABmp.VertLineS(Right, Top, Bottom, AValue);    //right
    ABmp.HorzLineS(Left, Bottom, Right, AValue);   //bottom
  end;
end;

function MakeArrayOfFixedPoints(const APointArray: TArrayOfPoint): TArrayOfFixedPoint;
var
  i, LPointCount: Integer;
begin
  LPointCount := Length(APointArray);
  SetLength(Result, LPointCount);
  
  for i := 0 to (LPointCount - 1) do
  begin
    Result[i] := FixedPoint(APointArray[i]);
  end;
end; { MakeArrayOfFixedPoints }

end.
