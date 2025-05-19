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

unit gmUtilsLineLibrary;

interface

uses
  LCLIntf, LCLType, LMessages,
  Forms,
  ExtCtrls,
  Math,
  Classes;

type
  TLineSelected = (lsNotSelected, lsPoint1, lsPoint2, lsLine);

  // TLineOrientation is used as part of the heuristic algorithm that decides
  // if a line is selected and if lines intersect}
  TLineOrientation = (loPoint, loHorizontal, loVertical);

  TVector = record
    StartPoint: TPoint;
    EndPoint: TPoint;
  end;

  TFigure = (TRectangle, TTriangle, TCircle);

  TPolygon = record
    X, Y: real;
    case Tipo: TFigure of
      TRectangle: (Height, Width: real);
      TTriangle: (Side1, Side2, Angle: real);
      TCircle: (Radius: real);
  end;

  TFigura = (toLine, toPolygon);

  TObjeto = record
    Numero: integer;
    case Tipo: TFigura of
      toLine: (linha: TVector);
      toPolygon: (polygon: TPolygon);
  end;


function AddPoints(const PointA, PointB: TPoint): TPoint;
function SubtractPoints(const PointA, PointB: TPoint): TPoint;

procedure CalcLineParameters(const PointA, PointB: TPoint; var Slope, Intercept: double;
  var LineOrientation: TLineOrientation);
function NearLine(const Target, Point1, Point2: TPoint): boolean;


procedure RestrictCursorToDrawingArea(const Image: TImage);
procedure RemoveCursorRestrictions;


function SquareContainsPoint(const SquareCenter: TPoint; const SquareHalfSize: integer; {pixels}
  const TestPoint: TPoint): boolean;

implementation

function AddPoints(const PointA, PointB: TPoint): TPoint;
begin
  with Result do
  begin
    X := PointA.X + PointB.X;
    Y := PointA.Y + PointB.Y;
  end;
end {AddPoints};


function SubtractPoints(const PointA, PointB: TPoint): TPoint;
begin
  with Result do
  begin
    X := PointA.X - PointB.X;
    Y := PointA.Y - PointB.Y;
  end;
end {SubtractPoints};


// Determine whether a line is ltHorizonal or ltVertical,  along with the
// appropriate slope and intercept FOR point-slope line  equations.  These
// parameters are used to determine if a line is selected.
procedure CalcLineParameters(const PointA, PointB: TPoint; var Slope, Intercept: double;
  var LineOrientation: TLineOrientation);
var
  Delta: TPoint;
begin
  Delta := SubtractPoints(PointB, PointA);

  if (Delta.X = 0) and (Delta.Y = 0) then
  begin
    // This special CASE should never happen if iMinPixels > 0
    LineOrientation := loPoint;
    Slope := 0.0;
    Intercept := 0.0;
  end
  else
  begin

    if ABS(Delta.X) >= ABS(Delta.Y) then
    begin
      // The line is more horizontal than vertical.  Determine values FOR
      // equation:  Y = slope*X + intercept
      LineOrientation := loHorizontal;
      try
        Slope := Delta.Y / Delta.X   {conventional slope in geometry}
      except
        Slope := 0.0
      end;
      Intercept := PointA.Y - PointA.X * Slope;
    end
    else
    begin
      // The line is more vertical than horizontal.  Determine values FOR
      // equation:  X = slope*Y + intercept
      LineOrientation := loVertical;
      try
        Slope := Delta.X / Delta.Y  {reciprocal of conventional slope}
      except
        Slope := 0.0
      end;
      Intercept := PointA.X - PointA.Y * Slope;
    end;

  end;
end {CalcLineParameters};


// Determine if Target1 is "near" line segment between Point1 and Point2
function NearLine(const Target, Point1, Point2: TPoint): boolean;
const
  LineSelectFuzz = 4;  // Pixel "fuzz" used in line selection
var
  Intercept: double;
  LineOrientation: TLineOrientation;
  maxX: integer;
  maxY: integer;
  minX: integer;
  minY: integer;
  Slope: double;
  xCalculated: integer;
  yCalculated: integer;
begin
  Result := False;

  // If an Endpoint is not selected, was part of line selected?
  CalcLineParameters(Point1, Point2, Slope, Intercept, LineOrientation);

  case LineOrientation of
    loHorizontal:
    begin
      minX := MinIntValue([Point1.X, Point2.X]);
      maxX := MaxIntValue([Point1.X, Point2.X]);
      // first check if selection within horizontal range of line
      if (Target.X >= minX) and (Target.X <= maxX) then
      begin
        // Since X is within range of line, now see if Y value is close
        // enough to the calculated Y value FOR the line to be selected.
        yCalculated := ROUND(Slope * Target.X + Intercept);
        if ABS(yCalculated - Target.Y) <= LineSelectFuzz then
          Result := True;
      end;
    end;

    loVertical:
    begin
      minY := MinIntValue([Point1.Y, Point2.Y]);
      maxY := MaxIntValue([Point1.Y, Point2.Y]);
      // first check if selection within vertical range of line
      if (Target.Y >= minY) and (Target.Y <= maxY) then
      begin
        // Since Y is within range of line, now see if X value is close
        // enough to the calculated X value FOR the line to be selected.
        xCalculated := ROUND(Slope * Target.Y + Intercept);
        if ABS(xCalculated - Target.X) <= LineSelectFuzz then
          Result := True;
      end;
    end;

    loPoint:
    // Do nothing -- should not occur
  end;
end {NearLine};


///////////////////////////////////////////////////////////////////////

procedure RestrictCursorToDrawingArea(const Image: TImage);
var
  CursorClipArea: TRect;
begin
  CursorClipArea := Bounds(Image.ClientOrigin.X, Image.ClientOrigin.Y,
    Image.Width, Image.Height);
 // ClipCursor(@CursorClipArea); ct9999
end ;


procedure RemoveCursorRestrictions;
begin
 // ClipCursor(nil);  ct9999
end ;


///////////////////////////////////////////////////////////////////////

function SquareContainsPoint(const SquareCenter: TPoint; const SquareHalfSize: integer; {pixels}
  const TestPoint: TPoint): boolean;
begin
  Result := (TestPoint.X >= SquareCenter.X - SquareHalfSize) and (TestPoint.X <= SquareCenter.X + SquareHalfSize) and
    (TestPoint.Y >= SquareCenter.Y - SquareHalfSize) and (TestPoint.Y <= SquareCenter.Y + SquareHalfSize);
end;


end.




