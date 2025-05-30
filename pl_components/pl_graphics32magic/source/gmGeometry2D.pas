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

unit gmGeometry2D;

interface

uses
  Math,
  Types,GR32,
  VGR32_Misc, GR32_Math;

type
  TDPoint = record
    X: Double;
    Y: Double;
  end;

  { The following two functions are written by Rodrigo Alves Pons.

    -- Quote --

    Author: Rodrigo Alves Pons
    email: rodpons@ig.com.br
    webpage: www.geocities.com/rodrigo_alves_pons/
    Version: 2006-12-07
    functions to calculate the area and centroid of a polygon,
    according to the algorithm defined at:
    http://local.wasp.uwa.edu.au/~pbourke/geometry/polyarea/ }

  function PolygonArea(N: Integer; Points: array of TDPoint): Double;
  function PolygonCentroid(N: Integer; Points: array of TDPoint; Area: Double): TDPoint;

  function GetEquilateralTriangleVertices(const ATipVertex, ABaseVertex: TPoint): TArrayOfPoint;

implementation

//-- Rodrigo Alves Pons -- Begin -----------------------------------------------

{ The following two functions are written by Rodrigo Alves Pons.

  -- Quote --

  Author: Rodrigo Alves Pons
  email: rodpons@ig.com.br
  webpage: www.geocities.com/rodrigo_alves_pons/
  Version: 2006-12-07
  functions to calculate the area and centroid of a polygon,
  according to the algorithm defined at:
  http://local.wasp.uwa.edu.au/~pbourke/geometry/polyarea/ }

function PolygonArea(N: Integer; Points: array of TDPoint): Double;
var
  i, j: Integer;
  Area: Double;
begin
  Area :=0;
  
  for i := 0 to N-1 do
  begin
    j    := (i + 1) mod N;
    Area := Area + Points[i].X * Points[j].Y - Points[j].X * Points[i].Y;
  end;
  
  PolygonArea := Area / 2;
end; { PolygonArea }

function PolygonCentroid(N: Integer; Points: array of TDPoint; Area: Double): TDPoint;
var
  i, j: Integer;
  C   : TDPoint;
  P   : Double;
begin
  C.X := 0;
  C.Y := 0;

  for i := 0 to N-1 do
  begin
    j := (i + 1) mod N;
    P := Points[i].X * Points[j].Y - Points[j].X * Points[i].Y;

    C.X := C.X + (Points[i].X + Points[j].X) * P;
    C.Y := C.Y + (Points[i].Y + Points[j].Y) * P;
  end;

  C.X := C.X / (6 * Area);
  C.Y := C.Y / (6 * Area);

  PolygonCentroid := C;
end; { PolygonCentroid }

//-- Rodrigo Alves Pons -- End -------------------------------------------------

{ Return 3 vertices for a equilateral triangle.
  @ATipVertex -- A vertex of the triangle.
  @ABaseVertex -- Coordinates of midpoint on the corresponding bottom line. }
function GetEquilateralTriangleVertices(
  const ATipVertex, ABaseVertex: TPoint): TArrayOfPoint;
var
  LTriangleHeight : Single;
  LHalfEdgeLength : Single;
  LHeightTheta    : Single;
  LVertexRadians  : Single;
  LDeltaX, LDeltaY: Integer;
begin
  Result := nil;

  if (ATipVertex.X = ABaseVertex.X) and (ATipVertex.Y = ABaseVertex.Y) then
  begin
    Exit;
  end;

  LTriangleHeight := DistBetweenPoints( FloatPoint(ATipVertex), FloatPoint(ABaseVertex) );

  if LTriangleHeight < 1.0 then
  begin
    Exit;
  end;

  SetLength(Result, 4);
  Result[0] := ATipVertex;

  LHalfEdgeLength := Tan(rad30) * LTriangleHeight;
  LDeltaX         := ATipVertex.X - ABaseVertex.X;
  LDeltaY         := ATipVertex.Y - ABaseVertex.Y;
  LHeightTheta    := ArcTan2(LDeltaY, LDeltaX);

  LVertexRadians := LHeightTheta + rad90;
  Result[1].X    := Round( ABaseVertex.X + LHalfEdgeLength * Cos(LVertexRadians) );
  Result[1].Y    := Round( ABaseVertex.Y + LHalfEdgeLength * Sin(LVertexRadians) ); 

  LVertexRadians := LHeightTheta - rad90;
  Result[2].X    := Round( ABaseVertex.X + LHalfEdgeLength * Cos(LVertexRadians) );
  Result[2].Y    := Round( ABaseVertex.Y + LHalfEdgeLength * Sin(LVertexRadians) );

  Result[3] := Result[0];
end; { GetEquilateralTriangleVertices }

end.
