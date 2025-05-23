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
unit gmGeometricObjects2D;

interface

uses
  Graphics, types,
  GR32,
  GR32_LowLevel,
  VGR32_Misc,
  gmGeometry2D, gmMiscFuncs,
  GR32_Polygons;

type
//-- TgmEquilateralTriangle2D --------------------------------------------------

  TgmEquilateralTriangle2D = class(TObject)
  private
    FTipVertex : TPoint; // a vertex of the triangle
    FBaseVertex: TPoint; // coordinates of midpoint on the corresponding bottom line
    FVertices  : TArrayOfPoint;
    FEdgeColor : TColor32;
    FFillColor : TColor32;
    FBackColor : TColor32;

    procedure Clear;
    procedure UpdateVertices;
    procedure SetTipVertex(const APoint: TPoint);
    procedure SetBaseVertex(const APoint: TPoint);

    function GetEdgeLength: Single;
    function GetVertices(AIndex: Integer): TPoint;
  public
    constructor Create(const ATipVertex, ABaseVertex: TPoint);
    destructor Destroy; override;
    
    function Draw(const ABmp: TBitmap32; const AOffsetX, AOffsetY: Integer): Boolean;
    function Erase(const ABmp: TBitmap32; const AOffsetX, AOffsetY: Integer): Boolean;
    function Translate(const ADeltaX, ADeltaY: Integer): Boolean;
    function HorizTranslate(const ADeltaX, ARangeMin, ARangeMax: Integer): Integer;
    function PointInTriangle(const AX, AY: Integer): Boolean;

    property EdgeLength             : Single   read GetEdgeLength;
    property Vertics[index: Integer]: TPoint   read GetVertices;
    property TipVertex              : TPoint   read FTipVertex  write SetTipVertex;
    property BaseVertex             : TPoint   read FBaseVertex write SetBaseVertex;
    property EdgeColor              : TColor32 read FEdgeColor  write FEdgeColor;
    property FillColor              : TColor32 read FFillColor  write FFillColor;
    property BackColor              : TColor32 read FBackColor  write FBackColor;
  end;

//-- TgmSimpleDiamond2D --------------------------------------------------------

  TgmSimpleDiamond2D = class(TObject)
  private
    FCenter     : TPoint;  // center point of the diamond
    FHorizRadius: Integer; // horizontal radius of the diamond
    FVertRadius : Integer; // vertical radius of the diamond
    FVertices   : TArrayOfPoint;
    FEdgeColor  : TColor32;
    FFillColor  : TColor32;
    FBackColor  : TColor32;

    procedure Clear;
    procedure UpdateVertices;
    procedure SetCenter(const APoint: TPoint);
    procedure SetHorizRadius(const ARadius: Integer);
    procedure SetVertRadius(const ARadius: Integer);
  public
    constructor Create(const AHorizRadius, AVertRadius: Integer);
    destructor Destroy; override;

    function Draw(const ABmp: TBitmap32; const AOffsetX, AOffsetY: Integer): Boolean;
    function Erase(const ABmp: TBitmap32; const AOffsetX, AOffsetY: Integer): Boolean;
    function HorizTranslate(const ADeltaX, ARangeMin, ARangeMax: Integer): Integer;
    function PointInDiamond(const AX, AY: Integer): Boolean;
    
    procedure Translate(const ADeltaX, ADeltaY: Integer);

    property Center     : TPoint   read FCenter      write SetCenter;
    property EdgeColor  : TColor32 read FEdgeColor   write FEdgeColor;
    property FillColor  : TColor32 read FFillColor   write FFillColor;
    property BackColor  : TColor32 read FBackColor   write FBackColor;
    property HorizRadius: Integer  read FHorizRadius write SetHorizRadius;
    property VertRadius : Integer  read FVertRadius  write SetVertRadius;
  end;

implementation

//-- TgmEquilateralTriangle2D --------------------------------------------------

constructor TgmEquilateralTriangle2D.Create(
  const ATipVertex, ABaseVertex: TPoint);
begin
  inherited Create;

  FTipVertex  := ATipVertex;
  FBaseVertex := ABaseVertex;
  FEdgeColor  := $FF000000;
  FFillColor  := $00000000;
  FBackColor  := Color32(clBtnFace);

  Self.UpdateVertices;
end; { Create }

destructor TgmEquilateralTriangle2D.Destroy;
begin
  Self.Clear;

  inherited Destroy;
end; { Destroy }

procedure TgmEquilateralTriangle2D.Clear;
begin
  SetLength(FVertices, 0);
  FVertices := nil;
end; { Clear }

procedure TgmEquilateralTriangle2D.UpdateVertices;
begin
  Self.Clear;
  FVertices := GetEquilateralTriangleVertices(FTipVertex, FBaseVertex);
end; { UpdateVertices }

procedure TgmEquilateralTriangle2D.SetTipVertex(const APoint: TPoint);
begin
  if (FTipVertex.X <> APoint.X) or (FTipVertex.Y <> APoint.Y) then
  begin
    FTipVertex := APoint;
    Self.UpdateVertices;
  end;
end; { SetTipVertex }

procedure TgmEquilateralTriangle2D.SetBaseVertex(const APoint: TPoint);
begin
  if (FBaseVertex.X <> APoint.X) or (FBaseVertex.Y <> APoint.Y) then
  begin
    FBaseVertex := APoint;
    Self.UpdateVertices;
  end;
end; { SetBaseVertex }

function TgmEquilateralTriangle2D.GetEdgeLength: Single;
begin
  Result := DistBetweenPoints( FloatPoint(FVertices[0]), FloatPoint(FVertices[1]) );
end; { GetEdgeLength }

function TgmEquilateralTriangle2D.GetVertices(AIndex: Integer): TPoint;
var
  LVertexCount: Integer;
begin
  Result       := Point(0, 0);
  LVertexCount := Length(FVertices);

  if (LVertexCount > 0) and 
     (AIndex >= 0) and (AIndex < LVertexCount) then
  begin
    Result := FVertices[AIndex];
  end;
end; { GetVertices }

function TgmEquilateralTriangle2D.Draw(const ABmp: TBitmap32;
  const AOffsetX, AOffsetY: Integer): Boolean;
var
  LPolygon    : TPolygon32;
  LVertex     : TPoint;
  LVertexCount: Integer;
  i           : Integer;
begin
  Result       := False;
  LVertexCount := Length(FVertices);

  if Assigned(ABmp) and (LVertexCount = 4) then
  begin
    LPolygon := TPolygon32.Create;
    try
      for i := 0 to (LVertexCount - 1) do
      begin
        LVertex := FVertices[i];
        OffsetPoint(LVertex, AOffsetX, AOffsetY);
        
        LPolygon.Add( FixedPoint(LVertex) );
      end;

      LPolygon.DrawFill(ABmp, FFillColor);
      LPolygon.DrawEdge(ABmp, FEdgeColor);
    finally
      LPolygon.Free;
    end;

    Result := True;
  end;
end; { Draw }

function TgmEquilateralTriangle2D.Erase(const ABmp: TBitmap32;
  const AOffsetX, AOffsetY: Integer): Boolean;
var
  LPolygon    : TPolygon32;
  LVertex     : TPoint;
  LVertexCount: Integer;
  i           : Integer;
begin
  Result       := False;
  LVertexCount := Length(FVertices);

  if Assigned(ABmp) and (LVertexCount = 4) then
  begin
    LPolygon := TPolygon32.Create;
    try
      for i := 0 to (LVertexCount - 1) do
      begin
        LVertex := FVertices[i];
        OffsetPoint(LVertex, AOffsetX, AOffsetY);

        LPolygon.Add( FixedPoint(LVertex) );
      end;

      LPolygon.DrawFill(ABmp, FBackColor);
    finally
      LPolygon.Free;
    end;

    Result := True;
  end;
end; { Erase }

function TgmEquilateralTriangle2D.Translate(
  const ADeltaX, ADeltaY: Integer): Boolean;
var
  LVertexCount: Integer;
  i           : Integer;
begin
  Result       := False;
  LVertexCount := Length(FVertices);

  if LVertexCount > 0 then
  begin
    OffsetPoint(FTipVertex, ADeltaX, ADeltaY);

    for i := 0 to (LVertexCount - 1) do
    begin
      OffsetPoint(FVertices[i], ADeltaX, ADeltaY);
    end;

    Result := True;
  end;
end; { Translate }

{ Returns horizontally translated amount. }
function TgmEquilateralTriangle2D.HorizTranslate(
  const ADeltaX, ARangeMin, ARangeMax: Integer): Integer;
var
  LVertexCount: Integer;
  LTranslatedX: Integer;
  LDeltaX     : Integer;
  i           : Integer;
begin
  LTranslatedX := Clamp(FTipVertex.X + ADeltaX, ARangeMin, ARangeMax);
  LDeltaX      := LTranslatedX - FTipVertex.X;

  FTipVertex.X  := FTipVertex.X + LDeltaX;
  FBaseVertex.X := FTipVertex.X;

  LVertexCount := Length(FVertices);
  
  if LVertexCount > 0 then
  begin
    for i := 0 to (LVertexCount - 1) do
    begin
      Inc(FVertices[i].X, LDeltaX);
    end;
  end;

  Result := LDeltaX;
end; { HorizTranslate }

function TgmEquilateralTriangle2D.PointInTriangle(
  const AX, AY: Integer): Boolean;
var
  LFixedVertices: TArrayOfFixedPoint;
begin
  LFixedVertices := MakeArrayOfFixedPoints(FVertices);
  try
    Result := PtInPolygon( FixedPoint(AX, AY), LFixedVertices);
  finally
    SetLength(LFixedVertices, 0);
    LFixedVertices := nil;
  end;
end; { PointInTriangle }

//-- TgmSimpleDiamond2D --------------------------------------------------------

constructor TgmSimpleDiamond2D.Create(const AHorizRadius, AVertRadius: Integer);
begin
  inherited Create;

  FCenter      := Point(0, 0);
  FHorizRadius := Abs(AHorizRadius);
  FVertRadius  := Abs(AVertRadius);
  FEdgeColor   := $FF000000;
  FFillColor   := $00000000;
  FBackColor   := Color32(clBtnFace);

  SetLength(FVertices, 5);
  UpdateVertices;
end; { Create }

destructor TgmSimpleDiamond2D.Destroy;
begin
  Self.Clear;
  inherited Destroy;
end; { Destroy }

procedure TgmSimpleDiamond2D.Clear;
begin
  SetLength(FVertices, 0);
  FVertices := nil;
end; { Clear }

procedure TgmSimpleDiamond2D.UpdateVertices;
begin
  FVertices[0] := Point(FCenter.X, FCenter.Y - FVertRadius);
  FVertices[1] := Point(FCenter.X - FHorizRadius, FCenter.Y);
  FVertices[2] := Point(FCenter.X, FCenter.Y + FVertRadius);
  FVertices[3] := Point(FCenter.X + FHorizRadius, FCenter.Y);
  FVertices[4] := FVertices[0];
end; { UpdateVertices }

procedure TgmSimpleDiamond2D.SetCenter(const APoint: TPoint);
begin
  FCenter := APoint;
  UpdateVertices;
end; { SetCenter }

procedure TgmSimpleDiamond2D.SetHorizRadius(const ARadius: Integer);
begin
  FHorizRadius := Abs(ARadius);
  UpdateVertices;
end; { SetHorizRadius }

procedure TgmSimpleDiamond2D.SetVertRadius(const ARadius: Integer);
begin
  FVertRadius := Abs(ARadius);
  UpdateVertices;
end; { SetVertRadius }

function TgmSimpleDiamond2D.Draw(const ABmp: TBitmap32;
  const AOffsetX, AOffsetY: Integer): Boolean;
var
  LPolygon: TPolygon32;
  LVertex : TPoint;
  i       : Integer;
begin
  Result := False;

  if Assigned(ABmp) then
  begin
    LPolygon := TPolygon32.Create;
    try
      for i := 0 to 4 do
      begin
        LVertex := FVertices[i];
        OffsetPoint(LVertex, AOffsetX, AOffsetY);
        
        LPolygon.Add( FixedPoint(LVertex) );
      end;

      LPolygon.DrawFill(ABmp, FFillColor);
      LPolygon.DrawEdge(ABmp, FEdgeColor);
    finally
      LPolygon.Free;
    end;

    Result := True;
  end;
end; { Draw }

function TgmSimpleDiamond2D.Erase(const ABmp: TBitmap32;
  const AOffsetX, AOffsetY: Integer): Boolean;
var
  LPolygon: TPolygon32;
  LVertex : TPoint;
  i       : Integer;
begin
  Result := False;

  if Assigned(ABmp) then
  begin
    LPolygon := TPolygon32.Create;
    try
      for i := 0 to 4 do
      begin
        LVertex := FVertices[i];
        OffsetPoint(LVertex, AOffsetX, AOffsetY);

        LPolygon.Add( FixedPoint(LVertex) );
      end;

      LPolygon.DrawFill(ABmp, FBackColor);
    finally
      LPolygon.Free;
    end;

    Result := True;
  end;
end; { Erase }

procedure TgmSimpleDiamond2D.Translate(const ADeltaX, ADeltaY: Integer);
begin
  OffsetPoint(FCenter, ADeltaX, ADeltaY);
  UpdateVertices;
end; { Translate }

function TgmSimpleDiamond2D.HorizTranslate(
  const ADeltaX, ARangeMin, ARangeMax: Integer): Integer;
var
  LTranslatedX: Integer;
  LDeltaX     : Integer;
begin
  LTranslatedX := Clamp(FCenter.X + ADeltaX, ARangeMin, ARangeMax);
  LDeltaX      := LTranslatedX - FCenter.X;

  FCenter.X := FCenter.X + LDeltaX;
  UpdateVertices;

  Result := LDeltaX;
end; { HorizTranslate }

function TgmSimpleDiamond2D.PointInDiamond(const AX, AY: Integer): Boolean;
var
  LFixedVertices: TArrayOfFixedPoint;
begin
  LFixedVertices := MakeArrayOfFixedPoints(FVertices);
  try
    Result := PtInPolygon( FixedPoint(AX, AY), LFixedVertices);
  finally
    SetLength(LFixedVertices, 0);
    LFixedVertices := nil;
  end;
end; { PointInDiamond }

end.
