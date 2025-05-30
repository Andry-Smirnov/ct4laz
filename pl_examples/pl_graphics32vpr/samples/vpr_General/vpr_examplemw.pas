unit vpr_examplemw;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  GR32, GR32_Blend, GR32_Filters, GR32_Image,  GR32_Polygons,
  GR32_LowLevel,  GR32_MicroTiles, GR32_Rasterizers,
  GR32_Resamplers, GR32_System,
  StdCtrls;

type
  TForm1 = class(TForm)
    Img: TImage32;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  VGR32_VectorGraphics, VGR32_PolygonsEx, GR32_VectorUtils;

function Ellipse(const X, Y, Rx, Ry: TFloat): TArrayOfFloatPoint;
const
  M: TFloat = 1/360*2*Pi;
var
  I: Integer;
  t: TFloat;
begin
  SetLength(Result, 360);
  for I := 0 to 359 do
  begin
    t := I * M;
    Result[I].X := Rx * Cos(t) + X;
    Result[I].Y := Ry * Sin(t) + Y;
  end;
end;

procedure DrawSimplePolygon(Renderer: TPathRenderer; Cx, Cy, Rx, Ry: TFloat);
var
  I: Integer;
begin
  Renderer.MoveTo(Cx, Cy);
  for I := 0 to 240 do
  begin
    Renderer.LineTo(Cx + Rx * I / 200 * Cos(I / 8), Cy + Ry * I / 200 * Sin(I / 8));
  end;
end;

procedure PaintBitmap(Dst: TBitmap32);
var
  Renderer: TPathRenderer;
  Polygon: TArrayOfFloatPoint;
  Dashed: TArrayOfArrayOfFloatPoint;
  xDashArray: TArrayOfFloat;
  I: Integer;
begin
  { (a) Fill and stroke a polygon using PolygonFS/PolylineFS }
  Polygon := Ellipse(240, 240, 200, 200);
  PolygonFS(Dst, Polygon, $ffff99cc);
  PolyLineFS(Dst, Polygon, $ffcc6699, True, 5);

  { (b) Create a rounded path with a dashed outline }
  SetLength(Polygon, 4);
  Polygon[0] := FloatPoint(400, 80);
  Polygon[1] := FloatPoint(80, 80);
  Polygon[2] := FloatPoint(400, 400);
  Polygon[3] := FloatPoint(80, 400);
  Polygon := BuildPolyline(Polygon, 50, jsRound, esRound);
  PolygonFS(Dst, Polygon, $7f66cc99);

  SetLength(xDashArray, 4);
  xDashArray[0] := 10;
  xDashArray[1] := 10;
  xDashArray[2] := 30;
  xDashArray[3] := 10;
  Dashed := BuildDashedLine(Polygon, xDashArray);

  for I := 0 to High(Dashed) do
  begin
    PolylineFS(Dst, Dashed[I], clBlack32, False, 4);
  end;

  { (c) Render a path with an outline using a path renderer }
  Renderer := TPathRenderer.Create;
  try
    Renderer.Bitmap := Dst;
    Renderer.EndStyle := esSquare;

    Renderer.BeginPath;
    Renderer.StrokeColor := clBlack32;
    Renderer.StrokeWidth := 8;
    DrawSimplePolygon(Renderer, 240, 240, 100, 100);
    Renderer.EndPath;

    Renderer.BeginPath;
    Renderer.StrokeColor := $ff6699cc;
    Renderer.StrokeWidth := 6;
    DrawSimplePolygon(Renderer, 240, 240, 100, 100);
    Renderer.EndPath;
  finally
    Dst.Changed;
    Renderer.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Img.SetupBitmap(True, clWhite32);
  PaintBitmap(Img.Bitmap);
end;

function MakeArrayOfFloatPoints(const a: array of single):
TArrayOfFloatPoint;
var
   i, len: integer;
begin
   len := length(a) div 2;
   setlength(result, len);
   if len = 0 then exit;
   for i := 0 to len -1 do
   begin
     result[i].X := a[i*2];
     result[i].Y := a[i*2 +1];
   end;
end;

end.
