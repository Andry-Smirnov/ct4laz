unit Growmw;

interface

uses
  LCLIntf, LResources,LCLType, LMessages ,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, Math, ExtDlgs, Menus, GR32_Paths, GR32_Polygons,
  GR32_VectorUtils, GR32, GR32_Blend, GR32_Image, GR32_Clipper;

type
  TFormGrow = class(TForm)
    Image: TImage32;
    PnlImage: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Refresh1: TMenuItem;
    Options1: TMenuItem;
    mnuInflatePolygon: TMenuItem;
    mnuInflatePolyLine: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure ImageClick(Sender: TObject);
    procedure ImageResize(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuInflatePolygonClick(Sender: TObject);
  public
    savedBmp: TBitmap32;
  end;

var
  FormGrow: TFormGrow;

implementation

{$R *.lfm}

{$IFDEF Darwin}
uses
  MacOSAll;
{$ENDIF}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function _Area(const path: TArrayOfFloatPoint): single;
var
  i, j, highI: Integer;
  d: single;
begin
  Result := 0.0;
  highI := High(path);
  if (highI < 2) then Exit;
  j := highI;
  for i := 0 to highI do
  begin
    d := (path[j].X + path[i].X);
    Result := Result + d * (path[j].Y - path[i].Y);
    j := i;
  end;
  Result := -Result * 0.5;
end;
//------------------------------------------------------------------------------

function MakePath(const pts: array of integer): TArrayOfFloatPoint;
var
  i, len: Integer;
begin
  Result := nil;
  len := length(pts) div 2;
  setlength(Result, len);
  for i := 0 to len -1 do
    Result[i] := FloatPoint(pts[i*2], pts[i*2 +1]);
end;
//------------------------------------------------------------------------------

function MakeRandomPath(maxWidth, maxHeight, count: Integer): TArrayOfFloatPoint;
var
  i: Integer;
begin
  setlength(Result, count);
  for i := 0 to count -1 do
    with Result[i] do
    begin
      X := 20 + Random(maxWidth - 40);
      Y := 20 + Random(maxHeight - 40);
    end;
end;
//------------------------------------------------------------------------------

function Union(const paths: TArrayOfArrayOfFloatPoint;
  fillRule: TFillRule = frEvenOdd): TArrayOfArrayOfFloatPoint;
begin
  with TClipper.Create do
  try
    AddPaths(paths, ptSubject, false);
    Execute(ctUnion, fillRule, Result);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function GeneratePolygon(maxWidth,
  maxHeight, edgeCount: integer): TArrayOfFloatPoint;
var
  PolyPts: TArrayOfArrayOfFloatPoint;
  i,j: integer;
  area, a: single;
begin
  setLength(polyPts, 1);
  polyPts[0] := MakeRandomPath(maxWidth, maxHeight, edgeCount);
  // NOTE: INFLATEPATHS WILL BEHAVE IN AN UNDERTERMINED FASHION
  // WHENEVER SELF-INTERSECTING POLYGONS ARE ENCOUNTERED.

  // so, remove self-intersections
  PolyPts := Union(PolyPts);
  // and find the largest polygon ...
  j := 0;
  area := Abs(_Area(polyPts[0]));
  for i := 1 to high(polyPts) do
  begin
    a := Abs(_Area(polyPts[i]));
    if a <= area then Continue;
    j := i;
    area := a;
  end;
  Result := polyPts[j];
end;
//------------------------------------------------------------------------------

procedure TFormGrow.FormCreate(Sender: TObject);
begin
  SetGamma(1.4);
end;

procedure TFormGrow.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormGrow.ImageResize(Sender: TObject);
begin
  Image.Bitmap.SetSize(PnlImage.ClientWidth, PnlImage.ClientHeight);
  Refresh1Click(nil);
end;

procedure TFormGrow.mnuInflatePolygonClick(Sender: TObject);
begin
  if not TMenuItem(sender).Checked then
  begin
    mnuInflatePolygon.Checked := not mnuInflatePolygon.Checked;
    mnuInflatePolyLine.Checked := not mnuInflatePolyLine.Checked;
  end;
  Refresh1Click(nil);
end;

procedure TFormGrow.ImageClick(Sender: TObject);
begin
  Refresh1Click(nil);
end;

procedure TFormGrow.Refresh1Click(Sender: TObject);
var
  i: integer;
  polyPts: TArrayOfArrayOfFloatPoint;
begin
  Image.Bitmap.Clear(clWhite32);
  if mnuInflatePolyLine.Checked then
  begin
    // INFLATE (GROW / OFFSET) A POLYLINE ...
    setLength(polyPts, 1);
    polyPts[0] := MakeRandomPath(Image.Bitmap.Width, Image.Bitmap.Height, 7);
    PolyPolylineFS(image.Bitmap, polyPts, clBlack32, false, 1);

    polyPts := InflatePaths(polyPts, 20, jtRoundEx, etOpenRound);

    PolyPolylineFS(image.Bitmap, polyPts, clRed32, true, 1);
    PolyPolygonFS(image.Bitmap, polyPts, $10FF0000);
  end else
  begin
    // INFLATE (GROW / OFFSET) A POLYGON ...
    setLength(polyPts, 1);
    repeat
      polyPts[0] := GeneratePolygon(Image.Bitmap.Width, Image.Bitmap.Height, 5);
    until Length(polyPts[0]) > 3;

    PolyPolygonFS(image.Bitmap, polyPts, $100000FF);
    PolyPolylineFS(image.Bitmap, polyPts, clBlack32, true, 1);

    polyPts := InflatePaths(polyPts, 10, jtRoundEx, etPolygon, 1);

    PolyPolylineFS(image.Bitmap, polyPts, clRed32, true, 1);
    PolyPolygonFS(image.Bitmap, polyPts, $10FF0000);
  end;
end;

end.
