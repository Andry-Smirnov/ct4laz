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

unit gmVectorLayer;

interface

uses
  Classes,
  Graphics,
  Math,
  types,
  GR32,
  gmCrop,
  gmFigures,
  gmLayers,
  gmResamplers,
  gmTypes,
  gmAlphaFuncs,
  gmMath;

type
  { TgmVectorLayer }

  TgmVectorLayer = class(TgmSpecialPixelizedLayer)
  private
    FFigureList: TgmFigureList;
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TgmLayerList; const ALayerWidth, ALayerHeight: integer);

    destructor Destroy; override;

    function GetCopy: TgmCustomLayer; override;

    procedure CropLayer(ACrop: TgmCrop; const ABackColor: TColor32); override;
    procedure CropLayerRect(const ACropArea: TRect; const ABackgroundColor: TColor32); override;
    procedure DrawAllFiguresOnLayer;
    procedure DrawUnselectedAndLockedLayersOnLayer;

    procedure ResizeLayerCanvas(const ANewWidth, ANewHeight: integer; const AAnchor: TgmAnchorDirection;
      const ABackgroundColor: TColor32); override;

    procedure ResizeLayerImage(const ANewWidth, ANewHeight: integer; const ASamplingOptions: TgmResamplingOptions); override;

    procedure RotateCanvas(const ADegrees: integer; const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32); override;

    property FigureList: TgmFigureList read FFigureList;
  end;

implementation

{ TgmVectorLayer }

constructor TgmVectorLayer.Create(AOwner: TgmLayerList; const ALayerWidth, ALayerHeight: integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FDefaultLayerName := 'Vector';
  FLayerThumbEnabled := True;

  FFigureList := TgmFigureList.Create();

  FLayerThumb := TBitmap32.Create();
  with FLayerThumb do
  begin
    SetSize(LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);
  end;

  UpdateLayerThumbnail();
end;

destructor TgmVectorLayer.Destroy;
begin
  FFigureList.Free();
  inherited;
end;

procedure TgmVectorLayer.CropLayer(ACrop: TgmCrop; const ABackColor: TColor32);
var
  i, j: integer;
  LCropWidth: integer;
  LCropHeight: integer;
  LNewWidth: integer;
  LNewHeight: integer;
  LOffsetVector: TPoint;
  LTopLeft: TPoint;
  LFigureObj: TgmFigureObject;
begin
  if Assigned(ACrop) then
  begin
    LCropWidth := Abs(ACrop.FCropEnd.X - ACrop.FCropStart.X);
    LCropHeight := Abs(ACrop.FCropEnd.Y - ACrop.FCropStart.Y);
    LNewWidth := LCropWidth;
    LNewHeight := LCropHeight;

    if ACrop.IsResized then
    begin
      LNewWidth := ACrop.ResizeW;
      LNewHeight := ACrop.ResizeH;
    end;

    if (LNewWidth > 0) and (LNewHeight > 0) then
    begin
      inherited;

      if FFigureList.Count > 0 then
      begin
        LTopLeft := FFigureList.GetTopLeftFromAllFigures();
        LOffsetVector.X := LTopLeft.X - ACrop.FCropStart.X;
        LOffsetVector.Y := LTopLeft.Y - ACrop.FCropStart.Y;

        for i := 0 to (FFigureList.Count - 1) do
        begin
          LFigureObj := TgmFigureObject(FFigureList.Items[i]);

          case LFigureObj.Flag of
            ffStraightLine,
            ffRectangle,
            ffSquare,
            ffRoundRectangle,
            ffRoundSquare,
            ffEllipse,
            ffCircle:
            begin
              // calculating the offset
              with LFigureObj do
              begin
                FStartPoint := Point(FStartPoint.X - LTopLeft.X + LOffsetVector.X, FStartPoint.Y -
                  LTopLeft.Y + LOffsetVector.Y);

                FEndPoint := Point(FEndPoint.X - LTopLeft.X + LOffsetVector.X, FEndPoint.Y -
                  LTopLeft.Y + LOffsetVector.Y);

                // scaling
                if ACrop.IsResized then
                begin
                  FStartPoint := ScalePoint(FStartPoint, LCropWidth, LCropHeight, LNewWidth, LNewHeight);
                  FEndPoint := ScalePoint(FEndPoint, LCropWidth, LCropHeight, LNewWidth, LNewHeight);
                end;
              end;
            end;

            ffCurve:
            begin
              // calculating the offset
              with LFigureObj do
              begin
                FStartPoint := Point(FStartPoint.X - LTopLeft.X + LOffsetVector.X, FStartPoint.Y -
                  LTopLeft.Y + LOffsetVector.Y);

                FEndPoint := Point(FEndPoint.X - LTopLeft.X + LOffsetVector.X, FEndPoint.Y -
                  LTopLeft.Y + LOffsetVector.Y);

                FCurvePoint1 := Point(FCurvePoint1.X - LTopLeft.X + LOffsetVector.X, FCurvePoint1.Y -
                  LTopLeft.Y + LOffsetVector.Y);

                FCurvePoint2 := Point(FCurvePoint2.X - LTopLeft.X + LOffsetVector.X, FCurvePoint2.Y -
                  LTopLeft.Y + LOffsetVector.Y);

                // scaling
                if ACrop.IsResized then
                begin
                  FStartPoint := ScalePoint(FStartPoint, LCropWidth, LCropHeight, LNewWidth, LNewHeight);
                  FEndPoint := ScalePoint(FEndPoint, LCropWidth, LCropHeight, LNewWidth, LNewHeight);
                  FCurvePoint1 := ScalePoint(FCurvePoint1, LCropWidth, LCropHeight, LNewWidth, LNewHeight);
                  FCurvePoint2 := ScalePoint(FCurvePoint2, LCropWidth, LCropHeight, LNewWidth, LNewHeight);
                end;
              end;
            end;

            ffPolygon,
            ffRegularPolygon:
            begin
              with LFigureObj do
              begin
                for j := Low(FPolygonPoints) to High(FPolygonPoints) do
                begin
                  // calculating the offset
                  FPolygonPoints[j] := Point(FPolygonPoints[j].X - LTopLeft.X + LOffsetVector.X,
                    FPolygonPoints[j].Y - LTopLeft.Y + LOffsetVector.Y);

                  // scaling
                  if ACrop.IsResized then
                  begin
                    FPolygonPoints[j] := ScalePoint(FPolygonPoints[j], LCropWidth, LCropHeight, LNewWidth, LNewHeight);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;

      DrawAllFiguresOnLayer();
      UpdateLayerThumbnail();
    end;
  end;
end;

procedure TgmVectorLayer.CropLayerRect(const ACropArea: TRect; const ABackgroundColor: TColor32);
var
  i, j: integer;
  LCropRect: TRect;
  LOffsetVector: TPoint;
  LTopLeft: TPoint;
  LFigureObj: TgmFigureObject;
begin
  if (ACropArea.Left = ACropArea.Right) or (ACropArea.Top = ACropArea.Bottom) then
  begin
    Exit;
  end;

  LCropRect.Left := Min(ACropArea.Left, ACropArea.Right);
  LCropRect.Top := Min(ACropArea.Top, ACropArea.Bottom);
  LCropRect.Right := Max(ACropArea.Left, ACropArea.Right);
  LCropRect.Bottom := Max(ACropArea.Top, ACropArea.Bottom);

  inherited;

  if FFigureList.Count > 0 then
  begin
    LTopLeft := FFigureList.GetTopLeftFromAllFigures();
    LOffsetVector.X := LTopLeft.X - LCropRect.Left;
    LOffsetVector.Y := LTopLeft.Y - LCropRect.Top;

    for i := 0 to (FFigureList.Count - 1) do
    begin
      LFigureObj := TgmFigureObject(FFigureList.Items[i]);

      case LFigureObj.Flag of
        ffStraightLine,
        ffRectangle,
        ffSquare,
        ffRoundRectangle,
        ffRoundSquare,
        ffEllipse,
        ffCircle:
        begin
          // calculating the offset
          with LFigureObj do
          begin
            FStartPoint := Point(FStartPoint.X - LTopLeft.X + LOffsetVector.X, FStartPoint.Y -
              LTopLeft.Y + LOffsetVector.Y);

            FEndPoint := Point(FEndPoint.X - LTopLeft.X + LOffsetVector.X, FEndPoint.Y -
              LTopLeft.Y + LOffsetVector.Y);
          end;
        end;

        ffCurve:
        begin
          // calculating the offset
          with LFigureObj do
          begin
            FStartPoint := Point(FStartPoint.X - LTopLeft.X + LOffsetVector.X, FStartPoint.Y -
              LTopLeft.Y + LOffsetVector.Y);

            FEndPoint := Point(FEndPoint.X - LTopLeft.X + LOffsetVector.X, FEndPoint.Y -
              LTopLeft.Y + LOffsetVector.Y);

            FCurvePoint1 := Point(FCurvePoint1.X - LTopLeft.X + LOffsetVector.X, FCurvePoint1.Y -
              LTopLeft.Y + LOffsetVector.Y);

            FCurvePoint2 := Point(FCurvePoint2.X - LTopLeft.X + LOffsetVector.X, FCurvePoint2.Y -
              LTopLeft.Y + LOffsetVector.Y);
          end;
        end;

        ffPolygon,
        ffRegularPolygon:
        begin
          with LFigureObj do
          begin
            for j := Low(FPolygonPoints) to High(FPolygonPoints) do
            begin
              // calculating the offset
              FPolygonPoints[j] := Point(FPolygonPoints[j].X - LTopLeft.X + LOffsetVector.X,
                FPolygonPoints[j].Y - LTopLeft.Y + LOffsetVector.Y);
            end;
          end;
        end;
      end;
    end;
  end;

  DrawAllFiguresOnLayer();
  UpdateLayerThumbnail();
end;

procedure TgmVectorLayer.DrawAllFiguresOnLayer;
begin
  // Because of the canvas' methods will make the processed pixels
  // on a bitmap to be as fully transparent, that is alpha channel
  // is set to zero, so we fill out the layer bitmap with opaque
  // white, and then drawing figures on canvas, and then invert
  // the alpha channel of each pixel on the bitmap, then we got
  // the correct drawing result, that is, the background is transparent
  // and the figures are opaque.

  FLayerBitmap.Clear($FFFFFFFF);
  FFigureList.DrawAllFigures(FLayerBitmap.Canvas, pmCopy, fdmRGB);
  InvertAlphaChannel(FLayerBitmap);
end;

// only draw figures that were unselected or locked on the layer
procedure TgmVectorLayer.DrawUnselectedAndLockedLayersOnLayer;
var
  i: integer;
  LFigureObj: TgmFigureObject;
begin
  FLayerBitmap.Clear($FFFFFFFF);

  for i := 0 to (FFigureList.Count - 1) do
  begin
    LFigureObj := TgmFigureObject(FFigureList.Items[i]);

    if LFigureObj.IsLocked or (LFigureObj.IsSelected = False) then
    begin
      LFigureObj.DrawFigure(FLayerBitmap.Canvas, pmCopy, Point(0, 0), 100, fdmRGB);
    end;
  end;

  InvertAlphaChannel(FLayerBitmap);
end;

function TgmVectorLayer.GetCopy: TgmCustomLayer;
var
  i, LPenWidth: integer;
  LPenStyle: TPenStyle;
  LPenColor: TColor;
  LBrushColor: TColor;
  LBrushStyle: TBrushStyle;
  LFigureObj: TgmFigureObject;
  LTempFigure: TgmFigureObject;
  LLayer: TgmVectorLayer;
begin
  LLayer := TgmVectorLayer.Create(Self.FOwner, Self.FLayerBitmap.Width, Self.FLayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);

  if Self.FFigureList.Count > 0 then
  begin
    for i := 0 to (Self.FFigureList.Count - 1) do
    begin
      LFigureObj := nil;
      LTempFigure := TgmFigureObject(Self.FFigureList.Items[i]);
      LPenWidth := LTempFigure.PenWidth;
      LPenStyle := LTempFigure.PenStyle;
      LPenColor := LTempFigure.PenColor;
      LBrushStyle := LTempFigure.BrushStyle;
      LBrushColor := LTempFigure.BrushColor;

      case LTempFigure.Flag of
        ffStraightLine:
        begin
          LFigureObj := TgmLineObject.Create(LPenColor, LBrushColor, LPenStyle, LBrushStyle,
            LPenWidth, LTempFigure.FStartPoint, LTempFigure.FEndPoint);
        end;

        ffCurve:
        begin
          LFigureObj := TgmCurveObject.Create(LPenColor, LBrushColor, LPenStyle,
            LBrushStyle, LPenWidth, LTempFigure.FStartPoint, LTempFigure.FCurvePoint1, LTempFigure.FCurvePoint2,
            LTempFigure.FEndPoint);

          LFigureObj.CurveControl := LTempFigure.CurveControl;
        end;

        ffPolygon:
        begin
          LFigureObj := TgmPolygonObject.Create(LPenColor, LBrushColor, LPenStyle, LBrushStyle,
            LPenWidth, LTempFigure.FPolygonPoints);
        end;

        ffRegularPolygon:
        begin
          LFigureObj := TgmRegularPolygonObject.Create(LPenColor, LBrushColor, LPenStyle, LBrushStyle,
            LPenWidth, LTempFigure.Sides, LTempFigure.FStartPoint, LTempFigure.FEndPoint);
        end;

        ffRectangle,
        ffSquare:
        begin
          LFigureObj := TgmRectangleObject.Create(LPenColor, LBrushColor, LPenStyle, LBrushStyle,
            LPenWidth, LTempFigure.FStartPoint, LTempFigure.FEndPoint, LTempFigure.IsRegular);
        end;

        ffRoundRectangle,
        ffRoundSquare:
        begin
          LFigureObj := TgmRoundRectangleObject.Create(LPenColor, LBrushColor, LPenStyle, LBrushStyle,
            LPenWidth, LTempFigure.FStartPoint, LTempFigure.FEndPoint, LTempFigure.RoundCornerRadius, LTempFigure.IsRegular);
        end;

        ffEllipse,
        ffCircle:
        begin
          LFigureObj := TgmEllipseObject.Create(LPenColor, LBrushColor, LPenStyle, LBrushStyle,
            LPenWidth, LTempFigure.FStartPoint, LTempFigure.FEndPoint, LTempFigure.IsRegular);
        end;
      end;

      LFigureObj.IsSelected := LTempFigure.IsSelected;
      LFigureObj.IsLocked := LTempFigure.IsLocked;
      LFigureObj.Name := LTempFigure.Name;

      LLayer.FigureList.Add(LFigureObj);
    end;
  end;

  LLayer.FigureList.SelectedIndex := Self.FFigureList.SelectedIndex;
  LLayer.FigureList.LineNumber := Self.FFigureList.LineNumber;
  LLayer.FigureList.CurveNumber := Self.FFigureList.CurveNumber;
  LLayer.FigureList.PolygonNumber := Self.FFigureList.PolygonNumber;
  LLayer.FigureList.RegularPolygonNumber := Self.FFigureList.RegularPolygonNumber;
  LLayer.FigureList.RectangleNumber := Self.FFigureList.RectangleNumber;
  LLayer.FigureList.SquareNumber := Self.FFigureList.SquareNumber;
  LLayer.FigureList.RoundRectangleNumber := Self.FFigureList.RoundRectangleNumber;
  LLayer.FigureList.RoundSquareNumber := Self.FFigureList.RoundSquareNumber;
  LLayer.FigureList.EllipseNumber := Self.FFigureList.EllipseNumber;
  LLayer.FigureList.CircleNumber := Self.FFigureList.CircleNumber;

  Result := LLayer;
end;

procedure TgmVectorLayer.LayerBlend(F: TColor32; var B: TColor32; M: TColor32);
begin
  FLayerBlendEvent(F, B, M);
end;

procedure TgmVectorLayer.ResizeLayerCanvas(const ANewWidth, ANewHeight: integer; const AAnchor: TgmAnchorDirection;
  const ABackgroundColor: TColor32);
var
  i, j: integer;
  LOffsetVector: TPoint;
  LOldWidth: integer;
  LOldHeight: integer;
  LFigureObj: TgmFigureObject;
begin
  LOldWidth := FLayerBitmap.Width;
  LOldHeight := FLayerBitmap.Height;

  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (LOldWidth <> ANewWidth) or (LOldHeight <> ANewHeight) then
    begin
      inherited;

      if FFigureList.Count > 0 then
      begin
        LOffsetVector := CalcOffsetCoordinateByAnchorDirection(LOldWidth, LOldHeight, ANewWidth, ANewHeight, AAnchor);

        for i := 0 to (FFigureList.Count - 1) do
        begin
          LFigureObj := TgmFigureObject(FFigureList.Items[i]);

          case LFigureObj.Flag of
            ffStraightLine,
            ffRectangle,
            ffSquare,
            ffRoundRectangle,
            ffRoundSquare,
            ffEllipse,
            ffCircle:
            begin
              LFigureObj.FStartPoint :=
                Point(LFigureObj.FStartPoint.X + LOffsetVector.X, LFigureObj.FStartPoint.Y + LOffsetVector.Y);

              LFigureObj.FEndPoint :=
                Point(LFigureObj.FEndPoint.X + LOffsetVector.X, LFigureObj.FEndPoint.Y + LOffsetVector.Y);
            end;

            ffCurve:
            begin
              LFigureObj.FStartPoint :=
                Point(LFigureObj.FStartPoint.X + LOffsetVector.X, LFigureObj.FStartPoint.Y + LOffsetVector.Y);

              LFigureObj.FEndPoint :=
                Point(LFigureObj.FEndPoint.X + LOffsetVector.X, LFigureObj.FEndPoint.Y + LOffsetVector.Y);

              LFigureObj.FCurvePoint1 :=
                Point(LFigureObj.FCurvePoint1.X + LOffsetVector.X, LFigureObj.FCurvePoint1.Y + LOffsetVector.Y);

              LFigureObj.FCurvePoint2 :=
                Point(LFigureObj.FCurvePoint2.X + LOffsetVector.X, LFigureObj.FCurvePoint2.Y + LOffsetVector.Y);
            end;

            ffPolygon,
            ffRegularPolygon:
            begin
              for j := Low(LFigureObj.FPolygonPoints) to High(LFigureObj.FPolygonPoints) do
              begin
                LFigureObj.FPolygonPoints[j] :=
                  Point(LFigureObj.FPolygonPoints[j].X + LOffsetVector.X, LFigureObj.FPolygonPoints[j].Y +
                  LOffsetVector.Y);
              end;
            end;
          end;
        end;

        DrawAllFiguresOnLayer();
        UpdateLayerThumbnail();
      end;
    end;
  end;
end;

procedure TgmVectorLayer.ResizeLayerImage(const ANewWidth, ANewHeight: integer; const ASamplingOptions: TgmResamplingOptions);
var
  i, j: integer;
  LOldWidth: integer;
  LOldHeight: integer;
  LOriginalRect: TRect;
  LCurrentRect: TRect;
  LFigureObj: TgmFigureObject;
begin
  LOldWidth := FLayerBitmap.Width;
  LOldHeight := FLayerBitmap.Height;

  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (LOldWidth <> ANewWidth) or (LOldHeight <> ANewHeight) then
    begin
      inherited;

      if FFigureList.Count > 0 then
      begin
        LOriginalRect := Rect(0, 0, LOldWidth, LOldHeight);
        LCurrentRect := Rect(0, 0, ANewWidth, ANewHeight);

        for i := 0 to (FFigureList.Count - 1) do
        begin
          LFigureObj := TgmFigureObject(FFigureList.Items[i]);

          case LFigureObj.Flag of
            ffStraightLine,
            ffRectangle,
            ffSquare,
            ffRoundRectangle,
            ffRoundSquare,
            ffEllipse,
            ffCircle:
            begin
              LFigureObj.FStartPoint := ScalePoint(LFigureObj.FStartPoint, LOldWidth,
                LOldHeight, ANewWidth, ANewHeight);

              LFigureObj.FEndPoint := ScalePoint(LFigureObj.FEndPoint, LOldWidth,
                LOldHeight, ANewWidth, ANewHeight);
            end;

            ffCurve:
            begin
              LFigureObj.FStartPoint := ScalePoint(LFigureObj.FStartPoint, LOldWidth,
                LOldHeight, ANewWidth, ANewHeight);

              LFigureObj.FEndPoint := ScalePoint(LFigureObj.FEndPoint, LOldWidth,
                LOldHeight, ANewWidth, ANewHeight);

              LFigureObj.FCurvePoint1 := ScalePoint(LFigureObj.FCurvePoint1, LOldWidth,
                LOldHeight, ANewWidth, ANewHeight);

              LFigureObj.FCurvePoint2 := ScalePoint(LFigureObj.FCurvePoint2, LOldWidth,
                LOldHeight, ANewWidth, ANewHeight);
            end;

            ffPolygon,
            ffRegularPolygon:
            begin
              for j := Low(LFigureObj.FPolygonPoints) to High(LFigureObj.FPolygonPoints) do
              begin
                LFigureObj.FPolygonPoints[j] := ScalePoint(LFigureObj.FPolygonPoints[j],
                  LOldWidth, LOldHeight,
                  ANewWidth, ANewHeight);
              end;
            end;
          end;
        end;

        DrawAllFiguresOnLayer();
        UpdateLayerThumbnail();
      end;
    end;
  end;
end;

procedure TgmVectorLayer.RotateCanvas(const ADegrees: integer; const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32);
begin
  inherited;
  DrawAllFiguresOnLayer();
  UpdateLayerThumbnail();
end;


end.
