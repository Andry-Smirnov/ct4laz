{**********************************************************************
                PilotLogic Software House

 Package pl_Cindy
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

  {   Component(s):
    tcyImage

    Description:
    Herited from TImage, allow zoom and paint into real GraphiControl canvas ...
                 €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€
    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyCustomImage;

{$I cyCompilerDefines.inc}

interface

uses
    LCLIntf, LCLType, LMessages,
    cyClasses, cyTypes, cyGraphics, Graphics, ExtCtrls, classes, Messages, {$IFDEF UNICODE} UXTheme, {$ENDIF} Controls, Math;

type
  TcyGraphicControl = class(TGraphicControl)
  end;

  TcyImageMouseOption = (moMove);
  TcyImageMouseOptions = set of TcyImageMouseOption;

  TcyCustomImage = class(TImage)
  private
    FOnPaint: TNotifyEvent;
    FZoom: Double;
    FBeforePaint: TNotifyEvent;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FMouseIsDown: Boolean;
    FMouseDownOffsetX: Integer;
    FMouseDownOffsetY: Integer;
    FMouseDownFromPixel: TPoint;
    FMouseDownFromPos: TPoint;
    FMouseOptions: TcyImageMouseOptions;
    FPosition: TBgPosition;
    procedure SetZoom(const Value: Double);
    function GetCanvasControl: TCanvas;
    procedure SetOffsetX(const Value: Integer);
    procedure SetOffsetY(const Value: Integer);
    procedure SetMouseOptions(const Value: TcyImageMouseOptions);
    procedure SetPosition(const Value: TBgPosition);
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; // Call OnMouseDown procedure ...
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; // Call OnMouseUp procedure ...
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    property CanvasControl: TCanvas read GetCanvasControl;
    property Position: TBgPosition read FPosition write SetPosition default bgTopLeft;
    property MouseOptions: TcyImageMouseOptions read FMouseOptions write SetMouseOptions default [moMove];
    property OffsetX: Integer read FOffsetX write SetOffsetX default 0;
    property OffsetY: Integer read FOffsetY write SetOffsetY default 0;
    property Zoom: Double read FZoom write SetZoom;
    property BeforePaint: TNotifyEvent read FBeforePaint write FBeforePaint;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyZoomFactor;
    function MousePosToPixelPos(const aPosition: TPoint; aOffsetX, aOffsetY: Integer): TPoint;
    function PixelPosToMousePos(const aPosition: TPoint; aOffsetX, aOffsetY: Integer): TPoint;
    procedure DragImage(const FromMousePos, ToMousePos: TPoint; const fromOffsetX, fromOffsetY: Integer);
    procedure DragImageFromMouseDown(const CurrentMousePos: TPoint);
    procedure ZoomFromMousePos(const aPosition: TPoint; ZoomValue: Extended);
    // Informative properties :
    property MouseIsDown: Boolean read FMouseIsDown default false;
    property MouseDownOffsetX: Integer read FMouseDownOffsetX;
    property MouseDownOffsetY: Integer read FMouseDownOffsetY;
    property MouseDownFromPos: TPoint read FMouseDownFromPos;
    property MouseDownFromPixel: TPoint read FMouseDownFromPixel;
  published
    // property Center: Boolean read FCenter;     // NOT WORKING... Hide inherited property (Replaced by new property Position)
  end;

  TcyImage = class(TcyCustomImage)
  private
  protected
  public
    // Herited from TcyCustomImage :
    property CanvasControl;
  published
    property Color;
    // Herited from TcyCustomImage :
    property Position;
    property MouseOptions;
    property OffsetX;
    property OffsetY;
    property Zoom;
    property BeforePaint;
    property OnPaint;
  end;

implementation

constructor TcyCustomImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPosition := bgTopLeft;
  FZoom := 1; // 100%
  FOffsetX := 0;
  FOffsetY := 0;
  FMouseOptions := [moMove];

  FMouseIsDown := false;
  FMouseDownOffsetX := 0;
  FMouseDownOffsetY := 0;
  FMouseDownFromPos.X := 0;
  FMouseDownFromPos.Y := 0;
  FMouseDownFromPixel.X := 0;
  FMouseDownFromPixel.Y := 0;
end;

destructor TcyCustomImage.Destroy;
begin
  inherited Destroy;
end;

// TImage.Canvas is in fact, TImage.Picture.Bitmap.Canvas
// What i want is paint into real TGraphic.Canvas!
function TcyCustomImage.GetCanvasControl: TCanvas;
begin
  Result := TcyGraphicControl(Self).Canvas;
end;

function TcyCustomImage.MousePosToPixelPos(const aPosition: TPoint; aOffsetX, aOffsetY: Integer): TPoint;
begin
  Result.X := Round(aPosition.X / Fzoom) - aOffsetX;
  Result.Y := Round(aPosition.Y / Fzoom) - aOffsetY;
end;

function TcyCustomImage.PixelPosToMousePos(const aPosition: TPoint; aOffsetX, aOffsetY: Integer): TPoint;
begin
  Result.X := Round((aPosition.X  + aOffsetX) * Fzoom);
  Result.Y := Round((aPosition.Y  + aOffsetY) * Fzoom);
end;

procedure TcyCustomImage.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseIsDown := true;

  FMouseDownOffsetX := FOffsetX;
  FMouseDownOffsetY := FOffsetY;
  FMouseDownFromPos := Point(X, Y);
  FMouseDownFromPixel := MousePosToPixelPos(FMouseDownFromPos, FOffsetX, FOffsetY);

  inherited;
end;

procedure TcyCustomImage.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FMouseIsDown then
    if moMove in FMouseOptions then
      DragImageFromMouseDown(Point(X, Y));

  inherited;
end;

procedure TcyCustomImage.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseIsDown := false;
  inherited;
end;

//=== ct9999 ====
procedure TcyCustomImage.ApplyZoomFactor;
var
  aCanvas: TCanvas;
  _Zoom: integer;
    {
    procedure SetCanvasZoomAndRotation(ACanvas: TCanvas; Zoom: Double; Angle: Double; CenterpointX, CenterpointY: Double);
    var
      form: tagXFORM;
      rAngle: Double;
    begin
      rAngle := DegToRad(Angle);
      SetGraphicsMode(ACanvas.Handle, GM_ADVANCED);
      SetMapMode(ACanvas.Handle, MM_ANISOTROPIC);
      form.eM11 := Zoom * Cos(rAngle);
      form.eM12 := Zoom * Sin(rAngle);
      form.eM21 := Zoom * (-Sin(rAngle));
      form.eM22 := Zoom * Cos(rAngle);
      form.eDx := CenterpointX;
      form.eDy := CenterpointY;
      SetWorldTransform(ACanvas.Handle, form);
    end;
    }
  procedure SetCanvasZoomFactor(aCanvas: TCanvas; AZoomFactor: Integer);
    var
      i: Integer;
    begin
      if AZoomFactor = 100 then
        SetMapMode(aCanvas.Handle, MM_TEXT)
      else
      begin
        SetMapMode(aCanvas.Handle, MM_ISOTROPIC);
        SetWindowExtEx(aCanvas.Handle, AZoomFactor, AZoomFactor, nil);
        SetViewportExtEx(aCanvas.Handle, 100, 100, nil);
      end;
    end;

begin
  aCanvas := TcyGraphicControl(Self).Canvas;

  if (FZoom = 1) or (Stretch) then
  begin
    // No need?!
    // SetCanvasZoomAndRotation(aCanvas, 1, 0, 0, 0); // SetMapMode(aCanvas.Handle, MM_TEXT)
  end
  else begin
    // _Zoom := Round(1/FZoom * 100);
    _Zoom := Round(FZoom);
    //SetCanvasZoomAndRotation(aCanvas, _Zoom, 0, 0, 0);
    SetCanvasZoomFactor(aCanvas, _Zoom);
  end;
end;

//=====================================

function TcyCustomImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  if csLoading in ComponentState then
  begin
    Result := true;
    NewWidth := Width;
    NewHeight := Height;
    Exit;
  end;

  Result := True;
  if not (csDesigning in ComponentState) or (Picture.Width > 0) and (Picture.Height > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := Round(Picture.Width * FZoom);
    if Align in [alNone, alTop, alBottom] then
      NewHeight := Round(Picture.Height * FZoom);
  end;
end;

procedure TcyCustomImage.Paint;
{$IFDEF UNICODE}
var
  aStyle: TBgStyle;
  // aPosition: TBgPosition;
  Rect1: TRect;    // Rect1 can be modified by DrawGraphic()

  procedure DoBufferedPaint(Canvas: TCanvas);
  var
    MemDC: HDC;
    Rect2: TRect;    // Rect1 can be modified by DrawGraphic()
    PaintBuffer: HPAINTBUFFER;
  begin
    Rect2 := Rect1;
    PaintBuffer := BeginBufferedPaint(Canvas.Handle, Rect1, BPBF_TOPDOWNDIB, nil, MemDC);
    try
      Canvas.Handle := MemDC;
      DrawGraphic(TcyGraphicControl(Self).Canvas, Rect1, Picture.Graphic, Transparent, aStyle, FPosition);
      BufferedPaintMakeOpaque(PaintBuffer, Rect2);
    finally
      EndBufferedPaint(PaintBuffer, True);
    end;
  end;

begin
  ApplyZoomFactor;

  if Assigned(FBeforePaint) then FBeforePaint(Self);

  aStyle := bgNormal;

  (* 2017-11-06
  Rect1 := DestRect;

  if Center
  then aPosition := bgCentered
  else aPosition := bgTopLeft; *)

  Rect1 := ClientRect;

  if Stretch then
    if Proportional
    then aStyle := bgStretchProportional
    else aStyle := bgStretch;

  if not Transparent then
    with TcyGraphicControl(Self).Canvas do
    begin
      Pen.Style := psSolid;
      Brush.Style := bsSolid;

      Brush.Color := Self.Color;
      FillRect(Rect(0, 0, Width, Height));
    end;

  if csDesigning in ComponentState then
    with TcyGraphicControl(Self).Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

  if (FOffsetX <> 0) or (FOffsetY <> 0) then
    if not Stretch then
      OffsetRect(Rect1, OffsetX, OffsetY);

  if (csGlassPaint in ControlState) and (Picture.Graphic <> nil) and not Picture.Graphic.SupportsPartialTransparency then
    DoBufferedPaint(TcyGraphicControl(Self).Canvas)
  else begin
    DrawGraphic(TcyGraphicControl(Self).Canvas, Rect1, Picture.Graphic, Transparent, aStyle, FPosition);
  end;

  if Assigned(FOnPaint) then FOnPaint(Self);
end;
{$ELSE}
begin
  ApplyZoomFactor;

  if Assigned(FBeforePaint) then FBeforePaint(Self);
  Inherited;
  if Assigned(FOnPaint) then FOnPaint(Self);
end;
{$ENDIF}

procedure TcyCustomImage.SetMouseOptions(const Value: TcyImageMouseOptions);
begin
  FMouseOptions := Value;
end;

procedure TcyCustomImage.SetOffsetX(const Value: Integer);
begin
  if FOffsetX = Value then Exit;

  FOffsetX := Value;
  Invalidate;
end;

procedure TcyCustomImage.SetOffsetY(const Value: Integer);
begin
  if FOffsetY = Value then Exit;

  FOffsetY := Value;
  Invalidate;
end;

procedure TcyCustomImage.SetPosition(const Value: TBgPosition);
begin
  FPosition := Value;
  Invalidate;
end;

procedure TcyCustomImage.SetZoom(const Value: Double);
var
  NewWidth, NewHeight: Integer;
  RedrawRect: TRect;
begin
  if Value <= 0 then Exit;
  if FZoom = Value then Exit;

  FZoom := Value;

  if Autosize then
  begin
    Invalidate;

    if (Picture.Width > 0) and (Picture.Height > 0) then
    begin
      if Align in [alNone, alLeft, alRight]
      then NewWidth := Round(Picture.Width * FZoom)
      else NewWidth := Width;

      if Align in [alNone, alTop, alBottom]
      then NewHeight := Round(Picture.Height * FZoom)
      else NewHeight := Height;

      if (NewWidth <> Width) or (NewHeight <> Height) then
        SetBounds(Left, Top, NewWidth, NewHeight);
    end;
  end
  else begin
    // Erase background :
    if Assigned(Self.Parent) then
    begin
      RedrawRect := classes.Rect(Left, Top, Left + Width, Top + Height);
      InvalidateRect(Self.Parent.Handle, @RedrawRect, true);
    end;
  end;
end;

procedure TcyCustomImage.DragImage(const FromMousePos, ToMousePos: TPoint; const fromOffsetX, fromOffsetY: Integer);
var
  FromPixel, CurrentPixel: TPoint;
begin
  FromPixel := MousePosToPixelPos(FromMousePos, fromOffsetX, fromOffsetY);
  CurrentPixel := MousePosToPixelPos(ToMousePos, fromOffsetX, fromOffsetY);

  FOffsetX := CurrentPixel.X - FromPixel.X + fromOffsetX;
  FOffsetY := CurrentPixel.Y - FromPixel.Y + fromOffsetY;
  Invalidate;
end;

procedure TcyCustomImage.DragImageFromMouseDown(const CurrentMousePos: TPoint);
var
  CurrentPixel: TPoint;
begin
  CurrentPixel := MousePosToPixelPos(CurrentMousePos, MouseDownOffsetX, MouseDownOffsetY);

  FOffsetX := CurrentPixel.X - MouseDownFromPixel.X + MouseDownOffsetX;
  FOffsetY := CurrentPixel.Y - MouseDownFromPixel.Y + MouseDownOffsetY;
  Invalidate;
end;

procedure TcyCustomImage.ZoomFromMousePos(const aPosition: TPoint; ZoomValue: Extended);
var
  IncOffsetX, IncOffsetY: Extended;
begin
  if ZoomValue = 0 then Exit;
  if Zoom = ZoomValue then Exit;

  if not Assigned(Picture.Graphic) then Exit;

  // Works but not consider aPosition as the center :
  // ClientWidth is not the center of Graphic !
  IncOffsetX := ( (aPosition.X - (ClientWidth / 2) * ZoomValue) - (aPosition.X - (ClientWidth / 2) * Zoom)) / 2 / ZoomValue;
  IncOffsetY := ((aPosition.Y - (ClientHeight / 2) * ZoomValue) - (aPosition.Y - (ClientHeight / 2) * Zoom)) / 2 / ZoomValue;


  Zoom := ZoomValue;


  // Apply offset :
  FOffsetX := FOffsetX + Round(IncOffsetX);
  FOffsetY := FOffsetY + Round(IncOffsetY);

  Invalidate;
end;


end.
