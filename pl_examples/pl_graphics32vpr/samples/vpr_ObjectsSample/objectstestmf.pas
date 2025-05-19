unit Objectstestmf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  GR32, GR32_Blend,  GR32_Filters,
  GR32_LowLevel,  GR32_MicroTiles, GR32_Rasterizers,
  GR32_Resamplers, GR32_System, GR32_Image, GR32_Layers,
  VGR32_Lines, VGR32_Misc, VGR32_Objects, ELDsgxObjectInsp;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image: TImage32;
    OI: TplObjectInspector;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  private
    savedAngle: integer;
    scrollbar_width: integer;

    lastDrawObj: TDrawObjLayerBase;
    SelectionRec: TRect;
    SelectionShape: TShape;
    procedure CountObjects(out count, indexLast: integer);
    procedure CountSelected(out count, indexLast: integer);
    procedure DesignerMoving(Sender: TObject; const OldLocation: TFloatRect;
                             var NewLocation: TFloatRect; DragState: integer; Shift: TShiftState);
    procedure DesignerBtnMoving(Sender: TObject);
    procedure ValidateDragState(Sender: TObject; var dragState: integer);
    procedure DrawFocusRec(Rec: TRect);
    procedure AngleChange(value: integer);
  public
    popupPos: TPoint;
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);

var
  rec: TRect;
  pts: TArrayOfFixedPoint;
  rs: TResourceStream;
begin
  LoadObjectsCursors;


 //in case I want to draw transparent images directly onto Image.Bitmap ...
  Image.Bitmap.SetSize(Image.Width,Image.Height);
  Image.Bitmap.DrawMode := dmBlend;
  Image.Bitmap.Clear($FF04F4F4);
 // mnuZoomPWClick(mnuZoom100);

  //draw a pale gray outline of the image bitmap surface ...
  rec := Image.Bitmap.BoundsRect;
  dec(rec.Right);
  dec(rec.Bottom);
  pts := MakeArrayOfFixedPoints(rec);

 { PolylineXS(Image.Bitmap, pts, clLightGray32, true);
  //now draw a faint printing margin on the page ...
//  InflateRect(rec, -printer_margin.cx,-printer_margin.cy);
  pts := MakeArrayOfFixedPoints(rec);
  PolylineXS(Image.Bitmap, pts, $FFF4F4F4, true);

                                            }
 //======================================================
  with TDrawObjGraphic.Create(Image.Layers) do
  begin
    //AllowResizing := false;      //defaults to true
    //ProportionalSizing := false; //defaults to true


   // rs := TResourceStream.Create(hInstance, 'BMP1', RT_RCDATA);
   // try Position(FloatRect(20,165,120,310), 0, rs); finally rs.Free; end;
    Transparent := true;

    //nb: painting will only be done following an explicit call to Repaint (or
    //following an EndUpdate). This avoids wasting CPU cycles with unnecessary
    //repainting that might otherwise occur when changing object properties.
    Repaint;
  end;

  with TDrawObjPolygon.Create(Image.Layers) do
  begin
    Regular := true; //ie forces a symmetrical shape
    StrokeColor := $FF009900;
    FillColors := MakeArrayOfColor32([$FFFFFFEE,$CCFFFF88,$FF66FF66]);
    FillStyle := fsRadial;
    StrokeWidth := 4;
    ShadowOffset := 3;
    Position(FloatRect(280,190,380,290),8);
    Repaint;
  end;


  with TDrawObjPoint.Create(Image.Layers) do
  begin
    Radius := 4;
    StrokeColor := clRed32;
    FillColor := clRed32;
    Position(FloatPoint(31,43));
    ShadowOffset := 2;
    Repaint;
  end;

  with TDrawObjPoint.Create(Image.Layers) do
  begin
    Radius := 7;
    Position(FloatPoint(50,70));
    ShadowOffset := 2;
    Repaint;
  end;

  with TDrawObjRectangle.Create(Image.Layers) do
  begin
    StrokeColor := $0;

    //Notes on sub-pixel anti-aliasing ...
    //(see http://en.wikipedia.org/wiki/Subpixel_rendering)
    //When text is drawn onto a full transparent layer, the background color
    //for that text is undefined and makes sub-pixel anti-aliasing impossible.
    //This isn't a problem when printing but text can look a bit fuzzy without
    //sub-pixel anti-aliasing on LCD monitors.) We can hardcode a background
    //color by setting the FillColor almost but not entirely transparent.
    //With a fill color of $02FFFFFF, the background will appear transparent
    //but text will be anti-aliased as if the background was white. This
    //isn't usually a good idea unless you can be confident the end-user
    //won't be dragging the text over colored objects.
    FillColor := $02FFFFFF;

    Text := 'A rectangular object with no stroke or fill';
    Position(FloatRect(500,20,590,120), 0);
    Repaint;
  end;

  with TDrawObjEllipse.Create(Image.Layers) do
  begin
    StrokeColor := $CCAA4400;
    Text := 'an ellipse with'#10'radial filling,'#10'balloon tip and'#10'drop shadow';
    Font.Color := clNavy;
    FillColors := MakeArrayOfColor32([$CCFFFFFF,$CCDDDD44]);
    FillStyle := fsRadial;
    StrokeWidth := 3;
    BalloonPos := bpBottomLeft;
    ShadowOffset := 3;
    Position(FloatRect(100,110,240,200),0);
    Repaint;
  end;

  with TDrawObjDiamond.Create(Image.Layers) do
  begin
    Text := 'Gradient'#10'filling';
    Font.Name := 'Verdana';
    Font.Color := clTeal;
    Font.Size := 11;
    StrokeColor := $FF009900;
    FillColors := MakeArrayOfColor32([$FFFFFF88,$CCCCFF88,$CC66FF66]);
    FillStyle := fsGradiant;
    StrokeWidth := 1;
    ShadowOffset := 0;
    Position(FloatRect(220,30,380,130),0);
    Repaint;
  end;


  with TDrawObjArc.Create(Image.Layers) do
  begin
    StrokeColor := $CC000099;
    FillColors := MakeArrayOfColor32([$FFFFCCCC,$CCFFFF99,$80AAAAEE]);
    FillStyle := fsRadial;
    StrokeWidth := 3;
    ShadowOffset := 3;
    AngleStart := 45;
    AngleEnd := 0;
    Position(FloatRect(470,240,570,380),0);
    Repaint;
  end;

  with TDrawObjRectangle.Create(Image.Layers) do
  begin
    StrokeColor := $FF0000CC;
    StrokeStyle := psDash;
    Text := 'A semi- transparent rectangle with rounded corners';
    Rounded := true;
    //Regular := true; //ie force to a square whenever the designer buttons moved
    FillColor := $CCDDDDFF;
    StrokeWidth := 3;
    TextPadding := 4;
    Position(FloatRect(410,70,490,220), 30);
    Repaint;
  end;

  with TDrawObjLine.Create(Image.Layers) do
  begin
    ArrowStart.Style := asCircle;
    ArrowStart.Color := $30990000;
    ArrowEnd.Style := asThreePoint;
    ArrowEnd.Color := $30990000;
   // Text := 'try this';
    StrokeColor := $FF990000;
    StrokeStyle := psDot;

    //see my comments on sub-pixel anti-aliasing above and try uncommenting the
    //FillColor statement below. (Also test this by moving the line over a
    //colored object.) With and without FillColor both have merits: if white,
    //the text usually looks a little clearer over white backgrounds but not so
    //good over colored objects; if the FillColor is fully transparent, the text
    //usually doesn't look quite a crisp on LCD monitors (though will still be
    //fine when printed).
    //FillColor := $FFFFFFFF;

    Font.Color := clMaroon;
    StrokeWidth := 3;
    Position(MakeArrayOfFloatPoints([260,150, 390,180]));
    Repaint;
  end;

  with TDrawObjStar.Create(Image.Layers) do
  begin
    StrokeColor := $FFAA0000;
    StrokeWidth := 3;
    Regular := true;
    FillColors := MakeArrayOfColor32([$80FFFF00, $FFFF6600]);
    FillStyle := fsRadial;
    Position(FloatPoint(200,270),10,60,7);
    ShadowOffset := 3;
    Repaint;
  end;

  with TDrawObjArrow.Create(Image.Layers) do
  begin
    StrokeColor := $FF006600;
    StrokeWidth := 3;
    FillColor := $3000FF00;
    Position(FloatRect(90,315,170,415), 60);
    ShadowOffset := 3;
    Repaint;
  end;

  with TDrawObjBezier.Create(Image.Layers) do
  begin
    ArrowEnd.Style := asThreePoint;
    ArrowEnd.Color := $30000066;
   // Text := 'This is really cool';
    StrokeWidth := 3;
    StrokeColor := $CC000066;

    //see my comments on sub-pixel anti-aliasing above and try uncommenting the
    //FillColor statement below. (Also test this by moving the line over a
    //colored object.) With and without FillColor both have merits: if white,
    //the text usually looks a little clearer over white backgrounds but not so
    //good over colored objects; if the FillColor is fully transparent, the text
    //usually doesn't look quite a crisp on LCD monitors (though will still be
    //fine when printed).
    //FillColor := $FFFFFFFF;

    Position(MakeArrayOfFloatPoints([300,340, 370,300, 370,380, 450,350]));
    Repaint;
  end;

  with TDrawObjWideBezier.Create(Image.Layers) do
  begin
    StrokeWidth := 15;
    FillWidth := 9;
    StrokeColor := $CCAA3300;
    FillColors := MakeArrayOfColor32([$AAFFFF00, $99FF6600]);
    FillStyle := fsGradiant;
    ShadowOffset := 3;
    Position(MakeArrayOfFloatPoints([200,380, 270,300, 270,400, 320,390]));
    //Smooth := false;
    Repaint;
  end;
 //==============================================================

  Timer1.Interval := 1000;
  Timer1.Enabled := true;
end;


procedure TForm1.CountObjects(out count, indexLast: integer);
var
  i: integer;
begin
  count := 0;
  for i := 0 to Image.Layers.Count -1 do
    if (image.Layers[i] is TDrawObjLayerBase) then
    begin
      inc(count);
      indexLast := i;
    end;
end;
//------------------------------------------------------------------------------

procedure TForm1.CountSelected(out count, indexLast: integer);
var
  i: integer;
begin
  count := 0;
  for i := 0 to Image.Layers.Count -1 do
    if (image.Layers[i] is TDesignerLayer) then
    begin
      inc(count);
      indexLast := TDesignerLayer(image.Layers[i]).ChildLayer.Index;
    end;
end;

procedure TForm1.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
 procedure ClearDesigners;
  var
    i: integer;
  begin
    for i := Image.Layers.Count -1 downto 0 do
      if image.Layers[i] is TDesignerLayer then
        image.Layers[i].Free;
  end;

var
  cnt, last: integer;
begin
  if Layer is TDesignerLayer then
  begin
    if (ssCtrl in Shift) then Layer.Free;

    CountSelected(cnt, last);
    if cnt = 1 then
    begin
      OI.Selected:=(image.Layers[last]);
    end else
    begin
     // OI.Selected:=(nil);
    end;
    exit;
  end
  else if Layer is TDrawObjLayerBase then
  begin
    if ([ssShift, ssCtrl] * Shift = []) then ClearDesigners;
    with TDesignerLayer.Create(image.Layers) do
    begin
      OnMoving := @DesignerMoving;
      OnButtonMoving := @DesignerBtnMoving;
      OnValidateDragState :=@ValidateDragState;
      ChildLayer := TDrawObjLayerBase(Layer);
    end;
    CountSelected(cnt, last);
    if cnt = 1 then
    begin
      OI.Selected:=(image.Layers[last]);
    end else
    begin
     // OI.Selected:=(nil);
    end;
  end else
  begin
    ClearDesigners;
    //start selection rubber-banding ...
    SelectionRec := Rect(X,Y,X,Y);
    if (ssLeft in Shift) then DrawFocusRec(SelectionRec);

  //  OI.Selected:=(nil);

  end;
end;

procedure TForm1.DesignerMoving(Sender: TObject; const OldLocation: TFloatRect;
  var NewLocation: TFloatRect; DragState: integer; Shift: TShiftState);
var
  i: integer;
  loc: TFloatRect;
begin
  //move all the selected objects to their new locations ...
  for i := 0 to Image.Layers.Count -1 do
    if (image.Layers[i] is TDesignerLayer) and (Sender <> image.Layers[i]) then
      with TDesignerLayer(image.Layers[i]) do
      begin
        loc := Location;
        OffsetFloatRect(loc,
          round(newLocation.Left-OldLocation.Left),
          round(newLocation.top-OldLocation.Top));
        Location := loc;
      end;
  OI.Repaint;
end;
//------------------------------------------------------------------------------

procedure TForm1.DesignerBtnMoving(Sender: TObject);
begin
  OI.Repaint;
end;
//------------------------------------------------------------------------------

procedure TForm1.ValidateDragState(Sender: TObject; var dragState: integer);
var
  cnt,last: integer;
begin
  //if the mouse is over an object control button, make sure only one object is
  //selected before allowing the designer to proceed with button moves ...
  CountSelected(cnt, last);
  //nb: dragState >= 0 means the mouse is over a control button ...
  if (dragState >= 0) and (cnt <> 1) then dragState := DRAG_MOVE;
end;
//------------------------------------------------------------------------------

procedure TForm1.DrawFocusRec(Rec: TRect);
begin
  if not Assigned(SelectionShape) then
  begin
    //simple selection 'rubber-banding' ...
    SelectionShape := TShape.create(self);
    SelectionShape.Parent := Image;
    SelectionShape.Brush.Style := bsClear;
    SelectionShape.Pen.Style := psDot;
  end;
  with Rec do
    SelectionShape.SetBounds(left,top,right-left, bottom-top);
end;


procedure TForm1.AngleChange(value: integer);
begin
  if not assigned(lastDrawObj) then exit;

  if lastDrawObj is TDrawObjTextBase then
    TDrawObjTextBase(lastDrawObj).Angle := value
  else if lastDrawObj is TDrawObjGraphic then
    TDrawObjGraphic(lastDrawObj).Angle := value
  else
  begin
    lastDrawObj.Rotate(value-savedAngle);
    savedAngle := value;
  end;
  lastDrawObj.RePaint;
  OI.Repaint;
end;

end.

