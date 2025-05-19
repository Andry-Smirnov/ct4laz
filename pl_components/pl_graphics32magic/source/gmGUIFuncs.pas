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


unit gmGUIFuncs;

{$MODE DELPHI}

interface

uses

  LCLIntf, LCLType, LMessages,types, SysUtils, StdCtrls, ExtCtrls, Forms, Dialogs, Controls,
  Classes, Graphics, ComCtrls,

  GR32, GR32_Image,

  gmTypes;

{ procedure }

  procedure CenterImageInPanel(const APanel: TPanel; const AImage: TImage32); overload;
  procedure CenterImageInPanel(const APanel: TPanel; const AImage: TImage); overload;

  procedure ScaleImage32(const ASourceBmp: TBitmap32; AImage: TImage32;
                         const AWidth, AHeight: Integer);

  procedure DrawRichTextOnBitmap(const ABitmap: TBitmap32; const ARect: TRect; ARichEdit: TMemo);

{ Functions }

  function GetScreenPixelsPerInch: Integer;

  function CheckIfEditEmpty(AEdit: TEdit): Boolean;
  
  // change cursor according to a given degrees
  function GetCursorByDegree(const ADeg: Extended): TCursor;

  // change cursor according to various handles
  function SetCursorByHandle(const ADrawingHandle: TgmDrawingHandle): TCursor;

  function GetBitmapTopLeftInImage32(const AImageControl: TCustomImage32): TPoint;

  function CheckInputNumberValid(ASender: TObject; var AKey: Word;
    AShift: TShiftState): Boolean;

  function CheckInputFloatNumberValid(ASender: TObject; var AKey: Word;
    AShift: TShiftState): Boolean;

  function CheckIfInRange(const ACheckValue, AMinValue, AMaxValue: Integer): Boolean;

  function PrintRTFToCanvas(ACanvas: TCanvas; ARichEdit: TMemo; const ARect: TRect): Longint;

  function PrintRTFToBitmap(ARichEdit: TMemo; ABitmap: TBitmap): Longint;

const
  SMALL_THUMBNAIL_SIZE: Integer = 32;
  LARGE_THUMBNAIL_SIZE: Integer = 64;

  { custom cursor indices }
  crPathComponentSelection = 1;
  crDirectSelection        = 2;
  crPenToolDeselected      = 3;
  crPenToolSelected        = 4;
  crAddAnchorPoint         = 5;
  crDeleteAnchorPoint      = 6;
  crMovePath               = 7;
  crConvertPoint           = 8;
  crClosePath              = 9;
  crHandLoosen             = 10;
  crHandGrip               = 11;
  crPaintBucket            = 12;
  crEyedropper             = 13;
  crPenToolLastEnd         = 14;
  crAddCornerPoint         = 15;
  crCloneStamp             = 16;
  crCrossAdd               = 17;
  crCrossSub               = 18;
  crCrossIntersect         = 19;
  crCrossInterSub          = 20;
  crPolygonSelection       = 21;
  crPolygonAdd             = 23;
  crPolygonSub             = 24;
  crPolygonIntersect       = 26;
  crPolygonInterSub        = 27;
  crLassoSelection         = 28;
  crLassoAdd               = 29;
  crLassoSub               = 30;
  crLassoIntersect         = 31;
  crLassoInterSub          = 32;
  crMagicWand              = 33;
  crMagicWandAdd           = 34;
  crMagicWandSub           = 35;
  crMagicWandIntersect     = 36;
  crMagicWandInterSub      = 37;
  crMoveSelection          = 38;
  crCrop                   = 39;
  crMeasure                = 40;
  crMeasureMove            = 41;
  crMeasureAngle           = 42;
  crMagicEraser            = 43;

  // for transformation -- rotation
  crRotatePointer1         = 44;
  crRotatePointer2         = 45;
  crRotatePointer3         = 46;
  crRotatePointer4         = 47;
  crRotatePointer5         = 48;
  crRotatePointer6         = 49;
  crRotatePointer7         = 50;
  crRotatePointer8         = 51;

  crBlackPicker            = 52;
  crGrayPicker             = 53;
  crWhitePicker            = 54;
  crMagneticLasso          = 55;

implementation

uses
  gmImageProcessFuncs;   // GetScaledDimension()

{$R gmguifuncs.res}

var
  GMaskRichEdit: TMemo;

procedure CenterImageInPanel(const APanel: TPanel; const AImage: TImage32);
begin
  if (APanel.Width  >= AImage.Width) and
     (APanel.Height >= AImage.Height) then
  begin
    AImage.Left := APanel.Width  div 2 - AImage.Width  div 2;
    AImage.Top  := APanel.Height div 2 - AImage.Height div 2;
  end;
end;

procedure CenterImageInPanel(const APanel: TPanel; const AImage: TImage);
begin
  if (APanel.Width  >= AImage.Width) and
     (APanel.Height >= AImage.Height) then
  begin
    AImage.Left := APanel.Width  div 2 - AImage.Width  div 2;
    AImage.Top  := APanel.Height div 2 - AImage.Height div 2;
  end;
end;

procedure ScaleImage32(const ASourceBmp: TBitmap32; AImage: TImage32;
  const AWidth, AHeight: Integer);
var
  sw, sh: Integer;
begin
  if AImage.AutoSize then
  begin
    AImage.AutoSize := False;
  end;

  if AImage.ScaleMode <> smStretch then
  begin
    AImage.ScaleMode := smStretch;
  end;

  GetScaledDimension(ASourceBmp.Width, ASourceBmp.Height, AWidth, AHeight, sw, sh);

  AImage.Width  := sw;
  AImage.Height := sh;

  AImage.Bitmap.Assign(ASourceBmp);
end;

procedure DrawRichTextOnBitmap(const ABitmap: TBitmap32; const ARect: TRect;
  ARichEdit: TMemo);
var
  MaskBitmap      : TBitmap32;
  RichTextStream  : TMemoryStream;
  i, j            : Integer;
  DestBit, MaskBit: PColor32Array;
  mr, mg, mb      : Byte;
  na              : Cardinal;
  AColor          : TColor32;
begin
{$RANGECHECKS OFF}

  MaskBitmap := TBitmap32.Create;
  try
    MaskBitmap.DrawMode := dmBlend;
    MaskBitmap.SetSize(ABitmap.Width, ABitmap.Height);
    MaskBitmap.Clear(clWhite32);

    RichTextStream := TMemoryStream.Create;
    try
      ARichEdit.Lines.SaveToStream(RichTextStream);
      RichTextStream.Position := 0;

      with GMaskRichEdit do
      begin
        GMaskRichEdit.Parent := ARichEdit.Parent;
        Lines.LoadFromStream(RichTextStream);
        SelectAll;
       // SelAttributes.Color := clBlack;
        SelLength := 0;
      end;
    finally
      RichTextStream.Free;
    end;

    PrintRTFToCanvas(ABitmap.Canvas, ARichEdit, ARect);
    PrintRTFToCanvas(MaskBitmap.Canvas, GMaskRichEdit, ARect);

    for j := 0 to ABitmap.Height - 1 do
    begin
      if (j < ARect.Top) or (j > ARect.Bottom) then
      begin
        Continue;
      end;

      DestBit := ABitmap.ScanLine[j];
      MaskBit := MaskBitmap.ScanLine[j];

      for i := 0 to ABitmap.Width - 1 do
      begin
        if (i < ARect.Left) or (i > ARect.Right) then
        begin
          Continue;
        end;

        mr := MaskBit[i] shr 16 and $FF;
        mg := MaskBit[i] shr  8 and $FF;
        mb := MaskBit[i]        and $FF;
        na := 255 - (mr + mg + mb) div 3;

        AColor     := DestBit[i] and $00FFFFFF;
        DestBit[i] := (na shl 24) or AColor;
      end;
    end;
  finally
    MaskBitmap.Free;
  end;

{$RANGECHECKS ON}
end;

function GetScreenPixelsPerInch: Integer;
begin
  Result := Screen.PixelsPerInch;
end;

function CheckIfEditEmpty(AEdit: TEdit): Boolean;
begin
  if AEdit.Text = '' then
  begin
    Result := True;
    MessageDlg('Empty value is a invalid value', mtInformation, [mbOK], 0);
  end
  else
  begin
    Result := False;
  end;
end;

// change cursor according to a given degrees
function GetCursorByDegree(const ADeg: Extended): TCursor;
begin
  Result := crDefault;
  
  if ADeg < 0 then
  begin
    if ADeg >= (-15.0) then
    begin
      Result := crRotatePointer1;
    end
    else
    if ( ADeg > (-75.0) ) and ( ADeg < (-15.0) ) then
    begin
      Result := crRotatePointer2;
    end
    else
    if ( ADeg >= (-105.0) ) and ( ADeg <= (-75.0) ) then
    begin
      Result := crRotatePointer3;
    end
    else
    if ( ADeg > (-165.0) ) and ( ADeg < (-105.0) ) then
    begin
      Result := crRotatePointer4;
    end
    else
    if ( ( ADeg >= (-180.0) ) and ( ADeg <= (-165.0) ) ) then
    begin
      Result := crRotatePointer5;
    end;
  end
  else if ADeg >= 0 then
  begin
    if ADeg <= 15.0 then
    begin
      Result := crRotatePointer1;
    end
    else
    if (ADeg < 75.0) and (ADeg > 15.0) then
    begin
      Result := crRotatePointer8;
    end
    else
    if (ADeg <= 105.0) and (ADeg >= 75.0) then
    begin
      Result := crRotatePointer7;
    end
    else
    if (ADeg < 165.0) and (ADeg > 105.0) then
    begin
      Result := crRotatePointer6;
    end
    else
    if (ADeg <= 180.0) and (ADeg >= 165.0) then
    begin
      Result := crRotatePointer5;
    end;
  end;
end;

// change cursor according to various handles
function SetCursorByHandle(const ADrawingHandle: TgmDrawingHandle): TCursor;
begin
  Result := crDefault;

  if (ADrawingHandle = dhAxAy) or (ADrawingHandle = dhBxBy) then
  begin
    Result := crSizeNWSE;
  end
  else
  if (ADrawingHandle = dhAxBy) or (ADrawingHandle = dhBxAy) then
  begin
    Result := crSizeNESW;
  end
  else
  if (ADrawingHandle = dhTopHalfAxBx) or (ADrawingHandle = dhBottomHalfAxBx) then
  begin
    Result := crSizeNS;
  end
  else
  if (ADrawingHandle = dhLeftHalfAyBy) or (ADrawingHandle = dhRightHalfAyBy) then
  begin
    Result := crSizeWE;
  end
  else
  if (ADrawingHandle in [dhLineStart, dhLineEnd, dhCurvePoint1, dhCurvePoint2, dhPolygonPoint]) then
  begin
    Result := crSizeAll;
  end
  else
  if (ADrawingHandle = dhNone) then
  begin
    Result := crDefault;
  end;
end;

function GetBitmapTopLeftInImage32(const AImageControl: TCustomImage32): TPoint;
var
  LRect: TRect;
begin
  // get the location of the bitmap in Image32
  LRect := AImageControl.GetBitmapRect;       
  
  // convert the upper left coordinate from image32 coordinate to bitmap coordinate
  Result := AImageControl.ControlToBitmap( Point(LRect.Left, LRect.Top) );
end;

function CheckInputNumberValid(ASender: TObject; var AKey: Word;
  AShift: TShiftState): Boolean;
begin
  if ( not (AKey in [Ord('0')..Ord('9')]) ) and
     ( not (AKey in [VK_NUMPAD0, VK_NUMPAD1, VK_NUMPAD2, VK_NUMPAD3,
                     VK_NUMPAD4, VK_NUMPAD5, VK_NUMPAD6, VK_NUMPAD7,
                     VK_NUMPAD8, VK_NUMPAD9, VK_RETURN,  VK_BACK,
                     VK_TAB,     VK_LEFT,    VK_RIGHT,   VK_UP,
                     VK_DOWN,    VK_DELETE,  VK_NUMLOCK]) ) then
  begin
    Result := False;
    MessageDlg('Please input a number!', mtInformation, [mbOK], 0);
  end
  else
  begin
    Result := True;
  end;
end;

function CheckInputFloatNumberValid(ASender: TObject; var AKey: Word;
  AShift: TShiftState): Boolean;
begin
  // ASCII code for decimal point is #190
  if ( not (AKey in [Ord('0')..Ord('9'), Ord(#190)]) ) and
     ( not (AKey in [VK_NUMPAD0, VK_NUMPAD1, VK_NUMPAD2, VK_NUMPAD3,
                     VK_NUMPAD4, VK_NUMPAD5, VK_NUMPAD6, VK_NUMPAD7,
                     VK_NUMPAD8, VK_NUMPAD9, VK_RETURN,  VK_BACK,
                     VK_TAB,     VK_LEFT,    VK_RIGHT,   VK_UP,
                     VK_DOWN,    VK_DELETE,  VK_NUMLOCK]) ) then
  begin
    Result := False;
    MessageDlg('Please input a number!', mtInformation, [mbOK], 0);
  end
  else
  begin
    Result := True;
  end;
end;

function CheckIfInRange(const ACheckValue, AMinValue, AMaxValue: Integer): Boolean;
begin
  if (ACheckValue >= AMinValue) and (ACheckValue <= AMaxValue) then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
    
    MessageDlg(Format('The value should be in from %d to %d!',
               [AMinValue, AMaxValue]), mtInformation, [mbOK], 0);
  end
end;

function PrintRTFToCanvas(ACanvas: TCanvas; ARichEdit: TMemo; const ARect: TRect): Longint;
begin

end;

function PrintRTFToBitmap(ARichEdit: TMemo; ABitmap: TBitmap): Longint;
begin

end;

procedure GlobalInitialize;
begin
  // loading custom cursors from resources
  Screen.Cursors[crPathComponentSelection] := LoadCursor(hInstance, 'RGGMPATHCOMPONENTSELECTIONTOOL');
  Screen.Cursors[crDirectSelection]        := LoadCursor(hInstance, 'RGGMDIRECTSELECTIONTOOL');
  Screen.Cursors[crPenToolDeselected]      := LoadCursor(hInstance, 'RGGMPENTOOLDESELECTED');
  Screen.Cursors[crPenToolSelected]        := LoadCursor(hInstance, 'RGGMPENTOOL');
  Screen.Cursors[crAddAnchorPoint]         := LoadCursor(hInstance, 'RGGMADDANCHORPOINT');
  Screen.Cursors[crDeleteAnchorPoint]      := LoadCursor(hInstance, 'RGGMDELETEANCHORPOINT');
  Screen.Cursors[crMovePath]               := LoadCursor(hInstance, 'RGGMMOVEPATH');
  Screen.Cursors[crConvertPoint]           := LoadCursor(hInstance, 'RGGMCONVERTPOINT');
  Screen.Cursors[crClosePath]              := LoadCursor(hInstance, 'RGGMCLOSEPATH');
  Screen.Cursors[crPenToolLastEnd]         := LoadCursor(hInstance, 'RGGMPENTOOLLASTEND');
  Screen.Cursors[crAddCornerPoint]         := LoadCursor(hInstance, 'RGGMADDCORNERPOINT');
  Screen.Cursors[crPaintBucket]            := LoadCursor(hInstance, 'RGGMPAINTBUCKET');
  Screen.Cursors[crEyedropper]             := LoadCursor(hInstance, 'RGGMEYEDROPPER');
  Screen.Cursors[crHandLoosen]             := LoadCursor(hInstance, 'RGGMHANDLOOSEN');
  Screen.Cursors[crHandGrip]               := LoadCursor(hInstance, 'RGGMHANDGRIP');
  Screen.Cursors[crCloneStamp]             := LoadCursor(hInstance, 'RGGMCLONESTAMP');
  Screen.Cursors[crCrossAdd]               := LoadCursor(hInstance, 'RGGMCROSSADD');
  Screen.Cursors[crCrossSub]               := LoadCursor(hInstance, 'RGGMCROSSSUB');
  Screen.Cursors[crCrossIntersect]         := LoadCursor(hInstance, 'RGGMCROSSINTERSECT');
  Screen.Cursors[crCrossInterSub]          := LoadCursor(hInstance, 'RGGMCROSSINTERSUB');
  Screen.Cursors[crPolygonSelection]       := LoadCursor(hInstance, 'RGGMPOLYGONSELECTION');
  Screen.Cursors[crPolygonAdd]             := LoadCursor(hInstance, 'RGGMPOLYGONADD');
  Screen.Cursors[crPolygonSub]             := LoadCursor(hInstance, 'RGGMPOLYGONSUB');
  Screen.Cursors[crPolygonIntersect]       := LoadCursor(hInstance, 'RGGMPOLYGONINTERSECT');
  Screen.Cursors[crPolygonInterSub]        := LoadCursor(hInstance, 'RGGMPOLYGONINTERSUB');
  Screen.Cursors[crLassoSelection]         := LoadCursor(hInstance, 'RGGMLASSOSELECTION');
  Screen.Cursors[crLassoAdd]               := LoadCursor(hInstance, 'RGGMLASSOADD');
  Screen.Cursors[crLassoSub]               := LoadCursor(hInstance, 'RGGMLASSOSUB');
  Screen.Cursors[crLassoIntersect]         := LoadCursor(hInstance, 'RGGMLASSOINTERSECT');
  Screen.Cursors[crLassoInterSub]          := LoadCursor(hInstance, 'RGGMLASSOINTERSUB');
  Screen.Cursors[crMagicWand]              := LoadCursor(hInstance, 'RGGMMAGICWAND');
  Screen.Cursors[crMagicWandAdd]           := LoadCursor(hInstance, 'RGGMMAGICWANDADD');
  Screen.Cursors[crMagicWandSub]           := LoadCursor(hInstance, 'RGGMMAGICWANDSUB');
  Screen.Cursors[crMagicWandIntersect]     := LoadCursor(hInstance, 'RGGMMAGICWANDINTERSECT');
  Screen.Cursors[crMagicWandInterSub]      := LoadCursor(hInstance, 'RGGMMAGICWANDINTERSUB');
  Screen.Cursors[crMoveSelection]          := LoadCursor(hInstance, 'RGGMMOVESELECTION');
  Screen.Cursors[crCrop]                   := LoadCursor(hInstance, 'RGGMCROP');
  Screen.Cursors[crMeasure]                := LoadCursor(hInstance, 'RGGMMEASURE');
  Screen.Cursors[crMeasureMove]            := LoadCursor(hInstance, 'RGGMMEASUREMOVE');
  Screen.Cursors[crMeasureAngle]           := LoadCursor(hInstance, 'RGGMMEASUREANGLE');
  Screen.Cursors[crMagicEraser]            := LoadCursor(hInstance, 'RGGMMAGICERASER');

  // for transformation -- rotation
  Screen.Cursors[crRotatePointer1]         := LoadCursor(hInstance, 'RGGMROTATEPOINTER1');
  Screen.Cursors[crRotatePointer2]         := LoadCursor(hInstance, 'RGGMROTATEPOINTER2');
  Screen.Cursors[crRotatePointer3]         := LoadCursor(hInstance, 'RGGMROTATEPOINTER3');
  Screen.Cursors[crRotatePointer4]         := LoadCursor(hInstance, 'RGGMROTATEPOINTER4');
  Screen.Cursors[crRotatePointer5]         := LoadCursor(hInstance, 'RGGMROTATEPOINTER5');
  Screen.Cursors[crRotatePointer6]         := LoadCursor(hInstance, 'RGGMROTATEPOINTER6');
  Screen.Cursors[crRotatePointer7]         := LoadCursor(hInstance, 'RGGMROTATEPOINTER7');
  Screen.Cursors[crRotatePointer8]         := LoadCursor(hInstance, 'RGGMROTATEPOINTER8');

  Screen.Cursors[crBlackPicker]            := LoadCursor(hInstance, 'RGGMBLACKPICKER');
  Screen.Cursors[crGrayPicker]             := LoadCursor(hInstance, 'RGGMGRAYPICKER');
  Screen.Cursors[crWhitePicker]            := LoadCursor(hInstance, 'RGGMWHITEPICKER');

  Screen.Cursors[crMagneticLasso]          := LoadCursor(hInstance, 'RGGMMAGNETICLASSO');

  GMaskRichEdit            := TMemo.Create(nil);
  GMaskRichEdit.Parent     := nil;
  GMaskRichEdit.ScrollBars := ssVertical;
  GMaskRichEdit.Visible    := False;
end;

procedure GlobalFinalize;
begin
  FreeAndNil(GMaskRichEdit);
end; 

initialization
  GlobalInitialize;

finalization


end.
