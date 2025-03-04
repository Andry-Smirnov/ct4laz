
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit TplSpeedButtonUnit;

interface

uses
  LCLIntf, LCLType, LMessages, Messages,
  Classes, Controls, Forms, Graphics, StdCtrls, ExtCtrls,
  Buttons, ImgList, ActnList, plUtils;

type
  TplSpeedButton = class(TGraphicControl)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FTransparent: TplTransparentMode;
    TextBounds: TRect;
    GlyphPos: TPoint;
    FNumGlyphs: TNumGlyphs;

    FDownToColor: TColor;
    FDisabledToColor: TColor;
    FFocusedToColor: TColor;
    FFocusedDownToColor: TColor;

    FBorderColor: TColor;
    FColorHighlight: TColor;
    FColorShadow: TColor;   

    FGroupIndex: Integer;
    FGlyph: TBitmap;
    FDown: Boolean;
    FDragging: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    FMouseInControl: Boolean;
    FModalResult: TModalResult;
    procedure SetColors (Index: Integer; Value: TColor);     
    procedure UpdateExclusive;
    procedure SetGlyph (Value: TBitmap);
    procedure SetNumGlyphs (Value: TNumGlyphs);
    procedure SetDown (Value: Boolean);
    procedure SetAllowAllUp (Value: Boolean);
    procedure SetGroupIndex (Value: Integer);
    procedure SetLayout (Value: TButtonLayout);
    procedure SetSpacing (Value: Integer);
    procedure SetMargin (Value: Integer);
    procedure UpdateTracking;
    procedure WMLButtonDBLCLK(var Message: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed (var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar (var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange (var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure CMMouseEnter(var Message :TLMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message :TLMessage); message CM_MOUSELEAVE;
    procedure SetTransparent (const Value: TplTransparentMode);
    procedure RemoveMouseTimer;
    procedure MouseTimerHandler (Sender: TObject);
  protected
    FState: TButtonState;
    function GetPalette: HPALETTE; override;
    procedure CalcAdvColors;
    procedure Loaded; override;
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove (Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;

  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure MouseEnter;
    procedure MouseLeave;
   
  published
    property TransparentMode: TplTransparentMode read FTransparent write SetTransparent default tmNone;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property Caption;
    property Enabled;
    property Font;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphTop;
    property Margin: Integer read FMargin write SetMargin default -1;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs default 1;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property ParentFont;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;

    property Action;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

var
  MouseInControl: TplSpeedButton = nil;

implementation

var
  MouseTimer: TTimer = nil;
  ControlCounter: Integer = 0;

constructor TplSpeedButton.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);

  if MouseTimer = nil then
  begin
    MouseTimer := TTimer.Create(nil);
    MouseTimer.Enabled := False;
    MouseTimer.Interval := 100; // 10 times a second
  end;

  ControlStyle := [csCaptureMouse, csDoubleClicks];

  self.SetInitialBounds(0, 0, 22,22);

  FGlyph := TBitmap.Create;
  FNumGlyphs := 1;
  ParentFont := True;
  ParentColor := false;

  FSpacing := 4;
  FMargin := -1;
  FLayout := blGlyphLeft;

  FModalResult := mrNone;
  FTransparent := tmNone;
  Inc(ControlCounter);
end;

destructor TplSpeedButton.Destroy;
begin
  RemoveMouseTimer;
  FGlyph.Free;
  Dec(ControlCounter);
  if ControlCounter = 0 then
  begin
    MouseTimer.Free;
    MouseTimer := nil;
  end;
  inherited Destroy;
end;

procedure TplSpeedButton.Paint;
var
  FTransColor: TColor;
  FImageList: TImageList;
  sourceRect, destRect: TRect;
  tempGlyph: TBitmap;
  Offset: TPoint;
  xToC:TColor;
begin
  if Color<>DefiBtnToColor then  Color:=DefiBtnToColor;
  FDownToColor:=DefiBtnDownToColor;
  FFocusedToColor:=DefiBtnFocusedToColor;
  FFocusedDownToColor:=DefiBtnDownFocusedToColor;
  FDisabledToColor:=DefiBtnDisabledToColor;

  FBorderColor:=DefiBtnBorderColor;
  FColorHighlight:=DefiBtnColorHighlight;
  FColorShadow:=DefiBtnColorShadow;

  // get the transparent color
  FTransColor := FGlyph.Canvas.Pixels[0, FGlyph.Height - 1];
  Canvas.Font := Self.Font;

  if FState in [bsDown, bsExclusive] then
    Offset := Point(1, 1) else
    Offset := Point(0, 0);

  CalcButtonLayout(Canvas, ClientRect, Offset, FLayout, FSpacing,
                    FMargin, FGlyph, FNumGlyphs, Caption, TextBounds, GlyphPos);

  if not Enabled then
  begin
    FState := bsDisabled;
    FDragging := False;
  end
  else
    if FState = bsDisabled then
      if FDown and (GroupIndex <> 0) then
        FState := bsExclusive  else
        FState := bsUp;

  // ----- DrawBackground --------------------------------------------------
  case FTransparent of
    tmAlways: ;
    tmNone:
      begin
        case FState of
          bsUp:
            if FMouseInControl then
              xToC := FFocusedToColor   else
              xToC := Self.Color;
          bsDown:
              xToC := FDownToColor;
          bsExclusive:
            if FMouseInControl then
                xToC := FFocusedDownToColor else
                xToC := FDownToColor;
          bsDisabled:
              xToC := FDisabledToColor;
        end;

        GradientFillRect(Canvas,ClientRect,DefiBtnFromColor,xToC,fdTopToBottom,32);
      end;
    tmNotFocused:
      if FMouseInControl then
      begin
        case FState of
          bsUp:
            if FMouseInControl then
              xToC := FFocusedToColor    else
              xToC := Self.Color;
          bsDown:
              xToC := FDownToColor;
          bsExclusive:
            if FMouseInControl then
              xToC := FFocusedToColor    else
              xToC := FDownToColor;
          bsDisabled:
              xToC := FDisabledToColor;
        end;
          GradientFillRect(Canvas,ClientRect,DefiBtnFromColor,xToC,fdTopToBottom,32);
      end;
  end;

  // ---- DrawBorder -------------------------------------------------------------
  case FState of
    bsUp:
      if FMouseInControl then
        Frame3DBorder(canvas, ClientRect, FColorHighlight, FColorShadow, 1) else
        Frame3DBorder(canvas, ClientRect, FBorderColor, FBorderColor, 1);
    bsDown, bsExclusive:
      Frame3DBorder(canvas, ClientRect, FColorShadow, FColorHighlight, 1);
    bsDisabled:
      Frame3DBorder(canvas, ClientRect, FBorderColor, FBorderColor, 1);
  end;

  // DrawGlyph
  if not FGlyph.Empty then
  begin
    tempGlyph := TBitmap.Create;
    case FNumGlyphs of
      1: case FState of
           bsUp:        sourceRect := Rect(0, 0, FGlyph.Width, FGlyph.Height);
           bsDisabled:  sourceRect := Rect(0, 0, FGlyph.Width, FGlyph.Height);
           bsDown:      sourceRect := Rect(0, 0, FGlyph.Width, FGlyph.Height);
           bsExclusive: sourceRect := Rect(0, 0, FGlyph.Width, FGlyph.Height);
         end;
      2: case FState of
           bsUp:        sourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
           bsDisabled:  sourceRect := Rect(FGlyph.Width div FNumGlyphs, 0, FGlyph.Width, FGlyph.Height);
           bsDown:      sourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
           bsExclusive: sourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
         end;
      3: case FState of
           bsUp:        SourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
           bsDisabled:  SourceRect := Rect(FGlyph.width div FNumGlyphs, 0, (FGlyph.Width div FNumGlyphs) * 2, FGlyph.Height);
           bsDown:      SourceRect := Rect((FGlyph.Width div FNumGlyphs) * 2, 0, FGlyph.Width, FGlyph.Height);
           bsExclusive: SourceRect := Rect((FGlyph.Width div FNumGlyphs) * 2, 0, FGlyph.Width, FGlyph.Height);
         end;
      4: case FState of
           bsUp:        SourceRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
           bsDisabled:  SourceRect := Rect(FGlyph.width div FNumGlyphs, 0, (FGlyph.Width div FNumGlyphs) * 2, FGlyph.Height);
           bsDown:      SourceRect := Rect((FGlyph.Width div FNumGlyphs) * 2, 0, (FGlyph.Width div FNumGlyphs) * 3, FGlyph.Height);
           bsExclusive: SourceRect := Rect((FGlyph.width div FNumGlyphs) * 3, 0, FGlyph.Width, FGlyph.Height);
         end;
    end;

    destRect := Rect(0, 0, FGlyph.Width div FNumGlyphs, FGlyph.Height);
    tempGlyph.Width := FGlyph.Width div FNumGlyphs;
    tempGlyph.Height := FGlyph.Height;
    tempGlyph.canvas.copyRect(destRect, FGlyph.canvas, sourcerect);

    if (FNumGlyphs = 1) and (FState = bsDisabled) then
    begin
      tempGlyph := CreateDisabledBitmap(tempGlyph, clBlack, clBtnFace, clBtnHighlight, clBtnShadow, True);
      FTransColor := tempGlyph.Canvas.Pixels[0, tempGlyph.Height - 1];
    end;

    if (FGlyph.Width>0) and (Glyph.Height>0) then
      begin
        try
         FImageList := TImageList.CreateSize(FGlyph.Width div FNumGlyphs, FGlyph.Height);
         FImageList.AddMasked(tempGlyph, FTransColor);
         FImageList.Draw(canvas, glyphpos.x, glyphpos.y, 0);
       finally
         FImageList.Free;
       end;

    end;
    tempGlyph.free;
  end;

  // DrawText
  Canvas.Brush.Style := bsClear;
  if FState = bsDisabled then
  begin
    OffsetRect(TextBounds, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    OffsetRect(TextBounds, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end
  else
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
end;

procedure TplSpeedButton.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled then
  begin
    GetCursorPos(P);
    FMouseInControl := not (FindDragTarget(P, True) = Self);
    if FMouseInControl then
      MouseLeave
    else
      MouseEnter;
  end;
end;

procedure TplSpeedButton.Loaded;
begin
  inherited Loaded;
  Invalidate;
end;

procedure TplSpeedButton.MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    if not FDown then
    begin
      FState := bsDown;
      Invalidate;
    end;
    FDragging := True;
  end;
end;

procedure TplSpeedButton.MouseMove (Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
  P: TPoint;
begin
  inherited;

  // mouse is in control ?
  P := ClientToScreen(Point(X, Y));
  if (MouseInControl <> Self) and (FindDragTarget(P, True) = Self) then
  begin
    if Assigned(MouseInControl) then
      MouseInControl.MouseLeave;
    // the application is active ?
    if (GetActiveWindow <> 0) then
    begin
      if MouseTimer.Enabled then
        MouseTimer.Enabled := False;
      MouseInControl := Self;
      MouseTimer.OnTimer := @MouseTimerHandler;
      MouseTimer.Enabled := True;
      MouseEnter;
    end;
  end;

  if FDragging then
  begin
    if not FDown then
      NewState := bsUp
    else
      NewState := bsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then
        NewState := bsExclusive
      else
        NewState := bsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end;
end;

procedure TplSpeedButton.MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      // Redraw face in-case mouse is captured
      FState := bsUp;
      FMouseInControl := False;
      if DoClick and not (FState in [bsExclusive, bsDown]) then
        Invalidate;
    end
    else
      if DoClick then
      begin
        SetDown(not FDown);
        if FDown then Repaint;
      end
      else
      begin
        if FDown then FState := bsExclusive;
        Repaint;
      end;
    if DoClick then Click else MouseLeave;
    UpdateTracking;
  end;
end;

procedure TplSpeedButton.Click;
begin
  if Parent <> nil then
    GetParentForm(self).ModalResult := FModalResult;
  inherited Click;
end;

function TplSpeedButton.GetPalette: HPALETTE;
begin
  Result := FGlyph.Palette;
end;

procedure TplSpeedButton.SetColors (Index: Integer; Value: TColor);
begin
  case Index of
    0: FFocusedToColor := Value;
    1: FDownToColor := Value;
    2: FBorderColor := Value;
    3: FColorHighlight := Value;
    4: FColorShadow := Value;
  end;
  Invalidate;
end;

procedure TplSpeedButton.CalcAdvColors;
begin
  if DefiUseAdvColors then
  begin
    FFocusedToColor := CalcAdvancedColor(Color, FFocusedToColor, DefiAdvColorFocused, lighten);
    FDownToColor := CalcAdvancedColor(Color, FDownToColor, DefiAdvColorDown, darken);
    FBorderColor := CalcAdvancedColor(Color, FBorderColor, DefiAdvColorBorder, darken);
  end;
end;


procedure TplSpeedButton.SetGlyph (value: TBitmap);
begin
  if value <> FGlyph then
  begin
    FGlyph.Assign(value);
    if not FGlyph.Empty then
    begin
      if FGlyph.Width mod FGlyph.Height = 0 then
      begin
        FNumGlyphs := FGlyph.Width div FGlyph.Height;
        if FNumGlyphs > 4 then FNumGlyphs := 1;
      end;
    end;
    Invalidate;
  end;
end;

procedure TplSpeedButton.SetNumGlyphs (value: TNumGlyphs);
begin
  if value <> FNumGlyphs then
  begin
    FNumGlyphs := value;
    Invalidate;
  end;
end;

procedure TplSpeedButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := PtrInt(@Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TplSpeedButton.SetDown (Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      if FState = bsUp then Invalidate;
      FState := bsExclusive
    end
    else
    begin
      FState := bsUp;
      Repaint;
    end;
    if Value then UpdateExclusive;
  end;
end;

procedure TplSpeedButton.SetGroupIndex (Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TplSpeedButton.SetLayout (Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TplSpeedButton.SetMargin (Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TplSpeedButton.SetSpacing (Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TplSpeedButton.SetAllowAllUp (Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TplSpeedButton.WMLButtonDBLCLK(var Message: TLMLButtonDblClk);
begin
  inherited;
  if FDown then DblClick;
end;

procedure TplSpeedButton.CMEnabledChanged (var Message: TMessage);
begin
  inherited;
  if not Enabled then
  begin
    FMouseInControl := False;
    FState := bsDisabled;
    RemoveMouseTimer;
  end;
  UpdateTracking;
  Invalidate;
end;

procedure TplSpeedButton.CMButtonPressed (var Message: TMessage);
var
  Sender: TplSpeedButton;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TplSpeedButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TplSpeedButton.CMDialogChar (var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TplSpeedButton.CMFontChanged (var Message: TMessage);
begin
  Invalidate;
end;

procedure TplSpeedButton.CMTextChanged (var Message: TMessage);
begin
  Invalidate;
end;

procedure TplSpeedButton.CMSysColorChange (var Message: TMessage);
begin
  if DefiUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplSpeedButton.CMParentColorChanged(var Message: TLMessage);
begin
  inherited;
  if DefiUseAdvColors then
  begin
    ParentColor := True;
    CalcAdvColors;
  end;
  Invalidate;
end;

procedure TplSpeedButton.MouseEnter;
begin
  if Enabled and not FMouseInControl  then
  begin
    FMouseInControl := True;
    Invalidate;
  end;
end;

procedure TplSpeedButton.MouseLeave;
begin
  if Enabled and FMouseInControl and not FDragging then
  begin
    FMouseInControl := False;
    RemoveMouseTimer;
    Invalidate;
  end;
end;

procedure TplSpeedButton.MouseTimerHandler (Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos (P);
  if FindDragTarget(P, True) <> Self then
    MouseLeave;
end;

procedure TplSpeedButton.RemoveMouseTimer;
begin
  if MouseInControl = Self then
  begin
    MouseTimer.Enabled := False;
    MouseInControl := nil;
  end;
end;

procedure TplSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);

  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia;//! for lack of a better color
      Canvas.FillRect(Rect(0,0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
    end;
  end;

begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      { Copy image from action's imagelist }
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images, ImageIndex);
    end;
end;


procedure TplSpeedButton.SetTransparent(const Value: TplTransparentMode);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TplSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TplSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

end.
