{*************************************************************************
                PilotLogic Software House

  Package pl_Graphics32
  This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)

 ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * https://www.mozilla.org/en-US/MPL/1.1/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All rights reserved
 *
 * ***** END LICENSE BLOCK *****
 ************************************************************************}

unit GR32_ColorSwatch;

interface

{$I GR32.inc}

uses
  LCLIntf, LCLType, LMessages, Types,
  Classes, Controls, Forms, GR32, GR32_Containers,
  Math, Graphics, GR32_Backends, GR32_Math, GR32_Blend, GR32_VectorUtils;

type
  TCustomColorSwatch = class(TCustomControl)
  private
    FBuffer: TBitmap32;
    FColor: TColor32;
    FBufferValid: Boolean;
    FBorder: Boolean;
    procedure SetBorder(const Value: Boolean);
    procedure SetColor(const Value: TColor32);
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TLMessage); message LM_GETDLGCODE;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Invalidate; override;
    procedure Resize; override;

    property Border: Boolean read FBorder write SetBorder default False;
    property Color: TColor32 read FColor write SetColor;
  end;

  TColorSwatch = class(TCustomColorSwatch)
  published
    property Align;
    property Anchors;
    property Border;
    property Color;
    property DragCursor;
    property DragKind;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    property OnStartDrag;
  end;

implementation

{ TCustomColorSwatch }

constructor TCustomColorSwatch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FBuffer := TBitmap32.Create;
  FColor := clSalmon32;

  SetInitialBounds(0,0,140,140);  //--- ct9999 ------
end;

destructor TCustomColorSwatch.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TCustomColorSwatch.Invalidate;
begin
  FBufferValid := False;
  inherited;
end;

procedure TCustomColorSwatch.Paint;
var
  X, Y: Integer;
  OddY: Boolean;
  ScanLine: PColor32Array;
const
  CCheckerBoardColor: array [Boolean] of TColor32 = ($FFA0A0A0, $FF5F5F5F);
begin
  if not Assigned(Parent) then
    Exit;

  if not FBufferValid then
  begin
    (FBuffer.Backend as IPaintSupport).ImageNeeded;

    // draw checker board
    if not (FColor and $FF000000 = $FF000000) then
    begin
      Y := 0;
      while Y < FBuffer.Height do
      begin
        ScanLine := FBuffer.Scanline[Y];
        OddY := Odd(Y shr 2);
        for X := 0 to FBuffer.Width - 1 do
          ScanLine[X] := CCheckerBoardColor[Odd(X shr 2) = OddY];
        Inc(Y);
      end;
    end;

    // draw color
    FBuffer.FillRectT(0, 0, FBuffer.Width, FBuffer.Height, FColor);

    // eventually draw border
    if FBorder then
    begin
      FBuffer.FrameRectTS(0, 0, FBuffer.Width, FBuffer.Height, $DF000000);
      FBuffer.RaiseRectTS(1, 1, FBuffer.Width - 1, FBuffer.Height - 1, 20);
    end;

    (FBuffer.Backend as IPaintSupport).CheckPixmap;
    FBufferValid := True;
  end;

  FBuffer.Lock;
  with Canvas do
  try
    (FBuffer.Backend as IDeviceContextSupport).DrawTo(Canvas.Handle, 0, 0);
  finally
    FBuffer.Unlock;
  end;
end;

procedure TCustomColorSwatch.Resize;
begin
  inherited;

  FBuffer.SetSize(Width, Height);
  FBufferValid := False;
end;

procedure TCustomColorSwatch.SetBorder(const Value: Boolean);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Invalidate;
  end;
end;

procedure TCustomColorSwatch.SetColor(const Value: TColor32);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TCustomColorSwatch.WMEraseBkgnd(var Message: {$IFDEF FPC}TLmEraseBkgnd{$ELSE}TWmEraseBkgnd{$ENDIF});
begin
  Message.Result := 1;
end;

procedure TCustomColorSwatch.WMGetDlgCode(var Msg: {$IFDEF FPC}TLMessage{$ELSE}TWmGetDlgCode{$ENDIF});
begin
  with Msg do
    Result := Result or DLGC_WANTARROWS;
end;

end.
