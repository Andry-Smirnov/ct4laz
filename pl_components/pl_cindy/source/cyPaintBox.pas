{**********************************************************************
                PilotLogic Software House
  
 Package pl_Cindy
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

{   Component(s):
    tcyPaintBox

    Description:
    A paintBox with degrade effect

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
    
unit cyPaintBox;

{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages,
     cyClasses, Graphics, ExtCtrls, classes, Messages, Controls;

type
  TcyCustomPaintBox = class(TGraphicControl)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FDegrade: TcyGradient;
    FOnPaint: TNotifyEvent;
    procedure CmMouseEnter(var Msg: TLMessage); message CM_MOUSEENTER;  // ct9999 for CodeTyphon
    procedure CmMouseLeave(var Msg: TLMessage); message CM_MOUSELEAVE;  // ct9999 for CodeTyphon
    procedure SetDegrade(const Value: TcyGradient);
  protected
    procedure Paint; override;
    procedure GradientChanged(Sender: TObject);
    procedure DrawBackground(const aRect: TRect); virtual;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property Degrade: TcyGradient read FDegrade write SetDegrade;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property Width default 105;
    property Height default 105;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
  end;

  TcyPaintBox = class(TcyCustomPaintBox)
  private
  protected
  public
  published
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    // Herited from TcyCustomPaintBox :
    property Degrade;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPaint;
  end;

implementation

constructor TcyCustomPaintBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width := 105;
  Height := 105;
  FDegrade := TcyGradient.Create(self);
  FDegrade.OnChange := GradientChanged;
end;

destructor TcyCustomPaintBox.Destroy;
begin
  FDegrade.Free;
  inherited Destroy;
end;

procedure TcyCustomPaintBox.GradientChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyCustomPaintBox.SetDegrade(const Value: TcyGradient);
begin
  FDegrade.Assign(Value);
end;

procedure TcyCustomPaintBox.Paint;
var Rect: TRect;
begin
  Rect := ClientRect;
  DrawBackground(Rect);
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TcyCustomPaintBox.DrawBackground(const aRect: TRect);
begin
  FDegrade.Draw(Canvas, aRect);

  // Draw box at design time :
  if csDesigning in ComponentState
  then
    with Canvas do
    begin
      Pen.Style := psDash;
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      Rectangle(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom);
    end;
end;

procedure TcyCustomPaintBox.CmMouseEnter(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TcyCustomPaintBox.CmMouseLeave(var Msg: TMessage);
begin
  inherited;
  if Assigned(FonMouseLeave) then FOnMouseLeave(Self);
end;

end.

