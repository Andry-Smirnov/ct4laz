{**********************************************************************
                PilotLogic Software House
  
 Package pl_Cindy
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

{   Component(s):
    tcyAdvPanel

    Description:
    A panel that paint a graphic (Bitmap, jpeg etc ...)

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
    
unit cyAdvPanel;

{$MODE Delphi}

{$I cyCompilerDefines.inc}

interface

uses LCLIntf, LCLType, LMessages,
     cyClasses, cyPanel,  Themes, ExtCtrls, Graphics, classes, Messages, Controls;

type
  TcyAdvPanel = class(TcyCustomPanel)
  private
    FWallpaper: TcyBgPicture;
    procedure SetWallpaper(const Value: TcyBgPicture);
  protected
    procedure DrawBackground(Const aRect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
    property Wallpaper: TcyBgPicture read FWallpaper write SetWallpaper;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    {$IFDEF DELPHI2009_OR_ABOVE} property Padding; {$ENDIF}
    property ParentBiDiMode;
  //  property ParentColor default false;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    {$IFDEF DELPHI2009_OR_ABOVE} property ShowCaption; {$ENDIF}
    property ShowHint;
    property TabOrder;
    property TabStop;
    // property VerticalAlignment;  -> replaced by Layout property ...
    property Visible;
  //  property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    // Herited from TcyBasePanel :
    property Bevels;
    property CaptionOrientation;
    property Layout;
    property RunTimeDesign;
    property Shadow;
    property WordWrap;
    property OnVisibleChanging;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnAfterDrawBackground;
    property OnPaint;
    property OnStartRunTimeDesign;
    property OnDoRunTimeDesign;
    property OnEndRunTimeDesign;
    // Herited from TcyCustomPanel :
    property Degrade;
  end;

implementation

uses Types;

constructor TcyAdvPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWallpaper := TcyBgPicture.Create(self);
  FWallpaper.OnChange := SubPropertiesChanged;
  ParentColor := false; // ct9999
end;

destructor TcyAdvPanel.Destroy;
begin
  FWallpaper.Free;
  inherited Destroy;
end;

procedure TcyAdvPanel.DrawBackground(Const aRect: TRect);
begin
  Inherited;    // Draw degrade ...
  cyDrawBgPicture(Canvas, aRect, FWallpaper);
end;

procedure TcyAdvPanel.SetWallpaper(const Value: TcyBgPicture);
begin
  FWallpaper := Value;
end;

end.
