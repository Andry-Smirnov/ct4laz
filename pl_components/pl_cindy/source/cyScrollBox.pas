{**********************************************************************
                PilotLogic Software House

 Package pl_Cindy
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

{   Component(s):
    tcyScrollbox

    Description:
    A ScrolBox with Canvas

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

unit cyScrollBox;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages,
  cyClasses, Forms,  Controls, Classes, Messages, Graphics;

type
  TProcFocusChanged = procedure (Sender: TObject; FocusedControl: TControl) of object;

  TcyBaseScrollBox = class(TScrollBox)
  private
    FCanvas: TCanvas;
    FOnFocusChanged: TProcFocusChanged;
    FOnPaint: TNotifyEvent;
    FOnVertScroll: TNotifyEvent;
    FOnHorzScroll: TNotifyEvent;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure WMHScroll(var message : TLMHScroll); message LM_HSCROLL;
    procedure WMVScroll(var message : TLMVScroll); message LM_VSCROLL;
  protected
    procedure Paint;  override;
    procedure PaintWindow(DC: HDC); override;
    property Canvas: TCanvas read FCanvas;
    property OnFocusChanged: TProcFocusChanged read FOnFocusChanged write FOnFocusChanged;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
   property OnVertScroll: TNotifyEvent read FOnVertScroll write FOnVertScroll;
   property OnHorzScroll: TNotifyEvent read FOnHorzScroll write FOnHorzScroll;
  end;


  TcyScrollBox = class(TcyBaseScrollBox)
  private
  protected
  public
    property Canvas;
  published
    property OnFocusChanged;
    property OnPaint;
  end;


implementation

{ TcyBaseScrollBox }
constructor TcyBaseScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TcyBaseScrollBox.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TcyBaseScrollBox.WMPaint(var Msg: TLMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

procedure TcyBaseScrollBox.WMVScroll(var message : TLMVScroll);
begin
  Inherited;

  if Assigned(FOnVertScroll) then
    FOnVertScroll(Self);
end;

procedure TcyBaseScrollBox.WMHScroll(var message : TLMHScroll);
begin
  Inherited;

  if Assigned(FOnHorzScroll) then
    FOnHorzScroll(Self);
end;

// Call Paint :
procedure TcyBaseScrollBox.PaintWindow(DC: HDC);
begin
  inherited;

  FCanvas.Lock;                     // Multithread drawing ...
  try
    FCanvas.Handle := DC;
    try
  //    TControlCanvas(FCanvas).UpdateTextFlags;   //=== ct9999 ====
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TcyBaseScrollBox.Paint;
begin


  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

end.
