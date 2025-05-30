{*************************************************************************
                       PilotLogic Software House

  Package pl_Graphics32MG
  This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)     
**************************************************************************}

unit igLayersListBox;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
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
 *
 * The Initial Developer of this unit are
 *   Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *   x2nie  < x2nie[at]yahoo[dot]com >
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$MODE Delphi}


uses
{ Delphi }
  Types, LCLIntf, LCLType, LMessages, Controls, Classes,
{ Graphics32 }
  GR32, GR32_Image, GR32_Layers, GR32_RangeBars,
{ miniGlue lib }
  igBase, igLayers, igLayerPanelManager;

type
  TigLayersListBox = class(TigLayerPanelManager)
  private
  protected
    FAgent : TigAgent;                    //integrator's event listener
    FLayerList : TLayerCollection;       //to compare between last & current 
    procedure ActivePaintBoxSwitched(Sender: TObject);
    procedure SoInvalidate(Sender: TObject; ALayer: TigLayer);
    procedure InvalidateEvent(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    
    property Agent: TigAgent read FAgent; //read only. for internal access
  published
    property Align;
  end;
  
implementation

{ TigLayersListBox }

procedure TigLayersListBox.ActivePaintBoxSwitched(Sender: TObject);
begin
  //set visual layers to new active paintbox
  if Assigned(GIntegrator.ActivePaintBox) then
  begin
    //remove event
    ///`if Assigned(LayerList) and  (LayerList <> GIntegrator.ActivePaintBox.LayerList) then
      ///LayerList.OnLayerChanged := nil;

    //install event
    Self.LayerList := GIntegrator.ActivePaintBox.Layers;
    ///LayerList.OnLayerChanged := SoInvalidate;
  end
  else
  begin
    //remove event
    //if Assigned(LayerList) and not (csDestroying in LayerList.code then
      //LayerList.OnLayerChanged := nil;

    self.LayerList := nil;
  end;
end;

constructor TigLayersListBox.Create(AOwner: TComponent);
begin
  inherited;
  FAgent := TigAgent.Create(Self); //autodestroy
  FAgent.OnActivePaintBoxSwitch := self.ActivePaintBoxSwitched;
  FAgent.OnInvalidateListener := InvalidateEvent;
end;

procedure TigLayersListBox.InvalidateEvent(Sender: TObject);
begin
  Invalidate;
end;

procedure TigLayersListBox.SoInvalidate(Sender: TObject;ALayer: TigLayer);
begin
  //Invalidate;
end;

end.
