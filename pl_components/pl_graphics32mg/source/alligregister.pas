{*************************************************************************
                       PilotLogic Software House

  Package pl_Graphics32MG
  This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)   
  
   ***** BEGIN LICENSE BLOCK *****
     Version: MPL 1.1 or LGPL 2.1 with linking exception
 
     The contents of this file are subject to the Mozilla Public License Version
     1.1 (the "License"); you may not use this file except in compliance with
     the License. You may obtain a copy of the License at
     http://www.mozilla.org/MPL/
 
     Software distributed under the License is distributed on an "AS IS" basis,
     WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
     for the specific language governing rights and limitations under the
     License.
 
     Alternatively, the contents of this file may be used under the terms of the
     Free Pascal modified version of the GNU Lesser General Public License
     Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
     of this license are applicable instead of those above.
     Please see the file LICENSE.txt for additional information concerning this
     license.
 
   ***** END LICENSE BLOCK ***** *
 
**************************************************************************}

unit alligregister;

{$MODE Delphi}

interface
uses
  Classes, TypInfo,
  LCLIntf, LResources, LazIDEIntf, PropEdits, ComponentEditors;

procedure Register;

implementation

{$R alligregister.res}

uses
  igBase, igLayersListBox, igComboboxBlendModes,
  igCore_Items,
  igGrid,
  //igGrid_Dsgn,
  igSwatch,
  //igSwatch_Dsgn,
 // igSwatch_ListView,
  igGradient;

procedure Register();
begin
  registerComponents('Graphics32 MiniGlue',[
                                            TigPaintBox,
                                            TigGridBox,
                                            TigLayersListBox,
                                            TigAgent,
                                            TigComboBoxBlendMode,
                                            TigSwatchList,
                                         //   TigSwatchListView,
                                            TigSwatchGrid,
                                            TigGradientList
                                            ]);

  //RegisterComponentEditor(TigSwatchList, TigSwatchListEditor);
 // RegisterComponentEditor(TigGridList, TigCellItemListEditor);
  //RegisterPropertyEditor(TypeInfo(TigCoreCollection), TigSwatchList, 'Collection', TigGridCollectionProperty);

end;

end.
