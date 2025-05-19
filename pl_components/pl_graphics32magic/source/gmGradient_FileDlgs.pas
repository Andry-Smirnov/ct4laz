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
unit gmGradient_FileDlgs;

interface

uses

  Classes,
{ GraphicsMagic }
  gmGridBased_FileDlg, gmGridBased_List, gmGridBased_ListView;

type
  TOpenGradientDialog = class(TOpenGridBasedDialog)
  protected
    procedure DoShow; override;

    function GetGridBasedListClass: TgmGridBasedListClass; override;
    function GetGridBasedListViewClass: TgmGridBasedListViewClass; override;
    function GetGridLabelCaption: string; override;
  end;

  TSaveGradientDialog = class(TOpenGradientDialog)
  protected
    function IsSaveDialog: Boolean; override;
  end;


implementation

uses
{ GraphicsMagic }
  gmGradient_List, gmGradient_ListView;


{ TOpenGradientDialog }

procedure TOpenGradientDialog.DoShow;
begin
  TgmGradientListView(FGridBasedListView).CellBorderStyle := borContrastGrid;
  inherited;
end;

function TOpenGradientDialog.GetGridBasedListClass: TgmGridBasedListClass;
begin
  Result := TgmGradientList;
end;

function TOpenGradientDialog.GetGridBasedListViewClass: TgmGridBasedListViewClass;
begin
  Result := TgmGradientListView;
end;

function TOpenGradientDialog.GetGridLabelCaption: string; 
begin
  Result := 'Gradients:';
end; 

{ TSaveGradientDialog }

function TSaveGradientDialog.IsSaveDialog: Boolean;
begin
  Result := True;
end;

end.
