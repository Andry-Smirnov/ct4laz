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
unit gmGradient_ListView;

interface

uses

  Classes,
{ GraphicsMagic }
  gmGridBased_ListView, gmGradient_List;

type
  TgmGradientListView = class(TgmGridBasedListView)
  private
    function GetGradientList: TgmGradientList;
    procedure SetGradientList(const AValue: TgmGradientList);
  protected

  public

  published
    property Align;
    property AutoSize;
    property Anchors;
    property Color;
    property ParentColor;

    property GrowFlow;
    property GradientList : TgmGradientList read GetGradientList write SetGradientList;
    property ThumbWidth;
    property ThumbHeight;
    property ParentShowHint;
    property PopupMenu;
    property RepaintMode;
    property Scale;
    property ScaleMode;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property GridOptions;
    property CellBorderStyle;
    property FrameColor;
    property SelectedColor;
    property ItemIndex;

    property OnChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;    
  end;


implementation


{ TgmGradientListView }

function TgmGradientListView.GetGradientList: TgmGradientList;
begin
  Result := ItemList as TgmGradientList;
end;

procedure TgmGradientListView.SetGradientList(const AValue: TgmGradientList);
begin
  ItemList := AValue;
end;

end.
