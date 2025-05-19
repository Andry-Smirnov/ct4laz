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
unit gmGradient_Preview;

interface

uses

  SysUtils, Classes, Controls,

  GR32, GR32_Image,
{ GraphicsMagic }
  gmGradient, gmGradient_List, gmGridBased_Preview;

type
  TgmGradientPreview = class(TgmGridBasedPreview)
  private
    function GetGradients: TgmGradientList;
    procedure SetGradients(const AValue: TgmGradientList);
  protected
  public
  published
    property GradientList : TgmGradientList read GetGradients write SetGradients;
    property ItemIndex;
  end;


implementation

{ TgmGradientPreview }

function TgmGradientPreview.GetGradients: TgmGradientList;
begin
  Result := ItemList as TgmGradientList;
end;

procedure TgmGradientPreview.SetGradients(const AValue: TgmGradientList);
begin
  ItemList := AValue;
end;

end.
