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
unit gmGradient_List;

interface

uses

  Classes, Graphics,
{ GraphicsMagic }
  gmGradient, gmGridBased, gmGridBased_List;


type
  TgmGradientIndex = type TgmGridBasedIndex; //used for display gradient in property editor

  TgmGradientList = class(TgmGridBasedList)
  private
    function GetItem(AIndex: TgmGradientIndex): TgmGradientItem;
    function GetGradients: TgmGradientCollection;
    function GetForegroundColor: TColor;
    function GetBackgroundColor: TColor;

    procedure SetItem(AIndex: TgmGradientIndex; const AValue: TgmGradientItem);
    procedure SetGradients(const AValue: TgmGradientCollection);
    procedure SetForegroundColor(const AValue: TColor);
    procedure SetBackgroundColor(const AValue: TColor);
  protected
    function CollectionClass: TgmGridBasedCollectionClass; override;
  public
    property Items[Index: TgmGradientIndex]: TgmGradientItem read GetItem write SetItem; default;
  published
    property Gradients : TgmGradientCollection read GetGradients write SetGradients;

    property ForegroundColor : TColor read GetForegroundColor write SetForegroundColor;
    property BackgroundColor : TColor read GetBackgroundColor write SetBackgroundColor;
  end;

  IgmGradientListSupport = interface
    ['{ACB97BAF-7F3F-425D-AB8C-90B197C31DB0}']
    function GetGradients: TgmGradientList;
  end;

implementation

{ TgmGradientList }

function TgmGradientList.CollectionClass: TgmGridBasedCollectionClass;
begin
  Result := TgmGradientCollection;
end;

function TgmGradientList.GetBackgroundColor: TColor;
begin
  Result := Gradients.BackgroundColor;
end;

function TgmGradientList.GetForegroundColor: TColor;
begin
  Result := Gradients.ForegroundColor;
end;

function TgmGradientList.GetGradients: TgmGradientCollection;
begin
  Result := TgmGradientCollection(Self.Collection);
end;

function TgmGradientList.GetItem(AIndex: TgmGradientIndex): TgmGradientItem;
begin
  Result := TgmGradientItem(Collection.Items[AIndex])
end;

procedure TgmGradientList.SetBackgroundColor(const AValue: TColor);
begin
  Gradients.BackgroundColor := AValue;
  Self.Change;
end;

procedure TgmGradientList.SetForegroundColor(const AValue: TColor);
begin
  Gradients.ForegroundColor := AValue;
  Self.Change;
end;

procedure TgmGradientList.SetGradients(const AValue: TgmGradientCollection);
begin
  Collection.Assign(AValue);
end;

procedure TgmGradientList.SetItem(AIndex: TgmGradientIndex;
  const AValue: TgmGradientItem);
begin
  Collection.Items[AIndex].Assign(AValue);
end;

end.
