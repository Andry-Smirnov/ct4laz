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
unit gmSolidColorLayer;

interface

uses

  GR32,

  gmLayers;

type
  { TgmSolidColorLayer }

  TgmSolidColorLayer = class(TgmNonPixelizedLayer)
  private
    FSolidColor : TColor32;

    procedure DrawLayerLogoInPhotoshopStyle;
    procedure SetSolidColor(const AColor: TColor32);
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    function GetCopy: TgmCustomLayer; override;

    procedure UpdateLogoThumbnail; override;

    property SolidColor: TColor32 read FSolidColor write SetSolidColor;
  end;


implementation

const
  LOGO_BITMAP_WIDTH  = 31;
  LOGO_BITMAP_HEIGHT = 31;

{ TgmSolidColorLayer }

constructor TgmSolidColorLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FDefaultLayerName := 'Color Fill';
  FLogoThumbEnabled := True;
  FSolidColor       := clBlack32;

  FLogoBitmap := TBitmap32.Create();
  FLogoBitmap.SetSize(LOGO_BITMAP_WIDTH, LOGO_BITMAP_HEIGHT);

  FLogoThumb := TBitmap32.Create();
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  UpdateLogoThumbnail();
end;

procedure TgmSolidColorLayer.DrawLayerLogoInPhotoshopStyle;
begin
  // drawing layer logo by hard-coded
  with FLogoBitmap do
  begin
    Clear(clWhite32);
    FillRect(0, 0, 31, 22, FSolidColor);

    PenColor := clBlack32;

    MoveTo(0, 22);
    LineToS(31, 22);

    MoveTo(4, 25);
    LineToS(26, 25);

    MoveTo(17, 26);
    LineToS(15, 28);

    MoveTo(17, 26);
    LineToS(19, 28);

    MoveTo(15, 28);
    LineToS(19, 28);
  end;
end;

function TgmSolidColorLayer.GetCopy: TgmCustomLayer;
var
  LLayer : TgmSolidColorLayer;
begin
  LLayer := TgmSolidColorLayer.Create(Self.FOwner,
    Self.FLayerBitmap.Width, Self.LayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);
  LLayer.SolidColor := Self.FSolidColor;

  Result := LLayer;
end;

procedure TgmSolidColorLayer.LayerBlend(F: TColor32;
  var B: TColor32; M: TColor32);
begin
  FLayerBlendEvent(FSolidColor, B, M);
end;

procedure TgmSolidColorLayer.SetSolidColor(const AColor: TColor32);
begin
  FSolidColor := $FF000000 or AColor;
end;

procedure TgmSolidColorLayer.UpdateLogoThumbnail;
begin
  DrawLayerLogoInPhotoshopStyle();

  inherited;
end; 

end.
