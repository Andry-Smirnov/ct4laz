{*************************************************************************
                       PilotLogic Software House

  Package pl_Graphics32MG
  This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)     
**************************************************************************}

unit igBrightContrastLayer;

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
 * Update Date: 25th, Mar, 2014
 *
 * The Initial Developer of this unit are
 *   Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
{ Graphics32 }
  GR32,
{ miniGlue lib }
  igLayers;

type
  { TigBrightContrastLayerPanel }

  TigBrightContrastLayerPanel = class(TigCustomLayerPanel)
  private
    FBrightAmount   : Integer;
    FContrastAmount : Integer;

    procedure SetBrightAmount(AValue: Integer);
    procedure SetContrastAmount(AValue: Integer);
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TigLayerPanelList); override;
    //constructor Create(AOwner: TigLayerPanelList;  const ALayerWidth, ALayerHeight: Integer);

    property BrightAmount   : Integer read FBrightAmount   write SetBrightAmount;
    property ContrastAmount : Integer read FContrastAmount write SetContrastAmount;
  end;

  function TigBrightContrastLayerPanel_Create(APanelList : TigLayerPanelList;
    AWidth,AHeight: Integer): TigBrightContrastLayerPanel;

implementation

uses
{ Delphi }
  Graphics,
{ Graphics32 }
  GR32_LowLevel;

{$R igBrightContrastLayer.res}


function TigBrightContrastLayerPanel_Create(APanelList : TigLayerPanelList;
  AWidth,AHeight: Integer): TigBrightContrastLayerPanel;
begin
  Result := TigBrightContrastLayerPanel.Create(nil);
  Result.LayerBitmap.SetSize(AWidth,AHeight);
 // Result.Collection := APanelList;   //=== ct9999 ?????????????????
end;    


const
  MIN_CHANGE_AMOUNT = -100;
  MAX_CHANGE_AMOUNT =  100;


function ColorBrightness(const AColor: TColor32;
  const AAmount: Integer): TColor32;
var
  r, g, b : Byte;
begin
  r      := AColor shr 16 and $FF;
  g      := AColor shr  8 and $FF;
  b      := AColor        and $FF;
  r      := Clamp(r + AAmount, 0, 255);
  g      := Clamp(g + AAmount, 0, 255);
  b      := Clamp(b + AAmount, 0, 255);
  Result := $FF000000 or (r shl 16) or (g shl 8) or b;
end;

function ColorContrast(const AColor: TColor32;
  const AAmount: Integer): TColor32;
var
  ir, ig, ib : Integer;
  r, g, b    : Byte;
begin
  r  := AColor shr 16 and $FF;
  g  := AColor shr  8 and $FF;
  b  := AColor        and $FF;

  ir := ( Abs(127 - r) * AAmount ) div 255;
  ig := ( Abs(127 - g) * AAmount ) div 255;
  ib := ( Abs(127 - b) * AAmount ) div 255;

  if r > 127 then
  begin
    r := Clamp(r + ir, 0, 255);
  end
  else
  begin
    r := Clamp(r - ir, 0, 255);
  end;

  if g > 127 then
  begin
    g := Clamp(g + ig, 0, 255);
  end
  else
  begin
    g := Clamp(g - ig, 0, 255);
  end;

  if b > 127 then
  begin
    b := Clamp(b + ib, 0, 255);
  end
  else
  begin
    b := Clamp(b - ib, 0, 255);
  end;

  Result := $FF000000 or (r shl 16) or (g shl 8) or b;
end;


{ TigBrightContrastLayerPanel }
constructor TigBrightContrastLayerPanel.Create(AOwner: TigLayerPanelList);
begin
  inherited;
  FPixelFeature     := lpfNonPixelized;
  FDefaultLayerName := 'Brightness/Contrast';
  FBrightAmount     := 0;
  FContrastAmount   := 0;
  FLogoThumbEnabled := True;

  FLogoBitmap := TBitmap32.Create;
  FLogoBitmap.LoadFromResourceName(HInstance, 'MGBCLAYERICON');

  FLogoThumb := TBitmap32.Create;
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  UpdateLogoThumbnail;
end;

{
// we have problem to deal with polymorphism when reconstruct with this:
constructor TigBrightContrastLayerPanel.Create(AOwner: TigLayerPanelList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FPixelFeature     := lpfNonPixelized;
  FDefaultLayerName := 'Brightness/Contrast';
  FBrightAmount     := 0;
  FContrastAmount   := 0;
  FLogoThumbEnabled := True;

  FLogoBitmap := TBitmap32.Create;
  FLogoBitmap.LoadFromResourceName(HInstance, 'BCLAYERICON');

  FLogoThumb := TBitmap32.Create;
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  UpdateLogoThumbnail;
end;}

procedure TigBrightContrastLayerPanel.SetBrightAmount(AValue: Integer);
begin
  FBrightAmount := Clamp(AValue, MIN_CHANGE_AMOUNT, MAX_CHANGE_AMOUNT);
end;

procedure TigBrightContrastLayerPanel.SetContrastAmount(AValue: Integer);
begin
  FContrastAmount := Clamp(AValue, MIN_CHANGE_AMOUNT, MAX_CHANGE_AMOUNT);
end;

procedure TigBrightContrastLayerPanel.LayerBlend(F: TColor32;
  var B: TColor32; M: TColor32);
var
  LForeColor : TColor32;
  LAlpha     : Byte;
begin
  LAlpha := B shr 24 and $FF;

  // only process when the background pixel is not transparent
  if LAlpha > 0 then
  begin
    if (FBrightAmount <> 0) or (FContrastAmount <> 0) then
    begin
      LForeColor := B;
      
      if FBrightAmount <> 0 then
      begin
        // adjust brightness first
        LForeColor := ColorBrightness(LForeColor, FBrightAmount);
      end;

      if FContrastAmount <> 0 then
      begin
        // adjust contrast after the brightness ajustment
        LForeColor := ColorContrast(LForeColor, FContrastAmount);
      end;

      // get new modulated color
      LForeColor := (B and $FF000000) or (LForeColor and $FFFFFF);

      // blending
      FLayerBlendEvent(LForeColor, B, M);
    end;
  end;
end;




end.
