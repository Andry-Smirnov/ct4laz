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


unit gmCommonDataModule;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, GR32_Image;

type
  TGMDataModule = class(TDataModule)
    bmp32lstInternalPatterns: TBitmap32List;
    bmp32lstBrushStrokes: TBitmap32List;
    bmp32lstLayers: TBitmap32List;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GMDataModule: TGMDataModule;

const

//-- Internal Patterns ---------------------------------------------------------

  INTERNAL_PATTERN_COUNT = 12;

  INTERNAL_PATTERN_NAME: array [0..11] of string =
    ('Bubbles',
     'Wrinkles',
     'Woven',
     'Wood',
     'Tie Dye',
     'Satin',
     'Optical Checkerboard',
     'Nebula',
     'Molecular',
     'Metal Landscape',
     'Herringbone 2',
     'Clouds');

//-- Internal Brush Strokes ----------------------------------------------------

  INTERNAL_STROKE_COUNT = 36;

  INTERNAL_STROKE_NAME: array [0..35] of String =
    ('HardRound1Pixels',
     'HardRound3Pixels',
     'HardRound5Pixels',
     'HardRound9Pixels',
     'HardRound13Pixels',
     'HardRound19Pixels',
     'SoftRound5Pixels',
     'SoftRound9Pixels',
     'SoftRound13Pixels',
     'SoftRound17Pixels',
     'SoftRound21Pixels',
     'SoftRound27Pixels',
     'SoftRound35Pixels',
     'SoftRound45Pixels',
     'SoftRound65Pixels',
     'SoftRound100Pixels',
     'SoftRound200Pixels',
     'SoftRound300Pixels',
     'Spatter12Pixels',
     'Spatter25Pixels',
     'Spatter28Pixels',
     'Spatter39Pixels',
     'Spatter46Pixels',
     'Spatter59Pixels',
     'Chalk10Pixels',
     'Chalk17Pixels',
     'Chalk23Pixels',
     'Chalk36Pixels',
     'Chalk44Pixels',
     'Chalk60Pixels',
     'Star15Pixels',
     'Star26Pixels',
     'Star33Pixels',
     'Star42Pixels',
     'Star55Pixels',
     'Star70Pixels');

//-- For Layer and Channels ----------------------------------------------------

  COLOR_BALANCE_LAYER_ICON_INDEX   = 2;
  BRIGHT_CONTRAST_LAYER_ICON_INDEX = 3;
  HLS_LAYER_ICON_INDEX             = 4;
  INVERT_LAYER_ICON_INDEX          = 5;
  THRESHOLD_LAYER_ICON_INDEX       = 6;
  POSTERIZE_LAYER_ICON_INDEX       = 7;
  CURVES_LAYER_ICON_INDEX          = 8;
  GRADIENT_MAP_LAYER_ICON_INDEX    = 9;
  TEXT_LAYER_INCON_INDEX           = 10;
  LEVELS_LAYER_ICON_INDEX          = 12;
  LAYER_MASK_CHAIN_BMP_INDEX       = 13;
  CHANNEL_MIXER_LAYER_ICON_INDEX   = 14;

implementation

{$R *.lfm}

end.
