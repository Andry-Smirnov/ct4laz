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


unit gmConstants;

interface

uses
  Graphics;

const
  MAX_PIXEL_COUNT                      = 65536;
  RUBBER_BAND_PEN_COLOR  : TColor      = clBlack;
  RUBBER_BAND_PEN_STYLE  : TPenStyle   = psDot;
  RUBBER_BAND_PEN_WIDTH  : Integer     = 1;
  RUBBER_BAND_BRUSH_COLOR: TColor      = clBlack;
  RUBBER_BAND_BRUSH_STYLE: TBrushStyle = bsClear;
  IMAGE_BORDER_PIXEL     : Integer     = 1;
  MAX_OPEN_RECENT_COUNT  : Integer     = 10;  // maximum count of OpenRecent menu items
  HANDLE_RADIUS          : Integer     = 3;
  FIGURE_HANDLE_RADIUS   : Integer     = 2;   // radius of handle for selected figures
  FILL_INSIDE            : Boolean     = True;
  DONOT_FILL_INSIDE      : Boolean     = False;

  THUMBNAIL_SIZE_LARGE = 64;
  THUMBNAIL_SIZE_SMALL = 32;

  MAX_GHOST_FADE_INTERVAL = 50;
  MIN_GHOST_FADE_INTERVAL = 10;
  MAX_GHOST_OPAQUE_UPPER  = 255;
  MAX_GHOST_OPAQUE_LOWER  = 230;

implementation

end.
