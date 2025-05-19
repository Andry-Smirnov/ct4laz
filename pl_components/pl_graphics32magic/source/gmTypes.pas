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
unit gmTypes;

interface

uses
  LCLIntf, LCLType, LMessages,types, Graphics, gmConstants;

type
{ Enumeration Types}

  TgmEdgeAction = (eaNone = -1,
                   eaZeroEdges,
                   eaClampEdges,
                   eaWrapEdges);

  TgmHueSaturationAdjustMode = (hsamHSL, hsamHSV);

  TgmOutputGraphicsFormat = (ogfNone,
                             ogfBMP,
                             ogfJPG,
                             ogfGIF,
                             ogfPNG,
                             ogfTIF,
                             ogfGMD);

  TgmColorMode         = (cmRGB, cmGrayscale);
  TgmColorSelector     = (csForeColor, csBackColor);
  TgmEditMode          = (emStandardMode, emQuickMaskMode);
  TgmFlipMode          = (fmNone, fmHorizontal, fmVertical);
  TgmThumbnailMode     = (tmSelection, tmImage); // for Color Selection and Replace Color commands
  TgmThumbnailSizeMode = (tsmSmall, tsmLarge);

  TgmRotateDirection   = (rdClockwise, rdCounterclockwise);
  TgmAppliedUnit       = (auInch, auCentimeter, auPoint, auPixel);
  TgmResolutionUnit    = (ruPixelsPerInch, ruPixelsPerCM);

  // channel types
  TgmChannelSelector   = (csRed, csGreen, csBlue, csGrayscale, csAlpha);
  TgmChannelSet        = set of TgmChannelSelector;

  TgmWorkingChannelType = (wctRGB,
                           wctRed,
                           wctGreen,
                           wctBlue,
                           wctAlpha,
                           wctQuickMask,
                           wctLayerMask);

  // the working state for selections, figures or shapes
  TgmDrawingState = (dsNotDrawing,
                     dsNewFigure,
                     dsStretchCorner,
                     dsTranslate);

  // type of end point "handles" for border of selected selection and figures
  TgmDrawingHandle = (dhNone,
                      dhAxAy,
                      dhBxBy,
                      dhAxBy,
                      dhBxAy,
                      dhLeftHalfAyBy,
                      dhRightHalfAyBy,
                      dhTopHalfAxBx,
                      dhBottomHalfAxBx,
                      dhLineStart,
                      dhLineEnd,
                      dhCurvePoint1,
                      dhCurvePoint2,
                      dhPolygonPoint);

  // used for Canvas Size command
  TgmAnchorDirection = (adTopLeft,
                        adTop,
                        adTopRight,
                        adLeft,
                        adCenter,
                        adRight,
                        adBottomLeft,
                        adBottom,
                        adBottomRight);
  
{ Pointer Types }

  // This is a function type for pointer to a routine to do something
  // when color mode is changed. }
  TgmColorModeChangedFunc = procedure (const AColorMode: TgmColorMode) of object;

  TgmPointCoordConvertFunc = function (const APoint: TPoint): TPoint of object;

  TgmSetEditModeProc = procedure (const AValue: TgmEditMode) of object;

  // Use SysUtils.pByteArray for 8-bit color
  pRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [0 .. MAX_PIXEL_COUNT - 1] of TRGBTriple;

  PColorArray = ^TColorArray;
  TColorArray = array of TColor;

  PArrayOfColor = ^TArrayOfColor;
  TArrayOfColor = array of TColor;

implementation

end.
