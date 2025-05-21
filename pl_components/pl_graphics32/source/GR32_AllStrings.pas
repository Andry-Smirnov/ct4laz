{*************************************************************************
                PilotLogic Software House

  Package pl_Graphics32
  This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)

 ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * https://www.mozilla.org/en-US/MPL/1.1/
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
 * ***** END LICENSE BLOCK *****
 ************************************************************************}

unit GR32_AllStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


Resourcestring

  RCStrUnmatchedReferenceCounting = 'Unmatched reference counting.';
  RCStrCannotSetSize = 'Can''t set size from ''%s''';
  RCStrInpropriateBackend = 'Inappropriate Backend';
  RCStrInsufficientPointsInArray = 'Insufficient points in array';
  RCStrCannotAllocateDIBHandle = 'Can''t allocate the DIB handle';
  RCStrCannotCreateCompatibleDC = 'Can''t create compatible DC';
  RCStrCannotAllocateMemory = 'Can''t allocate memory for the DIB';
  RCStrCannotAllocateThePixBuf = 'Can''t allocate the Pixbuf';
  RCStrCannotSelectAnObjectIntoDC = 'Can''t select an object into DC';
  RCStrFailedToMapFile = 'Failed to map file';
  RCStrFailedToCreateMapFile = 'Failed to create map file (%s)';
  RCStrFailedToMapViewOfFile = 'Failed to map view of file.';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrWrongFormat = 'Wrong format';
  RCStrOnlyExactly3Point = 'Only exactly 3 points expected!';
  RCStrPointCountMismatch = 'Point count mismatch';
  RCStrNoTColor32LookupTable = 'No TColor32LookupTable object specified';
  RCStrNoTColor32Gradient = 'No TColor32Gradient specified';
  RCStrNoLookupTablePassed = 'No lookup table passed!';
  RCStrInvalidStageIndex = 'Invalid stage index';
  RCStrNoSamplerSpecified = 'No sampler specified!';
  RCStrInvalidSrcRect = 'Invalid SrcRect';
  RCStrReverseTransformationNotImplemented = 'Reverse transformation is not implemented in %s.';
  RCStrForwardTransformationNotImplemented = 'Forward transformation is not implemented in %s.';
  RCStrTopBottomCurveNil = 'Top or bottom curve is nil';
  RCStrSrcRectIsEmpty = 'SrcRect is empty!';
  RCStrMappingRectIsEmpty = 'MappingRect is empty!';
  RStrStackEmpty = 'Stack empty';
  RCStrCantAllocateVectorMap = 'Can''t allocate VectorMap!';
  RCStrBadFormat = 'Bad format - Photoshop .msh expected!';
  RCStrFileNotFound = 'File not found!';
  RCStrSrcIsEmpty = 'Src is empty!';
  RCStrBaseIsEmpty = 'Base is empty!';
  SDstNil = 'Destination bitmap is nil';
  SSrcNil = 'Source bitmap is nil';
  SSrcInvalid = 'Source rectangle is invalid';
  SSamplerNil = 'Nested sampler is nil';     
  rsClipper_OpenPathErr = 'Only subject paths can be open.';
  rsClipper_ClippingErr = 'Undefined clipping error';

implementation

end.

