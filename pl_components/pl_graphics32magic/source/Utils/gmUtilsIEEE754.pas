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

unit gmUtilsIEEE754;

interface

uses
  LCLIntf, LCLType, LMessages,
  SysUtils;

{$J-}// Do not allow constants to be changed

const
  NANSignalingBits: int64 = $7FF0000000000001;

var
  // With $J- directive above and Absolute reference below, this
  // value cannot be changed
  NANSignaling: double ABSOLUTE NANSignalingBits;


function NAN: double;                // The "Quiet" Nan

function PositiveInfinity: double;   //  INF
function NegativeInfinity: double;   // -INF

// "Is" functions
function IsNAN(const d: double): boolean;
function IsInfinity(const d: double): boolean;

// Hex String Conversions
function DoubleToHex(const d: double): string;
function HexToDouble(const hex: string): double;

implementation

type
  EIEEEMath = class(Exception);


// With Int64s, the logical order of the floating point values isn't
// obfuscated by the "little endian" physical order
const
  NANQuietBits: int64 = $7FFFFFFFFFFFFFFF;
  PositiveInfinityBits: int64 = $7FF0000000000000;
  NegativeInfinityBits: int64 = $FFF0000000000000;

var
  dNANQuiet: double ABSOLUTE NANQuietBits;
  dPositiveInfinity: double ABSOLUTE PositiveInfinityBits;
  dNegativeInfinity: double ABSOLUTE NegativeInfinityBits;

// Since a NAN is not a single, unique value, a special function is needed
// for this test
function IsNAN(const d: double): boolean;
var
  Overlay: int64 ABSOLUTE d;
begin
  Result := ((Overlay and $7FF0000000000000) = $7FF0000000000000) and ((Overlay and $000FFFFFFFFFFFFF) <> $0000000000000000);
end ;


function IsInfinity(const d: double): boolean;
var
  Overlay: int64 ABSOLUTE d;
begin
  Result := (Overlay and $7FF0000000000000) = $7FF0000000000000;
end ;

function DoubleToHex(const d: double): string;
var
  Overlay: array[1..2] of longint ABSOLUTE d;
begin
  // Look at element 2 before element 1 because of "Little Endian" order.
  Result := IntToHex(Overlay[2], 8) + IntToHex(Overlay[1], 8);
end;


function HexToDouble(const hex: string): double;
var
  d: double;
  Overlay: array[1..2] of longint ABSOLUTE d;
begin
  if LENGTH(hex) <> 16 then
    raise EIEEEMath.Create('Invalid hex string for HexToDouble');

  Overlay[1] := StrToInt('$' + COPY(hex, 9, 8));
  Overlay[2] := StrToInt('$' + Copy(hex, 1, 8));

  Result := d;
end;

// Use functions to make sure values can never be changed.
function NAN: double;
begin
  Result := dNANQuiet;
end ;

function PositiveInfinity: double;
begin
  Result := dPositiveInfinity;
end;

function NegativeInfinity: double;
begin
  Result := dNegativeInfinity;
end ;


end.
