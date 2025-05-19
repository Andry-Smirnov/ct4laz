
{**********************************************************************
 Package pl_Graphics32VPR
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

unit VGR32_Pictures;

(* BEGIN LICENSE BLOCK *********************************************************
 * Version: MPL 1.1
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
 * The Original Code is GR32_Misc2.
 * The Initial Developer of the Original Code is Angus Johnson and is
 * Copyright (C) 2009-2010 the Initial Developer. All rights reserved
 *
 * Version 3.92 (Last updated 10-Nov-2010)
 *
 * END LICENSE BLOCK **********************************************************)


interface

uses
  LCLIntf, LCLType, LMessages,
  Types, SysUtils, Classes, Graphics, FileUtil,lazfileutils,
  GR32;

{$IFNDEF UNICODE}
type
  UnicodeString = WideString;

{$ENDIF}

function LoadPicFromStream(stream: TStream; pic: TBitmap32): boolean;
function LoadPicFromFile(const picFile: UnicodeString; pic: TBitmap32): boolean;

function SavePicToStream(stream: TStream; pic: TBitmap32; format: string = ''): boolean;
//format: 'bmp','jpg','png','gif' etc
function SavePicToFile(const picFile: UnicodeString; pic: TBitmap32): boolean;

implementation

uses Math;

type
  TImageHeaderType = (htUnknown, htBmpFile, htBmpCore, htBmpInfo,
    htPng, htJpg, htGif, htTif, htEmf, htWmf, htIco);

function GetImageHeaderType(stream: TStream): TImageHeaderType;
var
  size: integer;
  buff: DWord;
begin
  Result := htUnknown;
  size := stream.size - stream.Position;
  if size < 4 then
    exit;
  stream.Read(buff, sizeof(DWORD));
  stream.Seek(-sizeof(DWORD), soFromCurrent);
  if Buff = $474E5089 then
    Result := htPng
  else if Buff = $38464947 then
    Result := htGif
  else if LoWord(Buff) = $D8FF then
    Result := htJpg
  else if LoWord(Buff) = $4D42 then
    Result := htBmpFile
  else if (LoWord(Buff) = $4949) or (LoWord(Buff) = $4D4D) then
    Result := htTif
  else if Buff = $1 then
    Result := htEmf
  else if (Buff = $00090001) or (Buff = $9AC6CDD7) then
    Result := htWmf
  else if (Buff = $00010000) then
    Result := htIco
  else if Buff = sizeof(TBitmapInfoHeader) then
    Result := htBmpInfo
  else if Buff = sizeof(TBitmapCoreHeader) then
    Result := htBmpCore;
end;
//------------------------------------------------------------------------------

function LoadPicFromStreamGdi(stream: TStream; pic: TBitmap32): boolean;
var
  B: Graphics.TBitmap;
begin
  B := Graphics.TBitmap.Create;
  try
    try
      B.LoadFromStream(stream);
      pic.Assign(B);
    finally
      B.Free;
    end;
  except
    pic.Delete;
  end;
  Result := not pic.Empty;
end;
//------------------------------------------------------------------------------

function LoadPicFromFileGdi(const picFile: UnicodeString; pic: TBitmap32): boolean;
var
  B: Graphics.TBitmap;
begin
  Result := False;
  if not assigned(pic) or not FileExistsUTF8(picFile) { *Converted from FileExists*  } then
    exit;
  try
    B := Graphics.TBitmap.Create;
    try
      B.LoadFromFile(picFile);
      pic.Assign(B);
    finally
      B.Free;
    end;
  except
    pic.Delete;
  end;
end;
//------------------------------------------------------------------------------

function SavePicToFileGdi(const picFile: UnicodeString; pic: TBitmap32): boolean;
var
  ext: string;
begin
  ext := ExtractFileExt(picFile);
  ext := lowercase(ext);
  Result := ext = '.bmp';
  if Result then
    pic.SaveToFile(picFile);
end;
//------------------------------------------------------------------------------

function GetDInColors(BitCount: word): integer;
begin
  case BitCount of
    1, 4, 8: Result := 1 shl BitCount;
    else
      Result := 0;
  end;
end;
//------------------------------------------------------------------------------

function LoadPicFromStream(stream: TStream; pic: TBitmap32): boolean;
var
  size, ClrUsed: integer;
  ImageHeaderType: TImageHeaderType;
  BH: PBitmapFileHeader;
  BI: PBitmapInfoHeader;
  BC: PBitmapCoreHeader;
  ms: TMemoryStream;
begin
  Result := False;
  if not assigned(pic) or not assigned(stream) then
    exit;

  ImageHeaderType := GetImageHeaderType(stream);
  if ImageHeaderType in [htBmpInfo, htBmpCore] then
  begin
    //this stream is in BITMAP format but is missing its file header
    //(ie typically a resource stream). So, we need to fix that ...
    ms := TMemoryStream.Create;
    try
      try
        size := stream.Size - stream.Position;
        ms.SetSize(sizeof(TBitmapFileHeader) + size);
        FillChar(ms.memory^, sizeof(TBitmapFileHeader), #0);
        ms.Seek(sizeof(TBitmapFileHeader), soFromBeginning);
        ms.CopyFrom(stream, size);
        ms.Position := 0;
        BH := PBitmapFileHeader(ms.memory);
        BH^.bfType := $4D42;
        BH^.bfSize := ms.Size;
        if ImageHeaderType = htBmpInfo then
        begin
          BI := PBitmapInfoHeader(PAnsiChar(BH) + sizeof(TBitmapFileHeader));
          //this next line should not be necessary but the occasional image has
          //omitted this size which is required whenever the image is compressed.
          if BI^.biSizeImage = 0 then
            BI^.biSizeImage := size;
          ClrUsed := BI^.biClrUsed;
          if ClrUsed = 0 then
            ClrUsed := GetDInColors(BI^.biBitCount);
          BH^.bfOffBits := ClrUsed * SizeOf(TRgbQuad) + sizeof(TBitmapInfoHeader) + sizeof(TBitmapFileHeader);
        end
        else if ImageHeaderType = htBmpCore then
        begin
          BC := PBitmapCoreHeader(PAnsiChar(BH) + sizeof(TBitmapFileHeader));
          ClrUsed := GetDInColors(BC^.bcBitCount);
          BH^.bfOffBits := ClrUsed * SizeOf(TRGBTriple) + sizeof(TBitmapCoreHeader) + sizeof(TBitmapFileHeader);
        end;
        Result := LoadPicFromStreamGdi(ms, pic);
      finally
        ms.Free;
      end;
    except
      pic.Delete;
    end;
    exit;
  end;


  Result := LoadPicFromStreamGdi(stream, pic);

end;
//------------------------------------------------------------------------------

function LoadPicFromFile(const picFile: UnicodeString; pic: TBitmap32): boolean;
begin

  Result := (LowerCase(ExtractFileExt(picFile)) = '.bmp') and LoadPicFromFileGdi(picFile, pic);

end;
//------------------------------------------------------------------------------

function SavePicToStream(stream: TStream; pic: TBitmap32; format: string = ''): boolean;
begin
  format := lowercase(format);
  if (format <> '') and (format[1] = '.') then
    Delete(format, 1, 1);

  Result := assigned(pic) and assigned(stream) and ((format = 'bmp') or (format = ''));
  if Result then
    pic.SaveToStream(stream);

end;
//------------------------------------------------------------------------------

function SavePicToFile(const picFile: UnicodeString; pic: TBitmap32): boolean;
begin

  Result := SavePicToFileGdi(picFile, pic);

end;
//------------------------------------------------------------------------------

end.
