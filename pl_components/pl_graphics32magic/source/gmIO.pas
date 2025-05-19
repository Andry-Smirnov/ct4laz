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
unit gmIO;

interface


uses
  SysUtils,         // FileExists (Last in Use so the idfef system works
  Graphics,         // TBitmap
  Classes,
  GR32, GR32_OrdinalMaps,
  gmTypes;


function  LoadGraphicsFile(const AFileName: string): TBitmap32;
procedure SaveGraphicsFile(const AFileName: string; ABitmap: TBitmap32);


var
  GMIOErrorMsgOutput: string;

implementation

//-- Based on GraphicsConversionsLibrary.pas by written by efg2.com ------------

// Create TBitmap32 from BMP, JPG, WMF, EMF or GIF disk file.
// Could be easily extended to other image types.
function LoadGraphicsFile(const AFileName: string): TBitmap32;
begin
  Result := nil;  // In case anything goes wrong

  if FileExists(AFileName) then
  begin
    Result:=TBitmap32.Create;
    Result.LoadFromFile(AFileName);
  end;

end;

procedure SaveGraphicsFile(const AFileName: string; ABitmap: TBitmap32);
begin
  if ABitmap=nil then exit;

  if AFilename <> '' then
  begin
    ABitmap.SaveToFile(AFileName);
  end;
end;

//------------------------------------------------------------------------------

procedure InitGlobalVariables;
begin
  GMIOErrorMsgOutput := '';
end; 

initialization

   InitGlobalVariables();

end.
