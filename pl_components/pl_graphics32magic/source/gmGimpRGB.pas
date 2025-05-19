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


unit gmGimpRGB;

interface

uses
  gmGimpColorTypes;

  {  RGB functions  }

  procedure gimp_rgb_set(var rgb: TGimpRGB; const Red, Green, Blue: Double);

  {  Map RGB to intensity  }

  function Gimp_RGB_Intensity(const r, g, b: Byte): Double;

const
  GIMP_RGB_INTENSITY_RED   = 0.30;
  GIMP_RGB_INTENSITY_GREEN = 0.59;
  GIMP_RGB_INTENSITY_BLUE  = 0.11;

implementation

{  RGB functions  }

{ gimp_rgb_set:
  a_rgb: a TGimpRGB record type
  a_red:
  a_green:
  a_blue:

  Sets the red, green and blue components of @rgb and leaves the
  alpha component unchanged. The color values should be between 0.0
  and 1.0 but there is no check to enforce this and the values are
  set exactly as they are passed in. }
procedure gimp_rgb_set(var rgb: TGimpRGB; const Red, Green, Blue: Double);
begin
  with rgb do
  begin
    r := Red;
    g := Green;
    b := Blue;
  end;
end;

{  Map RGB to intensity  }
function Gimp_RGB_Intensity(const r, g, b: Byte): Double;
begin
  Result := r * GIMP_RGB_INTENSITY_RED   +
            g * GIMP_RGB_INTENSITY_GREEN +
	          b * GIMP_RGB_INTENSITY_BLUE;
end; 

end.
 
