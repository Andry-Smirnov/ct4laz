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


unit gmGimpBaseEnums;

interface

type
  TgmGimpConvolutionType = (gctNormalConvolve,     // negative numbers truncated
                            gctAbsoluteConvolve,   // absolute value
                            gctNegativeConvolve);  // add 127 to values

const
  GIMP_CURVE_SMOOTH    = 0;  { < desc="Smooth"   > }
  GIMP_CURVE_FREE      = 1;  { < desc="Freehand" > }

  GIMP_HISTOGRAM_VALUE = 0;  { < desc="Value" > }
  GIMP_HISTOGRAM_RED   = 1;  { < desc="Red"   > }
  GIMP_HISTOGRAM_GREEN = 2;  { < desc="Green" > }
  GIMP_HISTOGRAM_BLUE  = 3;  { < desc="Blue"  > }
  GIMP_HISTOGRAM_ALPHA = 4;  { < desc="Alpha" > }
  GIMP_HISTOGRAM_RGB   = 5;  { < desc="RGB", pdb-skip > }

  GRAPH_SIZE = 256;

implementation

end.
