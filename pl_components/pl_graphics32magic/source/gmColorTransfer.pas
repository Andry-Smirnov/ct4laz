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

unit gmColorTransfer;

interface

  // Color transfer pocedure
  procedure ColorTransferInit;

var
  // for lightening
  HighlightsAdd: array [0..255] of Double;
  MidtonesAdd  : array [0..255] of Double;
  ShadowsAdd   : array [0..255] of Double;

  // for darkening
  HighlightsSub: array [0..255] of Double;
  MidtonesSub  : array [0..255] of Double;
  ShadowsSub   : array [0..255] of Double;

implementation

// Color transfer pocedure
procedure ColorTransferInit;
var
  i: Integer;
begin
  for i := 0 to 255 do
  begin
    ShadowsSub[255 - i] := (  1.075 - 1 / ( i / 16.0 + 1 )  );
    HighlightsAdd[i]    := ShadowsSub[255 - i];

    MidtonesSub[i]      := 0.667 * (  1 - Sqr( (i - 127.0) / 127.0 )  );
    MidtonesAdd[i]      := MidtonesSub[i];

    HighlightsSub[i]    := 0.667 * (  1 - Sqr( (i - 127.0) / 127.0 )  );
    ShadowsAdd[i]       := HighlightsSub[i];
  end;
end; 

initialization
  // initialize the global varieties
  ColorTransferInit;

end.
