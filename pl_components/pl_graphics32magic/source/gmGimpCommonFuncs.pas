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


unit gmGimpCommonFuncs;

interface

  function CLAMP(const n, AMin, AMax: Integer): Integer; overload;
  function CLAMP(const n, AMin, AMax: Double): Double; overload;
  function CLAMP(const n: Double; const AMin, AMax: Integer): Integer; overload;
  function CLAMP0255(const n: Integer): Integer;

const
  G_MAXINT = 2147483647;

implementation

function CLAMP(const n, AMin, AMax: Integer): Integer;
begin
  if n < AMin then
  begin
    Result := AMin;
  end
  else if n > AMax then
  begin
    Result := AMax;
  end
  else
  begin
    Result := n;
  end;
end;

function CLAMP(const n, AMin, AMax: Double): Double;
begin
  if n < AMin then
  begin
    Result := AMin;
  end
  else if n > AMax then
  begin
    Result := AMax;
  end
  else
  begin
    Result := n;
  end;
end;

function CLAMP(const n: Double; const AMin, AMax: Integer): Integer;
var
  rn: Integer;
begin
  rn := Round(n);

  if rn < AMin then
  begin
    Result := AMin;
  end
  else if rn > AMax then
  begin
    Result := AMax;
  end
  else
  begin
    Result := rn;
  end;
end;

function CLAMP0255(const n: Integer): Integer;
begin
  Result := CLAMP(n, 0, 255);
end; 

end.
