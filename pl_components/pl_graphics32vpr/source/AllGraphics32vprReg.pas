{*************************************************************************
                       PilotLogic Software House

  Package pl_Graphics32VPR
  This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)

 ***** BEGIN LICENSE BLOCK *****

  The contents of this file are subject to the
  Mozilla Public License Version 1.1 (the "License");
  you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the
  License.

 ***** END LICENSE BLOCK ***** *

**************************************************************************}

unit AllGraphics32vprReg;

{$mode objfpc}{$H+}

interface

uses
  Classes,
 {$IFDEF WINDOWS}
   VGR32_Text, VGR32_Objects,
 {$ELSE}

 {$ENDIF}
  SysUtils ;

implementation

end.

