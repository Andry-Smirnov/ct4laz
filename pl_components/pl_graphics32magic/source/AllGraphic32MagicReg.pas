{*************************************************************************
                       PilotLogic Software House

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
    
**************************************************************************}

unit AllGraphic32MagicReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  gmGradient_List,
  gmGradientsGrid,
  gmGradient_FileDlgs,
  gmGradientEditor,
  gmGradient_Preview,
  gmGradient_ListView;


procedure Register;

implementation

{$R AllGraphic32MagicReg.res}

procedure Register;
begin
  RegisterComponents('Graphics32 Magic', [TgmGradientList,
                                          TgmGradientsGrid,
                                          TOpenGradientDialog,
                                          TSaveGradientDialog,
                                          TgmGradientEditor,
                                          TgmGradientPreview,
                                          TgmGradientListView]);
end;

end.

