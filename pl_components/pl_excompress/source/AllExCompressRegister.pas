{**********************************************************************
                PilotLogic Software House.
  
 Package pl_ExCompress
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

unit AllExCompressRegister;

interface


 uses
  Classes, SysUtils, TypInfo,lresources, PropEdits, ComponentEditors,
  TplZipUnit,
  TplZlibUnit,
  TplLzmaUnit;

procedure Register;

implementation

{$R AllExCompressRegister.res}

procedure Register;
begin

  RegisterComponents ('Extra Compress',[
                                       TplZipCompress,
                                       TplZipUnCompress,
                                       TplZLibCompress,
                                       TplZLibUnCompress,
                                       TplLzmaCompress,
                                       TplLzmaUNCompress
                                       ]);

end;

end.
