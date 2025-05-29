{***************************************************************
  This Application is part of CodeTyphon Studio
  by PiloLogic Software House (https://www.pilotlogic.com/)
****************************************************************} 

program triangle2;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Interfaces, Dialogs, Forms,
  umainform2,vulkanlib;

{$R *.res}

var
  heaptrcFile: String;

begin
  if not vkAPIInitialize then begin
    MessageDlg('Error', 'unable to initialize vulkan api', mtError, [mbOK], 0);
    halt;
  end;

  heaptrcFile := ChangeFileExt(Application.ExeName, '.heaptrc');
  if (FileExists(heaptrcFile)) then
    DeleteFile(heaptrcFile);
  SetHeapTraceOutput(heaptrcFile);

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

