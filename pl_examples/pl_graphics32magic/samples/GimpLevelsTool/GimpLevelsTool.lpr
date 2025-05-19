program GimpLevelsTool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  GimpLevelsToolmw,
  GimpLevelsToolDlg;

{.$R *.res}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmGimpLevelsTool, frmGimpLevelsTool);
  Application.Run;
end.
