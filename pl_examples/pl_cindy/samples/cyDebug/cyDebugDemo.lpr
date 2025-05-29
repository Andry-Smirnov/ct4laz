program cyDebugDemo;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  formcyDebug;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmCyDebug, FrmCyDebug);
  Application.Run;
end.
