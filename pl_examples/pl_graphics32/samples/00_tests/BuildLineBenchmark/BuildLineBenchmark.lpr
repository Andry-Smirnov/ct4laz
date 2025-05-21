program BuildLineBenchmark;


{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the CT adLCL widgetset
  Forms,
  BuildLineBenchmarkmw;


{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmBuildLineBenchmark, FrmBuildLineBenchmark);
  Application.Run;
end.

