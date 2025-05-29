program cyColorMatrixDemo;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  formcyColorMatrix;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmCyColorMatrix, FrmCyColorMatrix);
  Application.Run;
end.
