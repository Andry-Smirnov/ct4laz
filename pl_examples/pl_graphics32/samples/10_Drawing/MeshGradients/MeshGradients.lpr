program MeshGradients;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MeshGradientsmf;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMeshGradients, FrmMeshGradients);
  Application.Run;
end.

