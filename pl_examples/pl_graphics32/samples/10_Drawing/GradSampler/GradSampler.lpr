program GradSampler;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  GradSamplermf;

{$R *.res}

begin
  Application.Title:='Gradient Sampler Example';
  Application.Initialize;
  Application.CreateForm(TFrmGradientSampler, FrmGradientSampler);
  Application.Run;
end.

