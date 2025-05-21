program GammaCorrection;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  GammaCorrectionmf;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmGammaCorrection, FrmGammaCorrection);
  Application.Run;
end.
