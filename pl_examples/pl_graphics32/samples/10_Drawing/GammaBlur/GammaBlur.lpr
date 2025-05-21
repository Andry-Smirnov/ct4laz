program GammaBlur;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  GammaBlurmw;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormGammaBlur, FormGammaBlur);
  Application.Run;
end.
