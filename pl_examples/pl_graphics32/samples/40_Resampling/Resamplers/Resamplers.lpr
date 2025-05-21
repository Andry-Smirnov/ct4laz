program Resamplers;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,

  Resamplersmf;

{$R *.res}

begin
  Application.Title:='Resamplers Example';
  Application.Initialize;
  Application.CreateForm(TFrmResamplersExample, FrmResamplersExample);
  Application.Run;
end.
