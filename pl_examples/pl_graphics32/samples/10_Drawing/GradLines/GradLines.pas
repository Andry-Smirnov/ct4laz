program GradLines;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  GradLinesmf;

{$R *.res}

begin
  Application.Title:='GR32 Gradient Lines Example';
  Application.Initialize;
  Application.CreateForm(TFormGradientLines, FormGradientLines);
  Application.Run;
end.
