program ScatterPlot;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  ScatterPlotmf;

{$R *.res}

begin
  Application.Title:='Scatter Plot';
  Application.Initialize;
  Application.CreateForm(TFmScatterPlot, FmScatterPlot);
  Application.Run;
end.

