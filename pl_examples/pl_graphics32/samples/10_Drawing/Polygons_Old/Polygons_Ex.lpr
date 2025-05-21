program Polygons_Ex;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,

  Polygonsmf ;

{$R *.res}

begin
  Application.Title:='GR32 Polygons Example';
  Application.Initialize;
  Application.CreateForm(TFormPolygons, FormPolygons);
  Application.Run;
end.
