program CubicSpline;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  CubicSplinemf;

{$R *.res}

begin
  Application.Title:='Cubic Spline';
  Application.Initialize;
  Application.CreateForm(TFormBezier, FormBezier);
  Application.Run;
end.

