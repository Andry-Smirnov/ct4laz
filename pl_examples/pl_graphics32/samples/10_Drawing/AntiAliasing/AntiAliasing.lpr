program AntiAliasing;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  AntiAliasingmf;

{$R *.res}

begin
  Application.Title:='Anti-Aliasing Test';
  Application.Initialize;
  Application.CreateForm(TFrmAntiAliasingTest, FrmAntiAliasingTest);
  Application.Run;
end.
