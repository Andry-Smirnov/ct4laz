program Clipper;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Clippermw;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmClipper, FrmClipper);
  Application.Run;
end.
