program BlendTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the CT adLCL widgetset
  Forms,
  GUITestRunner,
  TestGR32Blend;

{$R *.res}

begin
  Application.Title := 'PNG Test';
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
