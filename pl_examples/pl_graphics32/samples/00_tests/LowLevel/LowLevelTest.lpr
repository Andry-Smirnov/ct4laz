program LowLevelTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the CT adLCL widgetset
  Forms,
  GUITestRunner,
  TestLowLevel;


{$R *.res}

begin
  Application.Title := 'PNG Test';
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
