program WebSocketServer;

{$mode DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, WebSocketservermw, LResources
  { you can add units after this };

{$R *.res}

begin
 // {$I ServerDemo.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

