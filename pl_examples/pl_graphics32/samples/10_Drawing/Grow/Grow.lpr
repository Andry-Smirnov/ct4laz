program Grow;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Growmw;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormGrow, FormGrow);
  Application.Run;
end.
