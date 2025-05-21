program Image32;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,

  Image32mf;

{$R *.res}

begin
  Application.Title:='TImage32 Example';
  Application.Initialize;
  Application.CreateForm(TFormImage32Example, FormImage32Example);
  Application.Run;
end.
