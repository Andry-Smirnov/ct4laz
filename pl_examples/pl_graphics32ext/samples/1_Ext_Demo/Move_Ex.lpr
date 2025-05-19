program Move_Ex;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Move_Exmw ;

{$R *.res}

begin
  Application.Title:='Rotate Example';
  Application.Initialize;
  Application.CreateForm(TFormRotateExample, FormRotateExample);
  Application.Run;
end.

