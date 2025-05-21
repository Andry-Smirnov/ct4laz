program LineSimplification;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,

  LineSimplificationmf ;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmLineSimplification, FrmLineSimplification);
  Application.Run;
end.

