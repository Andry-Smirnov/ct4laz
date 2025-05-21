program ArrowHead;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  ArrowHeadmf;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmArrowHead, FmArrowHead);
  Application.Run;
end.
