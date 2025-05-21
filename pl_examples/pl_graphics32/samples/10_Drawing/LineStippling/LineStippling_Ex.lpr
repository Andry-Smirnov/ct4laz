program LineStippling_Ex;

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  SysUtils,
  LineStipplingmf;

{$R *.res}

begin
  Application.Title:='GR32 Line Stippling Example';
  Application.Initialize;
  Application.CreateForm(TFormLineStippling, FormLineStippling);
  Application.Run;
end.
