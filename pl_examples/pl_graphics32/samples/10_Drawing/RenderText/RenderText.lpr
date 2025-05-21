program RenderText;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,

  RenderTextmf;

{$R *.res}

begin
  Application.Title:='GR32 Render Text Example';
  Application.Initialize;
  Application.CreateForm(TFormRenderText, FormRenderText);
  Application.Run;
end.
