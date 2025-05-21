program Mandelbrot;

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  Forms,
  Mandelbrotmf;

{$R *.res}

begin
  Application.Title:='Mandelbrot Example';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
