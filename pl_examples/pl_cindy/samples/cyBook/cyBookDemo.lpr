program cyBookDemo;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  formcyBook;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmCyBook, FrmCyBook);
  Application.Run;
end.
