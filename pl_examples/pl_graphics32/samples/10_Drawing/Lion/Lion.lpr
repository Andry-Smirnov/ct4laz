program Lion;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,

  Lionmf,
  LionData;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmLion, FrmLion);
  Application.Run;
end.

