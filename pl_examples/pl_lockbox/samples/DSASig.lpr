program DSASig;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  DSASig1,
  DSASig2;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmDSASig, frmDSASig);
  Application.CreateForm(TdlgKeySize, dlgKeySize);
  Application.Run;
end.
