program ButtonsPanel;

{$MODE Delphi}

uses
  Forms, Interfaces,
  ButtonsPanelmw in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
