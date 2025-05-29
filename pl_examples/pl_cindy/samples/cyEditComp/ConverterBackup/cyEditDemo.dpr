program cyEditDemo;

uses
  Forms,
  formcyEdit in 'formcyEdit.pas' {FrmCyEdit};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmCyEdit, FrmCyEdit);
  Application.Run;
end.
