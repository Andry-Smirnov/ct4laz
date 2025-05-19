unit DSASig2;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TdlgKeySize = class(TForm)
    Label9: TLabel;
    cbxKeySize: TComboBox;
    Label8: TLabel;
    Label5: TLabel;
    cbxIterations: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgKeySize: TdlgKeySize;

implementation

{$R *.lfm}

procedure TdlgKeySize.FormCreate(Sender: TObject);
begin
  cbxKeySize.ItemIndex := 2;
  cbxIterations.ItemIndex := 2;
end;

end.
