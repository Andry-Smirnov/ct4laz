unit Schema2;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TSchema2Form = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
  private
    { Dιclarations privιes }
  public
    { Dιclarations publiques }
  end;

var
  Schema2Form: TSchema2Form;

implementation

{$R *.lfm}

procedure TSchema2Form.Button1Click(Sender: TObject);
begin
  Close;
end;

end.

