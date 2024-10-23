unit plListBoxmw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TplListBoxUnit, TplButtonUnit, Forms, Controls,
  Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    plButton1: TplButton;
    plListBox1: TplListBox;
    procedure plButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.plButton1Click(Sender: TObject);
begin
  close;
end;

end.

