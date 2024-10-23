unit CheckListBoxmw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TplCheckListBoxUnit, Forms, Controls, Graphics,
  Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    plCheckListBox1: TplCheckListBox;
    procedure Button1Click(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  close;
end;

end.

