unit plScrollbarmw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TplScrollbarUnit, TplCheckBoxUnit, TplButtonUnit,
  Forms, Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    plButton1: TplButton;
    plCheckBox1: TplCheckBox;
    plScrollbar1: TplScrollbar;
    plScrollbar2: TplScrollbar;
    procedure plButton1Click(Sender: TObject);
    procedure plCheckBox1Click(Sender: TObject);
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

procedure TForm1.plCheckBox1Click(Sender: TObject);
begin

  if plCheckBox1.Checked then
   plScrollbar1.Kind:=sbHorizontal else
   plScrollbar1.Kind:=sbVertical;
end;

end.

