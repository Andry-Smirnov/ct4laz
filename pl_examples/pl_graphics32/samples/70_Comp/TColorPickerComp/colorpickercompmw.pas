unit ColorPickerCompmw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, GR32_ColorPicker, GR32_Panel32, Forms, Controls,
  Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    ColorPickerComponent1: TColorPickerComponent;
    GR32Panel1: TGR32Panel;
    procedure ColorPickerComponent1Changed(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ColorPickerComponent1Changed(Sender: TObject);
begin
  GR32Panel1.FillColor:=ColorPickerComponent1.SelectedColor;
end;

end.

