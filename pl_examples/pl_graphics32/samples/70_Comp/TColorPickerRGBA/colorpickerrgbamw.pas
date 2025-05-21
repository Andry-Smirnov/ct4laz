unit ColorPickerRGBAmw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, GR32_ColorPicker, GR32_Panel32, Forms, Controls,
  Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    ColorPickerRGBA1: TColorPickerRGBA;
    GR32Panel1: TGR32Panel;
    procedure ColorPickerRGBA1Changed(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ColorPickerRGBA1Changed(Sender: TObject);
begin
  GR32Panel1.FillColor:=ColorPickerRGBA1.SelectedColor;
end;

end.

