unit ColorPickerHSmw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,GR32, GR32_ColorPicker, GR32_Panel32, GR32_RangeBars,
  Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ColorPickerHS1: TColorPickerHS;
    GR32Panel1: TGR32Panel;
    Label1: TLabel;
    TrackBar1: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure ColorPickerHS1Changed(Sender: TObject);
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

procedure TForm1.ColorPickerHS1Changed(Sender: TObject);
 var R, G, B, A: Byte;
begin
  Color32ToRGBA(ColorPickerHS1.SelectedColor,R, G, B, A);
  a:=TrackBar1.Position;
  GR32Panel1.FillColor:=Color32(R, G, B, A);
end;


end.

