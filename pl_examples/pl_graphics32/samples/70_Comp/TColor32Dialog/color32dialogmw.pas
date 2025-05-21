unit Color32Dialogmw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, GR32_Dsgn_Color, GR32_Panel32, Forms, Controls,
  Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Color32Dialog1: TColor32Dialog;
    GR32Panel1: TGR32Panel;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin

  if Color32Dialog1.Execute then
   GR32Panel1.FillColor:=Color32Dialog1.Color;
end;

end.

