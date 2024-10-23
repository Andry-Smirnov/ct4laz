unit comboboxiesmw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TplComboBoxUnit, TplComboBoxColorUnit,
  TplComboBoxFontUnit, TplComboBoxBrushUnit, TplComboBoxPenStyleUnit,
  TplComboBoxPenWidthUnit, Forms, Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    plBrushComboBox1: TplBrushComboBox;
    plColorComboBox1: TplColorComboBox;
    plComboBox1: TplComboBox;
    plFontComboBox1: TplFontComboBox;
    plPenStyleComboBox1: TplPenStyleComboBox;
    plPenWidthComboBox1: TplPenWidthComboBox;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

