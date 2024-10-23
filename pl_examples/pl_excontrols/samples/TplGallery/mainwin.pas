unit mainwin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ColorBox, Spin, TplGalleryUnit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    ColorBox3: TColorBox;
    ColorBox4: TColorBox;
    ColorBox5: TColorBox;
    ColorBox6: TColorBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    plGallery1: TplGallery;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure ColorBox2Change(Sender: TObject);
    procedure ColorBox3Change(Sender: TObject);
    procedure ColorBox4Change(Sender: TObject);
    procedure ColorBox5Change(Sender: TObject);
    procedure ColorBox6Change(Sender: TObject);
    procedure plGallery1Click(Sender: TObject; Index: Integer);
    procedure plGallery1Hover(Sender: TObject; Index: Integer);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
    procedure SpinEdit4Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation


procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  plGallery1.Active:=CheckBox1.Checked;
end;

procedure TForm1.ColorBox1Change(Sender: TObject);
begin
  plGallery1.Color:=ColorBox1.Selected;
end;

procedure TForm1.ColorBox2Change(Sender: TObject);
begin
  plGallery1.SelectionColor:=ColorBox2.Selected;
end;

procedure TForm1.ColorBox3Change(Sender: TObject);
begin
  plGallery1.ScrollerArrowColor:=ColorBox3.Selected;
end;

procedure TForm1.ColorBox4Change(Sender: TObject);
begin
   plGallery1.ScrollerColor:=ColorBox4.Selected;
end;

procedure TForm1.ColorBox5Change(Sender: TObject);
begin
   plGallery1.ScrollerHoverArrowColor:=ColorBox5.Selected;
end;

procedure TForm1.ColorBox6Change(Sender: TObject);
begin
  plGallery1.ScrollerHoverColor:=ColorBox6.Selected;
end;

procedure TForm1.plGallery1Click(Sender: TObject; Index: Integer);
begin
  Label12.Caption:='Click Item NO: '+inttostr(index);
end;

procedure TForm1.plGallery1Hover(Sender: TObject; Index: Integer);
begin
  Label13.Caption:='Hover Item NO: '+inttostr(index);
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  plGallery1.LargeChange:=SpinEdit1.Value;
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
begin
  plGallery1.SmallChange:=SpinEdit2.Value;
end;

procedure TForm1.SpinEdit3Change(Sender: TObject);
begin
  plGallery1.Interval:=SpinEdit3.Value;
end;

procedure TForm1.SpinEdit4Change(Sender: TObject);
begin
  plGallery1.ScrollerSize:=SpinEdit4.Value;
end;

initialization
  {$I mainwin.lrs}

end.

