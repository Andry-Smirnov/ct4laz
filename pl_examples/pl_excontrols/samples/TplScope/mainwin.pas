unit mainwin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Spin, TplScopeUnit, TplColorPanelUnit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    ColorDialog1: TColorDialog;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    plColorPanel1: TplColorPanel;
    plColorPanel2: TplColorPanel;
    plColorPanel3: TplColorPanel;
    plColorPanel4: TplColorPanel;
    plScope1: TplScope;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    TrackBar1: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure plColorPanel1Click(Sender: TObject);
    procedure plScope1Update(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation



procedure TForm1.plScope1Update(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
  Randomize();
  TrackBar1.Position:=Random(100);
  end;

  plScope1.Position:=100-TrackBar1.Position;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  plScope1.Baseline:=SpinEdit1.Value;
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
begin
   plScope1.Gridsize:=SpinEdit2.Value;
end;

procedure TForm1.SpinEdit3Change(Sender: TObject);
begin
  plScope1.Interval:=SpinEdit3.Value;
end;

procedure TForm1.plColorPanel1Click(Sender: TObject);
 var i:integer;
begin
  i:=-1;
  if Sender is TplColorPanel then i:=TplColorPanel(Sender).Tag;

  case i of
   1:ColorDialog1.Color:=plColorPanel1.Color;
   2:ColorDialog1.Color:=plColorPanel2.Color;
   3:ColorDialog1.Color:=plColorPanel3.Color;
   4:ColorDialog1.Color:=plColorPanel4.Color;
  end;

  if NOT ColorDialog1.Execute then exit;

  case i of
   1:begin
      plColorPanel1.Color:=ColorDialog1.Color;
      plScope1.Basecolor:=ColorDialog1.Color;
     end;
   2:begin
      plColorPanel2.Color:=ColorDialog1.Color;
      plScope1.color:=ColorDialog1.Color;
     end;
   3:begin
      plColorPanel3.Color:=ColorDialog1.Color;
      plScope1.Gridcolor:=ColorDialog1.Color;
     end;
   4:begin
      plColorPanel4.Color:=ColorDialog1.Color;
      plScope1.Linecolor:=ColorDialog1.Color;
     end;
  end;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   plScope1.Active:=NOT plScope1.Active;

   if plScope1.Active then
     Button1.Caption:='Stop Test' else
     Button1.Caption:='Start Test';
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Close;
end;

initialization
  {$I mainwin.lrs}

end.

