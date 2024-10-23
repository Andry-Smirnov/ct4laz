unit BasicDemomw;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, TplSmartGridUnit;

type
  TForm1 = class(TForm)
    SmartGrid1: TplSmartGrid;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox6: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SmartGrid1DrawHeader(Sender: TObject; ACanvas: TCanvas;
      Rc: TRect; Str: string; var Handled: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SmartGrid1InsertRow(Sender: TObject; ARow: Integer);
    procedure Button4Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  SmartGrid1.Flat := CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
  begin
    with SmartGrid1 do
    begin
      //GridColor := clBtnShadow;
      HeaderColor := clBtnFace;
      HeaderDarkColor := clBtnShadow;
      HeaderLightColor := clBtnHighlight;
      HeaderFont.Color := clBlack;
      GutterFont.Color := clBlack;
    end;
  end else
  begin
    with SmartGrid1 do
    begin
      //GridColor := clGray;
      HeaderColor := $00DF0000;
      HeaderDarkColor := clBlack;
      HeaderLightColor := $00FF8000;
      HeaderFont.Color := clWhite;
      GutterFont.Color := clWhite;
    end;
  end;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  SmartGrid1.FitToWidth := CheckBox3.Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  SmartGrid1.AutoColWidth := CheckBox4.Checked;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  SmartGrid1.ShowGrid := CheckBox5.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  x: Integer;
begin
  SmartGrid1.BeginUpdate;
  for x := 0 to 9 do
  begin
    SmartGrid1[0, x] := 'Sample Text';
    SmartGrid1[1, x] := 'Centered Text';
    SmartGrid1[2, x] := 'Left Alignment';
    SmartGrid1[3, x] := FormatFloat('###,###,##0.##', Random(20000000));
    SmartGrid1[4, x] := IntToStr(Random(2000));
  end;
  SmartGrid1.EndUpdate;
  CheckBox2Click(nil);
end;

procedure TForm1.SmartGrid1DrawHeader(Sender: TObject; ACanvas: TCanvas;
  Rc: TRect; Str: String; var Handled: Boolean);
begin
  if (Str = 'One')
    then ACanvas.Font.Color := clRed;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SmartGrid1.Columns[2].Visible := not SmartGrid1.Columns[2].Visible;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SmartGrid1.InsertRow(SmartGrid1.Row);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  SmartGrid1.DeleteRow(SmartGrid1.Row);
end;

procedure TForm1.SmartGrid1InsertRow(Sender: TObject; ARow: Integer);
begin
  SmartGrid1.Cells[0, ARow] := 'New Row';
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  SmartGrid1.Columns[2].ReadOnly := not SmartGrid1.Columns[2].ReadOnly;
end;

procedure TForm1.CheckBox6Click(Sender: TObject);
begin
  SmartGrid1.ShowFooter := not SmartGrid1.ShowFooter;
end;

end.
