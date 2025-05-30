unit Index;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Db, DBCtrls, Grids, DBGrids, ComCtrls, Dbf_Cursor;

type

  { TIndexForm }

  TIndexForm = class(TForm)
    ApplyButton: TButton;
    Button1: TButton;
    DBGrid1: TDBGrid;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    SearchEdit: TEdit;
    SearchLabel: TLabel;
    SearchResultLabel: TLabel;
    RebuildButton: TButton;
    ExpressionCheckBox: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure RebuildButtonClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
  private
    { Dιclarations privιes }
  public
    { Dιclarations publiques }
  end;

var
  IndexForm: TIndexForm;

implementation

uses
  TDbf_Demomw, Dbf_Common;

{$R *.lfm}

procedure TIndexForm.Button2Click(Sender: TObject);
begin
  MainForm.DbfDisco.IndexName:='AUTHOR';
end;

procedure TIndexForm.Button3Click(Sender: TObject);
begin
  MainForm.DbfDisco.IndexName:='TITLE.NDX';
end;

procedure TIndexForm.Button4Click(Sender: TObject);
begin
  MainForm.DbfDisco.IndexName:='';
end;

procedure TIndexForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TIndexForm.Button5Click(Sender: TObject);
begin
  MainForm.DbfDisco.IndexName:='PRICE.NDX';
end;

procedure TIndexForm.RebuildButtonClick(Sender: TObject);
begin
  // need exclusive access
  MainForm.DbfDisco.Close;
  MainForm.DbfDisco.Exclusive := true;
  MainForm.DbfDisco.Open;
  // create descending MDX index for author field
  MainForm.DbfDisco.AddIndex('AUTHOR', 'AUTHOR', [ixDescending]);
  // create NDX index for title field
  MainForm.DbfDisco.AddIndex('TITLE.NDX', 'TITLE', []);
  if ExpressionCheckBox.Checked then
  begin
    // create NDX expression index for price; title
    MainForm.DbfDisco.AddIndex('PRICE.NDX', 'STR(PRICE, 7, 2)+TITLE', [ixExpression]);
  end else begin
    // create simple NDX index for price
    MainForm.DbfDisco.AddIndex('PRICE.NDX', 'PRICE', []);
  end;
  // close exclusive
  MainForm.DbfDisco.Close;
  MainForm.DbfDisco.Exclusive := false;
  MainForm.DbfDisco.Open;
  // show message
  Application.MessageBox('Index recreating done.', 'Done', MB_OK or MB_ICONINFORMATION);
end;

procedure TIndexForm.ApplyButtonClick(Sender: TObject);
begin
  MainForm.DbfDisco.SetRangePChar(PChar(Edit1.Text), PChar(Edit2.Text));
end;

procedure TIndexForm.SearchEditChange(Sender: TObject);
begin
  if MainForm.DbfDisco.SearchKeyPChar(PChar(SearchEdit.Text), stGreaterEqual) then
    SearchResultLabel.Color := clGreen
  else
    SearchResultLabel.Color := clRed
end;

end.

