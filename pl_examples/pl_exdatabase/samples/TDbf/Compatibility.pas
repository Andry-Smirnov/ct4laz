unit Compatibility;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, dbf, StdCtrls, DBCtrls, constvalues,ExtCtrls, Grids, DBGrids;

type

  { TCompatibilityForm }

  TCompatibilityForm = class(TForm)
    Dbf1: TDbf;
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    DBMemo1: TDBMemo;
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CompatibilityForm: TCompatibilityForm;

implementation

{$R *.lfm}

procedure TCompatibilityForm.RadioGroup1Click(Sender: TObject);
var
  filename:string;
begin
  case RadioGroup1.ItemIndex of
  0: filename:='dbase3+.dbf';
  1: filename:='dbase4.dbf';
  2: filename:='dbasewin.dbf';
  3: filename:='visualdbase.dbf';
  end;
  dbf1.Active:=false;
  dbf1.TableName:=filename;
  dbf1.Active:=true;
end;

procedure TCompatibilityForm.FormCreate(Sender: TObject);
begin
   Dbf1.FilePath:=pathMedia;
end;

end.
