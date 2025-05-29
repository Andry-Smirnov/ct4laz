unit multipleuse;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, Db,constvalues, dbf;

type
  TMultipleUseForm = class(TForm)
    Dbf1: TDbf;
    Dbf2: TDbf;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Dιclarations privιes }
  public
    { Dιclarations publiques }
  end;

var
  MultipleUseForm: TMultipleUseForm;

implementation

{$R *.lfm}

procedure TMultipleUseForm.FormCreate(Sender: TObject);
begin
  Dbf1.FilePath:=pathMedia;
  Dbf2.FilePath:=pathMedia;

  dbf1.Active:=true;
  dbf2.Active:=true;
end;

end.
