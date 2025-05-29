unit BufDatasetTempmw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  TpBufDatasetTempUnit, db, BufDataset;

type

  { TForm1 }

  TForm1 = class(TForm)
    BufDataset1: TBufDataset;
    BufDataset1ID1: TLongintField;
    BufDataset1NAME1: TStringField;
    BufDatasetTemp1: TpBufDatasetTemp;
    BufDatasetTemp1ID1: TLongintField;
    BufDatasetTemp1NAME1: TStringField;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    procedure FormShow(Sender: TObject);
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

procedure TForm1.FormShow(Sender: TObject);
begin
  BufDatasetTemp1.CreateDataset;
  BufDatasetTemp1.Open;
  BufDatasetTemp1.Append;
  BufDatasetTemp1ID1.AsInteger:=1;
  BufDatasetTemp1NAME1.AsString:='PATRICK';
  BufDatasetTemp1.Post;
  BufDatasetTemp1.Append;
  BufDatasetTemp1ID1.AsInteger:=2;
  BufDatasetTemp1NAME1.AsString:='MARY';
  BufDatasetTemp1.Post;
  BufDatasetTemp1.Append;
  BufDatasetTemp1ID1.AsInteger:=3;
  BufDatasetTemp1NAME1.AsString:='CARLOS';
  BufDatasetTemp1.Post;
  BufDatasetTemp1.Append;
  BufDatasetTemp1ID1.AsInteger:=4;
  BufDatasetTemp1NAME1.AsString:='JACK';
  BufDatasetTemp1.Post;
  BufDatasetTemp1.Append;
  BufDatasetTemp1ID1.AsInteger:=5;
  BufDatasetTemp1NAME1.AsString:='ROMEO';
  BufDatasetTemp1.Post;

  BufDataset1.CreateDataset;
  BufDataset1.Open;
  BufDataset1.Append;
  BufDataset1ID1.AsInteger:=1;
  BufDataset1NAME1.AsString:='PATRICK';
  BufDataset1.Post;
  BufDataset1.Append;
  BufDataset1ID1.AsInteger:=2;
  BufDataset1NAME1.AsString:='MARY';
  BufDataset1.Post;
  BufDataset1.Append;
  BufDataset1ID1.AsInteger:=3;
  BufDataset1NAME1.AsString:='CARLOS';
  BufDataset1.Post;
  BufDataset1.Append;
  BufDataset1ID1.AsInteger:=4;
  BufDataset1NAME1.AsString:='JACK';
  BufDataset1.Post;
  BufDataset1.Append;
  BufDataset1ID1.AsInteger:=5;
  BufDataset1NAME1.AsString:='ROMEO';
  BufDataset1.Post;
end;

end.

