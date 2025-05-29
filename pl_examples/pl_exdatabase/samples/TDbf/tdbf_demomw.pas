unit TDbf_Demomw;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, ExtCtrls, DBCtrls,constvalues, Grids, DBGrids, Menus, Buttons,
  ComCtrls, FileUtil, dbf;


type
  TMainForm = class(TForm)
    DemoButton: TButton;
    DbfDemo: TDbf;
    DataSourceDemo: TDataSource;
    DbfDisco: TDbf;
    DatasourceDisco: TDataSource;
    Button1: TButton;
    DBNavigator1: TDBNavigator;
    DBText1: TDBText;
    DBGrid1: TDBGrid;
    Label1: TLabel;
    Image1: TImage;
    DBRichEdit1: TDBMemo;
    DbfDiscoAUTHOR: TStringField;
    DbfDiscoTITLE: TStringField;
    DbfDiscoCOMPANY: TStringField;
    DbfDiscoCOUNTRY: TStringField;
    DbfDiscoYEAR: TSmallintField;
    DbfDiscoPRICE: TFloatField;
    DbfDiscoNOTE: TStringField;
    DbfDiscoQTY: TSmallintField;
    DbfDemoID: TStringField;
    DbfDemoTITLE: TStringField;
    DbfDemoDESCR: TMemoField;
    DbfDemoDEMO: TStringField;
    DbfDiscoCALCPRICE: TCurrencyField;
    DbfDiscoHIGHPRICE: TBooleanField;
    DbfDiscoLAST_SELL: TDateField;
    DbfDiscoIN_STOCK: TBooleanField;
    LabelVersion: TLabel;
    PopupMenu1: TPopupMenu;
    Edit1: TMenuItem;
    procedure DbfDemoAfterScroll(DataSet: TDataSet);
    procedure DemoButtonClick(Sender: TObject);
    procedure DbfDiscoFilterRecord(DataSet: TDataSet; var Accept: Boolean);
    procedure ButtonCloseClick(Sender: TObject);
    procedure DBGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DbfDiscoCalcFields(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure DataSourceDemoStateChange(Sender: TObject);
    procedure LabelEmailMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure label_websiteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Edit1Click(Sender: TObject);
  private
    { Dιclarations privιes }
  public
    { Dιclarations publiques }
    lastForm:TForm;
  end;
var
  MainForm: TMainForm;

implementation

uses EditTopics, Simple, Index, Search, Filter, Calc, Schema, Schema2,
  CreateTable, Pack, CopyTable, multipleuse, Compatibility;

{$R *.lfm}


procedure TMainForm.DbfDemoAfterScroll(DataSet: TDataSet);
begin
  DemoButton.Enabled:=length(trim(DbfDemo.FieldByName('DEMO').AsString))>0;
end;

procedure TMainForm.DemoButtonClick(Sender: TObject);
var
  demo:string;
  newForm:TForm;
begin
  newForm:=nil;
  demo:=trim(DbfDemo.FieldByName('DEMO').AsString);
  if demo='simple' then newForm:=simpleForm
  else if demo='index' then newForm:=indexForm
  else if demo='search' then newForm:=SearchForm
  else if demo='filter' then newForm:=FilterForm
  else if demo='memo' then newForm:=EditTopicsForm
  else if demo='calc' then newForm:=CalcForm
  else if demo='schema' then newForm:=Schema1Form
  else if demo='schema2' then newForm:=Schema2Form
  else if demo='create' then newForm:=CreateTableForm
  else if demo='pack' then newForm:=PackTableForm
  else if demo='copy' then newForm:=CopyTableForm
  else if demo='multiple' then newForm:=MultipleUseForm
  else if demo='compatib' then newForm:=CompatibilityForm
  else ;
  if (lastForm<>newForm) and (lastform<>nil) then lastForm.Hide;
  if (newform<>nil) then newForm.Show;
  lastForm:=newForm;
end;

procedure TMainForm.DbfDiscoFilterRecord(DataSet: TDataSet;
  var Accept: Boolean);
var
  year:integer;
  country:string;
  correct_year:boolean;
  correct_country:boolean;
begin
  year:=StrToIntDef(DbfDiscoYear.AsString,0);
  Country:=DbfDiscoCountry.AsString;
  correct_year:=
    ((year=0) and FilterForm.cbBlank.checked)
    or
    ((year >= FilterForm.Year_From.Tag)
    and (year <= FilterForm.Year_To.Tag));

  if Country='USA' then correct_country:=FilterForm.cbUSA.checked
  else if Country='USA' then correct_country:=FilterForm.cbUSA.checked
  else if Country='SWE' then correct_country:=FilterForm.cbSWE.checked
  else if Country='UK' then correct_country:=FilterForm.cbUK.checked
  else if Country='GER' then correct_country:=FilterForm.cbGER.checked
  else if Country='HOL' then correct_country:=FilterForm.cbHOL.checked
  else if Country='ITA' then correct_country:=FilterForm.cbITA.checked
  else correct_country:=FilterForm.cbOTH.checked;

  Accept:=correct_year and correct_country;

end;

procedure TMainForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.DBGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssShift in Shift) and (ssCtrl in Shift) then EditTopicsForm.showModal;
end;

procedure TMainForm.DbfDiscoCalcFields(DataSet: TDataSet);
var
  Price:double;
  Qty:double;
  CalcPrice:double;
begin
  try
    Price:=DbfDiscoPRICE.AsFloat;
    Qty:=DbfDiscoQTY.AsFloat;
    calcPrice:=Price*Qty;
    DbfDiscoCALCPRICE.AsFloat:=calcPrice;
    DbfDiscoHighPrice.AsBoolean:=calcPrice>=10;
  except
    DbfDiscoCALCPRICE.AsFloat:=0;
    DbfDiscoHighPrice.AsBoolean:=false;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin

  DbfDemo.FilePath:=pathMedia;
  DbfDisco.FilePath:=pathMedia;

  LabelVersion.Caption:='TDbf Version: '+DbfDemo.Version;
  DbfDemo.Active:=true;
  DbfDisco.Active:=true;
end;


procedure TMainForm.DataSourceDemoStateChange(Sender: TObject);
var
  ed:boolean;
begin
  ed:=DbfDemo.State in [dsEdit,dsInsert];
  if editTopicsForm=nil then exit;
  with editTopicsForm do begin
    speedButton1.enabled:=ed;
    speedButton2.enabled:=ed;
    speedButton3.enabled:=ed;
    speedButton4.enabled:=ed;
    speedButton5.enabled:=ed;
    speedButton6.enabled:=ed;
    speedButton7.enabled:=ed;
    speedButton8.enabled:=ed;
    speedButton9.enabled:=ed;
  end;
end;

procedure TMainForm.LabelEmailMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Nice and easy isn't it ?
   OpenDocument('mailto:Micha Nelissen <micha@neli.hopto.org>'); { *Converted from ShellExecute* }
end;

procedure TMainForm.label_websiteMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Nice and easy isn't it ?
  OpenURL('http://tdbf.sf.net'); { *Converted from ShellExecute* }
end;

procedure TMainForm.Edit1Click(Sender: TObject);
begin
  if (lastForm<>EditTopicsForm) and (lastform<>nil) then lastForm.Hide;
  EditTopicsForm.Show;
  lastForm:=EditTopicsForm;
end;

end.


