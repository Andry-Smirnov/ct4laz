program TDbf_Demo;

{$MODE Delphi}



uses

  Forms, Interfaces,

  EditTopics,
  Filter,
  Index,
  TDbf_Demomw,
  Schema,
  Schema2,
  Search,
  Simple,
  Pack,
  CopyTable,
  CreateTable,
  multipleuse,
  Compatibility,
  Calc;



{$R *.res}

begin

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TEditTopicsForm, EditTopicsForm);
  Application.CreateForm(TFilterForm, FilterForm);
  Application.CreateForm(TIndexForm, IndexForm);
  Application.CreateForm(TSchema1Form, Schema1Form);
  Application.CreateForm(TSchema2Form, Schema2Form);
  Application.CreateForm(TSearchForm, SearchForm);
  Application.CreateForm(TSimpleForm, SimpleForm);
  Application.CreateForm(TPackTableForm, PackTableForm);
  Application.CreateForm(TCopyTableForm, CopyTableForm);
  Application.CreateForm(TCreateTableForm, CreateTableForm);
  Application.CreateForm(TMultipleUseForm, MultipleUseForm);
  Application.CreateForm(TCompatibilityForm, CompatibilityForm);
  Application.CreateForm(TCalcForm, CalcForm);

  Application.Run;

end.

