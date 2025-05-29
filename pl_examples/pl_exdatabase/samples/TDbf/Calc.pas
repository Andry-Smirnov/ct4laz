unit Calc;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ExtCtrls, DBCtrls, DBGrids;

type
  TCalcForm = class(TForm)
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
  private
    { Dιclarations privιes }
  public
    { Dιclarations publiques }
  end;

var
  CalcForm: TCalcForm;

implementation

uses TDbf_Demomw;

{$R *.lfm}

end.


