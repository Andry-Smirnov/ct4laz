{**********************************************************************
                PilotLogic Software House

 Package pl_Cindy
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

unit cyDBMemo;

{$I cyCompilerDefines.inc}

interface

uses
    LCLIntf, LCLType, LMessages,
    Classes, Types, Graphics, StdCtrls, Forms, Messages, SysUtils, Controls, DB, DBCtrls, Dialogs, Variants;

type

  TcyDBMemo = class(TDBMemo)
  private
    FFocused: Boolean; // Know if we have focus on before destruction ...
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeforeDestruction; override;
  published
  end;

implementation

{ TcyDBMemo }
constructor TcyDBMemo.Create(AOwner: TComponent);
begin
  inherited;
  FFocused := false;
end;

procedure TcyDBMemo.BeforeDestruction;
begin
  if (not ReadOnly) and FFocused then


  inherited;
end;

procedure TcyDBMemo.CMEnter(var Message: TCMEnter);
begin
  FFocused := true;
  Inherited;
end;

procedure TcyDBMemo.CMExit(var Message: TCMExit);
var
  ModifyField: Boolean;
begin
  FFocused := false;
  ModifyField := false;

  if not ReadOnly then
    if Assigned(Field) then              // Control that Datasource and DataField assigned and dataset is active ...
      ModifyField := Field.CanModify;    // also work: and (not Field.ReadOnly) and (DataSource.DataSet.CanModify);

  if ModifyField then
  begin
    // 2016-07-14    // Inherited;

    // 2016-07-14 Corrected error  "Dataset not in edit or insert mode" exiting control when dataset state is dsBrowse (nothing modified)
    if DataSource.DataSet.State = dsBrowse then
      DoExit
    else
      Inherited;
  end
  else
    DoExit;    // We can' t call TDBRichEdit.CMExit !!!
end;

procedure TcyDBMemo.DoExit;
begin
  // 2016-09-16 : clear field if no text when exit control (may only contain rft format)
  if Assigned(Field) then              // Control that Datasource and DataField assigned and dataset is active ...
    if DataSource.DataSet.State in [dsEdit, dsInsert] then
      if Self.Text = '' then
        if Self.Field.AsString = '' then
        begin


        end
        else
          Self.Field.AsString := '';

  inherited;
end;

end.
