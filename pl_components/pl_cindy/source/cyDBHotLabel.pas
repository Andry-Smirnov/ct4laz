{**********************************************************************
                PilotLogic Software House
  
 Package pl_Cindy
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

{   Component(s):
    tcyDBHotLabel

    Description:
    A DBText with the functionnalities of the tcyHotLabel

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}
    
unit cyDBHotLabel;

{$MODE Delphi}

interface

uses  LCLIntf, LCLType, LMessages, Types,
      StdCtrls, Graphics, classes, Messages, Controls, DB, DBCtrls, cyHotLabel;

type
  TcyDBHotLabel = class(TcyCustomHotLabel)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldText: string;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    function GetLabelText: string; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetAutoSize(Value: Boolean); override;
  public
    property Canvas;
    property Caption;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
//    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
//    property Font;
    property FocusControl;
    property ParentBiDiMode;
//    property ParentColor;
//    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
    // Herited from TcyBaseLabel :
//    property Shadow;
    property Animation;
    property CaptionIndentLeft;
    property CaptionIndentRight;
    property CaptionIndentTop;
    property CaptionIndentBottom;
    property CaptionRender;
    property CaptionOrientation;
    property OnPaint;
    // Herited from TcyCustomHotLabel :
    property Bevels;
    property DegradeEnter;
    property DegradeLeave;
    property DegradeMouseDown;
    property FontEnter;
    property FontLeave;
    property FontMouseDown;
    property ShadowEnter;
    property ShadowLeave;
    property ShadowMouseDown;
  end;

implementation

constructor TcyDBHotLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
end;

destructor TcyDBHotLabel.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TcyDBHotLabel.Loaded;
begin
  inherited Loaded;
  // Show component name ...
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TcyDBHotLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TcyDBHotLabel.UseRightToLeftAlignment: Boolean;
begin
  Result := true;//DBUseRightToLeftAlignment(Self, Field);
end;

procedure TcyDBHotLabel.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    if Value and FDataLink.DataSourceFixed
    then DatabaseError('ERROR: DataSource is Fixed');

    inherited SetAutoSize(Value);
  end;
end;

function TcyDBHotLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TcyDBHotLabel.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TcyDBHotLabel.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TcyDBHotLabel.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TcyDBHotLabel.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TcyDBHotLabel.GetFieldText: string;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.DisplayText
  else
    if csDesigning in ComponentState then Result := Name else Result := '';
end;

procedure TcyDBHotLabel.DataChange(Sender: TObject);
begin
  Caption := GetFieldText;
end;

function TcyDBHotLabel.GetLabelText: string;
begin
  if csPaintCopy in ControlState then
    Result := GetFieldText else
    Result := Caption;
end;

procedure TcyDBHotLabel.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TcyDBHotLabel.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);     
end;

function TcyDBHotLabel.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

end.
