{**********************************************************************
                PilotLogic Software House
  
 Package pl_Cindy
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

{   Component(s):
    tcyDBEdit

    Description:
    DBEdit control with parser funcionality and Alignment

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
    
unit cyDBEdit;

{$MODE Delphi}

{$I cyCompilerDefines.inc}

interface

uses
     LCLIntf, LCLType, LMessages,
     Classes, Types, Graphics, StdCtrls, Forms,  Messages, SysUtils, Controls, DB, DBCtrls,
     cyGraphics, cyMathParser;

type
  TcyDBEdit = class(TDBEdit)
  private

    FTextHintFont: TFont;
    FCanvas: TControlCanvas;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT; //=== ct9999 ======
    procedure SetTextHintFont(const Value: TFont);

    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure DoParse;

    procedure FontTextHintChanged(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

    property Alignment;
    property TextHint;
    property TextHintFont: TFont read FTextHintFont write SetTextHintFont;

  end;

implementation

{ TcyDBEdit }
constructor TcyDBEdit.Create(AOwner: TComponent);
begin
  inherited;


  FCanvas := Nil;
  FTextHintFont := TFont.Create;
  FTextHintFont.OnChange := FontTextHintChanged;

end;

destructor TcyDBEdit.Destroy;
begin

  FTextHintFont.Free;
  FCanvas.Free;

  inherited;
end;

procedure TcyDBEdit.DoParse;
var
  IsIntegerField, IsFloatField: Boolean;
  Parser: TcyMathParser;
  Rslt: String;
begin
  IsIntegerField := Field.DataType in [ftSmallint, ftInteger, ftWord, ftLargeint];
  IsFloatField := Field.DataType in [ftFloat, ftCurrency];

  if IsIntegerField or IsFloatField then
  begin
    Parser := TcyMathParser.Create(Self);
    Parser.Expression := Text;
    if Parser.Parse then
    begin
      if IsIntegerField
      then Rslt := IntToStr(Round(Parser.ParserResult))
      else Rslt := floatToStr(Parser.ParserResult);

      if Text <> Rslt then
        Text := Rslt;
    end;
    Parser.Free;
  end;
end;

procedure TcyDBEdit.CMExit(var Message: TCMExit);
begin
  // Parse :
  if not ReadOnly then
  if Assigned(Field) then
    if not Field.ReadOnly then
      if Modified and (DataSource.DataSet.State in [dsEdit, dsInsert]) then
        DoParse;

  Inherited;
end;

procedure TcyDBEdit.KeyPress(var Key: Char);
var
  SavKey: Char;
begin
  // Bypass IsValidChar on inherited KeyPress(Key) event :
  SavKey := Key;

  // Avoid MessageBeep from inherited KeyPress(Key) event :
  if Assigned(Field) then
    if not Field.IsValidChar(Key) then
      Key := #0;

  inherited KeyPress(Key);

  if SavKey <> #27 then
    Key := SavKey;

  if Key = #13 then
    if not ReadOnly then
    if Assigned(Field) then
      if not Field.ReadOnly then
        if Modified and (DataSource.DataSet.State in [dsEdit, dsInsert]) then
          DoParse;
end;


procedure TcyDBEdit.FontTextHintChanged(Sender: TObject);
begin
  if (TextHint <> '') and (EditText = '') then
    Invalidate;
end;

procedure TcyDBEdit.SetTextHintFont(const Value: TFont);
begin
  FTextHintFont.Assign(Value);
end;

procedure TcyDBEdit.WMPaint(var Msg: TLMPaint);
var
  DC: HDC;
  PS: TPaintStruct;
  R: TRect;
begin
  if (not Focused) and (EditText = '') and (TextHint <> '') then
  begin
    if FCanvas = nil then
    begin
      FCanvas := TControlCanvas.Create;
      FCanvas.Control := Self;
    end;

    DC := Msg.DC;
    if DC = 0 then DC := BeginPaint(Handle, PS);
    FCanvas.Handle := DC;
    try
      FCanvas.Font := TextHintFont;
      R := ClientRect;

      with FCanvas do
      begin
       R := ClientRect;
        if (BorderStyle = bsSingle) then
        begin
          Brush.Color := clWindowFrame;
          FrameRect(R);
          InflateRect(R, -1, -1);
        end;
      end;

      FCanvas.Brush.Color := Color;
      cyGraphics.cyDrawSingleLineText(FCanvas, TextHint, R, Alignment, tlCenter, 0, 0);
    finally
      FCanvas.Handle := 0;
      if Msg.DC = 0 then EndPaint(Handle, PS);
    end;
  end
  else
    inherited;
end;

end.

