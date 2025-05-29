{**********************************************************************
                PilotLogic Software House

 Package pl_Cindy
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

{   Unit cyObjUtils

    Description:
    Unit with functions for classes, controls handling

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
    * Donations: see Donation section on Description.txt
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

unit cyObjUtils;

{$I cyCompilerDefines.inc}

interface

uses
  LCLIntf, LCLType, LMessages,
  Classes, Types, Graphics, Forms, ComCtrls, Controls, SysUtils, cyStrUtils;

type
  TStringsSortType = (stNone, stStringSensitive, stStringInsensitive, stExtended);
  TStringsValueKind = (skStringSensitive, skStringInsensitive, skExtended);

  TcyLocateOption = (lCaseInsensitive, lPartialKey);
  TcyLocateOptions = set of TcyLocateOption;

  // TStrings functions :
  function StringsLocate(aList : TStrings; Value: String; Options: TcyLocateOptions): Integer; overload;
  function StringsLocate(aList: TStrings; Value: String; ValueKind: TStringsValueKind): Integer; overload;
  function StringsAdd(aList: TStrings; Value: String; Unique: Boolean; SortType: TStringsSortType): Integer;
  procedure StringsReplace(aList: TStrings; OldStr: String; NewStr: String; ValueKind: TStringsValueKind);
  procedure StringsSort(aList: TStrings; SortType: TStringsSortType);

  // TTreeNode functions:
  function TreeNodeLocate(ParentNode: TTreeNode; Value: String): TTreeNode;
  function TreeNodeLocateOnLevel(TreeView: TTreeView; OnLevel: Integer; Value: String): TTreeNode;
  function TreeNodeGetChildFromIndex(TreeView: TTreeView; ParentNode: TTreeNode; ChildIndex: Integer): TTreeNode;
  function TreeNodeGetParentOnLevel(ChildNode: TTreeNode; ParentLevel: Integer): TTreeNode;
  function TreeNodeSelectionIndex(Node: TTreeNode): Integer;
  procedure TreeNodeCopy(FromNode: TTreeNode; ToNode: TTreeNode; const CopyChildren: Boolean = true; const CopySubChildren: Boolean = true);
  procedure TreeNodeMoveBeforeSibling(aNode, BeforeNode: TTreeNode);
  procedure TreeNodeMoveAfterSibling(aNode, AfterNode: TTreeNode);

  // Others :
  function GetTopMostControlAtPos(FromControl: TWinControl; aControlPoint: TPoint): TControl;
  procedure CenterControl(aControl: TControl);
  function GetLastParent(aControl: TControl): TWinControl;
  function MouseIsOverControl(aControl: TControl): Boolean;

implementation

function StringsLocate(aList: TStrings; Value: String; Options: TcyLocateOptions): Integer;
var
  i : Integer;
  StrItem: String;
  FindPartialKey, FindCaseInsensitive: Boolean;
begin
  Result := -1;
  FindPartialKey := lPartialKey in Options;
  FindCaseInsensitive := lCaseInsensitive in Options;

  if FindCaseInsensitive then
    Value := AnsiUpperCase(Value);

  for i  := 0 to aList.Count - 1 do
  begin
    if FindCaseInsensitive
    then StrItem := AnsiUpperCase(aList[i])
    else StrItem := aList[i];

    if FindPartialKey then
    begin
      if Pos(Value, StrItem) = 1 then // Must start as StrItem
      begin
        Result := i;
        Break;
      end;
    end
    else
      if StrItem = Value then
      begin
        Result := i;
        Break;
      end;
  end;
end;

function StringsLocate(aList: TStrings; Value: String; ValueKind: TStringsValueKind): Integer;
var
  i: Integer;
  fValue, f: Extended;
begin
  Result := -1;

  case ValueKind of
    skStringSensitive:
      Result := aList.IndexOf(Value);

    skStringInsensitive:
    begin
      Value := AnsiUppercase(Value);

      for i := 0 to aList.Count -1 do
        if Value = AnsiUppercase(aList[i]) then
        begin
          Result := i;
          Break;
        end;
    end;

    skExtended:
    begin
      if TryStrToFloat(Value, fValue) then
        for i := 0 to aList.Count -1 do
          if TryStrToFloat(aList[i], f) then
          if fValue = f then
            begin
              Result := i;
              Break;
            end;
    end;
  end;
end;

function StringsGetInsertPosition(aList: TStrings; Value: String; SortType: TStringsSortType): Integer;
var
  i: Integer;
  fValue, f: Extended;
begin
  Result := aList.Count;

  case SortType of
    stStringSensitive:
    begin
      for i := 0 to aList.Count -1 do
        if Value < aList[i] then
        begin
          Result := i;
          Break;
        end;
    end;

    stStringInsensitive:
    begin
      Value := AnsiUppercase(Value);

      for i := 0 to aList.Count -1 do
        if Value < AnsiUppercase(aList[i]) then
        begin
          Result := i;
          Break;
        end;
    end;

    stExtended:
    begin
      if TryStrToFloat(Value, fValue) then
        for i := 0 to aList.Count -1 do
          if TryStrToFloat(aList[i], f) then
          if fValue < f then
            begin
              Result := i;
              Break;
            end;
    end;
  end;
end;

function StringsAdd(aList: TStrings; Value: String; Unique: Boolean; SortType: TStringsSortType): Integer;
begin
  Result := -1;

  if Unique then
    case SortType of
      stNone, stStringSensitive: if aList.IndexOf(Value) <> -1 then Exit;
      stStringInsensitive: if StringsLocate(aList, Value, skStringInsensitive) <> -1 then Exit;
      stExtended: if StringsLocate(aList, Value, skExtended) <> -1 then Exit;
    end;

  if SortType <> stNone then
  begin
    Result := StringsGetInsertPosition(aList, Value, SortType);
    aList.Insert(Result, Value);
  end
  else
    Result := aList.Add(Value);
end;

procedure StringsReplace(aList: TStrings; OldStr: String; NewStr: String; ValueKind: TStringsValueKind);
var
  i: Integer;
  fValue, f: Extended;
begin
  case ValueKind of
    skStringSensitive:
    begin
      for i := 0 to aList.Count - 1 do
        if aList[i] = OldStr then
          aList[i] := NewStr;
    end;

    skStringInsensitive:
    begin
      for i := 0 to aList.Count - 1 do
        if AnsiUpperCase(aList[i]) = AnsiUpperCase(OldStr) then
          aList[i] := NewStr;
    end;

    skExtended:
    begin
      if TryStrToFloat(OldStr, fValue) then
        for i := 0 to aList.Count - 1 do
          if TryStrToFloat(aList[i], f) then
            if f = fValue then
              aList[i] := NewStr;
    end;
  end;
end;

procedure StringsSort(aList: TStrings; SortType: TStringsSortType);
var
  i, j, smallest: Integer;
  Str: String;
  f, smallVal: Extended;
begin
  case SortType of
    stStringSensitive:
      for i := 0 to aList.Count - 2 do
      begin
        smallest := i;

        for j := i + 1 to aList.Count - 1 do
          if aList[j] < aList[smallest] then
            smallest := j;

        if smallest <> i then
        begin
          Str := aList[i];
          aList[i] := aList[smallest];
          aList[smallest] := Str;
        end;
      end;

    stStringInsensitive:
      for i := 0 to aList.Count - 2 do
      begin
        smallest := i;

        for j := i + 1 to aList.Count - 1 do
          if AnsiUpperCase(aList[j]) < AnsiUpperCase(aList[smallest]) then
            smallest := j;

        if smallest <> i then
        begin
          Str := aList[i];
          aList[i] := aList[smallest];
          aList[smallest] := Str;
        end;
      end;

    stExtended:
      for i := 0 to aList.Count - 2 do
      begin
        smallest := i;

        if TryStrToFloat(aList[i], smallVal) then
        begin
          for j := i + 1 to aList.Count - 1 do
            if TryStrToFloat(aList[j], f)
            then
              if f < smallVal then
              begin
                smallest := j;
                smallVal := f;
              end;
        end
        else
          smallest := aList.Count - 1;

        if smallest <> i then
        begin
          Str := aList[i];
          aList[i] := aList[smallest];
          aList[smallest] := Str;
        end;
      end;
  end;
end;

function TreeNodeLocate(ParentNode: TTreeNode; Value: String): TTreeNode;
begin
  Result := ParentNode.GetFirstChild;

  while Result <> nil do
  begin
    if Result.Text = Value
    then Break
    else Result := ParentNode.GetNextChild(Result);
  end;
end;

function TreeNodeLocateOnLevel(TreeView: TTreeView; OnLevel: Integer; Value: String): TTreeNode;
var i : Integer;
begin
  Result := Nil;

  for i := 0 to TreeView.Items.Count - 1 do
    if TreeView.Items[i].Level = OnLevel
    then
      if TreeView.Items[i].Text = Value then
      begin
        Result := TreeView.Items[i];
        Break;
      end;
end;

function TreeNodeGetChildFromIndex(TreeView: TTreeView; ParentNode: TTreeNode; ChildIndex: Integer): TTreeNode;
var _Child: TTreeNode;
begin
  Result := Nil;

  if ParentNode = nil
  then _Child := TreeView.Items[0]
  else _Child := ParentNode.getFirstChild;

  while (_Child <> nil) and (Result = nil) do
    if _Child.Index = ChildIndex
    then Result := _Child
    else _Child := _Child.GetNextChild(_Child);
end;

function TreeNodeGetParentOnLevel(ChildNode: TTreeNode; ParentLevel: Integer): TTreeNode;
begin
  Result := Nil;

  if ChildNode <> nil then
  begin
    while (ChildNode.Level <> ParentLevel) and (ChildNode.Level > 0) do
      ChildNode := ChildNode.Parent;

    if ChildNode.Level = ParentLevel then
      Result := ChildNode;
  end;
end;

function TreeNodeSelectionIndex(Node: TTreeNode): Integer;
var
  s: Integer;
begin
  Result := -1;

  for s := 0 to Node.TreeView.SelectionCount - 1 do
    if Node.TreeView.Selections[s] = Node then
    begin
      Result := s;
      Break;
    end;
end;

procedure TreeNodeCopy(FromNode: TTreeNode; ToNode: TTreeNode; const CopyChildren: Boolean = true; const CopySubChildren: Boolean = true);
var
  Child: TTreeNode;
begin
  ToNode.ImageIndex    := FromNode.ImageIndex;
  ToNode.SelectedIndex := FromNode.SelectedIndex;
  ToNode.StateIndex    := FromNode.StateIndex;
  ToNode.Text          := FromNode.Text;

  if CopyChildren and FromNode.HasChildren then
  begin
    Child := FromNode.GetFirstChild;

    while Child <> Nil do
    begin
      if CopySubChildren then
        TreeNodeCopy(Child, ToNode.Owner.AddChild(ToNode, ''), true, true);

      Child := FromNode.GetNextChild(Child);
    end;
  end;
end;

procedure TreeNodeMoveBeforeSibling(aNode, BeforeNode: TTreeNode);
begin
  aNode.MoveTo(BeforeNode, naInsert); // Node will be moved into same level
end;

procedure TreeNodeMoveAfterSibling(aNode, AfterNode: TTreeNode);
var
  NextNode: TTreeNode;
begin
  // Find next node after AfterNode in same level :
  NextNode := AfterNode.getNextSibling;   // Next node in same level !

  if NextNode = Nil
  then aNode.MoveTo(AfterNode, naAdd)
  else aNode.MoveTo(NextNode, naInsert);  // Node will be moved into same level
end;

function GetTopMostControlAtPos(FromControl: TWinControl; aControlPoint: TPoint): TControl;
var FoundControl: TControl;
begin
  RESULT := nil;

  while FromControl <> nil do
  begin
    FoundControl := FromControl.ControlAtPos(aControlPoint, True, true);

    if FoundControl <> nil
    then begin
      RESULT := FoundControl;

      if FoundControl is TWinControl
      then begin
        aControlPoint := FromControl.ClientToScreen(aControlPoint);
        FromControl := TWinControl(RESULT);
        aControlPoint := FromControl.ScreenToClient(aControlPoint);
      end
      else
        FromControl := Nil;
    end
    else
      FromControl := nil;
  end;
end;

function GetLastParent(aControl: TControl): TWinControl;
begin
  Result := aControl.Parent;

  if Result <> Nil then
    while Result.Parent <> nil do
      Result := RESULT.Parent;
end;

procedure CenterControl(aControl: TControl);
begin
  if aControl.Parent = nil then
  begin
    aControl.Left := (Screen.Width - aControl.Width) Div 2;
    aControl.Top  := (Screen.Height - aControl.Height) Div 2;
  end
  else begin
    aControl.Left := (aControl.Parent.Width - aControl.Width) Div 2;
    aControl.Top  := (aControl.Parent.Height - aControl.Height) Div 2;
  end;
end;

function MouseIsOverControl(aControl: TControl): Boolean;
var
  pt: TPoint;
begin
  Result := True;

  GetCursorPos(pt);
  Pt := aControl.ScreenToClient(Pt);

  if Result then
    if Pt.X >= 0
    then Result := Pt.X < aControl.Width
    else Result := false;

  if Result then
    if Pt.Y >= 0
    then Result := Pt.Y < aControl.Height
    else Result := false;
end;

end.
