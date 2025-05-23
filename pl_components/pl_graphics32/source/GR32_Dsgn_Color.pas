{*************************************************************************
                PilotLogic Software House

  Package pl_Graphics32
  This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)

 ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * https://www.mozilla.org/en-US/MPL/1.1/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All rights reserved
 *
 * ***** END LICENSE BLOCK *****
 ************************************************************************}

unit GR32_Dsgn_Color;

interface

{$I GR32.inc}

uses
  Classes, SysUtils,
  RTLConsts, LazIDEIntf, PropEdits, Graphics, Dialogs, Forms,
  controls,
  GR32_Dsgn_ColorPicker,
  GR32, GR32_Image;

type
  { TColorManager }
  PColorEntry = ^TColorEntry;
  TColorEntry = record
    Name: string[31];
    Color: TColor32;
  end;

  TColorManager = class(TList)
  public
    destructor Destroy; override;
    procedure AddColor(const AName: string; AColor: TColor32);
    procedure EnumColors(Proc: TGetStrProc);
    function  FindColor(const AName: string): TColor32;
    function  GetColor(const AName: string): TColor32;
    function  GetColorName(AColor: TColor32): string;
    procedure RegisterDefaultColors;
    procedure RemoveColor(const AName: string);
  end;

//--- ct9999 --------------------------
  TColor32Dialog = class(TCommonDialog)
  private
    FColor: TColor32;
    FCustomColors: TStrings;
    procedure SetCustomColors(Value: TStrings);
  public
    function Execute: boolean; override;
  published
    property Color: TColor32 read FColor write FColor default clWhite32;
    property CustomColors: TStrings read FCustomColors write SetCustomColors;
  end;
//----------------------------------------

  { TColor32Property }
  TColor32Property = class(TIntegerProperty
{$IFDEF EXT_PROP_EDIT}
    , ICustomPropertyListDrawing, ICustomPropertyDrawing
    {$IFDEF COMPILER2005_UP}, ICustomPropertyDrawing80{$ENDIF}
{$ENDIF}
  )
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
{$IFDEF EXT_PROP_EDIT}
    procedure Edit; override;
    { ICustomPropertyListDrawing }
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    { ICustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  {$IFDEF COMPILER2005_UP}
    { ICustomPropertyDrawing80 }
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
  {$ENDIF}
{$ENDIF}
  end;

procedure RegisterColor(const AName: string; AColor: TColor32);
procedure UnregisterColor(const AName: string);

var ColorManager: TColorManager;

implementation

{ TColorManager }

destructor TColorManager.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do FreeMem(Items[I], SizeOf(TColorEntry));
  inherited;
end;

procedure TColorManager.AddColor(const AName: string; AColor: TColor32);
var
  NewEntry: PColorEntry;
begin
  New(NewEntry);
  if NewEntry = nil then
    raise Exception.Create('Could not allocate memory for color registration!');
  with NewEntry^ do
  begin
    Name := ShortString(AName);
    Color := AColor;
  end;
  Add(NewEntry);
end;

procedure TColorManager.EnumColors(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do Proc(string(TColorEntry(Items[I]^).Name));
end;

function TColorManager.FindColor(const AName: string): TColor32;
var
  I: Integer;
begin
  Result := clBlack32;
  for I := 0 to Count - 1 do
    with TColorEntry(Items[I]^) do
      if string(Name) = AName then
      begin
        Result := Color;
        Break;
      end;
end;

function TColorManager.GetColor(const AName: string): TColor32;
var
  S: string;

  function HexToClr(const HexStr: string): Cardinal;
  var
    I: Integer;
    C: Char;
  begin
    Result := 0;
    for I := 1 to Length(HexStr) do
    begin
      C := HexStr[I];
      case C of
        '0'..'9': Result := Int64(16) * Result + (Ord(C) - $30);
        'A'..'F': Result := Int64(16) * Result + (Ord(C) - $37);
        'a'..'f': Result := Int64(16) * Result + (Ord(C) - $57);
      else
        raise EConvertError.Create('Illegal character in hex string');
      end;
    end;
  end;

begin
  S := Trim(AName);
  if S[1] = '$' then S := Copy(S, 2, Length(S) - 1);
  if (S[1] = 'c') and (S[2] = 'l') then Result := FindColor(S)
  else
  try
    Result := HexToClr(S);
  except
    Result := clBlack32;
  end;
end;

function TColorManager.GetColorName(AColor: TColor32): string;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with TColorEntry(Items[I]^) do
      if Color = AColor then
      begin
        Result := string(TColorEntry(Items[I]^).Name);
        Exit;
      end;
  Result := '$' + IntToHex(AColor, 8);
end;

procedure TColorManager.RegisterDefaultColors;
begin
  Capacity := 50;
  AddColor('clBlack32',                clBlack32);
  AddColor('clDimGray32',              clDimGray32);
  AddColor('clGray32',                 clGray32);
  AddColor('clLightGray32',            clLightGray32);
  AddColor('clWhite32',                clWhite32);
  AddColor('clMaroon32',               clMaroon32);
  AddColor('clGreen32',                clGreen32);
  AddColor('clOlive32',                clOlive32);
  AddColor('clNavy32',                 clNavy32);
  AddColor('clPurple32',               clPurple32);
  AddColor('clTeal32',                 clTeal32);
  AddColor('clRed32',                  clRed32);
  AddColor('clLime32',                 clLime32);
  AddColor('clYellow32',               clYellow32);
  AddColor('clBlue32',                 clBlue32);
  AddColor('clFuchsia32',              clFuchsia32);
  AddColor('clAqua32',                 clAqua32);

  AddColor('clTrWhite32',              clTrWhite32);
  AddColor('clTrBlack32',              clTrBlack32);
  AddColor('clTrRed32',                clTrRed32);
  AddColor('clTrGreen32',              clTrGreen32);
  AddColor('clTrBlue32',               clTrBlue32);

  AddColor('clAliceBlue32',            clAliceBlue32);
  AddColor('clAntiqueWhite32',         clAntiqueWhite32);
  AddColor('clAquamarine32',           clAquamarine32);
  AddColor('clAzure32',                clAzure32);
  AddColor('clBeige32',                clBeige32);
  AddColor('clBisque32',               clBisque32);
  AddColor('clBlancheDalmond32',       clBlancheDalmond32);
  AddColor('clBlueViolet32',           clBlueViolet32);
  AddColor('clBrown32',                clBrown32);
  AddColor('clBurlyWood32',            clBurlyWood32);
  AddColor('clCadetblue32',            clCadetblue32);
  AddColor('clChartReuse32',           clChartReuse32);
  AddColor('clChocolate32',            clChocolate32);
  AddColor('clCoral32',                clCoral32);
  AddColor('clCornFlowerBlue32',       clCornFlowerBlue32);
  AddColor('clCornSilk32',             clCornSilk32);
  AddColor('clCrimson32',              clCrimson32);
  AddColor('clDarkBlue32',             clDarkBlue32);
  AddColor('clDarkCyan32',             clDarkCyan32);
  AddColor('clDarkGoldenRod32',        clDarkGoldenRod32);
  AddColor('clDarkGray32',             clDarkGray32);
  AddColor('clDarkGreen32',            clDarkGreen32);
  AddColor('clDarkGrey32',             clDarkGrey32);
  AddColor('clDarkKhaki32',            clDarkKhaki32);
  AddColor('clDarkMagenta32',          clDarkMagenta32);
  AddColor('clDarkOliveGreen32',       clDarkOliveGreen32);
  AddColor('clDarkOrange32',           clDarkOrange32);
  AddColor('clDarkOrchid32',           clDarkOrchid32);
  AddColor('clDarkRed32',              clDarkRed32);
  AddColor('clDarkSalmon32',           clDarkSalmon32);
  AddColor('clDarkSeaGreen32',         clDarkSeaGreen32);
  AddColor('clDarkSlateBlue32',        clDarkSlateBlue32);
  AddColor('clDarkSlateGray32',        clDarkSlateGray32);
  AddColor('clDarkSlateGrey32',        clDarkSlateGrey32);
  AddColor('clDarkTurquoise32',        clDarkTurquoise32);
  AddColor('clDarkViolet32',           clDarkViolet32);
  AddColor('clDeepPink32',             clDeepPink32);
  AddColor('clDeepSkyBlue32',          clDeepSkyBlue32);
  AddColor('clDodgerBlue32',           clDodgerBlue32);
  AddColor('clFireBrick32',            clFireBrick32);
  AddColor('clFloralWhite32',          clFloralWhite32);
  AddColor('clGainsBoro32',            clGainsBoro32);
  AddColor('clGhostWhite32',           clGhostWhite32);
  AddColor('clGold32',                 clGold32);
  AddColor('clGoldenRod32',            clGoldenRod32);
  AddColor('clGreenYellow32',          clGreenYellow32);
  AddColor('clGrey32',                 clGrey32);
  AddColor('clHoneyDew32',             clHoneyDew32);
  AddColor('clHotPink32',              clHotPink32);
  AddColor('clIndianRed32',            clIndianRed32);
  AddColor('clIndigo32',               clIndigo32);
  AddColor('clIvory32',                clIvory32);
  AddColor('clKhaki32',                clKhaki32);
  AddColor('clLavender32',             clLavender32);
  AddColor('clLavenderBlush32',        clLavenderBlush32);
  AddColor('clLawnGreen32',            clLawnGreen32);
  AddColor('clLemonChiffon32',         clLemonChiffon32);
  AddColor('clLightBlue32',            clLightBlue32);
  AddColor('clLightCoral32',           clLightCoral32);
  AddColor('clLightCyan32',            clLightCyan32);
  AddColor('clLightGoldenRodYellow32', clLightGoldenRodYellow32);
  AddColor('clLightGray32',            clLightGray32);
  AddColor('clLightGreen32',           clLightGreen32);
  AddColor('clLightGrey32',            clLightGrey32);
  AddColor('clLightPink32',            clLightPink32);
  AddColor('clLightSalmon32',          clLightSalmon32);
  AddColor('clLightSeagreen32',        clLightSeagreen32);
  AddColor('clLightSkyblue32',         clLightSkyblue32);
  AddColor('clLightSlategray32',       clLightSlategray32);
  AddColor('clLightSlategrey32',       clLightSlategrey32);
  AddColor('clLightSteelblue32',       clLightSteelblue32);
  AddColor('clLightYellow32',          clLightYellow32);
  AddColor('clLtGray32',               clLtGray32);
  AddColor('clMedGray32',              clMedGray32);
  AddColor('clDkGray32',               clDkGray32);
  AddColor('clMoneyGreen32',           clMoneyGreen32);
  AddColor('clLegacySkyBlue32',        clLegacySkyBlue32);
  AddColor('clCream32',                clCream32);
  AddColor('clLimeGreen32',            clLimeGreen32);
  AddColor('clLinen32',                clLinen32);
  AddColor('clMediumAquamarine32',     clMediumAquamarine32);
  AddColor('clMediumBlue32',           clMediumBlue32);
  AddColor('clMediumOrchid32',         clMediumOrchid32);
  AddColor('clMediumPurple32',         clMediumPurple32);
  AddColor('clMediumSeaGreen32',       clMediumSeaGreen32);
  AddColor('clMediumSlateBlue32',      clMediumSlateBlue32);
  AddColor('clMediumSpringGreen32',    clMediumSpringGreen32);
  AddColor('clMediumTurquoise32',      clMediumTurquoise32);
  AddColor('clMediumVioletRed32',      clMediumVioletRed32);
  AddColor('clMidnightBlue32',         clMidnightBlue32);
  AddColor('clMintCream32',            clMintCream32);
  AddColor('clMistyRose32',            clMistyRose32);
  AddColor('clMoccasin32',             clMoccasin32);
  AddColor('clNavajoWhite32',          clNavajoWhite32);
  AddColor('clOldLace32',              clOldLace32);
  AddColor('clOliveDrab32',            clOliveDrab32);
  AddColor('clOrange32',               clOrange32);
  AddColor('clOrangeRed32',            clOrangeRed32);
  AddColor('clOrchid32',               clOrchid32);
  AddColor('clPaleGoldenRod32',        clPaleGoldenRod32);
  AddColor('clPaleGreen32',            clPaleGreen32);
  AddColor('clPaleTurquoise32',        clPaleTurquoise32);
  AddColor('clPaleVioletred32',        clPaleVioletred32);
  AddColor('clPapayaWhip32',           clPapayaWhip32);
  AddColor('clPeachPuff32',            clPeachPuff32);
  AddColor('clPeru32',                 clPeru32);
  AddColor('clPlum32',                 clPlum32);
  AddColor('clPowderBlue32',           clPowderBlue32);
  AddColor('clPurple32',               clPurple32);
  AddColor('clRosyBrown32',            clRosyBrown32);
  AddColor('clRoyalBlue32',            clRoyalBlue32);
  AddColor('clSaddleBrown32',          clSaddleBrown32);
  AddColor('clSalmon32',               clSalmon32);
  AddColor('clSandyBrown32',           clSandyBrown32);
  AddColor('clSeaGreen32',             clSeaGreen32);
  AddColor('clSeaShell32',             clSeaShell32);
  AddColor('clSienna32',               clSienna32);
  AddColor('clSilver32',               clSilver32);
  AddColor('clSkyblue32',              clSkyblue32);
  AddColor('clSlateBlue32',            clSlateBlue32);
  AddColor('clSlateGray32',            clSlateGray32);
  AddColor('clSlateGrey32',            clSlateGrey32);
  AddColor('clSnow32',                 clSnow32);
  AddColor('clSpringgreen32',          clSpringgreen32);
  AddColor('clSteelblue32',            clSteelblue32);
  AddColor('clTan32',                  clTan32);
  AddColor('clThistle32',              clThistle32);
  AddColor('clTomato32',               clTomato32);
  AddColor('clTurquoise32',            clTurquoise32);
  AddColor('clViolet32',               clViolet32);
  AddColor('clWheat32',                clWheat32);
  AddColor('clWhitesmoke32',           clWhitesmoke32);
  AddColor('clYellowgreen32',          clYellowgreen32);
end;

procedure TColorManager.RemoveColor(const AName: string);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if CompareText(string(TColorEntry(Items[I]^).Name), AName) = 0 then
    begin
      Delete(I);
      Break;
    end;
end;

procedure RegisterColor(const AName: string; AColor: TColor32);
begin
  ColorManager.AddColor(AName, AColor);
end;

procedure UnregisterColor(const AName: string);
begin
  ColorManager.RemoveColor(AName);
end;


{ TColor32Dialog }
//--- ct9999 --------------------------
procedure TColor32Dialog.SetCustomColors(Value: TStrings);
begin
  FCustomColors.Assign(Value);
end;

function TColor32Dialog.Execute: boolean ;
var
  ColorPicker: TFormColorPicker;
begin
  ColorPicker := TFormColorPicker.Create(nil);
  try
    ColorPicker.Color := FColor;
    Result := ColorPicker.ShowModal = mrOK;
    if Result then
      FColor := ColorPicker.Color;
  finally
    ColorPicker.Free;
  end;
end;
//------------------------------------------

{ TColor32Property }

{$IFDEF EXT_PROP_EDIT}
procedure TColor32Property.Edit;
var
{$IFDEF COMPILER2010_UP}
  ColorDialog: TColor32Dialog;
{$ELSE}
  ColorDialog: TColorDialog;
{$ENDIF}
  IniFile: TRegIniFile;

  procedure GetCustomColors;
  begin
    if BaseRegistryKey = '' then Exit;
    IniFile := TRegIniFile.Create(BaseRegistryKey);
    try
      IniFile.ReadSectionValues(SCustomColors, ColorDialog.CustomColors);
    except
      { Ignore errors while reading values }
    end;
  end;

  procedure SaveCustomColors;
  var
    I, P: Integer;
    S: string;
  begin
    if IniFile <> nil then
      with ColorDialog do
        for I := 0 to CustomColors.Count - 1 do
        begin
          S := CustomColors.Strings[I];
          P := Pos('=', S);
          if P <> 0 then
          begin
            S := Copy(S, 1, P - 1);
            IniFile.WriteString(SCustomColors, S, CustomColors.Values[S]);
          end;
        end;
  end;

begin
  IniFile := nil;
{$IFDEF COMPILER2010_UP}
  ColorDialog := TColor32Dialog.Create(Application);
{$ELSE}
  ColorDialog := TColorDialog.Create(Application);
{$ENDIF}
  try
    GetCustomColors;
    ColorDialog.Color := GetOrdValue;
    ColorDialog.HelpContext := 25010;
{$IFDEF COMPILER2010_UP}
    ColorDialog.Options := [cdShowHelp];
{$ENDIF}
    if ColorDialog.Execute then
      SetOrdValue(Cardinal(ColorDialog.Color));
    SaveCustomColors;
  finally
    IniFile.Free;
    ColorDialog.Free;
  end;
end;
{$ENDIF}

function TColor32Property.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, {$IFDEF EXT_PROP_EDIT}paDialog,{$ENDIF} paValueList,
    paRevertable];
end;

procedure TColor32Property.GetValues(Proc: TGetStrProc);
begin
  try
    ColorManager.EnumColors(Proc);
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

function TColor32Property.GetValue: string;
begin
  try
    Result := ColorManager.GetColorName(Cardinal(GetOrdValue));
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

procedure TColor32Property.SetValue(const Value: string);
begin
  try
    SetOrdValue(Cardinal(ColorManager.GetColor(Value)));
    Modified;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

{$IFDEF EXT_PROP_EDIT}

procedure TColor32Property.ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
begin
  // implementation dummie to satisfy interface. Don't change default value.
end;

procedure TColor32Property.ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
begin
  // implementation dummie to satisfy interface. Don't change default value.
end;

procedure TColor32Property.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  Right: Integer;
  C: TColor32;
  i, j: Integer;
  W, H: Integer;
  Bitmap32: TBitmap32;
begin
  try
    Right := (ARect.Bottom - ARect.Top) + ARect.Left;
    Bitmap32 := TBitmap32.Create;
    try
      W := Right - ARect.Left - 2;
      H := ARect.Bottom - ARect.Top - 2;
      Bitmap32.SetSize(W, H);
      if Assigned(ColorManager) then
        C := ColorManager.GetColor(Value)
      else
        C := clWhite32;
      if (W > 8) and (H > 8) then
      begin
        if not (C and $FF000000 = $FF000000) then
        begin
          for j := 0 to H - 1 do
            for i := 0 to W - 1 do
              if Odd(i div 3) = Odd(j div 3) then
                Bitmap32[i, j] := clBlack32
              else
                Bitmap32[i, j] := clWhite32;
        end;
        Bitmap32.FillRectT(0, 0, W, H, C);
      end;
      Bitmap32.FrameRectTS(0, 0, W, H, $DF000000);
      Bitmap32.RaiseRectTS(1, 1, W - 1, H - 1, 20);
      Bitmap32.DrawTo(ACanvas.Handle, ARect.Left + 1, ARect.Top + 1);
    finally
      Bitmap32.Free;
      DefaultPropertyListDrawValue(Value, ACanvas,
        Rect(Right, ARect.Top, ARect.Right, ARect.Bottom), ASelected);
    end;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

procedure TColor32Property.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, True{ASelected})
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TColor32Property.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

{$IFDEF COMPILER2005_UP}
function TColor32Property.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result := ARect;
end;

function TColor32Property.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left, ARect.Top, (ARect.Bottom - ARect.Top) + ARect.Left, ARect.Bottom);
end;
{$ENDIF}

{$ENDIF}


initialization
  ColorManager := TColorManager.Create;
  ColorManager.RegisterDefaultColors;

finalization
  ColorManager.Free;

end.
