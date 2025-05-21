{***************************************************************************
               Copyright (c) PilotLogic Software House
                       All rights reserved
 
   MagicScript Import File for Library Graphics32
   Build from : GR32.pas file
   This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
 ***************************************************************************}
 
Unit msrtti_GR32;
 
{$MODE DELPHI}{$H+}
 
Interface
Uses
 Classes,
 Controls,
 GR32_AllStrings,
 Graphics,
 IntfGraphics,
 LCLIntf,
 LCLType,
 SysUtils,
 Types,
 GR32,
 mscoreengine;

Type
 TmscrRTTILibrary_GR32=class(TComponent);

Implementation

Type
 TLibrary_GR32=class(TmscrRTTILibrary)
  private
    function  CallMethod(aInstance: TObject; aClassType: TClass; const aMethodName: string; aCallVar: TmscrVarForMethod): variant;
    procedure RegisterConsts(AScript: TmscrScript);
    procedure RegisterTypes(AScript: TmscrScript);
    procedure RegisterProcs(AScript: TmscrScript);
    procedure RegisterClasses(AScript: TmscrScript);
    function  ProcCallMethod(aInstance: TObject; aClassType: TClass; const aMethodName: string; aCallVar: TmscrVarForMethod): variant;
    function  ClassCallMethod(aInstance: TObject; aClassType: TClass; const aMethodName: string; aCallVar: TmscrVarForMethod): variant;
    function  ClassGetProp(aInstance: TObject; aClassType: TClass; const aPropName: String; aCallVar: TmscrEngProperty): Variant;
    procedure ClassSetProp(aInstance: TObject; aClassType: TClass; const aPropName: String; aValue: Variant; aCallVar: TmscrEngProperty);
  public
    constructor Create(AScript: TmscrScript); override;
  end;

constructor TLibrary_GR32.Create(AScript: TmscrScript);
begin
  inherited Create(AScript);

  RegisterConsts(AScript);
  RegisterTypes(AScript);
  RegisterProcs(AScript);
  RegisterClasses(AScript);
end;

function TLibrary_GR32.CallMethod(aInstance: TObject; aClassType: TClass; const aMethodName: string; aCallVar: TmscrVarForMethod): variant;
begin
  Result := 0;
  if aInstance=NIL then
   begin
    Result:=ProcCallMethod(aInstance, aClassType, aMethodName, aCallVar);
    if Result <> 0 then Exit;
   end;
  if aInstance<>NIL then
    Result:=ClassCallMethod(aInstance, aClassType, aMethodName, aCallVar);
end;
 
procedure TLibrary_GR32.RegisterConsts(AScript: TmscrScript);
begin
  with AScript do
  begin

     Register_Type('TColor32',msvtInt64);  //==== SOS must register here ===============

     Register_Const('clAliceBlue32','TColor32',clAliceBlue32);
     Register_Const('clAntiqueWhite32','TColor32',clAntiqueWhite32);
     Register_Const('clAqua32','TColor32',clAqua32);
     Register_Const('clAquamarine32','TColor32',clAquamarine32);
     Register_Const('clAzure32','TColor32',clAzure32);
     Register_Const('clBeige32','TColor32',clBeige32);
     Register_Const('clBisque32','TColor32',clBisque32);
     Register_Const('clBlack32','TColor32',clBlack32);
     Register_Const('clBlancheDalmond32','TColor32',clBlancheDalmond32);
     Register_Const('clBlue32','TColor32',clBlue32);
     Register_Const('clBlueViolet32','TColor32',clBlueViolet32);
     Register_Const('clBrown32','TColor32',clBrown32);
     Register_Const('clBurlyWood32','TColor32',clBurlyWood32);
     Register_Const('clCadetblue32','TColor32',clCadetblue32);
     Register_Const('clChartReuse32','TColor32',clChartReuse32);
     Register_Const('clChocolate32','TColor32',clChocolate32);
     Register_Const('clCoral32','TColor32',clCoral32);
     Register_Const('clCornFlowerBlue32','TColor32',clCornFlowerBlue32);
     Register_Const('clCornSilk32','TColor32',clCornSilk32);
     Register_Const('clCream32','TColor32',clCream32);
     Register_Const('clCrimson32','TColor32',clCrimson32);
     Register_Const('clDarkBlue32','TColor32',clDarkBlue32);
     Register_Const('clDarkCyan32','TColor32',clDarkCyan32);
     Register_Const('clDarkGoldenRod32','TColor32',clDarkGoldenRod32);
     Register_Const('clDarkGray32','TColor32',clDarkGray32);
     Register_Const('clDarkGreen32','TColor32',clDarkGreen32);
     Register_Const('clDarkGrey32','TColor32',clDarkGrey32);
     Register_Const('clDarkKhaki32','TColor32',clDarkKhaki32);
     Register_Const('clDarkMagenta32','TColor32',clDarkMagenta32);
     Register_Const('clDarkOliveGreen32','TColor32',clDarkOliveGreen32);
     Register_Const('clDarkOrange32','TColor32',clDarkOrange32);
     Register_Const('clDarkOrchid32','TColor32',clDarkOrchid32);
     Register_Const('clDarkRed32','TColor32',clDarkRed32);
     Register_Const('clDarkSalmon32','TColor32',clDarkSalmon32);
     Register_Const('clDarkSeaGreen32','TColor32',clDarkSeaGreen32);
     Register_Const('clDarkSlateBlue32','TColor32',clDarkSlateBlue32);
     Register_Const('clDarkSlateGray32','TColor32',clDarkSlateGray32);
     Register_Const('clDarkSlateGrey32','TColor32',clDarkSlateGrey32);
     Register_Const('clDarkTurquoise32','TColor32',clDarkTurquoise32);
     Register_Const('clDarkViolet32','TColor32',clDarkViolet32);
     Register_Const('clDeepPink32','TColor32',clDeepPink32);
     Register_Const('clDeepSkyBlue32','TColor32',clDeepSkyBlue32);
     Register_Const('clDimGray32','TColor32',clDimGray32);
     Register_Const('clDkGray32','TColor32',clDkGray32);
     Register_Const('clDodgerBlue32','TColor32',clDodgerBlue32);
     Register_Const('clFireBrick32','TColor32',clFireBrick32);
     Register_Const('clFloralWhite32','TColor32',clFloralWhite32);
     Register_Const('clFuchsia32','TColor32',clFuchsia32);
     Register_Const('clGainsBoro32','TColor32',clGainsBoro32);
     Register_Const('clGhostWhite32','TColor32',clGhostWhite32);
     Register_Const('clGold32','TColor32',clGold32);
     Register_Const('clGoldenRod32','TColor32',clGoldenRod32);
     Register_Const('clGray32','TColor32',clGray32);
     Register_Const('clGreen32','TColor32',clGreen32);
     Register_Const('clGreenYellow32','TColor32',clGreenYellow32);
     Register_Const('clGrey32','TColor32',clGrey32);
     Register_Const('clHoneyDew32','TColor32',clHoneyDew32);
     Register_Const('clHotPink32','TColor32',clHotPink32);
     Register_Const('clIndianRed32','TColor32',clIndianRed32);
     Register_Const('clIndigo32','TColor32',clIndigo32);
     Register_Const('clIvory32','TColor32',clIvory32);
     Register_Const('clKhaki32','TColor32',clKhaki32);
     Register_Const('clLavender32','TColor32',clLavender32);
     Register_Const('clLavenderBlush32','TColor32',clLavenderBlush32);
     Register_Const('clLawnGreen32','TColor32',clLawnGreen32);
     Register_Const('clLegacySkyBlue32','TColor32',clLegacySkyBlue32);
     Register_Const('clLemonChiffon32','TColor32',clLemonChiffon32);
     Register_Const('clLightBlue32','TColor32',clLightBlue32);
     Register_Const('clLightCoral32','TColor32',clLightCoral32);
     Register_Const('clLightCyan32','TColor32',clLightCyan32);
     Register_Const('clLightGoldenRodYellow32','TColor32',clLightGoldenRodYellow32);
     Register_Const('clLightGray32','TColor32',clLightGray32);
     Register_Const('clLightGreen32','TColor32',clLightGreen32);
     Register_Const('clLightGrey32','TColor32',clLightGrey32);
     Register_Const('clLightPink32','TColor32',clLightPink32);
     Register_Const('clLightSalmon32','TColor32',clLightSalmon32);
     Register_Const('clLightSeagreen32','TColor32',clLightSeagreen32);
     Register_Const('clLightSkyblue32','TColor32',clLightSkyblue32);
     Register_Const('clLightSlategray32','TColor32',clLightSlategray32);
     Register_Const('clLightSlategrey32','TColor32',clLightSlategrey32);
     Register_Const('clLightSteelblue32','TColor32',clLightSteelblue32);
     Register_Const('clLightYellow32','TColor32',clLightYellow32);
     Register_Const('clLime32','TColor32',clLime32);
     Register_Const('clLimeGreen32','TColor32',clLimeGreen32);
     Register_Const('clLinen32','TColor32',clLinen32);
     Register_Const('clLtGray32','TColor32',clLtGray32);
     Register_Const('clMaroon32','TColor32',clMaroon32);
     Register_Const('clMedGray32','TColor32',clMedGray32);
     Register_Const('clMediumAquamarine32','TColor32',clMediumAquamarine32);
     Register_Const('clMediumBlue32','TColor32',clMediumBlue32);
     Register_Const('clMediumOrchid32','TColor32',clMediumOrchid32);
     Register_Const('clMediumPurple32','TColor32',clMediumPurple32);
     Register_Const('clMediumSeaGreen32','TColor32',clMediumSeaGreen32);
     Register_Const('clMediumSlateBlue32','TColor32',clMediumSlateBlue32);
     Register_Const('clMediumSpringGreen32','TColor32',clMediumSpringGreen32);
     Register_Const('clMediumTurquoise32','TColor32',clMediumTurquoise32);
     Register_Const('clMediumVioletRed32','TColor32',clMediumVioletRed32);
     Register_Const('clMidnightBlue32','TColor32',clMidnightBlue32);
     Register_Const('clMintCream32','TColor32',clMintCream32);
     Register_Const('clMistyRose32','TColor32',clMistyRose32);
     Register_Const('clMoccasin32','TColor32',clMoccasin32);
     Register_Const('clMoneyGreen32','TColor32',clMoneyGreen32);
     Register_Const('clNavajoWhite32','TColor32',clNavajoWhite32);
     Register_Const('clNavy32','TColor32',clNavy32);
     Register_Const('clOldLace32','TColor32',clOldLace32);
     Register_Const('clOlive32','TColor32',clOlive32);
     Register_Const('clOliveDrab32','TColor32',clOliveDrab32);
     Register_Const('clOrange32','TColor32',clOrange32);
     Register_Const('clOrangeRed32','TColor32',clOrangeRed32);
     Register_Const('clOrchid32','TColor32',clOrchid32);
     Register_Const('clPaleGoldenRod32','TColor32',clPaleGoldenRod32);
     Register_Const('clPaleGreen32','TColor32',clPaleGreen32);
     Register_Const('clPaleTurquoise32','TColor32',clPaleTurquoise32);
     Register_Const('clPaleVioletred32','TColor32',clPaleVioletred32);
     Register_Const('clPapayaWhip32','TColor32',clPapayaWhip32);
     Register_Const('clPeachPuff32','TColor32',clPeachPuff32);
     Register_Const('clPeru32','TColor32',clPeru32);
     Register_Const('clPlum32','TColor32',clPlum32);
     Register_Const('clPowderBlue32','TColor32',clPowderBlue32);
     Register_Const('clPurple32','TColor32',clPurple32);
     Register_Const('clRed32','TColor32',clRed32);
     Register_Const('clRosyBrown32','TColor32',clRosyBrown32);
     Register_Const('clRoyalBlue32','TColor32',clRoyalBlue32);
     Register_Const('clSaddleBrown32','TColor32',clSaddleBrown32);
     Register_Const('clSalmon32','TColor32',clSalmon32);
     Register_Const('clSandyBrown32','TColor32',clSandyBrown32);
     Register_Const('clSeaGreen32','TColor32',clSeaGreen32);
     Register_Const('clSeaShell32','TColor32',clSeaShell32);
     Register_Const('clSienna32','TColor32',clSienna32);
     Register_Const('clSilver32','TColor32',clSilver32);
     Register_Const('clSkyblue32','TColor32',clSkyblue32);
     Register_Const('clSlateBlue32','TColor32',clSlateBlue32);
     Register_Const('clSlateGray32','TColor32',clSlateGray32);
     Register_Const('clSlateGrey32','TColor32',clSlateGrey32);
     Register_Const('clSnow32','TColor32',clSnow32);
     Register_Const('clSpringgreen32','TColor32',clSpringgreen32);
     Register_Const('clSteelblue32','TColor32',clSteelblue32);
     Register_Const('clTan32','TColor32',clTan32);
     Register_Const('clTeal32','TColor32',clTeal32);
     Register_Const('clThistle32','TColor32',clThistle32);
     Register_Const('clTomato32','TColor32',clTomato32);
     Register_Const('clTrBlack32','TColor32',clTrBlack32);
     Register_Const('clTrBlue32','TColor32',clTrBlue32);
     Register_Const('clTrGray32','TColor32',clTrGray32);
     Register_Const('clTrGreen32','TColor32',clTrGreen32);
     Register_Const('clTrRed32','TColor32',clTrRed32);
     Register_Const('clTrWhite32','TColor32',clTrWhite32);
     Register_Const('clTurquoise32','TColor32',clTurquoise32);
     Register_Const('clViolet32','TColor32',clViolet32);
     Register_Const('clWheat32','TColor32',clWheat32);
     Register_Const('clWhite32','TColor32',clWhite32);
     Register_Const('clWhitesmoke32','TColor32',clWhitesmoke32);
     Register_Const('clYellow32','TColor32',clYellow32);
     Register_Const('clYellowgreen32','TColor32',clYellowgreen32);

  end;
end; 

procedure TLibrary_GR32.RegisterProcs(AScript: TmscrScript);
begin
  with AScript do
  begin                                                                    
    Register_Method('function Color32(R, G, B: Byte; A: Byte = $FF): TColor32;', ProcCallMethod, 'Graphics32');
    Register_Method('function Color32Win(WinColor: TColor): TColor32;', ProcCallMethod, 'Graphics32');
    Register_Method('function AlphaComponent(Color32: TColor32): Integer;', ProcCallMethod, 'Graphics32');      
    Register_Method('function BlueComponent(Color32: TColor32): Integer;', ProcCallMethod, 'Graphics32');     
    Register_Method('function GreenComponent(Color32: TColor32): Integer;', ProcCallMethod, 'Graphics32');    
    Register_Method('function RedComponent(Color32: TColor32): Integer;', ProcCallMethod, 'Graphics32');
    Register_Method('function ArrayOfColor32(Colors: array of TColor32): TArrayOfColor32;', ProcCallMethod, 'Graphics32');
    Register_Method('function Gray32(Intensity: Byte; Alpha: Byte = $FF): TColor32;', ProcCallMethod, 'Graphics32');
    Register_Method('function HSVtoRGB(H, S, V: Single): TColor32;', ProcCallMethod, 'Graphics32');
    Register_Method('function Intensity(Color32: TColor32): Integer;', ProcCallMethod, 'Graphics32');
    Register_Method('function InvertColor(Color32: TColor32): TColor32;', ProcCallMethod, 'Graphics32');
    Register_Method('procedure ScaleAlpha(var Color32: TColor32; Scale: Single);', ProcCallMethod, 'Graphics32');
    Register_Method('function SetAlpha(Color32: TColor32; NewAlpha: Integer): TColor32;', ProcCallMethod, 'Graphics32');
    Register_Method('function WinColor(Color32: TColor32): TColor;', ProcCallMethod, 'Graphics32');   
    Register_Method('procedure Color32ToRGB(Color32: TColor32; var R, G, B: Byte);', ProcCallMethod, 'Graphics32');
    Register_Method('procedure Color32ToRGBA(Color32: TColor32; var R, G, B, A: Byte);', ProcCallMethod, 'Graphics32');

  end;
end;

function TLibrary_GR32.ProcCallMethod(aInstance: TObject; aClassType: TClass; const aMethodName: string; aCallVar: TmscrVarForMethod): variant;
 var ic:Tcolor32;
     R, G, B, A: Byte;
begin
  Result := 0;

  if aMethodName='COLOR32' then begin
     Result:=Color32(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],aCallVar.Params[3]);
  exit; end;    
  if aMethodName='COLOR32WIN' then begin
     Result:=Color32(aCallVar.Params[0]);
  exit; end;
  if aMethodName='ALPHACOMPONENT' then begin
     Result:=AlphaComponent(Cardinal(aCallVar.Params[0]));
  exit; end;
  if aMethodName='BLUECOMPONENT' then begin
     Result:=BlueComponent(Cardinal(aCallVar.Params[0]));
  exit; end;                               
  if aMethodName='GREENCOMPONENT' then begin
     Result:=GreenComponent(Cardinal(aCallVar.Params[0]));
  exit; end;                             
  if aMethodName='REDCOMPONENT' then begin
     Result:=RedComponent(Cardinal(aCallVar.Params[0]));
  exit; end;           
  if aMethodName='ARRAYOFCOLOR32' then begin
     Result:=ArrayOfColor32(Cardinal(aCallVar.Params[0]));
  exit; end;
  if aMethodName='GRAY32' then begin
     Result:=Gray32(aCallVar.Params[0],aCallVar.Params[1]);
  exit; end;
  if aMethodName='HSVTORGB' then begin
     Result:=HSVtoRGB(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2]);
  exit; end;
  if aMethodName='INTENSITY' then begin
     Result:=Intensity(Cardinal(aCallVar.Params[0]));
  exit; end;
  if aMethodName='INVERTCOLOR' then begin
     Result:=InvertColor(Cardinal(aCallVar.Params[0]));
  exit; end;
  if aMethodName='SCALEALPHA' then begin
     ic:=Cardinal(aCallVar.Params[0]);
     ScaleAlpha(ic,aCallVar.Params[1]);
  exit; end;
  if aMethodName='SETALPHA' then begin
     ic:=Cardinal(aCallVar.Params[0]);
     Result:=SetAlpha(ic,aCallVar.Params[1]);
  exit; end;
  if aMethodName='WINCOLOR' then begin
     Result:=WinColor(Cardinal(aCallVar.Params[0]));
  exit; end; 
  if aMethodName='COLOR32TORGB' then begin
     R:=aCallVar.Params[1];
     G:=aCallVar.Params[2];
     B:=aCallVar.Params[3];
     Color32ToRGB(Cardinal(aCallVar.Params[0]),R,G,B);
  exit; end;
  if aMethodName='COLOR32TORGBA' then begin
     R:=aCallVar.Params[1];
     G:=aCallVar.Params[2];
     B:=aCallVar.Params[3];
     A:=aCallVar.Params[4];
     Color32ToRGBA(Cardinal(aCallVar.Params[0]),R,G,B,A);
  exit; end;

end;
 
procedure TLibrary_GR32.RegisterTypes(AScript: TmscrScript);
begin
  with AScript do
  begin
     Register_Type('TArrayOfArrayOfCardinal',msvtArray);  // Is DynArray
     Register_Type('TArrayOfArrayOfFixed',msvtArray);  // Is DynArray
     Register_Type('TArrayOfArrayOfFixedPoint',msvtArray);  // Is DynArray
     Register_Type('TArrayOfArrayOfFloatPoint',msvtArray);  // Is DynArray
     Register_Type('TArrayOfArrayOfInteger',msvtArray);  // Is DynArray
     Register_Type('TArrayOfArrayOfPoint',msvtArray);  // Is DynArray
     Register_Type('TArrayOfByte',msvtArray);  // Is DynArray
     Register_Type('TArrayOfCardinal',msvtArray);  // Is DynArray
     Register_Type('TArrayOfColor32',msvtArray);  // Is DynArray
     Register_Type('TArrayOfColor32Entry',msvtArray);  // Is DynArray
     Register_Type('TArrayOfFixed',msvtArray);  // Is DynArray
     Register_Type('TArrayOfFixedPoint',msvtArray);  // Is DynArray
     Register_Type('TArrayOfFloat',msvtArray);  // Is DynArray
     Register_Type('TArrayOfFloatPoint',msvtArray);  // Is DynArray
     Register_Type('TArrayOfInteger',msvtArray);  // Is DynArray
     Register_Type('TArrayOfPoint',msvtArray);  // Is DynArray
     Register_Type('TArrayOfSingle',msvtArray);  // Is DynArray
     Register_Type('TArrayOfWord',msvtArray);  // Is DynArray
     Register_Type('TByteArray',msvtArray);
     Register_Type('TCardinalArray',msvtArray);
     Register_Type('TColor32Array',msvtArray);
     Register_Enum('TColor32Component','ccRedccGreen, ccBlueccAlpha');
     Register_EnumSet('TColor32Components','ccRedccGreen, ccBlueccAlpha');
     Register_Type('TColor32EntryArray',msvtArray);
     Register_Enum('TCombineMode','cmBlendcmMerge');
     Register_Enum('TDrawMode','dmOpaquedmBlend, dmCustomdmTransparent');
     Register_Type('TFixed',msvtInt);
     Register_Type('TFixedArray',msvtArray);
     Register_Type('TFixedPointArray',msvtArray);
     Register_Type('TFloat',msvtFloat);
     Register_Type('TFloatArray',msvtArray);
     Register_Type('TFloatPointArray',msvtArray);
     Register_Type('TGammaTable8Bit',msvtArray);  // Is DynArray
     Register_Type('TIntegerArray',msvtArray);
     Register_Type('TPalette32',msvtArray);  // Is DynArray
     Register_Enum('TPixelAccessMode','pamUnsafepamSafe, pamWrappamTransparentEdge');
     Register_Type('TPointArray',msvtArray);
     Register_Enum('TRectRounding','rrClosestrrOutsiderrInside');
     Register_Type('TSingleArray',msvtArray);
     Register_Type('TWordArray',msvtArray);
     Register_Enum('TWrapMode','wmClampwmRepeatwmMirror');
  end;
end;

procedure TLibrary_GR32.RegisterClasses(AScript: TmscrScript);
begin
  with AScript do
  begin

    with Register_Class(TBitmap32,'TCustomBitmap32') do
    begin
      Register_Method('function CanvasAllocated: Boolean;', ClassCallMethod);
      Register_Method('function TextHeight(const Text: string): Integer;', ClassCallMethod);
      Register_Method('function TextHeightW(const Text: Widestring): Integer;', ClassCallMethod);
      Register_Method('function TextWidth(const Text: string): Integer;', ClassCallMethod);
      Register_Method('function TextWidthW(const Text: Widestring): Integer;', ClassCallMethod);
      Register_Method('procedure DeleteCanvas;', ClassCallMethod);
      Register_Method('procedure RenderText(X, Y: Integer; const Text: string; AALevel: Integer; Color: TColor32);', ClassCallMethod);
      Register_Method('procedure RenderTextW(X, Y: Integer; const Text: Widestring; AALevel: Integer; Color: TColor32);', ClassCallMethod);
      Register_Method('procedure UpdateFont;', ClassCallMethod);
      Register_Property('BitmapHandle','HBITMAP', ClassGetProp,Nil);
      Register_Property('Canvas','TCanvas', ClassGetProp,Nil);
      Register_Property('Font','TFont', ClassGetProp,ClassSetProp);
      Register_Property('Handle','HDC', ClassGetProp,Nil);
    end;
    with Register_Class(TCustomBackend,'TThreadPersistent') do
    begin
      Register_Method('function Empty: Boolean;', ClassCallMethod);
      Register_Method('procedure Assign(Source: TPersistent);', ClassCallMethod);
      Register_Method('procedure Clear;', ClassCallMethod);
      Register_Property('Bits','PColor32Array', ClassGetProp,Nil);
      Register_Event('OnChanging',TmscrNotifyEvent); //-- System Reg ---
    end;
    with Register_Class(TCustomBitmap32,'TCustomMap') do
    begin
      Register_Method('function Empty: Boolean;', ClassCallMethod);
      Register_Method('function GetStippleColor: TColor32;', ClassCallMethod);
      Register_Method('function ReleaseBackend: TCustomBackend;', ClassCallMethod);
      Register_Method('procedure AdvanceStippleCounter(LengthPixels: Single);', ClassCallMethod);
      Register_Method('procedure Assign(Source: TPersistent);', ClassCallMethod);
      Register_Method('procedure Delete;', ClassCallMethod);
      Register_Method('procedure EndMeasuring;', ClassCallMethod);
      Register_Method('procedure FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32);', ClassCallMethod);
      Register_Method('procedure FillRectT(X1, Y1, X2, Y2: Integer; Value: TColor32);', ClassCallMethod);
      Register_Method('procedure FlipHorz(Dst: TCustomBitmap32 = nil);', ClassCallMethod);
      Register_Method('procedure FlipVert(Dst: TCustomBitmap32 = nil);', ClassCallMethod);
      Register_Method('procedure FrameRectTSP(X1, Y1, X2, Y2: Integer);', ClassCallMethod);
      Register_Method('procedure HorzLine(X1, Y, X2: Integer; Value: TColor32);', ClassCallMethod);
      Register_Method('procedure HorzLineS(X1, Y, X2: Integer; Value: TColor32);', ClassCallMethod);
      Register_Method('procedure HorzLineT(X1, Y, X2: Integer; Value: TColor32);', ClassCallMethod);
      Register_Method('procedure HorzLineTS(X1, Y, X2: Integer; Value: TColor32);', ClassCallMethod);
      Register_Method('procedure HorzLineTSP(X1, Y, X2: Integer);', ClassCallMethod);
      Register_Method('procedure HorzLineX(X1, Y, X2: TFixed; Value: TColor32);', ClassCallMethod);
      Register_Method('procedure HorzLineXS(X1, Y, X2: TFixed; Value: TColor32);', ClassCallMethod);
      Register_Method('procedure Line(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);', ClassCallMethod);
      Register_Method('procedure LineA(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);', ClassCallMethod);
      Register_Method('procedure LineAS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);', ClassCallMethod);
      Register_Method('procedure LineS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);', ClassCallMethod);
      Register_Method('procedure LineT(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);', ClassCallMethod);
      Register_Method('procedure LineToAS(X, Y: Integer);', ClassCallMethod);
      Register_Method('procedure LineToFS(X, Y: Single);', ClassCallMethod);
      Register_Method('procedure LineToFSP(X, Y: Single);', ClassCallMethod);
      Register_Method('procedure LineToS(X, Y: Integer);', ClassCallMethod);
      Register_Method('procedure LineToTS(X, Y: Integer);', ClassCallMethod);
      Register_Method('procedure LineToXS(X, Y: TFixed);', ClassCallMethod);
      Register_Method('procedure LineToXSP(X, Y: TFixed);', ClassCallMethod);
      Register_Method('procedure LineTS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);', ClassCallMethod);
      Register_Method('procedure LoadFromFile(const FileName: string);', ClassCallMethod);
      Register_Method('procedure LoadFromResourceID(aInstance: THandle; ResID: Integer);', ClassCallMethod);
      Register_Method('procedure LoadFromResourceName(aInstance: THandle; const ResName: string);', ClassCallMethod);
      Register_Method('procedure LoadFromStream(Stream: TStream);', ClassCallMethod);
      Register_Method('procedure MoveTo(X, Y: Integer);', ClassCallMethod);
      Register_Method('procedure MoveToF(X, Y: Single);', ClassCallMethod);
      Register_Method('procedure MoveToX(X, Y: TFixed);', ClassCallMethod);
      Register_Method('procedure PropertyChanged;', ClassCallMethod);
      Register_Method('procedure ResetClipRect;', ClassCallMethod);
      Register_Method('procedure Roll(Dx, Dy: Integer; FillBack: Boolean; FillColor: TColor32);', ClassCallMethod);
      Register_Method('procedure Rotate180(Dst: TCustomBitmap32 = nil);', ClassCallMethod);
      Register_Method('procedure Rotate270(Dst: TCustomBitmap32 = nil);', ClassCallMethod);
      Register_Method('procedure Rotate90(Dst: TCustomBitmap32 = nil);', ClassCallMethod);
      Register_Method('procedure SaveToFile(const FileName: string; SaveTopDown: Boolean = False);', ClassCallMethod);
      Register_Method('procedure SaveToStream(Stream: TStream; SaveTopDown: Boolean = False);', ClassCallMethod);
      Register_Method('procedure SetPixelTS(X, Y: Integer; Value: TColor32);', ClassCallMethod);
      Register_Method('procedure SwapRedBlue;', ClassCallMethod);
      Register_Method('procedure VertLine(X, Y1, Y2: Integer; Value: TColor32);', ClassCallMethod);
      Register_Method('procedure VertLineS(X, Y1, Y2: Integer; Value: TColor32);', ClassCallMethod);
      Register_Method('procedure VertLineT(X, Y1, Y2: Integer; Value: TColor32);', ClassCallMethod);
      Register_Method('procedure VertLineTS(X, Y1, Y2: Integer; Value: TColor32);', ClassCallMethod);
      Register_Method('procedure VertLineTSP(X, Y1, Y2: Integer);', ClassCallMethod);
      Register_Method('procedure VertLineX(X, Y1, Y2: TFixed; Value: TColor32);', ClassCallMethod);
      Register_Method('procedure VertLineXS(X, Y1, Y2: TFixed; Value: TColor32);', ClassCallMethod);
      Register_Property('Backend','TCustomBackend', ClassGetProp,ClassSetProp);
      Register_Property('Bits','PColor32Array', ClassGetProp,Nil);
      Register_Property('Clipping','Boolean', ClassGetProp,Nil);
      Register_Property('MeasuringMode','Boolean', ClassGetProp,Nil);
      Register_Property('PenColor','TColor32', ClassGetProp,ClassSetProp);
      Register_IndexProperty('Pixel', 'Integer, Integer' ,'TColor32', ClassCallMethod);
      Register_IndexProperty('PixelF', 'Single, Single' ,'TColor32', ClassCallMethod);
      Register_IndexProperty('PixelFR', 'Single, Single' ,'TColor32', ClassCallMethod, True);
      Register_IndexProperty('PixelFS', 'Single, Single' ,'TColor32', ClassCallMethod);
      Register_IndexProperty('PixelFW', 'Single, Single' ,'TColor32', ClassCallMethod);
      Register_IndexProperty('PixelS', 'Integer, Integer' ,'TColor32', ClassCallMethod);
      Register_IndexProperty('PixelW', 'Integer, Integer' ,'TColor32', ClassCallMethod);
      Register_IndexProperty('PixelX', 'TFixed, TFixed' ,'TColor32', ClassCallMethod);
      Register_IndexProperty('PixelXR', 'TFixed, TFixed' ,'TColor32', ClassCallMethod, True);
      Register_IndexProperty('PixelXS', 'TFixed, TFixed' ,'TColor32', ClassCallMethod);
      Register_IndexProperty('PixelXW', 'TFixed, TFixed' ,'TColor32', ClassCallMethod);
      Register_IndexProperty('ScanLine', 'Integer' ,'PColor32Array', ClassCallMethod, True);
      Register_Property('StippleCounter','Single', ClassGetProp,ClassSetProp);
      Register_Property('StippleStep','Single', ClassGetProp,ClassSetProp);
    end;
    with Register_Class(TCustomMap,'TThreadPersistent') do
    begin
      Register_Method('function Empty: Boolean;', ClassCallMethod);
      Register_Method('function SetSize(NewWidth, NewHeight: Integer): Boolean;', ClassCallMethod);
      Register_Method('function SetSizeFrom(Source: TPersistent): Boolean;', ClassCallMethod);
      Register_Method('procedure Delete;', ClassCallMethod);
      Register_Method('procedure Resized;', ClassCallMethod);
      Register_Property('Height','Integer', ClassGetProp,ClassSetProp);
      Register_Event('OnResize',TmscrNotifyEvent); //-- System Reg ---
      Register_Property('Width','Integer', ClassGetProp,ClassSetProp);
    end;
    with Register_Class(TCustomResampler,'TCustomSampler') do
    begin
      Register_Method('function HasBounds: Boolean;', ClassCallMethod);
      Register_Method('procedure Changed;', ClassCallMethod);
      Register_Method('procedure PrepareSampling;', ClassCallMethod);
      Register_Property('Bitmap','TCustomBitmap32', ClassGetProp,ClassSetProp);
      Register_Property('Width','TFloat', ClassGetProp,Nil);
    end;
    with Register_Class(TCustomSampler,'TNotifiablePersistent') do
    begin
      Register_Method('function GetSampleFixed(X, Y: TFixed): TColor32;', ClassCallMethod);
      Register_Method('function GetSampleFloat(X, Y: TFloat): TColor32;', ClassCallMethod);
      Register_Method('function GetSampleInt(X, Y: Integer): TColor32;', ClassCallMethod);
      Register_Method('function HasBounds: Boolean;', ClassCallMethod);
      Register_Method('procedure FinalizeSampling;', ClassCallMethod);
      Register_Method('procedure PrepareSampling;', ClassCallMethod);
    end;
    with Register_Class(TNotifiablePersistent,'TPlainInterfacedPersistent') do
    begin
      Register_Method('procedure BeforeDestruction;', ClassCallMethod);
      Register_Method('procedure BeginUpdate;', ClassCallMethod);
      Register_Method('procedure Changed;', ClassCallMethod);
      Register_Method('procedure EndUpdate;', ClassCallMethod);
      Register_Event('OnChange',TmscrNotifyEvent); //-- System Reg ---
    end;
    with Register_Class(TPlainInterfacedPersistent,'TPersistent') do
    begin
      Register_Method('procedure AfterConstruction;', ClassCallMethod);
      Register_Method('procedure BeforeDestruction;', ClassCallMethod);
      Register_Property('RefCount','Integer', ClassGetProp,Nil);
    end;
    with Register_Class(TThreadPersistent,'TNotifiablePersistent') do
    begin
      Register_Constructor('constructor Create;', ClassCallMethod);
      Register_Method('procedure Lock;', ClassCallMethod);
      Register_Method('procedure Unlock;', ClassCallMethod);
    end;

  end;
end;

function TLibrary_GR32.ClassCallMethod(aInstance: TObject; aClassType: TClass; const aMethodName: string; aCallVar: TmscrVarForMethod): variant;
begin
  Result := 0;

  if aClassType=TBitmap32 then begin
    if aMethodName='CANVASALLOCATED' then begin
         Result:=fmscrInteger(TBitmap32(aInstance).CanvasAllocated); exit; end;
    if aMethodName='TEXTHEIGHT' then begin
         Result:=fmscrInteger(TBitmap32(aInstance).TextHeight(aCallVar.Params[0])); exit; end;
    if aMethodName='TEXTHEIGHTW' then begin
         Result:=fmscrInteger(TBitmap32(aInstance).TextHeightW(aCallVar.Params[0])); exit; end;
    if aMethodName='TEXTWIDTH' then begin
         Result:=fmscrInteger(TBitmap32(aInstance).TextWidth(aCallVar.Params[0])); exit; end;
    if aMethodName='TEXTWIDTHW' then begin
         Result:=fmscrInteger(TBitmap32(aInstance).TextWidthW(aCallVar.Params[0])); exit; end;
    if aMethodName='DELETECANVAS' then begin
         TBitmap32(aInstance).DeleteCanvas; exit; end;
    if aMethodName='RENDERTEXT' then begin
         TBitmap32(aInstance).RenderText(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],aCallVar.Params[3],Cardinal(aCallVar.Params[4])); exit; end;
    if aMethodName='RENDERTEXTW' then begin
         TBitmap32(aInstance).RenderTextW(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],aCallVar.Params[3],Cardinal(aCallVar.Params[4])); exit; end;
    if aMethodName='UPDATEFONT' then begin
         TBitmap32(aInstance).UpdateFont; exit; end;
   exit; end;

  if aClassType=TCustomBackend then begin
    if aMethodName='EMPTY' then begin
         Result:=fmscrInteger(TCustomBackend(aInstance).Empty); exit; end;
    if aMethodName='ASSIGN' then begin
         TCustomBackend(aInstance).Assign(TPersistent(fmscrInteger(aCallVar.Params[0]))); exit; end;
    if aMethodName='CLEAR' then begin
         TCustomBackend(aInstance).Clear; exit; end;
   exit; end;

  if aClassType=TCustomBitmap32 then begin
    if aMethodName='EMPTY' then begin
         Result:=fmscrInteger(TCustomBitmap32(aInstance).Empty); exit; end;
    if aMethodName='GETSTIPPLECOLOR' then begin
         Result:=fmscrInteger(TCustomBitmap32(aInstance).GetStippleColor); exit; end;
    if aMethodName='RELEASEBACKEND' then begin
         Result:=fmscrInteger(TCustomBitmap32(aInstance).ReleaseBackend); exit; end;
    if aMethodName='ADVANCESTIPPLECOUNTER' then begin
         TCustomBitmap32(aInstance).AdvanceStippleCounter(aCallVar.Params[0]); exit; end;
    if aMethodName='ASSIGN' then begin
         TCustomBitmap32(aInstance).Assign(TPersistent(fmscrInteger(aCallVar.Params[0]))); exit; end;
    if aMethodName='DELETE' then begin
         TCustomBitmap32(aInstance).Delete; exit; end;
    if aMethodName='ENDMEASURING' then begin
         TCustomBitmap32(aInstance).EndMeasuring; exit; end;
    if aMethodName='FILLRECT' then begin
         TCustomBitmap32(aInstance).FillRect(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],aCallVar.Params[3],Cardinal(aCallVar.Params[4])); exit; end;
    if aMethodName='FILLRECTT' then begin
         TCustomBitmap32(aInstance).FillRectT(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],aCallVar.Params[3],Cardinal(aCallVar.Params[4])); exit; end;
    if aMethodName='FLIPHORZ' then begin
         TCustomBitmap32(aInstance).FlipHorz(TCustomBitmap32(fmscrInteger(aCallVar.Params[0]))); exit; end;
    if aMethodName='FLIPVERT' then begin
         TCustomBitmap32(aInstance).FlipVert(TCustomBitmap32(fmscrInteger(aCallVar.Params[0]))); exit; end;
    if aMethodName='FRAMERECTTSP' then begin
         TCustomBitmap32(aInstance).FrameRectTSP(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],aCallVar.Params[3]); exit; end;
    if aMethodName='HORZLINE' then begin
         TCustomBitmap32(aInstance).HorzLine(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],Cardinal(aCallVar.Params[3])); exit; end;
    if aMethodName='HORZLINES' then begin
         TCustomBitmap32(aInstance).HorzLineS(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],Cardinal(aCallVar.Params[3])); exit; end;
    if aMethodName='HORZLINET' then begin
         TCustomBitmap32(aInstance).HorzLineT(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],Cardinal(aCallVar.Params[3])); exit; end;
    if aMethodName='HORZLINETS' then begin
         TCustomBitmap32(aInstance).HorzLineTS(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],Cardinal(aCallVar.Params[3])); exit; end;
    if aMethodName='HORZLINETSP' then begin
         TCustomBitmap32(aInstance).HorzLineTSP(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2]); exit; end;
    if aMethodName='HORZLINEX' then begin
         TCustomBitmap32(aInstance).HorzLineX(Integer(aCallVar.Params[0]),Integer(aCallVar.Params[1]),Integer(aCallVar.Params[2]),Cardinal(aCallVar.Params[3])); exit; end;
    if aMethodName='HORZLINEXS' then begin
         TCustomBitmap32(aInstance).HorzLineXS(Integer(aCallVar.Params[0]),Integer(aCallVar.Params[1]),Integer(aCallVar.Params[2]),Cardinal(aCallVar.Params[3])); exit; end;
    if aMethodName='LINE' then begin
         TCustomBitmap32(aInstance).Line(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],aCallVar.Params[3],Cardinal(aCallVar.Params[4]),aCallVar.Params[5]); exit; end;
    if aMethodName='LINEA' then begin
         TCustomBitmap32(aInstance).LineA(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],aCallVar.Params[3],Cardinal(aCallVar.Params[4]),aCallVar.Params[5]); exit; end;
    if aMethodName='LINEAS' then begin
         TCustomBitmap32(aInstance).LineAS(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],aCallVar.Params[3],Cardinal(aCallVar.Params[4]),aCallVar.Params[5]); exit; end;
    if aMethodName='LINES' then begin
         TCustomBitmap32(aInstance).LineS(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],aCallVar.Params[3],Cardinal(aCallVar.Params[4]),aCallVar.Params[5]); exit; end;
    if aMethodName='LINET' then begin
         TCustomBitmap32(aInstance).LineT(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],aCallVar.Params[3],Cardinal(aCallVar.Params[4]),aCallVar.Params[5]); exit; end;
    if aMethodName='LINETOAS' then begin
         TCustomBitmap32(aInstance).LineToAS(aCallVar.Params[0],aCallVar.Params[1]); exit; end;
    if aMethodName='LINETOFS' then begin
         TCustomBitmap32(aInstance).LineToFS(aCallVar.Params[0],aCallVar.Params[1]); exit; end;
    if aMethodName='LINETOFSP' then begin
         TCustomBitmap32(aInstance).LineToFSP(aCallVar.Params[0],aCallVar.Params[1]); exit; end;
    if aMethodName='LINETOS' then begin
         TCustomBitmap32(aInstance).LineToS(aCallVar.Params[0],aCallVar.Params[1]); exit; end;
    if aMethodName='LINETOTS' then begin
         TCustomBitmap32(aInstance).LineToTS(aCallVar.Params[0],aCallVar.Params[1]); exit; end;
    if aMethodName='LINETOXS' then begin
         TCustomBitmap32(aInstance).LineToXS(Integer(aCallVar.Params[0]),Integer(aCallVar.Params[1])); exit; end;
    if aMethodName='LINETOXSP' then begin
         TCustomBitmap32(aInstance).LineToXSP(Integer(aCallVar.Params[0]),Integer(aCallVar.Params[1])); exit; end;
    if aMethodName='LINETS' then begin
         TCustomBitmap32(aInstance).LineTS(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],aCallVar.Params[3],Cardinal(aCallVar.Params[4]),aCallVar.Params[5]); exit; end;
    if aMethodName='LOADFROMFILE' then begin
         TCustomBitmap32(aInstance).LoadFromFile(aCallVar.Params[0]); exit; end;
    if aMethodName='LOADFROMRESOURCEID' then begin
         TCustomBitmap32(aInstance).LoadFromResourceID(aCallVar.Params[0],aCallVar.Params[1]); exit; end;
    if aMethodName='LOADFROMRESOURCENAME' then begin
         TCustomBitmap32(aInstance).LoadFromResourceName(aCallVar.Params[0],aCallVar.Params[1]); exit; end;
    if aMethodName='LOADFROMSTREAM' then begin
         TCustomBitmap32(aInstance).LoadFromStream(TStream(fmscrInteger(aCallVar.Params[0]))); exit; end;
    if aMethodName='MOVETO' then begin
         TCustomBitmap32(aInstance).MoveTo(aCallVar.Params[0],aCallVar.Params[1]); exit; end;
    if aMethodName='MOVETOF' then begin
         TCustomBitmap32(aInstance).MoveToF(aCallVar.Params[0],aCallVar.Params[1]); exit; end;
    if aMethodName='MOVETOX' then begin
         TCustomBitmap32(aInstance).MoveToX(Integer(aCallVar.Params[0]),Integer(aCallVar.Params[1])); exit; end;
    if aMethodName='PROPERTYCHANGED' then begin
         TCustomBitmap32(aInstance).PropertyChanged; exit; end;
    if aMethodName='RESETCLIPRECT' then begin
         TCustomBitmap32(aInstance).ResetClipRect; exit; end;
    if aMethodName='ROLL' then begin
         TCustomBitmap32(aInstance).Roll(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],Cardinal(aCallVar.Params[3])); exit; end;
    if aMethodName='ROTATE180' then begin
         TCustomBitmap32(aInstance).Rotate180(TCustomBitmap32(fmscrInteger(aCallVar.Params[0]))); exit; end;
    if aMethodName='ROTATE270' then begin
         TCustomBitmap32(aInstance).Rotate270(TCustomBitmap32(fmscrInteger(aCallVar.Params[0]))); exit; end;
    if aMethodName='ROTATE90' then begin
         TCustomBitmap32(aInstance).Rotate90(TCustomBitmap32(fmscrInteger(aCallVar.Params[0]))); exit; end;
    if aMethodName='SAVETOFILE' then begin
         TCustomBitmap32(aInstance).SaveToFile(aCallVar.Params[0],aCallVar.Params[1]); exit; end;
    if aMethodName='SAVETOSTREAM' then begin
         TCustomBitmap32(aInstance).SaveToStream(TStream(fmscrInteger(aCallVar.Params[0])),aCallVar.Params[1]); exit; end;
    if aMethodName='SETPIXELTS' then begin
         TCustomBitmap32(aInstance).SetPixelTS(aCallVar.Params[0],aCallVar.Params[1],Cardinal(aCallVar.Params[2])); exit; end;
    if aMethodName='SWAPREDBLUE' then begin
         TCustomBitmap32(aInstance).SwapRedBlue; exit; end;
    if aMethodName='VERTLINE' then begin
         TCustomBitmap32(aInstance).VertLine(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],Cardinal(aCallVar.Params[3])); exit; end;
    if aMethodName='VERTLINES' then begin
         TCustomBitmap32(aInstance).VertLineS(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],Cardinal(aCallVar.Params[3])); exit; end;
    if aMethodName='VERTLINET' then begin
         TCustomBitmap32(aInstance).VertLineT(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],Cardinal(aCallVar.Params[3])); exit; end;
    if aMethodName='VERTLINETS' then begin
         TCustomBitmap32(aInstance).VertLineTS(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],Cardinal(aCallVar.Params[3])); exit; end;
    if aMethodName='VERTLINETSP' then begin
         TCustomBitmap32(aInstance).VertLineTSP(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2]); exit; end;
    if aMethodName='VERTLINEX' then begin
         TCustomBitmap32(aInstance).VertLineX(Integer(aCallVar.Params[0]),Integer(aCallVar.Params[1]),Integer(aCallVar.Params[2]),Cardinal(aCallVar.Params[3])); exit; end;
    if aMethodName='VERTLINEXS' then begin
         TCustomBitmap32(aInstance).VertLineXS(Integer(aCallVar.Params[0]),Integer(aCallVar.Params[1]),Integer(aCallVar.Params[2]),Cardinal(aCallVar.Params[3])); exit; end;
    if aMethodName = 'PIXEL.GET' then begin
      Result:=TCustomBitmap32(aInstance).Pixel[aCallVar.Params[0], aCallVar.Params[1]]; exit; end;
    if aMethodName = 'PIXEL.SET' then begin
      TCustomBitmap32(aInstance).Pixel[aCallVar.Params[0], aCallVar.Params[1]]:= Cardinal(aCallVar.Params[2]); exit; end;
    if aMethodName = 'PIXELF.GET' then begin
      Result:=TCustomBitmap32(aInstance).PixelF[aCallVar.Params[0], aCallVar.Params[1]]; exit; end;
    if aMethodName = 'PIXELF.SET' then begin
      TCustomBitmap32(aInstance).PixelF[aCallVar.Params[0], aCallVar.Params[1]]:= Cardinal(aCallVar.Params[2]); exit; end;
    if aMethodName = 'PIXELFR.GET' then begin
      Result:=TCustomBitmap32(aInstance).PixelFR[aCallVar.Params[0], aCallVar.Params[1]]; exit; end;
    if aMethodName = 'PIXELFS.GET' then begin
      Result:=TCustomBitmap32(aInstance).PixelFS[aCallVar.Params[0], aCallVar.Params[1]]; exit; end;
    if aMethodName = 'PIXELFS.SET' then begin
      TCustomBitmap32(aInstance).PixelFS[aCallVar.Params[0], aCallVar.Params[1]]:= Cardinal(aCallVar.Params[2]); exit; end;
    if aMethodName = 'PIXELFW.GET' then begin
      Result:=TCustomBitmap32(aInstance).PixelFW[aCallVar.Params[0], aCallVar.Params[1]]; exit; end;
    if aMethodName = 'PIXELFW.SET' then begin
      TCustomBitmap32(aInstance).PixelFW[aCallVar.Params[0], aCallVar.Params[1]]:= Cardinal(aCallVar.Params[2]); exit; end;
    if aMethodName = 'PIXELS.GET' then begin
      Result:=TCustomBitmap32(aInstance).PixelS[aCallVar.Params[0], aCallVar.Params[1]]; exit; end;
    if aMethodName = 'PIXELS.SET' then begin
      TCustomBitmap32(aInstance).PixelS[aCallVar.Params[0], aCallVar.Params[1]]:= Cardinal(aCallVar.Params[2]); exit; end;
    if aMethodName = 'PIXELW.GET' then begin
      Result:=TCustomBitmap32(aInstance).PixelW[aCallVar.Params[0], aCallVar.Params[1]]; exit; end;
    if aMethodName = 'PIXELW.SET' then begin
      TCustomBitmap32(aInstance).PixelW[aCallVar.Params[0], aCallVar.Params[1]]:= Cardinal(aCallVar.Params[2]); exit; end;
    if aMethodName = 'PIXELX.GET' then begin
      Result:=TCustomBitmap32(aInstance).PixelX[Integer(aCallVar.Params[0]), Integer(aCallVar.Params[1])]; exit; end;
    if aMethodName = 'PIXELX.SET' then begin
      TCustomBitmap32(aInstance).PixelX[Integer(aCallVar.Params[0]), Integer(aCallVar.Params[1])]:= Cardinal(aCallVar.Params[2]); exit; end;
    if aMethodName = 'PIXELXR.GET' then begin
      Result:=TCustomBitmap32(aInstance).PixelXR[Integer(aCallVar.Params[0]), Integer(aCallVar.Params[1])]; exit; end;
    if aMethodName = 'PIXELXS.GET' then begin
      Result:=TCustomBitmap32(aInstance).PixelXS[Integer(aCallVar.Params[0]), Integer(aCallVar.Params[1])]; exit; end;
    if aMethodName = 'PIXELXS.SET' then begin
      TCustomBitmap32(aInstance).PixelXS[Integer(aCallVar.Params[0]), Integer(aCallVar.Params[1])]:= Cardinal(aCallVar.Params[2]); exit; end;
    if aMethodName = 'PIXELXW.GET' then begin
      Result:=TCustomBitmap32(aInstance).PixelXW[Integer(aCallVar.Params[0]), Integer(aCallVar.Params[1])]; exit; end;
    if aMethodName = 'PIXELXW.SET' then begin
      TCustomBitmap32(aInstance).PixelXW[Integer(aCallVar.Params[0]), Integer(aCallVar.Params[1])]:= Cardinal(aCallVar.Params[2]); exit; end;
    if aMethodName = 'SCANLINE.GET' then begin
      Result:=Pointer(TCustomBitmap32(aInstance).ScanLine[aCallVar.Params[0]]); exit; end;
   exit; end;

  if aClassType=TCustomMap then begin
    if aMethodName='EMPTY' then begin
         Result:=fmscrInteger(TCustomMap(aInstance).Empty); exit; end;
    if aMethodName='SETSIZE' then begin
         Result:=fmscrInteger(TCustomMap(aInstance).SetSize(aCallVar.Params[0],aCallVar.Params[1])); exit; end;
    if aMethodName='SETSIZEFROM' then begin
         Result:=fmscrInteger(TCustomMap(aInstance).SetSizeFrom(TPersistent(fmscrInteger(aCallVar.Params[0])))); exit; end;
    if aMethodName='DELETE' then begin
         TCustomMap(aInstance).Delete; exit; end;
    if aMethodName='RESIZED' then begin
         TCustomMap(aInstance).Resized; exit; end;
   exit; end;

  if aClassType=TCustomResampler then begin
    if aMethodName='HASBOUNDS' then begin
         Result:=fmscrInteger(TCustomResampler(aInstance).HasBounds); exit; end;
    if aMethodName='CHANGED' then begin
         TCustomResampler(aInstance).Changed; exit; end;
    if aMethodName='PREPARESAMPLING' then begin
         TCustomResampler(aInstance).PrepareSampling; exit; end;
   exit; end;

  if aClassType=TCustomSampler then begin
    if aMethodName='GETSAMPLEFIXED' then begin
         Result:=fmscrInteger(TCustomSampler(aInstance).GetSampleFixed(Integer(aCallVar.Params[0]),Integer(aCallVar.Params[1]))); exit; end;
    if aMethodName='GETSAMPLEFLOAT' then begin
         Result:=fmscrInteger(TCustomSampler(aInstance).GetSampleFloat(aCallVar.Params[0],aCallVar.Params[1])); exit; end;
    if aMethodName='GETSAMPLEINT' then begin
         Result:=fmscrInteger(TCustomSampler(aInstance).GetSampleInt(aCallVar.Params[0],aCallVar.Params[1])); exit; end;
    if aMethodName='HASBOUNDS' then begin
         Result:=fmscrInteger(TCustomSampler(aInstance).HasBounds); exit; end;
    if aMethodName='FINALIZESAMPLING' then begin
         TCustomSampler(aInstance).FinalizeSampling; exit; end;
    if aMethodName='PREPARESAMPLING' then begin
         TCustomSampler(aInstance).PrepareSampling; exit; end;
   exit; end;

  if aClassType=TNotifiablePersistent then begin
    if aMethodName='BEFOREDESTRUCTION' then begin
         TNotifiablePersistent(aInstance).BeforeDestruction; exit; end;
    if aMethodName='BEGINUPDATE' then begin
         TNotifiablePersistent(aInstance).BeginUpdate; exit; end;
    if aMethodName='CHANGED' then begin
         TNotifiablePersistent(aInstance).Changed; exit; end;
    if aMethodName='ENDUPDATE' then begin
         TNotifiablePersistent(aInstance).EndUpdate; exit; end;
   exit; end;

  if aClassType=TPlainInterfacedPersistent then begin
    if aMethodName='AFTERCONSTRUCTION' then begin
         TPlainInterfacedPersistent(aInstance).AfterConstruction; exit; end;
    if aMethodName='BEFOREDESTRUCTION' then begin
         TPlainInterfacedPersistent(aInstance).BeforeDestruction; exit; end;
   exit; end;

  if aClassType=TThreadPersistent then begin
    if aMethodName='CREATE' then begin
         Result:=fmscrInteger(TThreadPersistent(aInstance).Create); exit; end;
    if aMethodName='LOCK' then begin
         TThreadPersistent(aInstance).Lock; exit; end;
    if aMethodName='UNLOCK' then begin
         TThreadPersistent(aInstance).Unlock; exit; end;
   exit; end;


end;

function TLibrary_GR32.ClassGetProp(aInstance: TObject; aClassType: TClass; const aPropName: String; aCallVar: TmscrEngProperty): Variant;
begin
  Result := 0;

  if aClassType=TBitmap32 then begin
    if aPropName = 'BITMAPHANDLE' then begin
      Result:=TBitmap32(aInstance).BitmapHandle; exit; end;
    if aPropName = 'CANVAS' then begin
      Result:=fmscrInteger(TBitmap32(aInstance).Canvas); exit; end;
    if aPropName = 'FONT' then begin
      Result:=fmscrInteger(TBitmap32(aInstance).Font); exit; end;
    if aPropName = 'HANDLE' then begin
      Result:=TBitmap32(aInstance).Handle; exit; end;
  exit; end;

  if aClassType=TCustomBackend then begin
    if aPropName = 'BITS' then begin
      Result:=Pointer(TCustomBackend(aInstance).Bits); exit; end;
  exit; end;

  if aClassType=TCustomBitmap32 then begin
    if aPropName = 'BACKEND' then begin
      Result:=fmscrInteger(TCustomBitmap32(aInstance).Backend); exit; end;
    if aPropName = 'BITS' then begin
      Result:=Pointer(TCustomBitmap32(aInstance).Bits); exit; end;
    if aPropName = 'CLIPPING' then begin
      Result:=TCustomBitmap32(aInstance).Clipping; exit; end;
    if aPropName = 'MEASURINGMODE' then begin
      Result:=TCustomBitmap32(aInstance).MeasuringMode; exit; end;
    if aPropName = 'PENCOLOR' then begin
      Result:=TCustomBitmap32(aInstance).PenColor; exit; end;
    if aPropName = 'STIPPLECOUNTER' then begin
      Result:=TCustomBitmap32(aInstance).StippleCounter; exit; end;
    if aPropName = 'STIPPLESTEP' then begin
      Result:=TCustomBitmap32(aInstance).StippleStep; exit; end;
  exit; end;

  if aClassType=TCustomMap then begin
    if aPropName = 'HEIGHT' then begin
      Result:=TCustomMap(aInstance).Height; exit; end;
    if aPropName = 'WIDTH' then begin
      Result:=TCustomMap(aInstance).Width; exit; end;
  exit; end;

  if aClassType=TCustomResampler then begin
    if aPropName = 'BITMAP' then begin
      Result:=fmscrInteger(TCustomBitmap32(TCustomResampler(aInstance).Bitmap)); exit; end;
    if aPropName = 'WIDTH' then begin
      Result:=TCustomResampler(aInstance).Width; exit; end;
  exit; end;

  if aClassType=TNotifiablePersistent then begin
  exit; end;

  if aClassType=TPlainInterfacedPersistent then begin
    if aPropName = 'REFCOUNT' then begin
      Result:=TPlainInterfacedPersistent(aInstance).RefCount; exit; end;
  exit; end;


end;

procedure TLibrary_GR32.ClassSetProp(aInstance: TObject; aClassType: TClass; const aPropName: String; aValue: Variant; aCallVar: TmscrEngProperty);
begin

  if aClassType=TBitmap32 then begin
    if aPropName = 'FONT' then begin
      TBitmap32(aInstance).Font:=TFont(fmscrInteger(aValue)); exit; end;
  exit; end;

  if aClassType=TCustomBackend then begin
  exit; end;

  if aClassType=TCustomBitmap32 then begin
    if aPropName = 'BACKEND' then begin
      TCustomBitmap32(aInstance).Backend:=TCustomBackend(fmscrInteger(aValue)); exit; end;
    if aPropName = 'PENCOLOR' then begin
      TCustomBitmap32(aInstance).PenColor:=Cardinal(aValue); exit; end;
    if aPropName = 'STIPPLECOUNTER' then begin
      TCustomBitmap32(aInstance).StippleCounter:=aValue; exit; end;
    if aPropName = 'STIPPLESTEP' then begin
      TCustomBitmap32(aInstance).StippleStep:=aValue; exit; end;
  exit; end;

  if aClassType=TCustomMap then begin
    if aPropName = 'HEIGHT' then begin
      TCustomMap(aInstance).Height:=aValue; exit; end;
    if aPropName = 'WIDTH' then begin
      TCustomMap(aInstance).Width:=aValue; exit; end;
  exit; end;

  if aClassType=TCustomResampler then begin
    if aPropName = 'BITMAP' then begin
      TCustomResampler(aInstance).Bitmap:=TCustomBitmap32(fmscrInteger(aValue)); exit; end;
  exit; end;

  if aClassType=TNotifiablePersistent then begin
  exit; end;

  if aClassType=TPlainInterfacedPersistent then begin
  exit; end;


end;


//===============================================
Initialization
  MSCR_RTTILibraries.Add(TLibrary_GR32);
Finalization
  MSCR_RTTILibraries.Remove(TLibrary_GR32);
End.

