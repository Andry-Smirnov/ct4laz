{***************************************************************************
               Copyright (c) PilotLogic Software House
                       All rights reserved
 
   MagicScript Import File for Library Graphics32
   Build from : GR32_Image.pas file
   This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
 ***************************************************************************}
 
Unit msrtti_GR32_Image;
 
{$MODE DELPHI}{$H+}
 
Interface
Uses
 Classes,
 Controls,
 Forms,
 GR32,
 GR32_AllStrings,
 GR32_Containers,
 GR32_Layers,
 GR32_MicroTiles,
 GR32_RangeBars,
 GR32_RepaintOpt,
 GR32_XPThemes,
 Graphics,
 LCLIntf,
 LCLType,
 LMessages,
 Math,
 SysUtils,
 Types,
 TypInfo,
 GR32_Image,
 mscoreengine;

Type
 TmscrRTTILibrary_GR32_Image=class(TComponent);

 TmscrImgMouseEvent = class(TmscrCustomEvent)
 public
  procedure DoEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  function  GetMethod: Pointer; override;
 end;

 TmscrImgMouseMoveEvent = class(TmscrCustomEvent)
 public
  procedure DoEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  function  GetMethod: Pointer; override;
 end;

 TmscrPaintStageEvent = class(TmscrCustomEvent)
 public
  procedure DoEvent(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
  function  GetMethod: Pointer; override;
 end;

Implementation   

//--------- TmscrImgMouseEvent -----------
procedure TmscrImgMouseEvent.DoEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  CallHandler([Sender,Button,@Shift,X,Y,Layer]);
end;
function TmscrImgMouseEvent.GetMethod: Pointer;
begin
  Result := @TmscrImgMouseEvent.DoEvent;
end;

//--------- TmscrImgMouseMoveEvent -----------
procedure TmscrImgMouseMoveEvent.DoEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  CallHandler([Sender,@Shift,X,Y,Layer]);
end;
function TmscrImgMouseMoveEvent.GetMethod: Pointer;
begin
  Result := @TmscrImgMouseMoveEvent.DoEvent;
end;

//--------- TmscrPaintStageEvent -----------
procedure TmscrPaintStageEvent.DoEvent(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
begin
  CallHandler([Sender,Buffer,StageNum]);
end;
function TmscrPaintStageEvent.GetMethod: Pointer;
begin
  Result := @TmscrPaintStageEvent.DoEvent;
end;



//===================================================

Type
 TLibrary_GR32_Image=class(TmscrRTTILibrary)
  private
    function  CallMethod(aInstance: TObject; aClassType: TClass; const aMethodName: string; aCallVar: TmscrVarForMethod): variant;
    procedure RegisterConsts(AScript: TmscrScript);
    procedure RegisterTypes(AScript: TmscrScript);
    procedure RegisterClasses(AScript: TmscrScript);
    function  ClassCallMethod(aInstance: TObject; aClassType: TClass; const aMethodName: string; aCallVar: TmscrVarForMethod): variant;
    function  ClassGetProp(aInstance: TObject; aClassType: TClass; const aPropName: String; aCallVar: TmscrEngProperty): Variant;
    procedure ClassSetProp(aInstance: TObject; aClassType: TClass; const aPropName: String; aValue: Variant; aCallVar: TmscrEngProperty);
  public
    constructor Create(AScript: TmscrScript); override;
  end;
 
constructor TLibrary_GR32_Image.Create(AScript: TmscrScript);
begin
  inherited Create(AScript);
  
  RegisterConsts(AScript);
  RegisterTypes(AScript);
  RegisterClasses(AScript);
end;
 
function TLibrary_GR32_Image.CallMethod(aInstance: TObject; aClassType: TClass; const aMethodName: string; aCallVar: TmscrVarForMethod): variant;
begin
  Result := 0;
  if aInstance<>NIL then
    Result:=ClassCallMethod(aInstance, aClassType, aMethodName, aCallVar);
end;
 
procedure TLibrary_GR32_Image.RegisterConsts(AScript: TmscrScript);
begin
  with AScript do
  begin
     Register_Const('PST_BITMAP_FRAME','Integer',PST_BITMAP_FRAME);
     Register_Const('PST_CLEAR_BACKGND','Integer',PST_CLEAR_BACKGND);
     Register_Const('PST_CLEAR_BUFFER','Integer',PST_CLEAR_BUFFER);
     Register_Const('PST_CONTROL_FRAME','Integer',PST_CONTROL_FRAME);
     Register_Const('PST_CUSTOM','Integer',PST_CUSTOM);
     Register_Const('PST_DRAW_BITMAP','Integer',PST_DRAW_BITMAP);
     Register_Const('PST_DRAW_LAYERS','Integer',PST_DRAW_LAYERS);

  end;
end;
 
procedure TLibrary_GR32_Image.RegisterTypes(AScript: TmscrScript);
begin
  with AScript do
  begin
     Register_Enum('TBitmapAlign','baTopLeftbaCenter, baTilebaCustom');
     Register_EnumSet('TPaintBoxOptions','TPaintBoxOptions = set of (pboWantArrowKeys, pboAutoFocus);');
     Register_Enum('TRepaintMode','rmFullrmDirectrmOptimizer');
     Register_Enum('TScaleMode','smNormalsmStretch, smScale, smResize, smOptimalsmOptimalScaled');
     Register_Enum('TScrollBarVisibility','svAlwayssvHiddensvAuto');
     Register_Enum('TSizeGripStyle','sgAutosgNonesgAlways');

  end;
end;

procedure TLibrary_GR32_Image.RegisterClasses(AScript: TmscrScript);
begin
  with AScript do
  begin

    with Register_Class(TBitmap32Collection,'TCollection') do
    begin
      Register_Constructor('constructor Create(AOwner: TPersistent; ItemClass: TBitmap32ItemClass);', ClassCallMethod);
      Register_Method('function Add: TBitmap32Item;', ClassCallMethod);
      Register_IndexProperty('Items', 'Integer' ,'TBitmap32Item', ClassCallMethod);
    end;
    with Register_Class(TBitmap32Item,'TCollectionItem') do
    begin
      Register_Constructor('constructor Create(Collection: TCollection);', ClassCallMethod);
    end;
    with Register_Class(TBitmap32List,'TComponent') do
    begin
      Register_Constructor('constructor Create(AOwner: TComponent);', ClassCallMethod);
      Register_IndexProperty('Bitmap', 'Integer' ,'TBitmap32', ClassCallMethod);
    end;
    with Register_Class(TCustomImage32,'TCustomPaintBox32') do
    begin
      Register_Constructor('constructor Create(AOwner: TComponent);', ClassCallMethod);
      Register_Method('procedure BeginUpdate;', ClassCallMethod);
      Register_Method('procedure Changed;', ClassCallMethod);
      Register_Method('procedure EndUpdate;', ClassCallMethod);
      Register_Method('procedure ExecBitmapFrame(Dest: TBitmap32; StageNum: Integer);', ClassCallMethod);
      Register_Method('procedure ExecClearBackgnd(Dest: TBitmap32; StageNum: Integer);', ClassCallMethod);
      Register_Method('procedure ExecClearBuffer(Dest: TBitmap32; StageNum: Integer);', ClassCallMethod);
      Register_Method('procedure ExecControlFrame(Dest: TBitmap32; StageNum: Integer);', ClassCallMethod);
      Register_Method('procedure ExecCustom(Dest: TBitmap32; StageNum: Integer);', ClassCallMethod);
      Register_Method('procedure ExecDrawBitmap(Dest: TBitmap32; StageNum: Integer);', ClassCallMethod);
      Register_Method('procedure ExecDrawLayers(Dest: TBitmap32; StageNum: Integer);', ClassCallMethod);
      Register_Method('procedure Invalidate;', ClassCallMethod);
      Register_Method('procedure Loaded;', ClassCallMethod);
      Register_Method('procedure Resize;', ClassCallMethod);
      Register_Method('procedure SetupBitmap(DoClear: Boolean = False; ClearColor: TColor32 = $FF000000);', ClassCallMethod);
      Register_Property('Bitmap','TBitmap32', ClassGetProp,ClassSetProp);
      Register_Property('BitmapAlign','TBitmapAlign', ClassGetProp,ClassSetProp);
      Register_Property('Layers','TLayerCollection', ClassGetProp,ClassSetProp);
      Register_Property('OffsetHorz','TFloat', ClassGetProp,ClassSetProp);
      Register_Property('OffsetVert','TFloat', ClassGetProp,ClassSetProp);
      Register_Event('OnBitmapResize',TmscrNotifyEvent); //-- System Reg ---
      Register_Event('OnChange',TmscrNotifyEvent); //-- System Reg ---
      Register_Event('OnInitStages',TmscrNotifyEvent); //-- System Reg ---
      Register_Event('OnMouseDown',TmscrImgMouseEvent);
      Register_Event('OnMouseMove',TmscrImgMouseMoveEvent);
      Register_Event('OnMouseUp',TmscrImgMouseEvent);
      Register_Event('OnPaintStage',TmscrPaintStageEvent);
      Register_Event('OnScaleChange',TmscrNotifyEvent); //-- System Reg ---
      Register_Property('PaintStages','TPaintStages', ClassGetProp,Nil);
      Register_Property('Scale','TFloat', ClassGetProp,ClassSetProp);
      Register_Property('ScaleMode','TScaleMode', ClassGetProp,ClassSetProp);
      Register_Property('ScaleX','TFloat', ClassGetProp,ClassSetProp);
      Register_Property('ScaleY','TFloat', ClassGetProp,ClassSetProp);
    end;
    with Register_Class(TCustomImgView32,'TCustomImage32') do
    begin
      Register_Constructor('constructor Create(AOwner: TComponent);', ClassCallMethod);
      Register_Method('procedure Loaded;', ClassCallMethod);
      Register_Method('procedure Resize;', ClassCallMethod);
      Register_Method('procedure Scroll(Dx, Dy: Integer);', ClassCallMethod);
      Register_Method('procedure ScrollToCenter(X, Y: Integer);', ClassCallMethod);
      Register_Property('Centered','Boolean', ClassGetProp,ClassSetProp);
      Register_Event('OnScroll',TmscrNotifyEvent); //-- System Reg ---
      Register_Property('OverSize','Integer', ClassGetProp,ClassSetProp);
      Register_Property('ScrollBars','TIVScrollProperties', ClassGetProp,ClassSetProp);
      Register_Property('SizeGrip','TSizeGripStyle', ClassGetProp,ClassSetProp);
    end;
    with Register_Class(TCustomPaintBox32,'TCustomControl') do
    begin
      Register_Constructor('constructor Create(AOwner: TComponent);', ClassCallMethod);
      Register_Method('procedure AssignTo(Dest: TPersistent);', ClassCallMethod);
      Register_Method('procedure ForceFullInvalidate;', ClassCallMethod);
      Register_Method('procedure Invalidate;', ClassCallMethod);
      Register_Method('procedure Loaded;', ClassCallMethod);
      Register_Method('procedure Resize;', ClassCallMethod);
      Register_Method('procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);', ClassCallMethod);
      Register_Property('Buffer','TBitmap32', ClassGetProp,Nil);
      Register_Property('BufferOversize','Integer', ClassGetProp,ClassSetProp);
      Register_Property('MouseInControl','Boolean', ClassGetProp,Nil);
      Register_Event('OnGDIOverlay',TmscrNotifyEvent); //-- System Reg ---
      Register_Event('OnMouseEnter',TmscrNotifyEvent); //-- System Reg ---
      Register_Event('OnMouseLeave',TmscrNotifyEvent); //-- System Reg ---
      Register_Property('RepaintMode','TRepaintMode', ClassGetProp,ClassSetProp);
    end;
    with Register_Class(TImage32,'TCustomImage32') do
    begin
    end;
    with Register_Class(TImgView32,'TCustomImgView32') do
    begin
    end;
    with Register_Class(TIVScrollProperties,'TArrowBarAccess') do
    begin
    end;
    with Register_Class(TPaintBox32,'TCustomPaintBox32') do
    begin
    end;
    with Register_Class(TPaintStages,'TObject') do
    begin
      Register_Method('function Add: PPaintStage;', ClassCallMethod);
      Register_Method('function Count: Integer;', ClassCallMethod);
      Register_Method('function Insert(Index: Integer): PPaintStage;', ClassCallMethod);
      Register_Method('procedure Clear;', ClassCallMethod);
      Register_Method('procedure Delete(Index: Integer);', ClassCallMethod);
      Register_IndexProperty('Items', 'Integer' ,'PPaintStage', ClassCallMethod, True);
    end;

  end;
end;

function TLibrary_GR32_Image.ClassCallMethod(aInstance: TObject; aClassType: TClass; const aMethodName: string; aCallVar: TmscrVarForMethod): variant;
begin
  Result := 0;

  if aClassType=TBitmap32Collection then begin
    if aMethodName='CREATE' then begin
         Result:=fmscrInteger(TBitmap32Collection(aInstance).Create(TPersistent(fmscrInteger(aCallVar.Params[0])),TBitmap32ItemClass(fmscrInteger(aCallVar.Params[1])))); exit; end;
    if aMethodName='ADD' then begin
         Result:=fmscrInteger(TBitmap32Collection(aInstance).Add); exit; end;
    if aMethodName = 'ITEMS.GET' then begin
      Result:=fmscrInteger(TBitmap32Collection(aInstance).Items[aCallVar.Params[0]]); exit; end;
    if aMethodName = 'ITEMS.SET' then begin
      TBitmap32Collection(aInstance).Items[aCallVar.Params[0]]:= TBitmap32Item(fmscrInteger(aCallVar.Params[1])); exit; end;
   exit; end;

  if aClassType=TBitmap32Item then begin
    if aMethodName='CREATE' then begin
         Result:=fmscrInteger(TBitmap32Item(aInstance).Create(TCollection(fmscrInteger(aCallVar.Params[0])))); exit; end;
   exit; end;

  if aClassType=TBitmap32List then begin
    if aMethodName='CREATE' then begin
         Result:=fmscrInteger(TBitmap32List(aInstance).Create(TComponent(fmscrInteger(aCallVar.Params[0])))); exit; end;
    if aMethodName = 'BITMAP.GET' then begin
      Result:=fmscrInteger(TBitmap32List(aInstance).Bitmap[aCallVar.Params[0]]); exit; end;
    if aMethodName = 'BITMAP.SET' then begin
      TBitmap32List(aInstance).Bitmap[aCallVar.Params[0]]:= TBitmap32(fmscrInteger(aCallVar.Params[1])); exit; end;
   exit; end;

  if aClassType=TCustomImage32 then begin
    if aMethodName='CREATE' then begin
         Result:=fmscrInteger(TCustomImage32(aInstance).Create(TComponent(fmscrInteger(aCallVar.Params[0])))); exit; end;
    if aMethodName='BEGINUPDATE' then begin
         TCustomImage32(aInstance).BeginUpdate; exit; end;
    if aMethodName='CHANGED' then begin
         TCustomImage32(aInstance).Changed; exit; end;
    if aMethodName='ENDUPDATE' then begin
         TCustomImage32(aInstance).EndUpdate; exit; end;
    if aMethodName='EXECBITMAPFRAME' then begin
         TCustomImage32(aInstance).ExecBitmapFrame(TBitmap32(fmscrInteger(aCallVar.Params[0])),aCallVar.Params[1]); exit; end;
    if aMethodName='EXECCLEARBACKGND' then begin
         TCustomImage32(aInstance).ExecClearBackgnd(TBitmap32(fmscrInteger(aCallVar.Params[0])),aCallVar.Params[1]); exit; end;
    if aMethodName='EXECCLEARBUFFER' then begin
         TCustomImage32(aInstance).ExecClearBuffer(TBitmap32(fmscrInteger(aCallVar.Params[0])),aCallVar.Params[1]); exit; end;
    if aMethodName='EXECCONTROLFRAME' then begin
         TCustomImage32(aInstance).ExecControlFrame(TBitmap32(fmscrInteger(aCallVar.Params[0])),aCallVar.Params[1]); exit; end;
    if aMethodName='EXECCUSTOM' then begin
         TCustomImage32(aInstance).ExecCustom(TBitmap32(fmscrInteger(aCallVar.Params[0])),aCallVar.Params[1]); exit; end;
    if aMethodName='EXECDRAWBITMAP' then begin
         TCustomImage32(aInstance).ExecDrawBitmap(TBitmap32(fmscrInteger(aCallVar.Params[0])),aCallVar.Params[1]); exit; end;
    if aMethodName='EXECDRAWLAYERS' then begin
         TCustomImage32(aInstance).ExecDrawLayers(TBitmap32(fmscrInteger(aCallVar.Params[0])),aCallVar.Params[1]); exit; end;
    if aMethodName='INVALIDATE' then begin
         TCustomImage32(aInstance).Invalidate; exit; end;
    if aMethodName='LOADED' then begin
         TCustomImage32(aInstance).Loaded; exit; end;
    if aMethodName='RESIZE' then begin
         TCustomImage32(aInstance).Resize; exit; end;
    if aMethodName='SETUPBITMAP' then begin
         TCustomImage32(aInstance).SetupBitmap(aCallVar.Params[0],Cardinal(aCallVar.Params[1])); exit; end;
   exit; end;

  if aClassType=TCustomImgView32 then begin
    if aMethodName='CREATE' then begin
         Result:=fmscrInteger(TCustomImgView32(aInstance).Create(TComponent(fmscrInteger(aCallVar.Params[0])))); exit; end;
    if aMethodName='LOADED' then begin
         TCustomImgView32(aInstance).Loaded; exit; end;
    if aMethodName='RESIZE' then begin
         TCustomImgView32(aInstance).Resize; exit; end;
    if aMethodName='SCROLL' then begin
         TCustomImgView32(aInstance).Scroll(aCallVar.Params[0],aCallVar.Params[1]); exit; end;
    if aMethodName='SCROLLTOCENTER' then begin
         TCustomImgView32(aInstance).ScrollToCenter(aCallVar.Params[0],aCallVar.Params[1]); exit; end;
   exit; end;

  if aClassType=TCustomPaintBox32 then begin
    if aMethodName='CREATE' then begin
         Result:=fmscrInteger(TCustomPaintBox32(aInstance).Create(TComponent(fmscrInteger(aCallVar.Params[0])))); exit; end;
    if aMethodName='ASSIGNTO' then begin
         TCustomPaintBox32(aInstance).AssignTo(TPersistent(fmscrInteger(aCallVar.Params[0]))); exit; end;
    if aMethodName='FORCEFULLINVALIDATE' then begin
         TCustomPaintBox32(aInstance).ForceFullInvalidate; exit; end;
    if aMethodName='INVALIDATE' then begin
         TCustomPaintBox32(aInstance).Invalidate; exit; end;
    if aMethodName='LOADED' then begin
         TCustomPaintBox32(aInstance).Loaded; exit; end;
    if aMethodName='RESIZE' then begin
         TCustomPaintBox32(aInstance).Resize; exit; end;
    if aMethodName='SETBOUNDS' then begin
         TCustomPaintBox32(aInstance).SetBounds(aCallVar.Params[0],aCallVar.Params[1],aCallVar.Params[2],aCallVar.Params[3]); exit; end;
   exit; end;

  if aClassType=TPaintStages then begin
    if aMethodName='ADD' then begin
         Result:=fmscrInteger(TPaintStages(aInstance).Add); exit; end;
    if aMethodName='COUNT' then begin
         Result:=fmscrInteger(TPaintStages(aInstance).Count); exit; end;
    if aMethodName='INSERT' then begin
         Result:=fmscrInteger(TPaintStages(aInstance).Insert(aCallVar.Params[0])); exit; end;
    if aMethodName='CLEAR' then begin
         TPaintStages(aInstance).Clear; exit; end;
    if aMethodName='DELETE' then begin
         TPaintStages(aInstance).Delete(aCallVar.Params[0]); exit; end;
    if aMethodName = 'ITEMS.GET' then begin
      Result:=Pointer(TPaintStages(aInstance).Items[aCallVar.Params[0]]); exit; end;
   exit; end;

end;

function TLibrary_GR32_Image.ClassGetProp(aInstance: TObject; aClassType: TClass; const aPropName: String; aCallVar: TmscrEngProperty): Variant;
begin
  Result := 0;

  if aClassType=TBitmap32Collection then begin
  exit; end;

  if aClassType=TBitmap32Item then begin
  exit; end;

  if aClassType=TBitmap32List then begin
  exit; end;

  if aClassType=TCustomImage32 then begin
    if aPropName = 'BITMAP' then begin
      Result:=fmscrInteger(TCustomImage32(aInstance).Bitmap); exit; end;
    if aPropName = 'BITMAPALIGN' then begin
      Result:=TCustomImage32(aInstance).BitmapAlign; exit; end;
    if aPropName = 'LAYERS' then begin
      Result:=fmscrInteger(TCustomImage32(aInstance).Layers); exit; end;
    if aPropName = 'OFFSETHORZ' then begin
      Result:=TCustomImage32(aInstance).OffsetHorz; exit; end;
    if aPropName = 'OFFSETVERT' then begin
      Result:=TCustomImage32(aInstance).OffsetVert; exit; end;
    if aPropName = 'PAINTSTAGES' then begin
      Result:=fmscrInteger(TCustomImage32(aInstance).PaintStages); exit; end;
    if aPropName = 'SCALE' then begin
      Result:=TCustomImage32(aInstance).Scale; exit; end;
    if aPropName = 'SCALEMODE' then begin
      Result:=TCustomImage32(aInstance).ScaleMode; exit; end;
    if aPropName = 'SCALEX' then begin
      Result:=TCustomImage32(aInstance).ScaleX; exit; end;
    if aPropName = 'SCALEY' then begin
      Result:=TCustomImage32(aInstance).ScaleY; exit; end;
  exit; end;

  if aClassType=TCustomImgView32 then begin
    if aPropName = 'CENTERED' then begin
      Result:=TCustomImgView32(aInstance).Centered; exit; end;
    if aPropName = 'OVERSIZE' then begin
      Result:=TCustomImgView32(aInstance).OverSize; exit; end;
    if aPropName = 'SCROLLBARS' then begin
      Result:=fmscrInteger(TCustomImgView32(aInstance).ScrollBars); exit; end;
    if aPropName = 'SIZEGRIP' then begin
      Result:=TCustomImgView32(aInstance).SizeGrip; exit; end;
  exit; end;

  if aClassType=TCustomPaintBox32 then begin
    if aPropName = 'BUFFER' then begin
      Result:=fmscrInteger(TCustomPaintBox32(aInstance).Buffer); exit; end;
    if aPropName = 'BUFFEROVERSIZE' then begin
      Result:=TCustomPaintBox32(aInstance).BufferOversize; exit; end;
    if aPropName = 'MOUSEINCONTROL' then begin
      Result:=TCustomPaintBox32(aInstance).MouseInControl; exit; end;
    if aPropName = 'REPAINTMODE' then begin
      Result:=TCustomPaintBox32(aInstance).RepaintMode; exit; end;
  exit; end;

  if aClassType=TImage32 then begin
  exit; end;

  if aClassType=TImgView32 then begin
  exit; end;

  if aClassType=TIVScrollProperties then begin
  exit; end;

  if aClassType=TPaintBox32 then begin
  exit; end;

  if aClassType=TPaintStages then begin
  exit; end;


end;

procedure TLibrary_GR32_Image.ClassSetProp(aInstance: TObject; aClassType: TClass; const aPropName: String; aValue: Variant; aCallVar: TmscrEngProperty);
begin

  if aClassType=TBitmap32Collection then begin
  exit; end;

  if aClassType=TBitmap32Item then begin
  exit; end;

  if aClassType=TBitmap32List then begin
  exit; end;

  if aClassType=TCustomImage32 then begin
    if aPropName = 'BITMAP' then begin
      TCustomImage32(aInstance).Bitmap:=TBitmap32(fmscrInteger(aValue)); exit; end;
    if aPropName = 'BITMAPALIGN' then begin
      TCustomImage32(aInstance).BitmapAlign:=aValue; exit; end;
    if aPropName = 'LAYERS' then begin
      TCustomImage32(aInstance).Layers:=TLayerCollection(fmscrInteger(aValue)); exit; end;
    if aPropName = 'OFFSETHORZ' then begin
      TCustomImage32(aInstance).OffsetHorz:=aValue; exit; end;
    if aPropName = 'OFFSETVERT' then begin
      TCustomImage32(aInstance).OffsetVert:=aValue; exit; end;
    if aPropName = 'SCALE' then begin
      TCustomImage32(aInstance).Scale:=aValue; exit; end;
    if aPropName = 'SCALEMODE' then begin
      TCustomImage32(aInstance).ScaleMode:=aValue; exit; end;
    if aPropName = 'SCALEX' then begin
      TCustomImage32(aInstance).ScaleX:=aValue; exit; end;
    if aPropName = 'SCALEY' then begin
      TCustomImage32(aInstance).ScaleY:=aValue; exit; end;
  exit; end;

  if aClassType=TCustomImgView32 then begin
    if aPropName = 'CENTERED' then begin
      TCustomImgView32(aInstance).Centered:=aValue; exit; end;
    if aPropName = 'OVERSIZE' then begin
      TCustomImgView32(aInstance).OverSize:=aValue; exit; end;
    if aPropName = 'SCROLLBARS' then begin
      TCustomImgView32(aInstance).ScrollBars:=TIVScrollProperties(fmscrInteger(aValue)); exit; end;
    if aPropName = 'SIZEGRIP' then begin
      TCustomImgView32(aInstance).SizeGrip:=aValue; exit; end;
  exit; end;

  if aClassType=TCustomPaintBox32 then begin
    if aPropName = 'BUFFEROVERSIZE' then begin
      TCustomPaintBox32(aInstance).BufferOversize:=aValue; exit; end;
    if aPropName = 'REPAINTMODE' then begin
      TCustomPaintBox32(aInstance).RepaintMode:=aValue; exit; end;
  exit; end;

  if aClassType=TImage32 then begin
  exit; end;

  if aClassType=TImgView32 then begin
  exit; end;

  if aClassType=TIVScrollProperties then begin
  exit; end;

  if aClassType=TPaintBox32 then begin
  exit; end;

  if aClassType=TPaintStages then begin
  exit; end;


end;


//===============================================
Initialization
  MSCR_RTTILibraries.Add(TLibrary_GR32_Image);
Finalization
  MSCR_RTTILibraries.Remove(TLibrary_GR32_Image);
End.

