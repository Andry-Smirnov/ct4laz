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
 * ***** END LICENSE BLOCK *****
 ************************************************************************}

unit GR32_Backends_LCL_Wince;

interface

{$I GR32.inc}

uses
  Windows, LCLIntf, LCLType, Types, Controls,
  SysUtils, Classes, Graphics, GR32, GR32_Backends, GR32_Backends_Generic,
  GR32_Containers, GR32_Image, GR32_Paths;

type
  { TLCLBackend }
  { This backend uses the LCL to manage and provide the buffer and additional
    graphics sub system features. The backing buffer is kept in memory. }

  TLCLBackend = class(TCustomBackend, IPaintSupport,
    IBitmapContextSupport, IDeviceContextSupport,
    ITextSupport, IFontSupport, ITextToPathSupport, ICanvasSupport)
  private
    procedure FontChangedHandler(Sender: TObject);
    procedure CanvasChangedHandler(Sender: TObject);
    procedure CanvasChanged;
    procedure FontChanged;
  protected
    FBitmapInfo: TBitmapInfo;
    FBitmapHandle: HBITMAP;
    FHDC: HDC;
    FFont: TFont;
    FCanvas: TCanvas;
    FFontHandle: HFont;
    FMapHandle: THandle;

    FOnFontChange: TNotifyEvent;
    FOnCanvasChange: TNotifyEvent;

    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;

    procedure PrepareFileMapping(NewWidth, NewHeight: Integer); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Changed; override;

    function Empty: Boolean; override;
  public
    { IPaintSupport }
    procedure ImageNeeded;
    procedure CheckPixmap;
    procedure DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList;
      ACanvas: TCanvas; APaintBox: TCustomPaintBox32);

    { IBitmapContextSupport }
    function GetBitmapInfo: TBitmapInfo;
    function GetBitmapHandle: THandle;

    property BitmapInfo: TBitmapInfo read GetBitmapInfo;
    property BitmapHandle: THandle read GetBitmapHandle;

    { IDeviceContextSupport }
    function GetHandle: HDC;

    procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;
    procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;

    property Handle: HDC read GetHandle;

    { ITextSupport }
    procedure Textout(X, Y: Integer; const Text: String); overload;
    procedure Textout(X, Y: Integer; const ClipRect: TRect; const Text: String); overload;
    procedure Textout(var DstRect: TRect; const Flags: Cardinal; const Text: String); overload;
    function  TextExtent(const Text: String): TSize;

    procedure TextoutW(X, Y: Integer; const Text: Widestring); overload;
    procedure TextoutW(X, Y: Integer; const ClipRect: TRect; const Text: Widestring); overload;
    procedure TextoutW(var DstRect: TRect; const Flags: Cardinal; const Text: Widestring); overload;
    function  TextExtentW(const Text: Widestring): TSize;

    { IFontSupport }
    function GetOnFontChange: TNotifyEvent;
    procedure SetOnFontChange(Handler: TNotifyEvent);
    function GetFont: TFont;
    procedure SetFont(const Font: TFont);

    procedure UpdateFont;
    property Font: TFont read GetFont write SetFont;
    property OnFontChange: TNotifyEvent read FOnFontChange write FOnFontChange;

    { ITextToPathSupport }
    procedure TextToPath(Path: TCustomPath; const X, Y: TFloat; const Text: WideString); overload;
    procedure TextToPath(Path: TCustomPath; const DstRect: TFloatRect; const Text: WideString; Flags: Cardinal); overload;
    function MeasureText(const DstRect: TFloatRect; const Text: WideString; Flags: Cardinal): TFloatRect;

    { ICanvasSupport }
    function GetCanvasChange: TNotifyEvent;
    procedure SetCanvasChange(Handler: TNotifyEvent);
    function GetCanvas: TCanvas;

    procedure DeleteCanvas;
    function CanvasAllocated: Boolean;

    property Canvas: TCanvas read GetCanvas;
    property OnCanvasChange: TNotifyEvent read GetCanvasChange write SetCanvasChange;
  end;

  { TLCLGDIMMFBackend }
  { Same as TGDIBackend but relies on memory mapped files or mapped swap space
    for the backing buffer. }

  TLCLMMFBackend = class(TLCLBackend)
  private
    FMapFileHandle: THandle;
    FMapIsTemporary: Boolean;
    FMapFileName: string;
  protected
    procedure PrepareFileMapping(NewWidth, NewHeight: Integer); override;
  public
    constructor Create(Owner: TBitmap32; IsTemporary: Boolean = True; const MapFileName: string = ''); virtual;
    destructor Destroy; override;
  end;

  { TGDIMemoryBackend }
  { A backend that keeps the backing buffer entirely in memory and offers
    IPaintSupport without allocating a GDI handle }

  TLCLMemoryBackend = class(TMemoryBackend, IPaintSupport, IDeviceContextSupport)
  private
    procedure DoPaintRect(ABuffer: TBitmap32; ARect: TRect; ACanvas: TCanvas);

    function GetHandle: HDC; // Dummy
  protected
    FBitmapInfo: TBitmapInfo;

    procedure InitializeSurface(NewWidth: Integer; NewHeight: Integer;
      ClearBuffer: Boolean); override;
  public
    constructor Create; override;

    { IPaintSupport }
    procedure ImageNeeded;
    procedure CheckPixmap;
    procedure DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList; ACanvas: TCanvas; APaintBox: TCustomPaintBox32);

    { IDeviceContextSupport }
    procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;
    procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;
  end;

implementation

uses
  GR32_Text_LCL_Wince;

var
  StockFont: HFONT;

{ TLCLBackend }

constructor TLCLBackend.Create;
begin
  inherited;

  FillChar(FBitmapInfo, SizeOf(TBitmapInfo), 0);
  with FBitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
  end;

  FMapHandle := 0;

  FFont := TFont.Create;
  FFont.OnChange := FontChangedHandler;
end;

destructor TLCLBackend.Destroy;
begin
  DeleteCanvas;
  FFont.Free;

  inherited;
end;

procedure TLCLBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  with FBitmapInfo.bmiHeader do
  begin
    biWidth := NewWidth;
    biHeight := -NewHeight;
    biSizeImage := NewWidth * NewHeight * 4;
  end;

  PrepareFileMapping(NewWidth, NewHeight);

  FBitmapHandle := LCLIntf.CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS, Pointer(FBits), FMapHandle, 0);

  if FBits = nil then
    raise Exception.Create(RCStrCannotAllocateDIBHandle);

  FHDC := CreateCompatibleDC(0);
  if FHDC = 0 then
  begin
    DeleteObject(FBitmapHandle);
    FBitmapHandle := 0;
    FBits := nil;
    raise Exception.Create(RCStrCannotCreateCompatibleDC);
  end;

  if SelectObject(FHDC, FBitmapHandle) = 0 then
  begin
    DeleteDC(FHDC);
    DeleteObject(FBitmapHandle);
    FHDC := 0;
    FBitmapHandle := 0;
    FBits := nil;
    raise Exception.Create(RCStrCannotSelectAnObjectIntoDC);
  end;
end;

procedure TLCLBackend.FinalizeSurface;
begin
  if FHDC <> 0 then DeleteDC(FHDC);
  FHDC := 0;
  if FBitmapHandle <> 0 then DeleteObject(FBitmapHandle);
  FBitmapHandle := 0;

  FBits := nil;
end;

procedure TLCLBackend.DeleteCanvas;
begin
  if Assigned(FCanvas) then
  begin
    FCanvas.Handle := 0;
    FCanvas.Free;
    FCanvas := nil;
  end;
end;

procedure TLCLBackend.PrepareFileMapping(NewWidth, NewHeight: Integer);
begin
  // to be implemented by descendants
end;

procedure TLCLBackend.Changed;
begin
  if FCanvas <> nil then FCanvas.Handle := Self.Handle;
  inherited;
end;

procedure TLCLBackend.CanvasChanged;
begin
  if Assigned(FOnCanvasChange) then
    FOnCanvasChange(Self);
end;

procedure TLCLBackend.FontChanged;
begin
  if Assigned(FOnFontChange) then
    FOnFontChange(Self);
end;

function TLCLBackend.TextExtent(const Text: String): TSize;
var
  DC: HDC;
  OldFont: HGDIOBJ;
begin
  UpdateFont;
  Result.cX := 0;
  Result.cY := 0;
  if Handle <> 0 then
    GetTextExtentPoint32(Handle, PChar(Text), Length(Text), Result)
  else
  begin
    StockBitmap.Canvas.Lock;
    try
      DC := StockBitmap.Canvas.Handle;
      OldFont := SelectObject(DC, Font.Handle);
      GetTextExtentPoint32(DC, PChar(Text), Length(Text), Result);
      SelectObject(DC, OldFont);
    finally
      StockBitmap.Canvas.Unlock;
    end;
  end;
end;

function TLCLBackend.TextExtentW(const Text: Widestring): TSize;
var
  DC: HDC;
  OldFont: HGDIOBJ;
begin
  UpdateFont;
  Result.cX := 0;
  Result.cY := 0;

  if Handle <> 0 then
    GetTextExtentPoint32W(Handle, PWideChar(Text), Length(Text), Result)
  else
  begin
    StockBitmap.Canvas.Lock;
    try
      DC := StockBitmap.Canvas.Handle;
      OldFont := SelectObject(DC, Font.Handle);
      GetTextExtentPoint32W(DC, PWideChar(Text), Length(Text), Result);
      SelectObject(DC, OldFont);
    finally
      StockBitmap.Canvas.Unlock;
    end;
  end;
end;

procedure TLCLBackend.Textout(X, Y: Integer; const Text: String);
var
  Extent: TSize;
begin
  UpdateFont;

  if not FOwner.MeasuringMode then
  begin
    if FOwner.Clipping then
      ExtTextout(Handle, X, Y, ETO_CLIPPED, @FOwner.ClipRect, PChar(Text), Length(Text), nil)
    else
      ExtTextout(Handle, X, Y, 0, nil, PChar(Text), Length(Text), nil);
  end;

  Extent := TextExtent(Text);
  FOwner.Changed(MakeRect(X, Y, X + Extent.cx + 1, Y + Extent.cy + 1));
end;

procedure TLCLBackend.TextoutW(X, Y: Integer; const Text: Widestring);
var
  Extent: TSize;
begin
  UpdateFont;

  if not FOwner.MeasuringMode then
  begin
    if FOwner.Clipping then
      ExtTextoutW(Handle, X, Y, ETO_CLIPPED, @FOwner.ClipRect, PWideChar(Text), Length(Text), nil)
    else
      ExtTextoutW(Handle, X, Y, 0, nil, PWideChar(Text), Length(Text), nil);
  end;

  Extent := TextExtentW(Text);
  FOwner.Changed(MakeRect(X, Y, X + Extent.cx + 1, Y + Extent.cy + 1));
end;

procedure TLCLBackend.TextoutW(X, Y: Integer; const ClipRect: TRect; const Text: Widestring);
var
  Extent: TSize;
begin
  UpdateFont;

  if not FOwner.MeasuringMode then
    ExtTextoutW(Handle, X, Y, ETO_CLIPPED, @ClipRect, PWideChar(Text), Length(Text), nil);

  Extent := TextExtentW(Text);
  FOwner.Changed(MakeRect(X, Y, X + Extent.cx + 1, Y + Extent.cy + 1));
end;

procedure TLCLBackend.Textout(X, Y: Integer; const ClipRect: TRect; const Text: String);
var
  Extent: TSize;
begin
  UpdateFont;

  if not FOwner.MeasuringMode then
    ExtTextout(Handle, X, Y, ETO_CLIPPED, @ClipRect, PChar(Text), Length(Text), nil);

  Extent := TextExtent(Text);
  FOwner.Changed(MakeRect(X, Y, X + Extent.cx + 1, Y + Extent.cy + 1));
end;

procedure TLCLBackend.TextoutW(var DstRect: TRect; const Flags: Cardinal; const Text: Widestring);
begin
  UpdateFont;

  if not FOwner.MeasuringMode then
    DrawTextW(Handle, PWideChar(Text), Length(Text), DstRect, Flags);

  FOwner.Changed(DstRect);
end;

procedure TLCLBackend.UpdateFont;
begin
  if (FFontHandle = 0) and (Handle <> 0) then
  begin
    SelectObject(Handle, Font.Handle);
    SetTextColor(Handle, ColorToRGB(Font.Color));
    SetBkMode(Handle, TRANSPARENT);
    FFontHandle := Font.Handle;
  end
  else
  begin
    SelectObject(Handle, FFontHandle);
    SetTextColor(Handle, ColorToRGB(Font.Color));
    SetBkMode(Handle, TRANSPARENT);
  end;
end;

procedure TLCLBackend.TextToPath(Path: TCustomPath; const X, Y: TFloat;
  const Text: WideString);
var
  R: TFloatRect;
begin
  R := FloatRect(X, Y, X, Y);
  GR32_Text_LCL_Wince.TextToPath(Font.Handle, Path, R, Text, 0);
end;

procedure TLCLBackend.TextToPath(Path: TCustomPath; const DstRect: TFloatRect;
  const Text: WideString; Flags: Cardinal);
begin
  GR32_Text_LCL_Wince.TextToPath(Font.Handle, Path, DstRect, Text, Flags);
end;

function TLCLBackend.MeasureText(const DstRect: TFloatRect;
  const Text: WideString; Flags: Cardinal): TFloatRect;
begin
  Result := GR32_Text_LCL_Wince.MeasureText(Font.Handle, DstRect, Text, Flags);
end;

procedure TLCLBackend.Textout(var DstRect: TRect; const Flags: Cardinal; const Text: String);
begin
  UpdateFont;

  if not FOwner.MeasuringMode then
    DrawText(Handle, PChar(Text), Length(Text), DstRect, Flags);

  FOwner.Changed(DstRect);
end;

procedure TLCLBackend.DrawTo(hDst: HDC; DstX, DstY: Integer);
begin
  Windows.BitBlt(hDst, DstX, DstY, FOwner.Width, FOwner.Height, Handle, 0, 0,
    SRCCOPY);
(*
StretchDIBits(
    hDst, DstX, DstY, FOwner.Width, FOwner.Height,
    0, 0, FOwner.Width, FOwner.Height, Bits, FBitmapInfo, DIB_RGB_COLORS, SRCCOPY);
*)
end;

procedure TLCLBackend.DrawTo(hDst: HDC; const DstRect, SrcRect: TRect);
begin
  Windows.StretchBlt(hDst,
    DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top, Handle,
    SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, SRCCOPY);
end;

function TLCLBackend.GetBitmapHandle: THandle;
begin
  Result := FBitmapHandle;
end;

function TLCLBackend.GetBitmapInfo: TBitmapInfo;
begin
  Result := FBitmapInfo;
end;

function TLCLBackend.GetCanvas: TCanvas;
begin
  if not Assigned(FCanvas) then
  begin
    FCanvas := TCanvas.Create;
    FCanvas.Handle := Handle;
    FCanvas.OnChange := CanvasChangedHandler;
  end;
  Result := FCanvas;
end;

function TLCLBackend.GetCanvasChange: TNotifyEvent;
begin
  Result := FOnCanvasChange;
end;

function TLCLBackend.GetFont: TFont;
begin
  Result := FFont;
end;

function TLCLBackend.GetHandle: HDC;
begin
  Result := FHDC;
end;

function TLCLBackend.GetOnFontChange: TNotifyEvent;
begin
  Result := FOnFontChange;
end;

procedure TLCLBackend.SetCanvasChange(Handler: TNotifyEvent);
begin
  FOnCanvasChange := Handler;
end;

procedure TLCLBackend.SetFont(const Font: TFont);
begin
  FFont.Assign(Font);
  FontChanged;
end;

procedure TLCLBackend.SetOnFontChange(Handler: TNotifyEvent);
begin
  FOnFontChange := Handler;
end;

procedure TLCLBackend.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
begin
  if FOwner.Empty then Exit;

  if not FOwner.MeasuringMode then
    Windows.StretchBlt(Handle, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
      DstRect.Bottom - DstRect.Top, hSrc, SrcRect.Left, SrcRect.Top,
      SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, SRCCOPY);

  FOwner.Changed(DstRect);
end;

function TLCLBackend.CanvasAllocated: Boolean;
begin
  Result := Assigned(FCanvas);
end;

function TLCLBackend.Empty: Boolean;
begin
  Result := FBitmapHandle = 0;
end;

procedure TLCLBackend.FontChangedHandler(Sender: TObject);
begin
  if FFontHandle <> 0 then
  begin
    if Handle <> 0 then SelectObject(Handle, StockFont);
    FFontHandle := 0;
  end;

  FontChanged;
end;

procedure TLCLBackend.CanvasChangedHandler(Sender: TObject);
begin
  CanvasChanged;
end;

{ IPaintSupport }

procedure TLCLBackend.ImageNeeded;
begin

end;

procedure TLCLBackend.CheckPixmap;
begin

end;

procedure TLCLBackend.DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList;
  ACanvas: TCanvas; APaintBox: TCustomPaintBox32);
var
  i: Integer;
begin
  if AInvalidRects.Count > 0 then
    for i := 0 to AInvalidRects.Count - 1 do
      with AInvalidRects[i]^ do
        Windows.BitBlt(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top, ABuffer.Handle, Left, Top, SRCCOPY)
  else
    with APaintBox.GetViewportRect do
      Windows.BitBlt(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top, ABuffer.Handle, Left, Top, SRCCOPY);
end;


{ TLCLMMFBackend }

constructor TLCLMMFBackend.Create(Owner: TBitmap32; IsTemporary: Boolean = True; const MapFileName: string = '');
begin
  FMapFileName := MapFileName;
  FMapIsTemporary := IsTemporary;
  //TMMFBackend.InitializeFileMapping(FMapHandle, FMapFileHandle, FMapFileName);
  inherited Create(Owner);
end;

destructor TLCLMMFBackend.Destroy;
begin
  //TMMFBackend.DeinitializeFileMapping(FMapHandle, FMapFileHandle, FMapFileName);
  inherited;
end;

procedure TLCLMMFBackend.PrepareFileMapping(NewWidth, NewHeight: Integer);
begin
  //TMMFBackend.CreateFileMapping(FMapHandle, FMapFileHandle, FMapFileName, FMapIsTemporary, NewWidth, NewHeight);
end;


{ TLCLMemoryBackend }

constructor TLCLMemoryBackend.Create;
begin
  inherited;
  FillChar(FBitmapInfo, SizeOf(TBitmapInfo), 0);
  with FBitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biXPelsPerMeter := 96;
    biYPelsPerMeter := 96;
    biClrUsed := 0;
  end;
end;

procedure TLCLMemoryBackend.InitializeSurface(NewWidth, NewHeight: Integer;
  ClearBuffer: Boolean);
begin
  inherited;
  with FBitmapInfo.bmiHeader do
  begin
    biWidth := NewWidth;
    biHeight := -NewHeight;
  end;
end;

procedure TLCLMemoryBackend.ImageNeeded;
begin

end;

procedure TLCLMemoryBackend.CheckPixmap;
begin

end;

procedure TLCLMemoryBackend.DoPaintRect(ABuffer: TBitmap32;
  ARect: TRect; ACanvas: TCanvas);
var
  Bitmap        : HBITMAP;
  DeviceContext : HDC;
  Buffer        : Pointer;
  OldObject     : HGDIOBJ;
begin
  {$IFDEF LCLWin32}
  if SetDIBitsToDevice(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Right -
    ARect.Left, ARect.Bottom - ARect.Top, ARect.Left, ARect.Top, 0,
    ARect.Bottom - ARect.Top, ABuffer.Bits, Windows.BITMAPINFO(FBitmapInfo), DIB_RGB_COLORS) = 0 then
  begin
    // create compatible device context
    DeviceContext := CreateCompatibleDC(ACanvas.Handle);
    if DeviceContext <> 0 then
    try
      Bitmap := CreateDIBSection(DeviceContext, FBitmapInfo, DIB_RGB_COLORS,
        Buffer, 0, 0);

      if Bitmap <> 0 then
      begin
        OldObject := SelectObject(DeviceContext, Bitmap);
        try
          Move(ABuffer.Bits^, Buffer^, FBitmapInfo.bmiHeader.biWidth *
            FBitmapInfo.bmiHeader.biHeight * SizeOf(Cardinal));
          Windows.BitBlt(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Right -
            ARect.Left, ARect.Bottom - ARect.Top, DeviceContext, 0, 0, SRCCOPY);
        finally
          if OldObject <> 0 then
            SelectObject(DeviceContext, OldObject);
          DeleteObject(Bitmap);
        end;
      end else
        raise Exception.Create(RCStrCannotCreateCompatibleDC);
    finally
      DeleteDC(DeviceContext);
    end;
  end;
  {$ELSE}
  raise Exception.Create('"SetDIBitsToDevice" is only included in windows unit!')
  {$ENDIF}
end;

procedure TLCLMemoryBackend.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
begin
  if FOwner.Empty then Exit;

  if not FOwner.MeasuringMode then
    raise Exception.Create('Not yet supported!');

  FOwner.Changed(DstRect);
end;

procedure TLCLMemoryBackend.DrawTo(hDst: HDC; DstX, DstY: Integer);
var
  Bitmap: HBITMAP;
  DeviceContext: HDC;
  Buffer: Pointer;
  OldObject: HGDIOBJ;
begin
  {$IFDEF LCLWin32}
  if SetDIBitsToDevice(hDst, DstX, DstY,
    FOwner.Width, FOwner.Height, 0, 0, 0, FOwner.Height, FBits,
    Windows.BITMAPINFO(FBitmapInfo), DIB_RGB_COLORS) = 0 then
  begin
    // create compatible device context
    DeviceContext := CreateCompatibleDC(hDst);
    if DeviceContext <> 0 then
    try
      Bitmap := CreateDIBSection(DeviceContext, FBitmapInfo, DIB_RGB_COLORS,
        Buffer, 0, 0);

      if Bitmap <> 0 then
      begin
        OldObject := SelectObject(DeviceContext, Bitmap);
        try
          Move(FBits^, Buffer^, FBitmapInfo.bmiHeader.biWidth *
            FBitmapInfo.bmiHeader.biHeight * SizeOf(Cardinal));
          Windows.BitBlt(hDst, DstX, DstY, FOwner.Width, FOwner.Height, DeviceContext,
            0, 0, SRCCOPY);
        finally
          if OldObject <> 0 then
            SelectObject(DeviceContext, OldObject);
          DeleteObject(Bitmap);
        end;
      end else
        raise Exception.Create('Can''t create compatible DC''');
    finally
      DeleteDC(DeviceContext);
    end;
  end;
  {$ELSE}
  raise Exception.Create('"SetDIBitsToDevice" is only included in windows unit!')
  {$ENDIF}
end;

procedure TLCLMemoryBackend.DrawTo(hDst: HDC; const DstRect, SrcRect: TRect);
var
  Bitmap: HBITMAP;
  DeviceContext: HDC;
  Buffer: Pointer;
  OldObject: HGDIOBJ;
begin
  {$IFDEF LCLWin32}
  if SetDIBitsToDevice(hDst, DstRect.Left, DstRect.Top,
    DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top, SrcRect.Left,
    SrcRect.Top, 0, SrcRect.Bottom - SrcRect.Top, FBits,
    Windows.BITMAPINFO(FBitmapInfo), DIB_RGB_COLORS) = 0 then
  begin
    // create compatible device context
    DeviceContext := CreateCompatibleDC(hDst);
    if DeviceContext <> 0 then
    try
      Buffer := nil;
      Bitmap := CreateDIBSection(DeviceContext, FBitmapInfo, DIB_RGB_COLORS,
        Buffer, 0, 0);

      if Bitmap <> 0 then
      begin
        OldObject := SelectObject(DeviceContext, Bitmap);
        try
          Move(FBits^, Buffer^, FBitmapInfo.bmiHeader.biWidth *
            FBitmapInfo.bmiHeader.biHeight * SizeOf(Cardinal));
          Windows.BitBlt(hDst, DstRect.Left, DstRect.Top, DstRect.Right -
            DstRect.Left, DstRect.Bottom - DstRect.Top, DeviceContext, 0, 0, SRCCOPY);
        finally
          if OldObject <> 0 then
            SelectObject(DeviceContext, OldObject);
          DeleteObject(Bitmap);
        end;
      end else
        raise Exception.Create('Can''t create compatible DC''');
    finally
      DeleteDC(DeviceContext);
    end;
  end;
  {$ELSE}
  raise Exception.Create('"SetDIBitsToDevice" is only included in windows unit!')
  {$ENDIF}
end;

function TLCLMemoryBackend.GetHandle: HDC;
begin
  Result := 0;
end;

procedure TLCLMemoryBackend.DoPaint(ABuffer: TBitmap32;
  AInvalidRects: TRectList; ACanvas: TCanvas; APaintBox: TCustomPaintBox32);
var
  i : Integer;
begin
  if AInvalidRects.Count > 0 then
    for i := 0 to AInvalidRects.Count - 1 do
      DoPaintRect(ABuffer, AInvalidRects[i]^, ACanvas)
  else
    DoPaintRect(ABuffer, APaintBox.GetViewportRect, ACanvas);
end;

initialization
  StockFont := GetStockObject(SYSTEM_FONT);

finalization

end.
