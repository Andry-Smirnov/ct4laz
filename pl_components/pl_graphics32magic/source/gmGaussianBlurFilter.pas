{*************************************************************************
  Package pl_Graphics32Magic
  This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)  

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/
 
  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the
  License.
 
  Initial Developers:
    Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com > 
    x2nie - Fathony Luthfillah  <x2nie@yahoo.com> 
  
**************************************************************************}

unit gmGaussianBlurFilter;

interface

uses
  Math,
  GR32,
  GR32_LowLevel,
  gmImageProcessFuncs,
  gmKernel, gmTypes;

type
  { A filter which applies Gaussian blur to an image. The filter simply creates
    a kernel with a Gaussian distribution for blurring.
    
    @original author Jerry Huxtable }
    
  TgmGaussianFilter = class(TObject)
  private
    FAlpha  : Boolean;
    FKernel : TgmKernel;

    function GetRadius: Single;
    procedure SetRadius(const ARadius: Single);
  protected
    FRadius : Single;

    procedure ConvolveAndTranspose(const AKernel: TgmKernel;
      const AInPixels, AOutPixels: TArrayOfColor32;
      const AWidth, AHeight: Integer; const AIsAlpha: Boolean;
      const AEdgeAction: TgmEdgeAction);
  public
    constructor Create; overload;
    constructor Create(const ARadius: Single); overload;

    destructor Destroy; override;

    function Execute(const ASourceBmp, ADestBmp: TBitmap32): Boolean;
    function ToString: string;

    property Radius  : Single  read GetRadius write SetRadius;
    property IsAlpha : Boolean read FAlpha    write FAlpha;
  end;

function MakeKernel(const ARadius: Single): TgmKernel;

implementation

// Make a Gaussian blur kernel.
function MakeKernel(const ARadius: Single): TgmKernel;
var
  i, r, LIndex  : Integer;
  LRow, LRows   : Integer;
  LSigma        : Single;
  LSigma22      : Single;
  LSigmaPi2     : Single;
  LSqrtSigmaPi2 : Single;
  LRadius2      : Single;
  LTotal        : Single;
  LDistance     : Single;
  LMatrix       : array of Single;
begin
  r             := Ceil(ARadius);
  LRows         := r * 2 + 1;
  LSigma        := ARadius / 3;
  LSigma22      := 2 * LSigma * LSigma;
  LSigmaPi2     := 2 * PI * LSigma;
  LSqrtSigmaPi2 := Sqrt(LSigmaPi2);
  LRadius2      := ARadius * ARadius;
  LTotal        := 0.0;
  LIndex        := 0;
  
  SetLength(LMatrix, LRows);
  try
    for LRow := -r to r do
    begin
      LDistance := LRow * LRow;

      if LDistance > LRadius2 then
      begin
        LMatrix[LIndex] := 0.0;
      end
      else
      begin
        LMatrix[LIndex] := Exp(-LDistance / LSigma22) / LSqrtSigmaPi2;
      end;

      LTotal := LTotal + LMatrix[LIndex];

      Inc(LIndex);
    end;

    for i := 0 to (LRows - 1) do
    begin
      LMatrix[i] := LMatrix[i] / LTotal;
    end;

    Result := TgmKernel.Create(LRows, 1, LMatrix);
  finally
    SetLength(LMatrix, 0);
    LMatrix := nil;
  end;
end;


{ TgmGaussianFilter }

// construct a Gaussian filter
constructor TgmGaussianFilter.Create;
begin
  inherited Create;

  FAlpha := True;
  SetRadius(2.0);
end;

{ construct a Gaussian filter
  @param ARadius blur radius in pixels }
constructor TgmGaussianFilter.Create(const ARadius: Single);
begin
  inherited Create;
  
  FAlpha := True;
  SetRadius(ARadius);
end;

destructor TgmGaussianFilter.Destroy;
begin
  FKernel.Free;
  inherited Destroy;
end;

{ Get the radius of the kernel.
	@return the radius }
function TgmGaussianFilter.GetRadius: Single;
begin
  Result := FRadius;
end;

{ Set the radius of the kernel, and hence the amount of blur. The bigger the
  radius, the longer this filter will take.
  @param radius the radius of the blur in pixels. }
procedure TgmGaussianFilter.SetRadius(const ARadius: Single);
begin
  FRadius := ARadius;
  FKernel := MakeKernel(FRadius);
end;

procedure TgmGaussianFilter.ConvolveAndTranspose(const AKernel: TgmKernel;
  const AInPixels, AOutPixels: TArrayOfColor32;
  const AWidth, AHeight: Integer; const AIsAlpha: Boolean;
  const AEdgeAction: TgmEdgeAction);
var
  x, y, ix, LIndex    : Integer;
  LIOffset, LMOffset  : Integer;
  LCol, LCols, LCols2 : Integer;
  a, r, g, b, f       : Single;
  ia, ir, ig, ib      : Byte;
  LRGB                : TColor32;
begin
{$RANGECHECKS OFF}
  LCols  := AKernel.Width;
  LCols2 := LCols div 2;

  for y := 0 to (AHeight - 1) do
  begin
    LIndex   := y;
    LIOffset := y * AWidth;

    for x := 0 to (AWidth - 1) do
    begin
      a := 0.0;
      r := 0.0;
      g := 0.0;
      b := 0.0;

      LMOffset := LCols2;

      for LCol := -LCols2 to LCols2 do
      begin
        f := AKernel.Data[LMOffset + LCol];

        if f <> 0.0 then
        begin
          ix := x + LCol;

          if ix < 0 then
          begin
            if AEdgeAction = eaClampEdges then
            begin
              ix := 0;
            end
            else if AEdgeAction = eaWrapEdges then
            begin
              //ix := (x + AWidth) mod AWidth;  // the original code

              // Enlightened by PAEz: Result := Lower - (Result - Lower);
              ix := -ix;
            end;
          end
          else if ix >= AWidth then
          begin
            if AEdgeAction = eaClampEdges then
            begin
              ix := AWidth - 1;
            end
            else if AEdgeAction = eaWrapEdges then
            begin
              //ix := (x + AWidth) mod AWidth;  // the original code

              // Enlightened by PAEz: Result := Upper - (Result - Upper);
              ix := (AWidth - 1) - (ix - AWidth + 1);
            end;
          end;

          LRGB := AInPixels[LIOffset + ix];
          ia   := LRGB shr 24 and $FF;
          ir   := LRGB shr 16 and $FF;
          ig   := LRGB shr  8 and $FF;
          ib   := LRGB        and $FF;

          a := a + f * ia;
          r := r + f * ir;
          g := g + f * ig;
          b := b + f * ib;
        end;
      end;

      if AIsAlpha then
      begin
        ia := Clamp( Round(a), 0, 255 );
      end
      else
      begin
        ia := 255;
      end;

      ir := Clamp( Round(r), 0, 255 );
      ig := Clamp( Round(g), 0, 255 );
      ib := Clamp( Round(b), 0, 255 );

      AOutPixels[LIndex] := (ia shl 24) or (ir shl 16) or (ig shl 8) or ib;

      LIndex := LIndex + AHeight;
    end;
  end;
{$RANGECHECKS ON}
end;

function TgmGaussianFilter.Execute(
  const ASourceBmp, ADestBmp: TBitmap32): Boolean;
var
  LWidth      : Integer;
  LHeight     : Integer;
  LPixelCount : Integer;
  LInPixels   : TArrayOfColor32;
  LOutPixels  : TArrayOfColor32;
begin
  Result := False;
  
  if (not Assigned(ASourceBmp)) or
     (not Assigned(ADestBmp)) or
     (ASourceBmp.Width <= 0) or
     (ASourceBmp.Height <= 0) then
  begin
    Exit;
  end;

  LWidth      := ASourceBmp.Width;
  LHeight     := ASourceBmp.Height;
  LPixelCount := LWidth * LHeight;

  SetLength(LInPixels,  LPixelCount);
  SetLength(LOutPixels, LPixelCount);
  try
    GetRGB(ASourceBmp, LInPixels);

    ConvolveAndTranspose(FKernel, LInPixels, LOutPixels, LWidth, LHeight, FAlpha, eaWrapEdges);
    ConvolveAndTranspose(FKernel, LOutPixels, LInPixels, LHeight, LWidth, FAlpha, eaWrapEdges);

    Result := SetRGB(ADestBmp, LWidth, LHeight, LInPixels);
  finally
    SetLength(LInPixels,  0);
    SetLength(LOutPixels, 0);

    LInPixels  := nil;
    LOutPixels := nil;
  end;
end;

function TgmGaussianFilter.ToString: string;
begin
  Result := 'Blur/Gaussian Blur...';
end;

end.
