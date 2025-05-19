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


unit gmResamplers;

interface

uses
  Classes, GR32, GR32_Resamplers;

type
  TgmResamplerSelector = (rsNearest,
                          rsLinear,
                          rsDraft,
                          rsKernel);

  TgmKernelSelector = (ksBox,
                       ksLinear,
                       ksCosine,
                       ksSpline,
                       ksCubic,
                       ksMitchell,
                       ksAlbrecht,
                       ksLanczos,
                       ksGaussian,
                       ksBlackman,
                       ksHann,
                       ksHamming,
                       ksSinsh,
                       ksHermite);

  TgmResamplingOptions = record
    Resampler      : TgmResamplerSelector;
    PixelAccessMode: TPixelAccessMode;
    WrapMode       : TWrapMode;
    Kernel         : TgmKernelSelector;
    KernelMode     : TKernelMode;
    TableSize      : Integer
  end;

  procedure ExecuteResample(const ADestBmp: TBitmap32;
    const ANewWidth, ANewHeight: Integer; const AOptions: TgmResamplingOptions);

  function ResamplerNameList: TStringList;
  function PixelAccessModeList: TStringList;
  function WrapModeList: TStringList;
  function KernelNameList: TStringList;
  function KernelModeList: TStringList;

implementation

uses
  gmImageProcessFuncs;

procedure ExecuteResample(const ADestBmp: TBitmap32;
  const ANewWidth, ANewHeight: Integer; const AOptions: TgmResamplingOptions);
var
  R     : TCustomResampler;
  KR    : TKernelResampler;
  SrcBmp: TBitmap32;
begin
  SrcBmp := TBitmap32.Create;
  try
    SrcBmp.Assign(ADestBmp);
    SrcBmp.WrapMode := AOptions.WrapMode;

    ADestBmp.Width  := ANewWidth;
    ADestBmp.Height := ANewHeight;

    case AOptions.Resampler of
      rsNearest:
        begin
          TNearestResampler.Create(SrcBmp);
        end;

      rsLinear:
        begin
          TLinearResampler.Create(SrcBmp);
        end;

      rsDraft:
        begin
          TDraftResampler.Create(SrcBmp);
        end;

      rsKernel:
        begin
          R  := TKernelResampler.Create(SrcBmp);
          KR := TKernelResampler(R);

          case AOptions.Kernel of
            ksBox:
              begin
                KR.Kernel := TBoxKernel.Create;
              end;

            ksLinear:
              begin
                KR.Kernel := TLinearKernel.Create;
              end;

            ksCosine:
              begin
                KR.Kernel := TCosineKernel.Create;
              end;

            ksSpline:
              begin
                KR.Kernel := TSplineKernel.Create;
              end;

            ksCubic:
              begin
                KR.Kernel := TCubicKernel.Create;
              end;

            ksMitchell:
              begin
                KR.Kernel := TMitchellKernel.Create;
              end;

            ksAlbrecht:
              begin
                KR.Kernel := TAlbrechtKernel.Create;
              end;

            ksLanczos:
              begin
                KR.Kernel := TLanczosKernel.Create;
              end;

            ksGaussian:
              begin
                KR.Kernel := TGaussianKernel.Create;
              end;

            ksBlackman:
              begin
                KR.Kernel := TBlackmanKernel.Create;
              end;

            ksHann:
              begin
                KR.Kernel := THannKernel.Create;
              end;

            ksHamming:
              begin
                KR.Kernel := THammingKernel.Create;
              end;

            ksSinsh:
              begin
                KR.Kernel := TSinshKernel.Create;
              end;

            ksHermite:
              begin
                KR.Kernel := THermiteKernel.Create;
              end;
          end;

          KR.KernelMode := AOptions.KernelMode;
          KR.TableSize  := AOptions.TableSize;
        end;
    end;

    TCustomResampler(SrcBmp.Resampler).PixelAccessMode := AOptions.PixelAccessMode;

    ADestBmp.Draw(ADestBmp.Canvas.ClipRect, SrcBmp.Canvas.ClipRect, SrcBmp);
  finally
    SrcBmp.Free;
  end;
end;

function ResamplerNameList: TStringList;
begin
  Result := TStringList.Create;
  
  with Result do
  begin
    Add('Nearest');
    Add('Liner');
    Add('Draft');
    Add('Kernel');
  end;
end;

function PixelAccessModeList: TStringList;
begin
  Result := TStringList.Create;
  
  with Result do
  begin
    Add('Unsafe');
    Add('Safe');
    Add('Wrap');
  end;
end;

function WrapModeList: TStringList;
begin
  Result := TStringList.Create;
  
  with Result do
  begin
    Add('Clamp');
    Add('Repeat');
    Add('Mirror');
  end;
end;

function KernelNameList: TStringList;
begin
  Result := TStringList.Create;
  
  with Result do
  begin
    Add('Box');
    Add('Linear');
    Add('Cosine');
    Add('Spline');
    Add('Cubic');
    Add('Mitchell');
    Add('Albrecht');
    Add('Lanczos');
    Add('Gaussian');
    Add('Blackman');
    Add('Hann');
    Add('Hamming');
    Add('Sinsh');
    Add('Hermite');
  end;
end;

function KernelModeList: TStringList;
begin
  Result := TStringList.Create;
  
  with Result do
  begin
    Add('Dynamic (precise, slow)');
    Add('Table Nearest (truncate)');
    Add('Table Linear (interpolation)');
  end;
end;

end.
