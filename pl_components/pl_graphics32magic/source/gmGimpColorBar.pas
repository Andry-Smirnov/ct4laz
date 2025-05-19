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


unit gmGimpColorBar;

{$MODE DELPHI}

interface

uses
  GR32, gmGtkEnums, gmGimpColorTypes,
  gmGimpBaseEnums, gmGimpRGB;

type
  TgmGimpColorBar = class(TObject)
  private
    FHalfProcess : Boolean; // Added by ourselves -- whether process the half part of the color bar.
    FChannel     : Integer; // Added by ourselves.

    FOrientation : TGtkOrientation;
    FBuf         : array [0..3 * 256 - 1] of Byte;

    procedure gimp_color_bar_set_channel(const AChannel: Integer);
    procedure gimp_color_bar_set_color(const AColor: TGimpRGB);
  public
    constructor Create;

    procedure gimp_color_bar_expose(const ADestBmp: TBitmap32);
    procedure gimp_color_bar_set_buffers(const ARedArray, AGreenArray, ABlueArray: array of Byte);

    property Channel      : Integer         read FChannel     write gimp_color_bar_set_channel;
    property Orientation  : TGtkOrientation read FOrientation write FOrientation;
    property IsHalfProcess: Boolean         read FHalfProcess write FHalfProcess;
  end;

implementation

{ Private Methods }

procedure TgmGimpColorBar.gimp_color_bar_set_channel(const AChannel: Integer);
var
  LColor: TGimpRGB;
begin
  with LColor do
  begin
    r := 1.0;
    g := 1.0;
    b := 1.0;
    a := 1.0;
  end;

  case AChannel of
    GIMP_HISTOGRAM_VALUE,
    GIMP_HISTOGRAM_ALPHA,
    GIMP_HISTOGRAM_RGB:
      begin
        gimp_rgb_set(LColor, 1.0, 1.0, 1.0);
      end;

    GIMP_HISTOGRAM_RED:
      begin
        gimp_rgb_set(LColor, 1.0, 0.0, 0.0);
      end;

    GIMP_HISTOGRAM_GREEN:
      begin
        gimp_rgb_set(LColor, 0.0, 1.0, 0.0);
      end;

    GIMP_HISTOGRAM_BLUE:
      begin
        gimp_rgb_set(LColor, 0.0, 0.0, 1.0);
      end;
  end;
  
  gimp_color_bar_set_color(LColor);
end; 

{ Makes the @bar display a gradient from black (on the left or the
  bottom), to the given @color (on the right or at the top). }
procedure TgmGimpColorBar.gimp_color_bar_set_color(const AColor: TGimpRGB);
var
  i, j: Integer;
begin
  j := 0;
  
  for i := 0 to 255 do
  begin
    FBuf[j + 0] := Round(AColor.b * i);
    FBuf[j + 1] := Round(AColor.g * i);
    FBuf[j + 2] := Round(AColor.r * i);
    
    Inc(j, 3);
  end;
end; 

{ Public Methods }

constructor TgmGimpColorBar.Create;
begin
  inherited Create;
  
  FHalfProcess := False;
  FChannel     := GIMP_HISTOGRAM_VALUE;
  FOrientation := GTK_ORIENTATION_HORIZONTAL;
end; 

procedure TgmGimpColorBar.gimp_color_bar_expose(const ADestBmp: TBitmap32);
var
  LBuf           : array of PColor32Array;
  LWidth, LHeight: Cardinal;
  i, j, LIndex   : Cardinal;
  a              : Cardinal;
  r, g, b        : Cardinal;
begin
{$RANGECHECKS OFF}

  LWidth  := ADestBmp.Width;
  LHeight := ADestBmp.Height;

  if (LWidth < 1) or (LHeight < 1) then
  begin
    Exit;
  end;

  SetLength(LBuf, LHeight);
  for j := 0 to (LHeight - 1) do
  begin
    LBuf[j] := ADestBmp.ScanLine[j];
  end;

  case FOrientation of
    GTK_ORIENTATION_HORIZONTAL:
      begin
        if FHalfProcess then
        begin
          LHeight := ADestBmp.Height div 2;
        end;

        for j := 0 to (LHeight - 1) do
        begin
          for i := 0 to (LWidth - 1) do
          begin
            LIndex := 3 * ( (i * 256) div LWidth );

            a := LBuf[j, i] shr 24 and $FF;
            r := FBuf[LIndex + 2];
            g := FBuf[LIndex + 1];
            b := FBuf[LIndex + 0];

            LBuf[j, i] := (a shl 24) or (r shl 16) or (g shl 8) or b;
          end;
        end;

        if FHalfProcess = False then
        begin
          FHalfProcess := True;
        end;
      end;

    GTK_ORIENTATION_VERTICAL:
      begin
        for i := 0 to (LWidth - 1) do
        begin
          for j := 0 to (LHeight - 1) do
          begin
            LIndex := 3 * (  255 - ( (j * 256) div LHeight )  );

            a := LBuf[j, i] shr 24 and $FF;
            r := FBuf[LIndex + 2];
            g := FBuf[LIndex + 1];
            b := FBuf[LIndex + 0];

            LBuf[j, i] := (a shl 24) or (r shl 16) or (g shl 8) or b;
          end;
        end;
      end;
  end;
{$RANGECHECKS ON}
end; 

procedure TgmGimpColorBar.gimp_color_bar_set_buffers(
  const ARedArray, AGreenArray, ABlueArray: array of Byte);
var
  i, j: Integer;
begin
  if ( High(ARedArray)   < 0 ) or
     ( High(AGreenArray) < 0 ) or
     ( High(ABlueArray)  < 0 ) then
  begin
    Exit;
  end;

  j := 0;
  
  for i := 0 to 255 do
  begin
    FBuf[j + 0] := ABlueArray[i];
    FBuf[j + 1] := AGreenArray[i];
    FBuf[j + 2] := ARedArray[i];
    
    Inc(j, 3);
  end;
end;

end.
