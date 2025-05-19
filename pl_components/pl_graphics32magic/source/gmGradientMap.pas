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


unit gmGradientMap;

interface

uses

  Graphics,

  GR32,

  gmGradient;


type

  TgmGradientMap = class(TObject)
  private
    FSourceBitmap: TBitmap32;
  public
    constructor Create(const ASourceBmp: TBitmap32);
    destructor Destroy; override;

    procedure Draw(const ADestBmp: TBitmap32; const AGradient: TgmGradientItem;
      const AReversed: Boolean);
  end;

implementation

constructor TgmGradientMap.Create(const ASourceBmp: TBitmap32);
begin
  inherited Create;

  FSourceBitmap := TBitmap32.Create;
  FSourceBitmap.Assign(ASourceBmp);
  FSourceBitmap.DrawMode := dmBlend;
end; 

destructor TgmGradientMap.Destroy;
begin
  FSourceBitmap.Free;
  
  inherited Destroy;
end;

procedure TgmGradientMap.Draw(const ADestBmp: TBitmap32;
  const AGradient: TgmGradientItem; const AReversed: Boolean);
var
  i, r, g, b        : Cardinal;
  LSrcBits, LDstBits: PColor32;
  LGrayscale, LIndex: Cardinal;
  LColor            : TColor32;
begin
  if not ( Assigned(ADestBmp) and Assigned(AGradient) ) then
  begin
    Exit;
  end;

  if (ADestBmp.Width <= 0) or (ADestBmp.Height <= 0) then
  begin
    Exit;
  end;

  if (ADestBmp.Width  <> FSourceBitmap.Width) or
     (ADestBmp.Height <> FSourceBitmap.Height) then
  begin
    Exit;
  end;

  AGradient.GradientLength := 256;
  AGradient.RefreshColorArray;

  LSrcBits := @FSourceBitmap.Bits[0];
  LDstBits := @ADestBmp.Bits[0];
  
  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    r := LSrcBits^ shr 16 and $FF;
    g := LSrcBits^ shr  8 and $FF;
    b := LSrcBits^        and $FF;

    LGrayscale := (r + g + b) div 3;

    if AReversed then
    begin
      LIndex := 255 - LGrayscale;
    end
    else
    begin
      LIndex := LGrayscale;
    end;
    
    LColor := AGradient.OutputColors[LIndex];

    r := LColor and $FF0000;
    g := LColor and $FF00;
    b := LColor and $FF;

    LDstBits^ := (LDstBits^ and $FF000000) or r or g or b;

    Inc(LSrcBits);
    Inc(LDstBits);
  end;
end;


end.
