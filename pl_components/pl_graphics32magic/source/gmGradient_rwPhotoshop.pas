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

unit gmGradient_rwPhotoshop;

interface

uses
  Types, Classes, SysUtils, Graphics,
  GR32, GR32_LowLevel, gmbestream,
  gmFileFormatList, gmGradient;

const
  PS_GRADIENT_FILE_ID = $52474238; // 8BGR

type
  TgrdPhotoshopReader = class(TgmConverter)
  private
    procedure LoadColorStopItem(Node: TgmGradientStopItem);
    procedure LoadAlphaStopItem(Node: TgmGradientStopItem);
    function GetStopItemRGB: TColor;
    function GetStopItemCMYK: TColor;
    //function GetStopItemHSB: TColor;
    procedure SetStopItemHSB(Node: TgmGradientStopItem);
  public
    //constructor Create; override;
    class function WantThis(const AStream: TStream): boolean; override;
    procedure LoadFromStream(const AStream: TStream; const ACollection: TCollection); override;
    procedure LoadItemFromStream(const AStream: TStream; const AGradient: TgmGradientItem); reintroduce;
  end;

implementation

type

  ps_hdr = record
    signature: DWORD;
    version: smallint;
    unknown5: word;
    unknown1, unknown2, unknown3{,unknown4}: DWORD;
    unknownNull: DWORD;
    unknown6, unknown7: DWORD;
    GrdL, VlLs: DWORD;
    GradientCount: integer;
    //obj : DWORD;
  end;

  ps_chunkO = record
    zero: DWORD;
    chunk: String4;
    tipe: string4;
  end;

  ps_chunkL = record
    zero: DWORD;
    chunk: String4;
    L: DWORD;
  end;

var
  swide: WideString;
  chunkO: ps_chunkO;
  chunkL: ps_chunkL;

{-------------------------------- TgrdPhotoshopReader --------------------------}

{constructor TgrdPhotoshopReader.Create;
begin
  inherited;
  self.FileSignature := PS_GRADIENT_FILE_ID;
end;}

function TgrdPhotoshopReader.GetStopItemRGB: TColor;
var
  dr, dg, db: integer;
begin
  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag; //Rd__
  chunkO.tipe := BE_ReadFlag;//doub
  dr := Round(BE_ReadDouble);

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag; //Grn_
  chunkO.tipe := BE_ReadFlag;//doub
  dg := Round(BE_ReadDouble);

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag; //Bl__
  chunkO.tipe := BE_ReadFlag;//doub
  db := Round(BE_ReadDouble);

  clamp(dr);
  clamp(dg);
  clamp(db);
  Result := db shl 16 + dg shl 8 + dr;
end;

procedure TgrdPhotoshopReader.LoadFromStream(const AStream: TStream; const ACollection: TCollection);
var
  i: integer;
  LPShdr: ps_hdr;
  LGradients: TgmGradientCollection;
begin
  gmbestream.GStream := AStream;
  LGradients := TgmGradientCollection(ACollection);

  AStream.Read(LPShdr, SizeOf(ps_hdr));

  LPShdr.version := SwapEndian(LPShdr.version);
  LPShdr.GradientCount := SwapEndian(LPShdr.GradientCount);

  //only support ver 5
  if (LPShdr.signature <> PS_GRADIENT_FILE_ID) and (LPShdr.version <> 5) then
  begin
    raise Exception.Create('Cannot open because the file is not supported by GraphicsMagic.');
  end;

  for i := 1 to LPShdr.GradientCount do
    LoadItemFromStream(AStream, LGradients.Add);
end;

class function TgrdPhotoshopReader.WantThis(const AStream: TStream): boolean;
var
  LFileHeader: TgmGradientFileHeader;
begin
  AStream.Read(LFileHeader, SizeOf(LFileHeader));

  Result := (LFileHeader.FileID = PS_GRADIENT_FILE_ID) and (SwapEndian(LFileHeader.FileVersion.BEMajor) = 5);
  //only support ver 5 of PS.GRD
end;

procedure TgrdPhotoshopReader.LoadAlphaStopItem(Node: TgmGradientStopItem);
begin
  chunkO.chunk := BE_ReadFlag;       //Objc
  swide := BE_ReadWideString; //""

  chunkL.zero := BE_ReadDWord;
  chunkL.chunk := BE_ReadFlag;  //TrnS
  chunkL.L := BE_ReadDWord; //3

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag; //Opct
  chunkO.tipe := BE_ReadFlag; //UntF
  chunkO.tipe := BE_ReadFlag; //#Prc
  Node.Value := round(BE_ReadDouble / 100 * 255);

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag;  //Lctn
  chunkO.tipe := BE_ReadFlag;  //long
  chunkL.L := BE_ReadDWord; //984579247592
  Node.LocationScale := chunkL.L / $1000;
  ;

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag;     //Mdpn
  chunkO.tipe := BE_ReadFlag;     //long
  chunkL.L := BE_ReadDWord;    //274234609655
  Node.MidPoint := chunkL.L * 0.01; //= XX%
end;

procedure TgrdPhotoshopReader.LoadColorStopItem(Node: TgmGradientStopItem);
begin
  chunkO.chunk := BE_ReadFlag;       //Objc
  swide := BE_ReadWideString; //""

  chunkL.zero := BE_ReadDWord;
  chunkL.chunk := BE_ReadFlag;  //Clrt
  chunkL.L := BE_ReadDWord; //4

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag; //Clr_ | Type

  if chunkO.chunk = 'Clr ' then
  begin
    chunkO.tipe := BE_ReadFlag;       //Objc
    swide := BE_ReadWideString; //""

    chunkO.zero := BE_ReadDWord;
    chunkO.chunk := BE_ReadFlag;  //RGBC | CMYC  | HSBC
    chunkL.L := BE_ReadDWord; //3    |  4    |  3

    TgmGradientItem(Node.Collection.Owner).ColorSpace := chunkO.chunk;

    if chunkO.chunk = 'RGBC' then
      Node.Value := GetStopItemRGB
    else if chunkO.chunk = 'CMYC' then
      Node.Value := GetStopItemCMYK
    else if chunkO.chunk = 'HSBC' then
      //Node.Value := GetStopItemHSB
      self.SetStopItemHSB(Node)
    else
      raise Exception.Create('Unknown color format: ' + chunkO.chunk);

    chunkO.zero := BE_ReadDWord;
    chunkO.chunk := BE_ReadFlag; //Type
  end;

  chunkO.tipe := BE_ReadFlag;  //enum
  chunkL.L := BE_ReadDWord; //0
  chunkL.chunk := BE_ReadFlag;  //Clry
  chunkL.L := BE_ReadDWord; //0
  chunkO.chunk := BE_ReadFlag;  //UsrS
  if chunkO.chunk <> 'UsrS' then
  begin
    if chunkO.chunk = 'BckC' then
      Node.Value := clBackground
    else
    if chunkO.chunk = 'FrgC' then
      Node.Value := clDefault;
  end;

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag;  //Lctn
  chunkO.tipe := BE_ReadFlag;  //long
  chunkL.L := BE_ReadDWord; //984579247592
  Node.LocationScale := chunkL.L / $1000;

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag;     //Mdpn
  chunkO.tipe := BE_ReadFlag;     //long
  chunkL.L := BE_ReadDWord;    //274234609655
  Node.MidPoint := chunkL.L * 0.01; //= XX%
end;

procedure TgrdPhotoshopReader.LoadItemFromStream(const AStream: TStream; const AGradient: TgmGradientItem);
var
  i: integer;
begin
  chunkO.chunk := BE_ReadFlag;       //Objc
  swide := BE_ReadWideString; //Gradient

  chunkL.zero := BE_ReadDWord;
  chunkL.chunk := BE_ReadFlag;  //Grdn
  chunkL.L := BE_ReadDWord; //1

  chunkL.zero := BE_ReadDWord;
  chunkL.chunk := BE_ReadFlag;       //Grad
  chunkO.chunk := BE_ReadFlag;       //Objc
  swide := BE_ReadWideString; //Gradient

  chunkL.zero := BE_ReadDWord;
  chunkL.chunk := BE_ReadFlag;   //Grdn
  chunkL.L := BE_ReadDWord;  //5

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag;       //Nm__
  chunkO.tipe := BE_ReadFlag;       //TEXT
  swide := BE_ReadWideString; //Gradient
  AGradient.DisplayName := swide;

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag;  //GrdF
  chunkO.tipe := BE_ReadFlag;  //enum
  chunkL.L := BE_ReadDWord; //0
  chunkL.chunk := BE_ReadFlag;  //GrdF

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag;  //CstS

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag; //Intr
  chunkO.tipe := BE_ReadFlag; //doub
  BE_ReadDouble;

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag;  //Clrs
  chunkO.tipe := BE_ReadFlag;  //VlLs
  chunkL.L := BE_ReadDWord; //6
  AGradient.RGBGradient.Clear;
  for i := 1 to chunkL.L do
  begin
    LoadColorStopItem(AGradient.RGBGradient.Add);
  end;
  //-----------------

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag;  //Trns
  chunkO.tipe := BE_ReadFlag;  //VlLs
  chunkL.L := BE_ReadDWord; //2
  AGradient.AlphaGradient.Clear;
  for i := 1 to chunkL.L do
  begin
    LoadAlphaStopItem(AGradient.AlphaGradient.Add);
  end;
  //-----------------
end;

function TgrdPhotoshopReader.GetStopItemCMYK: TColor;
var
  dc, dm, dy, dk: integer;
begin
  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag; //Cyn_
  chunkO.tipe := BE_ReadFlag; //doub
  dc := Round(BE_ReadDouble);

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag; //Mgnt
  chunkO.tipe := BE_ReadFlag; //doub
  dm := Round(BE_ReadDouble);

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag; //Ylw_
  chunkO.tipe := BE_ReadFlag; //doub
  dy := Round(BE_ReadDouble);

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag; //Blck
  chunkO.tipe := BE_ReadFlag; //doub
  dk := Round(BE_ReadDouble);

  clamp(dc);
  clamp(dm);
  clamp(dy);
  Result := dc shl 16 + dm shl 8 + dy;
end;

procedure TgrdPhotoshopReader.SetStopItemHSB(Node: TgmGradientStopItem);

  function Hue_2_RGB(v1, v2, vH: double): double;
  begin
    if (vH < 0) then
      vH := vH + 1;
    if (vH > 1) then
      vH := vH - 1;

    if ((6 * vH) < 1) then
      Result := (v1 + (v2 - v1) * 6 * vH)
    else if ((2 * vH) < 1) then
      Result := v2
    else if ((3 * vH) < 2) then
      Result := (v1 + (v2 - v1) * ((2 / 3) - vH) * 6)
    else
      Result := v1;
  end;

var
  H, S, L, var_1, var_2: double;
  R, G, B: integer;
begin
  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag; //H___  = Hue
  chunkO.tipe := BE_ReadFlag; //UntF
  chunkO.tipe := BE_ReadFlag; //Ang    = Angle
  H := BE_ReadDouble / 360;

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag; //Strt  = Saturation
  chunkO.tipe := BE_ReadFlag; //doub
  S := (BE_ReadDouble / 100);

  chunkO.zero := BE_ReadDWord;
  chunkO.chunk := BE_ReadFlag; //Brgh  = Brightness
  chunkO.tipe := BE_ReadFlag; //doub
  L := (BE_ReadDouble / 200);

  Node.Collection.Owner.ColorSpace := Format('%s  (H:%f, S:%f, L:%f', [Node.Collection.Owner.ColorSpace, H, S, L]);

  if (S = 0) then                       //HSL from 0 to 1
  begin
    R := Round(L * 255);                 //RGB results from 0 to 255
    G := Round(L * 255);
    B := Round(L * 255);
  end
  else
  begin
    if (L < 0.5) then
      var_2 := L * (1 + S)
    else
      var_2 := (L + S) - (S * L);

    var_1 := 2 * L - var_2;

    R := Round(255 * Hue_2_RGB(var_1, var_2, H + (1 / 3)));
    G := Round(255 * Hue_2_RGB(var_1, var_2, H));
    B := Round(255 * Hue_2_RGB(var_1, var_2, H - (1 / 3)));
  end;

  clamp(R);
  clamp(G);
  clamp(B);

  //Result :=  B shl 16 + G shl 8 + R;
  Node.Value := B shl 16 + G shl 8 + R;
end;

initialization
  TgmGradientCollection.RegisterConverterReader('GRD', 'Photoshop gradient', 0, TgrdPhotoshopReader);


end.
