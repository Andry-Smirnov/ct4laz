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

unit gmGradient_rwPegtopNew;
 
interface

uses
  Types, Classes, SysUtils, Graphics,
  PegtopChunkFiles, PegtopColorGradients,
  PegtopColorGradientLists,
  gmFileFormatList, gmGradient;

type
  TxgrPegtopNewReader = class(TgmConverter)
  private
  public
    //constructor Create; override;
    class function WantThis(const AStream: TStream): Boolean; override;
    procedure LoadFromStream(const AStream: TStream; const ACollection: TCollection); override;
    procedure ParseItem(gmItem: TgmGradientItem; ptItem: TPegtopCustomColorGradient);
  end;

const
  //GradientCollectionChunkId: TPegtopChunkId = 'XGRC';
  GradientCollectionDWord  = $43524758; //XGRC
  {GradientChunkId:           TPegtopChunkId = 'XGRD';
  GradientDataChunkId:       TPegtopChunkId = 'GDAT';
  ColorChunkId:              TPegtopChunkId = 'CDEF';
  OpacityChunkId:            TPegtopChunkId = 'ODEF';
  ColorDataChunkId:          TPegtopChunkId = 'CDAT';
  OpacityDataChunkId:        TPegtopChunkId = 'ODAT';
  ColorNoiseChunkId:         TPegtopChunkId = 'CNOI';
  OpacityNoiseChunkId:       TPegtopChunkId = 'ONOI';
  ColorNoiseDataChunkId:     TPegtopChunkId = 'CNDT';
  OpacityNoiseDataChunkId:   TPegtopChunkId = 'ONDT';
  KeysChunkId:               TPegtopChunkId = 'KEYS';
  ColorKeyChunkId:           TPegtopChunkId = 'CKEY';
  OpacityKeyChunkId:         TPegtopChunkId = 'OKEY';

  GradientCollectionDataChunkId: TPegtopChunkId = 'COLD';
  GradientsChunkId:              TPegtopChunkId = 'GRDS';
  }
  
implementation

{ TxgrPegtopNewReader }

procedure TxgrPegtopNewReader.LoadFromStream(const AStream: TStream;
  const ACollection: TCollection);
var
  LPGC : TPegtopColorGradientCollection;
  LPGI : TPegtopColorGradientItem;
  LPGO : TPegtopCustomColorGradient;
  i    : Integer;
begin
  LPGC := TPegtopColorGradientCollection.Create(nil,nil);
  LPGC.LoadFromStream(AStream);
  
  for i := 0 to (LPGC.Count - 1) do
  begin
    LPGI := LPGC[i];
    LPGO := LPGI.Gradient;
    
    ParseItem(TgmGradientCollection(ACollection).Add,LPGO);
  end;

  LPGC.Free;
end;

procedure TxgrPegtopNewReader.ParseItem(gmItem: TgmGradientItem;
  ptItem: TPegtopCustomColorGradient);
var
  i,j     : Integer;
  LgmStop : TgmGradientStopItem;
begin
  gmItem.Clear;
  gmItem.DisplayName := ptItem.Name;
  for i := 0 to ptItem.Color.Keys.Count -1 do
  begin
    LgmStop := gmItem.RGBGradient.Add;
    LgmStop.LocationScale := ptItem.Color.Keys[i].Position * 0.001;
    LgmStop.Value := ptItem.Color.Keys[i].Color;
  end;

  for i := 0 to ptItem.Opacity.Keys.Count -1 do
  begin
    LgmStop := gmItem.AlphaGradient.Add;
    j := ptItem.Opacity.Keys[i].Opacity;
    //j := round(ptItem.Opacity.Keys[i].Opacity * 0.01 * 255);
    LgmStop.Value := j-1;
    LgmStop.LocationScale := ptItem.Opacity.Keys[i].Position * 0.001;
  end;
end;

class function TxgrPegtopNewReader.WantThis(const AStream: TStream): Boolean;
var
  LFileHeader : TgmGradientFileHeader;
begin
  AStream.Read( LFileHeader, SizeOf(LFileHeader) ); 

  Result := (LFileHeader.FileID = GradientCollectionDWord) ;  //XGRC
end;

initialization
  TgmGradientCollection.RegisterConverterReader('XGR', 'Pegtop gradient files', 0, TxgrPegtopNewReader);


end.
