unit Ext_ShadowWin;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  GR32,
  GR32_Resamplers,
  GR32_Layers,
  GR32_Image,
  GR32_MicroTiles,  GR32_LowLevel,
  XGR32_Bmp32Func, XGR32_Effects ;

type

  { TForm1 }

  TForm1 = class(TForm)
    FImgU: TImage32;
    procedure FormCreate(Sender: TObject);
  private

    FImgRef: TImage32;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
Const
    // Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ENDIF}

{$IFDEF UNIX}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TForm1.FormCreate(Sender: TObject);
var
  vBmp: TBitmap32;
  vEff: TShadowEffect;
begin

  FImgU.Bitmap.DrawMode := dmBlend;

  vBmp := TBitmap32.Create;
  vEff := TShadowEffect.Create(nil);
  vEff.Color:=clBlack;
  vEff.OffsetX:=100;
  vEff.OffsetY:=100;
  vEff.Blur:=10;
  vEff.Opacity:=255;
try
    vBmp.DrawMode := dmBlend;
    Bmp32_LoadFromFile(vBmp,pathMedia+'star.png');

    FImgU.Bitmap.Assign(vBmp);

    vEff.GenerateShadow(vBmp, FImgU.Bitmap, vBmp.ClipRect);
    vBmp.DrawTo(FImgU.Bitmap);
finally
  vBmp.Free;
  vEff.Free;
end;
end;

end.
