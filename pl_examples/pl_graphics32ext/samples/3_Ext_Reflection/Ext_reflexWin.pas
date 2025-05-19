unit Ext_reflexWin;

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
  XGR32_Bmp32Func, XGR32_Effects;

type

  { TForm1 }

  TForm1 = class(TForm)
    FImgU: TImage32;
    FImgD: TImage32;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FImgRef: TImage32;
  public
    { Public declarations }
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
  vEff: TReflectionEffect;
begin

  FImgU.Bitmap.DrawMode := dmBlend;
  FImgD.Bitmap.DrawMode := dmBlend;

  vBmp := TBitmap32.Create;
  vEff := TReflectionEffect.Create(nil);
try
    vBmp.DrawMode := dmBlend;

    Bmp32_LoadFromFile(vBmp,pathMedia+'vcl.png');

    FImgU.Bitmap.Assign(vBmp);
    FImgD.Height := vBmp.Height;

    vEff.Generate(vBmp, FImgD.Bitmap, vBmp.ClipRect);

finally
  vBmp.Free;
  vEff.Free;
end;
end;

end.
