unit Ext_Move2mw;

interface

{$IFDEF WINCE}
   {$r hiresaware.res}
{$ENDIF}

uses
  LCLIntf, LResources,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, FileUtil, ExtCtrls, StdCtrls,Math,
  lazfileutils, LazUtf8,
  GR32,
  GR32_Resamplers,
  GR32_Layers,
  GR32_Image,
  GR32_MicroTiles,  GR32_LowLevel,
  XGR32_Color,XGR32_Bmp32Draw,XGR32_Bmp32Func,XGR32_Effects;

type

  { TFormRotateExample }

  TFormRotateExample = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    Src: TImage32;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure DstMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

  public
    fx,fy:integer;
    flay:TBitmapLayer;
    frub:TRubberbandLayer;
    fgamma:integer;
    fd:integer;
  end;

var
  FormRotateExample: TFormRotateExample;

implementation


{$IFDEF Darwin}
uses
  MacOSAll;
{$ENDIF}

Const
    // Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ENDIF}

{$IFDEF UNIX}
  pathMedia = '../xmedia/';
{$ENDIF}


Function ssfixpath(afile:string):string;
    var  ss:string;
 begin
    ss:=ExtractFilePath(ParamStrUTF8(0));
    Result:=UTF8ToSys(ss+afile);
 end;


procedure TFormRotateExample.FormCreate(Sender: TObject);
begin
  fgamma:=150;
  fd:= 3;
  Src.Bitmap.SetSize(Src.Width,Src.Height);
  Src.Bitmap.DrawMode:=dmBlend;
  Src.Bitmap.Clear(clwhite32);
  Bmp32_LoadFromFile(Src.Bitmap,pathMedia+'icon1.png');

 // Effect_GreyScaleErosion(Src.Bitmap) ;

  flay:=TBitmapLayer.Create(Src.Layers);
  //flay.Bitmap.LoadFromFile(ssfixpath('delphi.jpg'));
  flay.Bitmap.DrawMode:=dmBlend;

  Bmp32_LoadFromFile(flay.Bitmap,pathMedia+'icon1.png');
 // flay.Bitmap.Clear(XClearColor);

  //=====================================================
  frub:=TRubberbandLayer.Create(Src.Layers);
 // frub.HandleSize:=9;
  frub.ChildLayer:=flay;
end;


procedure TFormRotateExample.Timer1Timer(Sender: TObject);
begin

 // if (fgamma>=0.9) or (fgamma<=1.1) then d:= ;
  if fgamma>252 then fd:=-3 ;
  if fgamma<1   then fd:= 3 ;

  fgamma:=fgamma+fd ;
  Bmp32_IncDecRGB(flay.Bitmap,0,fd,0);
 // Bmp32_Rotate(flay.Bitmap,fgamma);
  //Bmp32_FlipHoriz(flay.Bitmap);
end;


procedure TFormRotateExample.DstMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  fx:=x;
  fy:=y;

end;

procedure TFormRotateExample.Button1Click(Sender: TObject);
 var p:TPoint;
     r:Trect;
begin

  p.x:=Src.Width div 2;
  p.y:=Src.Height div 2;
  PaintSpiral(Src.Bitmap,p,100,5,[clSmDimGray32,clGray32]);
  PaintCircle(Src.Bitmap,p,100,clRed32);

  r:=rect(100,100,200,200);

  //PaintGradient(Src.Bitmap,r,clRed32,500) ;

  //PaintGradientRound(Src.Bitmap, r ,p,200,clgreen32,500) ;

  r:=rect(10,10,200,200);
  FillGridRectS(Src.Bitmap, r ,3,clred32) ;

  Src.Invalidate;
end;




initialization
  {$I Ext_Move2mw.lrs}


end.
