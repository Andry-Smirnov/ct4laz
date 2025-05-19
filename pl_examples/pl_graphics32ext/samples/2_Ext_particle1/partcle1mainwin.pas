unit partcle1mainwin;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  GR32,
  GR32_Resamplers,
  GR32_Layers,
  GR32_Image,
  GR32_MicroTiles,  GR32_LowLevel

  , XGR32_AniEffects
  , XGR32_Sprites
  , XGR32_AniGEffetcts
  , XGR32_ParticleAniEffects
  , XGR32_ParticleStar
  , XGR32_ParticleSnow
  , XGR32_Bmp32Func
  ;

type
  TForm1 = class(TForm)
    Image: TImage32;
    OpenPlay: TButton;
    Snapshot: TButton;
    OpenDialog: TOpenDialog;
    CallBack: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FEffEngine: TGRAnimationEffects;
    FStarSprites: TGRSprites;  
    FSnowSprites: TGRSprites;

  public
    constructor Create(aOwner: TComponent);override;
    destructor Destroy;override;
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



constructor TForm1.Create(aOwner: TComponent);
var
  LPic: TPicture;
begin
  inherited;
  OpenPlay.Visible := False;
  Snapshot.Visible := False;
  CallBack.Visible := False;

  Image.Bitmap.LoadFromFile(pathMedia+'sky.jpg');

  FEffEngine := TGRAnimationEffects.Create;
  FStarSprites := TGRSprites.Create;  
  FSnowSprites := TGRSprites.Create;

  //the star particle animation effect
  with TGRParticlesEffect.Create(FEffEngine) do
  begin
    Sprites := FStarSprites;

    UseCustomParticle(TGRStarParticle);
    //Init the Particle property:
    with Particle do
    begin
      //Looped := False;
      Bmp32_LoadFromFile(Picture,pathMedia+'pao.png');
    end;
    Enabled := True;
    MaxParticles := 50;
    NumOfParticles := 2;
  end;

  //the snow particle animation effect
  with TGRParticlesEffect.Create(FEffEngine) do
  begin
    Sprites := FSnowSprites;
    UseCustomParticle(TGRSnowParticle);

    with Particle do
    begin
      //Looped := False;
      Bmp32_LoadFromFile(Picture,pathMedia+'snow.png');
    end;
  
    Enabled := True;
    MaxParticles := 500;
    NumOfParticles := 200;
  end;

  FEffEngine.Control := Image;
  FEffEngine.Enabled := True;

end;

destructor TForm1.Destroy;
begin
	FEffEngine.Enabled := False;
	FreeAndNil(FStarSprites);
	FreeAndNil(FSnowSprites);
	FEffEngine.Free;
  inherited;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CallBack.Checked := false;
end;

end.
