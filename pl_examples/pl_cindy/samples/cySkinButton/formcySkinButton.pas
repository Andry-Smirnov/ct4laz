unit formcySkinButton;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons, cySpeedButton, cyLabel, cySplitter, cyBaseLabel,
  cyHotLabel, cySkinButton, {GIFImg,} cySkinArea, cyBaseSpeedButton;

type
  TFrmCySkinButton = class(TForm)
    CyPanel1: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    StatusBar1: TStatusBar;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    cyLabel1: TcyLabel;
    LblWebSite: TcyHotLabel;
    Panel1: TPanel;
    Label14: TLabel;
    cySkinButton1: TcySkinButton;
    cySkinButton2: TcySkinButton;
    cySkinButton3: TcySkinButton;
    BtnPlayer: TcySkinButton;
    Label1: TLabel;
    LblPlayerStatus: TLabel;
    Image1: TImage;
    cySkinButton4: TcySkinButton;
    cySkinButton5: TcySkinButton;
    cySkinButton6: TcySkinButton;
    cySkinButton7: TcySkinButton;
    Label2: TLabel;
    cySkinArea2: TcySkinArea;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure cySkinButton1Click(Sender: TObject);
    procedure BtnPlayerClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCySkinButton: TFrmCySkinButton;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCySkinButton.BtnPlayerClick(Sender: TObject);
begin
  if BtnPlayer.Down
  then LblPlayerStatus.Caption := 'Playing ...'
  else LblPlayerStatus.Caption := 'Paused';
end;

procedure TFrmCySkinButton.cySkinButton1Click(Sender: TObject);
begin
  caption := caption + ' *';
end;

procedure TFrmCySkinButton.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCySkinButton.FormCreate(Sender: TObject);
var
  L: Integer;
  cyCompName: String;
  RtfFile: TFilename;
begin
  L := Length(Name);
  cyCompName := Copy(Name, 4, L-3);
  Caption := cyCompName + ' demo';
  RtfFile := pathMedia+ cyCompName+'.txt';

  try
    RichEditInfo.Lines.LoadFromFile(RtfFile);
  finally

  end;
end;

procedure TFrmCySkinButton.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCySkinButton.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
