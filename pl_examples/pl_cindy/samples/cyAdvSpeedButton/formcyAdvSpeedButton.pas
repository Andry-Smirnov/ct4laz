unit formcyAdvSpeedButton;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyAdvSpeedButton, cySpeedButton, cyBaseSpeedButton, ImgList;

type
  TFrmCyAdvSpeedButton = class(TForm)
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
    cyAdvSpeedButton2: TcyAdvSpeedButton;
    cyAdvSpeedButton3: TcyAdvSpeedButton;
    cyAdvSpeedButton4: TcyAdvSpeedButton;
    ImageListCmd: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    cyAdvSpeedButton1: TcyAdvSpeedButton;
    cyAdvSpeedButton6: TcyAdvSpeedButton;
    cyAdvSpeedButton7: TcyAdvSpeedButton;
    cyAdvSpeedButton8: TcyAdvSpeedButton;
    cyAdvSpeedButton9: TcyAdvSpeedButton;
    cyAdvSpeedButton10: TcyAdvSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyAdvSpeedButton: TFrmCyAdvSpeedButton;

implementation

{$R *.lfm}

const
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyAdvSpeedButton.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyAdvSpeedButton.FormCreate(Sender: TObject);
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

procedure TFrmCyAdvSpeedButton.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyAdvSpeedButton.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
