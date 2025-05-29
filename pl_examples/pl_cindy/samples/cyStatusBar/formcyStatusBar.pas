unit formcyStatusBar;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBaseSpeedButton, cyStatusBar, cyBaseLed, cyLed, cyBaseMeasure, cyCustomGauge, cySimpleGauge;

type
  TFrmCyStatusBar = class(TForm)
    CyPanel1: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    cyLabel1: TcyLabel;
    LblWebSite: TcyHotLabel;
    Panel1: TPanel;
    cyStatusBar1: TcyStatusBar;
    LedDeviceStatus: TcyLed;
    cySimpleGauge1: TcySimpleGauge;
    CyPanel3: TCyPanel;
    cySpeedButton1: TcySpeedButton;
    cySpeedButton2: TcySpeedButton;
    cySpeedButton3: TcySpeedButton;
    cySpeedButton5: TcySpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
  private
  public
  end;

var
  FrmCyStatusBar: TFrmCyStatusBar;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ENDIF}
{$IFDEF UNIX}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyStatusBar.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyStatusBar.FormCreate(Sender: TObject);
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

procedure TFrmCyStatusBar.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyStatusBar.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
