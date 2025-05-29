unit formcyAdvProgressionPanel;

{$MODE Delphi}

interface

uses
 // Windows, ShellAPI,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel, LCLIntf,
  cyLabel,  cyCustomProgressionPanel, cyAdvProgressionPanel, cyBaseSpeedButton;

type
  TFrmCyAdvProgressionPanel = class(TForm)
    CyPanel1: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    StatusBar1: TStatusBar;
    SBShowMessage: TcySpeedButton;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    cyLabel1: TcyLabel;
    LblWebSite: TcyHotLabel;
    Panel1: TPanel;
    cyAdvProgressionPanel1: TcyAdvProgressionPanel;
    CBClearMessages: TCheckBox;
    SBTestAdvProgressionPanel: TSpeedButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure SBTestAdvProgressionPanelClick(Sender: TObject);
    procedure SBShowMessageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyAdvProgressionPanel: TFrmCyAdvProgressionPanel;

implementation

{$R *.lfm}


const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyAdvProgressionPanel.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyAdvProgressionPanel.FormCreate(Sender: TObject);
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

procedure TFrmCyAdvProgressionPanel.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyAdvProgressionPanel.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCyAdvProgressionPanel.SBShowMessageClick(Sender: TObject);
begin
  ShowMessage('Hello');
end;

procedure TFrmCyAdvProgressionPanel.SBTestAdvProgressionPanelClick(
  Sender: TObject);
var TC1, TC2: Cardinal;
begin
  Screen.Cursor := crHourGlass;
  cyAdvProgressionPanel1.Open(Panel1);

  // Do long blocking code here !!!
  TC1 := GetTickCount64;
  TC2 := TC1;

  while TC2 - TC1 < 5000 do
    TC2 := GetTickCount64;

  if CBClearMessages.Checked
  then cyAdvProgressionPanel1.DropMessages(nil);

  cyAdvProgressionPanel1.Close;
  Screen.Cursor := crDefault;
end;

end.
