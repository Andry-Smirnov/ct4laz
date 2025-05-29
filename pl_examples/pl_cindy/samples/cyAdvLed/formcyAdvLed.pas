unit formcyAdvLed;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBaseLed, cyAdvLed, cyBaseSpeedButton;

type

  { TFrmCyAdvLed }

  TFrmCyAdvLed = class(TForm)
    cyAdvLed1: TcyAdvLed;
    cyAdvLed2: TcyAdvLed;
    cyAdvLed3: TcyAdvLed;
    cyAdvLed4: TcyAdvLed;
    cyAdvLed5: TcyAdvLed;
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
    Label9: TLabel;
    cyAdvLed21: TcyAdvLed;
    cyAdvLed22: TcyAdvLed;
    cyAdvLed23: TcyAdvLed;
    Label6: TLabel;
    Label69: TLabel;
    Label70: TLabel;
    cyAdvLed24: TcyAdvLed;
    cyAdvLed25: TcyAdvLed;
    Label7: TLabel;
    cyAdvLed27: TcyAdvLed;
    cyAdvLed28: TcyAdvLed;
    cyAdvLed29: TcyAdvLed;
    Label8: TLabel;
    Label71: TLabel;
    cyAdvLed210: TcyAdvLed;
    cyAdvLed212: TcyAdvLed;
    cyAdvLed213: TcyAdvLed;
    cyAdvLed214: TcyAdvLed;
    Label72: TLabel;
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
  FrmCyAdvLed: TFrmCyAdvLed;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyAdvLed.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyAdvLed.FormCreate(Sender: TObject);
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

procedure TFrmCyAdvLed.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyAdvLed.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
