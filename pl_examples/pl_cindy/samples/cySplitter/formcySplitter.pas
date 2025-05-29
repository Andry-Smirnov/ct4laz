unit formcySplitter;

{$MODE Delphi}

interface

uses

  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,  LCLIntf,
  cyLabel,  cyBaseSpeedButton;

type
  TFrmCySplitter = class(TForm)
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
    Label104: TLabel;
    Label105: TLabel;
    Panel18: TPanel;
    cySplitter6: TcySplitter;
    Panel15: TPanel;
    cySplitter2: TcySplitter;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel19: TPanel;
    cySplitter7: TcySplitter;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel23: TPanel;
    cySplitter3: TcySplitter;
    cySplitter4: TcySplitter;
    ListBox1: TListBox;
    Panel24: TPanel;
    cySplitter5: TcySplitter;
    cySplitter9: TcySplitter;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Panel3: TPanel;
    cySplitter8: TcySplitter;
    cySplitter10: TcySplitter;
    cySplitter11: TcySplitter;
    cySplitter12: TcySplitter;
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
  FrmCySplitter: TFrmCySplitter;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCySplitter.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCySplitter.FormCreate(Sender: TObject);
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

procedure TFrmCySplitter.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCySplitter.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
