unit formcyProgressionPanel;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyCustomProgressionPanel, cyProgressionPanel, cyBaseSpeedButton;

type
  TFrmCyProgressionPanel = class(TForm)
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
    PanInfo: TPanel;
    Label103: TLabel;
    Shape2: TShape;
    SBTestProgressionPanel: TSpeedButton;
    cyProgressionPanel1: TcyProgressionPanel;
    RBDuringProgression: TRadioGroup;
    SBShowMessage: TcySpeedButton;
    LblStatus: TLabel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure SBTestProgressionPanelClick(Sender: TObject);
    procedure SBShowMessageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyProgressionPanel: TFrmCyProgressionPanel;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyProgressionPanel.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyProgressionPanel.FormCreate(Sender: TObject);
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

procedure TFrmCyProgressionPanel.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyProgressionPanel.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCyProgressionPanel.SBShowMessageClick(Sender: TObject);
begin
  ShowMessage('Button clicked!');
end;

procedure TFrmCyProgressionPanel.SBTestProgressionPanelClick(
  Sender: TObject);
var i: Integer;
begin
  Screen.Cursor := crHourGlass;
  LblStatus.Caption := 'Processing...';
  LblStatus.Update;
  cyProgressionPanel1.Open(PanInfo);

  for i := 1 to 10 do
  begin
    cyProgressionPanel1.Caption := 'Wait please ... phase ' + intToStr(i) + '/10';
    cyProgressionPanel1.GetLabel.Update;
    cyProgressionPanel1.GetGauge.StepIt;

    // Do long process code here !!!
    Sleep(500);

    case RBDuringProgression.ItemIndex of
    0: Application.ProcessMessages;
    1: cyProgressionPanel1.ProcessMessagesFromPanel;
    2: cyProgressionPanel1.DropMessages(Nil);
    end;

    if cyProgressionPanel1.Canceled
    then Break;
  end;

  if cyProgressionPanel1.Canceled
  then LblStatus.Caption := 'Aborted'
  else LblStatus.Caption := 'Completed';

  cyProgressionPanel1.Close;
  Screen.Cursor := crDefault;
end;

end.
