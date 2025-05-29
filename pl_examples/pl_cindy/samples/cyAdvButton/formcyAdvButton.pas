unit formcyAdvButton;

{$MODE Delphi}

interface

uses
 // Windows,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons, LCLIntf,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyAdvButton, cyBaseButton, cyBaseSpeedButton, ImgList;

type
  TFrmCyAdvButton = class(TForm)
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
    LblClicks: TLabel;
    cyAdvButton1: TcyAdvButton;
    cyAdvButton2: TcyAdvButton;
    cyAdvButton3: TcyAdvButton;
    cyAdvButton4: TcyAdvButton;
    cyAdvButton8: TcyAdvButton;
    Label1: TLabel;
    ImageListCmd: TImageList;
    cyAdvButton5: TcyAdvButton;
    cyAdvButton6: TcyAdvButton;
    cyAdvButton7: TcyAdvButton;
    cyAdvButton9: TcyAdvButton;
    cyAdvButton10: TcyAdvButton;
    cyAdvButton11: TcyAdvButton;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure cyAdvButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyAdvButton: TFrmCyAdvButton;

  nbClicks: Integer = 0;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}


procedure TFrmCyAdvButton.cyAdvButton1Click(Sender: TObject);
begin
  inc(nbClicks, 1);

  if nbClicks = 1
  then LblClicks.Caption := '1 click'
  else LblClicks.Caption := intToStr(nbClicks) + ' clicks';
end;

procedure TFrmCyAdvButton.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyAdvButton.FormCreate(Sender: TObject);
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

procedure TFrmCyAdvButton.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyAdvButton.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
