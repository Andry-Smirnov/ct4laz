unit formcySpeedButton;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBaseSpeedButton, ImgList;

type
  TFrmCySpeedButton = class(TForm)
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
    cySpeedButton3: TcySpeedButton;
    cySpeedButton5: TcySpeedButton;
    cySpeedButton4: TcySpeedButton;
    cySpeedButton6: TcySpeedButton;
    cySpeedButton7: TcySpeedButton;
    cySpeedButton8: TcySpeedButton;
    cySpeedButton10: TcySpeedButton;
    cySpeedButton11: TcySpeedButton;
    cySpeedButton13: TcySpeedButton;
    cySpeedButton14: TcySpeedButton;
    cySpeedButton21: TcySpeedButton;
    cySpeedButton22: TcySpeedButton;
    cySpeedButton23: TcySpeedButton;
    cySpeedButton24: TcySpeedButton;
    cySpeedButton25: TcySpeedButton;
    cySpeedButton26: TcySpeedButton;
    cySpeedButton27: TcySpeedButton;
    cySpeedButton28: TcySpeedButton;
    cySpeedButton29: TcySpeedButton;
    cySpeedButton30: TcySpeedButton;
    cySpeedButton31: TcySpeedButton;
    cySpeedButton32: TcySpeedButton;
    cySpeedButton33: TcySpeedButton;
    cySpeedButton34: TcySpeedButton;
    cySpeedButton35: TcySpeedButton;
    cySpeedButton36: TcySpeedButton;
    cySpeedButton37: TcySpeedButton;
    cySpeedButton38: TcySpeedButton;
    cySpeedButton39: TcySpeedButton;
    cySpeedButton40: TcySpeedButton;
    cySpeedButton41: TcySpeedButton;
    cySpeedButton42: TcySpeedButton;
    cySpeedButton43: TcySpeedButton;
    cySpeedButton44: TcySpeedButton;
    cySpeedButton15: TcySpeedButton;
    cySpeedButton16: TcySpeedButton;
    cySpeedButton17: TcySpeedButton;
    cySpeedButton18: TcySpeedButton;
    cySpeedButton19: TcySpeedButton;
    cySpeedButton20: TcySpeedButton;
    cySpeedButton45: TcySpeedButton;
    cySpeedButton46: TcySpeedButton;
    cySpeedButton49: TcySpeedButton;
    cySpeedButton50: TcySpeedButton;
    cySpeedButton51: TcySpeedButton;
    cySpeedButton52: TcySpeedButton;
    cySpeedButton53: TcySpeedButton;
    cySpeedButton54: TcySpeedButton;
    ImageListCmd: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    cySpeedButton1: TcySpeedButton;
    cySpeedButton2: TcySpeedButton;
    cySpeedButton9: TcySpeedButton;
    cySpeedButton12: TcySpeedButton;
    cySpeedButton47: TcySpeedButton;
    cySpeedButton48: TcySpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure cySpeedButton12Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCySpeedButton: TFrmCySpeedButton;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCySpeedButton.cySpeedButton12Click(Sender: TObject);
begin
  cySpeedButton47.FlatDownStyle := dsClassic;
end;

procedure TFrmCySpeedButton.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCySpeedButton.FormCreate(Sender: TObject);
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

procedure TFrmCySpeedButton.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCySpeedButton.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
