unit formcyHotLabel;

{$MODE Delphi}

interface

uses
 // Windows, ShellAPI,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,LCLIntf,
  cyLabel,  cyBaseSpeedButton;

type

  { TFrmCyHotLabel }

  TFrmCyHotLabel = class(TForm)
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
    cyActiveLabel1: TcyHotLabel;
    cyActiveLabel2: TcyHotLabel;
    cyActiveLabel4: TcyHotLabel;
    cyActiveLabel7: TcyHotLabel;
    cyActiveLabel9: TcyHotLabel;
    cyActiveLabel11: TcyHotLabel;
    cyActiveLabel13: TcyHotLabel;
    cyActiveLabel14: TcyHotLabel;
    cyActiveLabel15: TcyHotLabel;
    cyActiveLabel16: TcyHotLabel;
    cyActiveLabel17: TcyHotLabel;
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
  FrmCyHotLabel: TFrmCyHotLabel;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyHotLabel.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyHotLabel.FormCreate(Sender: TObject);
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

procedure TFrmCyHotLabel.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyHotLabel.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
