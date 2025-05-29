unit formcyLed;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBaseLed, cyLed, cyBaseSpeedButton;

type
  TFrmCyLed = class(TForm)
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
    cyLed27: TcyLed;
    cyLed217: TcyLed;
    cyLed218: TcyLed;
    cyLed25: TcyLed;
    cyLed28: TcyLed;
    cyLed29: TcyLed;
    cyLed26: TcyLed;
    cyLed210: TcyLed;
    cyLed211: TcyLed;
    cyLed213: TcyLed;
    cyLed214: TcyLed;
    cyLed215: TcyLed;
    cyLed216: TcyLed;
    cyLed219: TcyLed;
    cyLed220: TcyLed;
    cyLed221: TcyLed;
    cyLed222: TcyLed;
    cyLed223: TcyLed;
    cyLed224: TcyLed;
    cyLed225: TcyLed;
    cyLed226: TcyLed;
    cyLed227: TcyLed;
    cyLed228: TcyLed;
    cyLed229: TcyLed;
    cyLed230: TcyLed;
    cyLed231: TcyLed;
    cyLed232: TcyLed;
    Label67: TLabel;
    cyLed234: TcyLed;
    cyLed235: TcyLed;
    cyLed236: TcyLed;
    cyLed237: TcyLed;
    cyLed238: TcyLed;
    cyLed239: TcyLed;
    cyLed240: TcyLed;
    cyLed241: TcyLed;
    cyLed242: TcyLed;
    cyLed243: TcyLed;
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
  FrmCyLed: TFrmCyLed;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyLed.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyLed.FormCreate(Sender: TObject);
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

procedure TFrmCyLed.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyLed.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
