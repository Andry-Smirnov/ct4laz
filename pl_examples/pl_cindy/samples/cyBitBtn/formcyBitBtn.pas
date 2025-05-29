unit formcyBitBtn;

{$MODE Delphi}

interface

uses
  //Windows, ShellAPI,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel, LCLIntf,
  cyLabel,  cyBitBtn, cyBaseButton, cyBaseSpeedButton, ImgList;

type
  TFrmCyBitBtn = class(TForm)
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
    cyBitBtn1: TcyBitBtn;
    cyBitBtn2: TcyBitBtn;
    cyBitBtn3: TcyBitBtn;
    cyBitBtn4: TcyBitBtn;
    cyBitBtn5: TcyBitBtn;
    cyBitBtn8: TcyBitBtn;
    ImageListCmd: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    cyBitBtn9: TcyBitBtn;
    cyBitBtn10: TcyBitBtn;
    cyBitBtn11: TcyBitBtn;
    cyBitBtn12: TcyBitBtn;
    cyBitBtn13: TcyBitBtn;
    cyBitBtn14: TcyBitBtn;
    cyBitBtn6: TcyBitBtn;
    cyBitBtn7: TcyBitBtn;
    cyBitBtn15: TcyBitBtn;
    cyBitBtn16: TcyBitBtn;
    cyBitBtn17: TcyBitBtn;
    cyBitBtn18: TcyBitBtn;
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
  FrmCyBitBtn: TFrmCyBitBtn;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyBitBtn.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyBitBtn.FormCreate(Sender: TObject);
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

procedure TFrmCyBitBtn.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyBitBtn.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
