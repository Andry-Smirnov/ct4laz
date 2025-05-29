unit formcyCheckBox;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBevel, cyBaseSpeedButton, cyCheckbox;

type
  TFrmCyCheckBox = class(TForm)
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
    cyCheckBox1: TcyCheckBox;
    BtnSwitch: TButton;
    LBLog: TListBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure BtnSwitchClick(Sender: TObject);
    procedure cyCheckBox1Click(Sender: TObject);
    procedure cyCheckBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyCheckBox: TFrmCyCheckBox;

implementation

{$R *.lfm}


const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xMedia\';
{$ELSE}
  pathMedia = '../xMedia/';
{$ENDIF}

procedure TFrmCyCheckBox.BtnSwitchClick(Sender: TObject);
begin
  cyCheckBox1.Checked := not cyCheckBox1.Checked;
end;

procedure TFrmCyCheckBox.cyCheckBox1Change(Sender: TObject);
begin
  LBLog.Items.Insert(0, 'OnChange');
end;

procedure TFrmCyCheckBox.cyCheckBox1Click(Sender: TObject);
begin
  LBLog.Items.Insert(0, 'OnClick');
end;

procedure TFrmCyCheckBox.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyCheckBox.FormCreate(Sender: TObject);
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

procedure TFrmCyCheckBox.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyCheckBox.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
