unit formcymaskEdit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBevel, cyBaseSpeedButton, cyEdit, cyMaskEdit, MaskEdit;

type
  TFrmCyMaskEdit = class(TForm)
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
    cyMaskEdit1: TcyMaskEdit;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cyMaskEdit2: TcyMaskEdit;
    Label4: TLabel;
    Label5: TLabel;
    cyMaskEdit3: TcyMaskEdit;
    Label6: TLabel;
    Label7: TLabel;
    cyMaskEdit4: TcyMaskEdit;
    Label8: TLabel;
    Label9: TLabel;
    cyMaskEdit5: TcyMaskEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    CBAllowEmpty: TCheckBox;
    Label13: TLabel;
    Label14: TLabel;
    cyMaskEdit6: TcyMaskEdit;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure CBAllowEmptyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyMaskEdit: TFrmCyMaskEdit;

implementation

{$R *.lfm} 
const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyMaskEdit.CBAllowEmptyClick(Sender: TObject);
begin
  cyMaskEdit1.AllowEmpty := CBAllowEmpty.Checked;
end;

procedure TFrmCyMaskEdit.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyMaskEdit.FormCreate(Sender: TObject);
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

procedure TFrmCyMaskEdit.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyMaskEdit.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
