unit formcyRadioGroup;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBevel, cyBaseSpeedButton, cyRadioGroup,cyCheckbox, UxTheme;

type
  TFrmCyRadioGroup = class(TForm)
    CyPanel1: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    StatusBar1: TStatusBar;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    LblWebSite: TcyHotLabel;
    Panel1: TPanel;
    cyRadioGroup1: TcyRadioGroup;
    BtnChange: TButton;
    RadioGroup1: TRadioGroup;
    cyRadioButton1: TcyRadioButton;
    cyRadioButton2: TcyRadioButton;
    cyRadioGroup2: TcyRadioGroup;
    cyCheckBox2: TcyCheckBox;
    cyCheckBox1: TcyCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure BtnChangeClick(Sender: TObject);
    // procedure OnCtlColorStatic(var msg : TWMCtlColorStatic); message WM_CTLCOLORSTATIC;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyRadioGroup: TFrmCyRadioGroup;

implementation

{$R *.lfm}
const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyRadioGroup.BtnChangeClick(Sender: TObject);
begin
//  UxTheme.SetWindowTheme(Self.Handle, '', '');

//  UxTheme.SetWindowTheme(cyRadioButton1.Handle, '', '');
//  UxTheme.SetWindowTheme(cyCheckBox1.Handle, 'wstr', 'wstr');
//  UxTheme.SetWindowTheme(cyRadioGroup1.Handle, '', '');


  with TRadioButton(cyRadioGroup1.Controls[0]) do
  begin
    Font.Size := 12;
    Font.Color := clGreen;
  end;

  with TRadioButton(cyRadioGroup1.Controls[1]) do
  begin
    Font.Size := 6;
    Font.Color := clBlue;
  end;

  with TRadioButton(cyRadioGroup1.Controls[2]) do
  begin
    Enabled := false;
  end;

  cyRadioButton1.Font.Color := clFuchsia;
  cyCheckBox1.Font.Color := clFuchsia;
end;

procedure TFrmCyRadioGroup.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyRadioGroup.FormCreate(Sender: TObject);
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

procedure TFrmCyRadioGroup.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;


(*procedure TFrmCyRadioGroup.OnCtlColorStatic(var msg: TWMCtlColorStatic);
begin
  inherited;

  // Only works if not theme enabled !
  SetTextColor(msg.ChildDC, clRed);
end;          *)

procedure TFrmCyRadioGroup.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
