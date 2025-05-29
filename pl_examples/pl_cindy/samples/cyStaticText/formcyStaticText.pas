unit formcyStaticText;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel, cyLabel, cyBevel,
  cyBaseSpeedButton, cyBaseStaticText, cyStaticText, cyTypes;

type
  TFrmCyStaticText = class(TForm)
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
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    cyStaticText1: TcyStaticText;
    cyStaticText3: TcyStaticText;
    cyStaticText4: TcyStaticText;
    cyStaticText5: TcyStaticText;
    cyStaticText6: TcyStaticText;
    cyStaticText7: TcyStaticText;
    cyStaticText8: TcyStaticText;
    cyStaticText9: TcyStaticText;
    cyStaticText10: TcyStaticText;
    cyStaticText11: TcyStaticText;
    cyStaticText12: TcyStaticText;
    cyStaticText13: TcyStaticText;
    CBTextAlignment: TComboBox;
    CBTextLayout: TComboBox;
    cyStaticText14: TcyStaticText;
    CBTextRender: TComboBox;
    CBWordwrap: TCheckBox;
    CBTransparent: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure CBTextAlignmentClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyStaticText: TFrmCyStaticText;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
 pathMedia = '..\xmedia\';
{$ELSE}
 pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyStaticText.CBTextAlignmentClick(Sender: TObject);
var c: Integer;
begin
  for c := 0 to ComponentCount-1 do
    if Components[c] is TcyStaticText then
      with TcyStaticText(Components[c]) do
      begin
        CaptionAlignment := TAlignment(CBTextAlignment.ItemIndex);
        CaptionLayout    := TTextLayout(CBTextLayout.ItemIndex);
        CaptionRender    := TCaptionRender(CBTextRender.ItemIndex);
        WordWrap         := CBWordwrap.Checked;
        Transparent      := CBTransparent.Checked;
      end;
end;

procedure TFrmCyStaticText.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyStaticText.FormCreate(Sender: TObject);
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

procedure TFrmCyStaticText.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyStaticText.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
