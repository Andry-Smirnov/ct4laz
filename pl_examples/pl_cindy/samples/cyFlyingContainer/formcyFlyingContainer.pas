unit formcyFlyingContainer;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyColorGrid, cyFlyingContainer, cyBaseSpeedButton, cyBaseContainer, cyAdvPanel;

type
  TFrmCyFlyingContainer = class(TForm)
    CyPanel1: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    StatusBar1: TStatusBar;
    SBEscKey: TcySpeedButton;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    cyLabel1: TcyLabel;
    LblWebSite: TcyHotLabel;
    Panel1: TPanel;
    PanSelectFontColor: TPanel;
    Shape1: TShape;
    cyColorGridFont: TcyColorGrid;
    SBFontMoreColor: TSpeedButton;
    Shape5: TShape;
    SBAutoFontColor: TSpeedButton;
    Panel5: TPanel;
    Panel6: TPanel;
    btnBGColor: TSpeedButton;
    cyFlyingContainer1: TcyFlyingContainer;
    Memo1: TMemo;
    Label1: TLabel;
    ECaption: TEdit;
    Label2: TLabel;
    RBNone: TRadioButton;
    RBSingle: TRadioButton;
    PanSplashScreen: TcyAdvPanel;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure btnBGColorClick(Sender: TObject);
    procedure SBAutoFontColorClick(Sender: TObject);
    procedure cyColorGridFontBoxClick(Sender: TObject; aRow, aCol: Integer;
      aColor: TColor);
    procedure SBEscKeyClick(Sender: TObject);
    procedure SBFontMoreColorClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyFlyingContainer: TFrmCyFlyingContainer;

implementation

{$R *.lfm}
const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}


procedure TFrmCyFlyingContainer.btnBGColorClick(Sender: TObject);
begin
  cyFlyingContainer1.Control := PanSelectFontColor;
  cyFlyingContainer1.Caption := ECaption.Text;

  if RBNone.Checked
  then cyFlyingContainer1.BorderStyle := bsNone
  else cyFlyingContainer1.BorderStyle := bsSingle;

  cyFlyingContainer1.ExecuteFromControl(btnBGColor, 0, btnBGColor.Height);
end;

procedure TFrmCyFlyingContainer.cyColorGridFontBoxClick(Sender: TObject;
  aRow, aCol: Integer; aColor: TColor);
begin
  Memo1.Color := aColor;
  Memo1.SetFocus;
end;

procedure TFrmCyFlyingContainer.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyFlyingContainer.FormCreate(Sender: TObject);
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

  cyFlyingContainer1.Control := PanSplashScreen;
  cyFlyingContainer1.ExecuteAsSplashScreen;

  Sleep(1000);
  PanSplashScreen.Caption := ' Loading step 1';
  Application.ProcessMessages;
  Sleep(1000);
  PanSplashScreen.Caption := ' Loading step 2';
  Application.ProcessMessages;
  Sleep(1000);
  PanSplashScreen.Caption := ' Loading step 3';
  Application.ProcessMessages;
  Sleep(1000);

  cyFlyingContainer1.Close;
end;

procedure TFrmCyFlyingContainer.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyFlyingContainer.SBAutoFontColorClick(Sender: TObject);
begin
  Memo1.Color := clWindow;
  Memo1.SetFocus;
end;

procedure TFrmCyFlyingContainer.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCyFlyingContainer.SBEscKeyClick(Sender: TObject);
begin
  cyFlyingContainer1.CloseOnEscKey := SBEscKey.Down;
end;

procedure TFrmCyFlyingContainer.SBFontMoreColorClick(Sender: TObject);
begin
  cyFlyingContainer1.Control := nil;
end;

end.
