unit formcyPageControl;

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, ExtCtrls,
  cyBasePanel, cyPanel, StdCtrls, Buttons, cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBevel, cyBaseSpeedButton, cyPageControl, cyBaseTabControl, cyColorGrid, ImgList;

type
  TFrmCyPageControl = class(TForm)
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
    RGStyle: TRadioGroup;
    CBThinBorder: TCheckBox;
    ImageList1: TImageList;
    Button1: TButton;
    Panel3: TPanel;
    cyPageControl1: TcyPageControl;
    TabSheet7: TTabSheet;
    CyPanel10: TCyPanel;
    Label1: TLabel;
    TabSheet8: TTabSheet;
    CyPanel3: TCyPanel;
    cyColorGrid1: TcyColorGrid;
    TabSheet9: TTabSheet;
    CyPanel4: TCyPanel;
    cySpeedButton1: TcySpeedButton;
    cySpeedButton2: TcySpeedButton;
    cySpeedButton3: TcySpeedButton;
    cyPageControl2: TcyPageControl;
    TabSheet1: TTabSheet;
    CyPanel5: TCyPanel;
    Label2: TLabel;
    TabSheet2: TTabSheet;
    CyPanel6: TCyPanel;
    cyColorGrid2: TcyColorGrid;
    TabSheet3: TTabSheet;
    CyPanel7: TCyPanel;
    cySpeedButton4: TcySpeedButton;
    cySpeedButton5: TcySpeedButton;
    cySpeedButton6: TcySpeedButton;
    TabSheet4: TTabSheet;
    CyPanel9: TCyPanel;
    cySpeedButton10: TcySpeedButton;
    cySpeedButton11: TcySpeedButton;
    cySpeedButton12: TcySpeedButton;
    TabSheet5: TTabSheet;
    CyPanel8: TCyPanel;
    cySpeedButton7: TcySpeedButton;
    cySpeedButton8: TcySpeedButton;
    cySpeedButton9: TcySpeedButton;
    TabSheet6: TTabSheet;
    CyPanel11: TCyPanel;
    cySpeedButton13: TcySpeedButton;
    cySpeedButton14: TcySpeedButton;
    cySpeedButton15: TcySpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure RGStyleClick(Sender: TObject);
    procedure CBThinBorderClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  FrmCyPageControl: TFrmCyPageControl;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ENDIF}
{$IFDEF UNIX}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyPageControl.Button1Click(Sender: TObject);
begin
//  TabSheet7.TabVisible := not TabSheet7.TabVisible;
  TabSheet8.TabVisible := not TabSheet8.TabVisible;
end;

procedure TFrmCyPageControl.CBThinBorderClick(Sender: TObject);
begin
  cyPageControl1.ThinBorder := CBThinBorder.Checked;
  cyPageControl2.ThinBorder := CBThinBorder.Checked;
end;

procedure TFrmCyPageControl.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyPageControl.FormCreate(Sender: TObject);
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

procedure TFrmCyPageControl.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyPageControl.RGStyleClick(Sender: TObject);
begin
  case RGStyle.ItemIndex of
    0: begin
         cyPageControl1.Style := tsTab;
         cyPageControl1.ShowTabs;
       end;

    1: begin
         cyPageControl1.Style := tsButton;
         cyPageControl1.ShowTabs;
       end;

    2: begin
         cyPageControl1.Style := tsFlatButton;
         cyPageControl1.ShowTabs;
       end;

    3: begin
         cyPageControl1.HideTabs;
       end;
  end;

  cyPageControl2.Style := cyPageControl1.Style;
end;

procedure TFrmCyPageControl.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
