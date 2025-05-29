unit formcyTabControl;


interface

uses
  LCLIntf, LCLType, LMessages, Messages,
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, ExtCtrls,
  cyBasePanel, cyPanel, StdCtrls, Buttons, cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBevel, cyBaseSpeedButton, cyTabControl, cyColorGrid, cyBaseTabControl, ImgList;

type
  TFrmCyTabControl = class(TForm)
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
    Panel4: TPanel;
    cyTabControl2: TcyTabControl;
    CyPanel3: TCyPanel;
    cyColorGrid2: TcyColorGrid;
    cyTabControl1: TcyTabControl;
    CyPanel4: TCyPanel;
    cyColorGrid1: TcyColorGrid;
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
  FrmCyTabControl: TFrmCyTabControl;

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

procedure TFrmCyTabControl.Button1Click(Sender: TObject);
begin
  if cyTabControl2.TabHeight = 0 then
  begin
    cyTabControl2.TabHeight := 1;
    cyTabControl2.TabWidth := 1;
  end
  else begin
    cyTabControl2.TabHeight := 0;
    cyTabControl2.TabWidth := 0;
  end;
end;

procedure TFrmCyTabControl.CBThinBorderClick(Sender: TObject);
begin
  cyTabControl1.ThinBorder := CBThinBorder.Checked;
  cyTabControl2.ThinBorder := CBThinBorder.Checked;
end;

procedure TFrmCyTabControl.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyTabControl.FormCreate(Sender: TObject);
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

procedure TFrmCyTabControl.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyTabControl.RGStyleClick(Sender: TObject);
begin
  case RGStyle.ItemIndex of
    0: begin
         cyTabControl1.Style := tsTab;
       end;

    1: begin
         cyTabControl1.Style := tsButton;
       end;

    2: begin
         cyTabControl1.Style := tsFlatButton;
       end;
  end;

  cyTabControl2.Style := cyTabControl1.Style;
end;

procedure TFrmCyTabControl.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
