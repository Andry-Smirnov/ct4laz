unit formcyScrollbox;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, ExtCtrls,
  cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel, cyLabel, cyBevel, cyBaseSpeedButton, cyScrollBox, cyPaintBox,
  cyGraphics, cyTypes;

type
  TFrmCyScrollbox = class(TForm)
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
    cyScrollBox1: TcyScrollBox;
    Edit1: TEdit;
    Label1: TLabel;
    Panel3: TPanel;
    Edit2: TEdit;
    Label2: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure cyScrollBox1VertScroll(Sender: TObject);
    procedure cyScrollBox1HorzScroll(Sender: TObject);
    procedure cyScrollBox1Resize(Sender: TObject);
    procedure cyScrollBox1Paint(Sender: TObject);
  private
  public
  end;

var
  FrmCyScrollbox: TFrmCyScrollbox;

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

procedure TFrmCyScrollbox.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyScrollbox.FormCreate(Sender: TObject);
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

procedure TFrmCyScrollbox.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyScrollbox.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCyScrollbox.cyScrollBox1Paint(Sender: TObject);
var
  aRect: TRect;
begin
  // Draw rectangle at top left position :
  // aRect := classes.Rect(10, 10, 100, 100);
  aRect := cyScrollBox1.ClientRect;

  cyGradientFill(cyScrollBox1.Canvas, aRect, clRed, clGray, dgdVertical, 50, 0, bmNormal, 255, 100);

  cyScrollBox1.Canvas.Font.Color := clWhite;
  cyScrollBox1.Canvas.TextOut(10, 10, 'TcyScollbox demo');
end;

procedure TFrmCyScrollbox.cyScrollBox1Resize(Sender: TObject);
begin
  cyScrollBox1.Invalidate;
end;

procedure TFrmCyScrollbox.cyScrollBox1VertScroll(Sender: TObject);
begin
  cyScrollBox1.Invalidate;
end;

procedure TFrmCyScrollbox.cyScrollBox1HorzScroll(Sender: TObject);
begin
  cyScrollBox1.Invalidate;
end;

end.
