unit formcySkinArea;

{$MODE Delphi}

interface

uses
 // Windows, ShellAPI,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel, LCLIntf,
  cyLabel,  cySkinArea, cyGraphics, cyBaseSpeedButton;

type

  { TFrmCySkinArea }

  TFrmCySkinArea = class(TForm)
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
    ImgArea: TImage;
    cySkinArea1: TcySkinArea;
    SBDoubleBuffer: TcySpeedButton;
    Label1: TLabel;
    procedure cySkinArea1ClickArea(Sender: TObject; Item: tcyArea);
    procedure cySkinArea1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure SBDoubleBufferClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCySkinArea: TFrmCySkinArea;

implementation

{$R *.lfm}


const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCySkinArea.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCySkinArea.FormCreate(Sender: TObject);
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

procedure TFrmCySkinArea.cySkinArea1ClickArea(Sender: TObject; Item: tcyArea);
var
  aBmp: TBitmap;
  AreaRect: TRect;
begin
  aBmp := TBitmap.Create;
  cySkinArea1.GetBitmapArea(aBmp, AreaRect, Item, Item.State, clNone);
  ImgArea.Picture.Bitmap.Assign(aBmp);
  ImgArea.Picture.Bitmap.PixelFormat := aBmp.PixelFormat;
  ImgArea.Picture.Bitmap.Width := AreaRect.Right - AreaRect.Left;
  ImgArea.Picture.Bitmap.Height := AreaRect.Bottom - AreaRect.Top;
  ImgArea.Picture.Bitmap.Canvas.CopyRect(
      classes.Rect(0, 0, ImgArea.Picture.Bitmap.width, ImgArea.Picture.Bitmap.height),
        aBmp.Canvas, AreaRect);
  aBmp.Free;
end;

procedure TFrmCySkinArea.cySkinArea1Paint(Sender: TObject);
begin

end;

procedure TFrmCySkinArea.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCySkinArea.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCySkinArea.SBDoubleBufferClick(Sender: TObject);
begin
  DoubleBuffered := SBDoubleBuffer.Down;
end;

end.
