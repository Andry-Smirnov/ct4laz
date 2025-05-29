unit formcySimpleGauge;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyTypes, cyBaseMeasure, cyCustomGauge, cySimpleGauge,
  cyPaintBox, cyGraphics, cyBaseSpeedButton;

type
  TFrmCySimpleGauge = class(TForm)
    CyPanel1: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    StatusBar1: TStatusBar;
    SBReadOnly: TcySpeedButton;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    cyLabel1: TcyLabel;
    LblWebSite: TcyHotLabel;
    Panel1: TPanel;
    cyDgradPaintbox6: TcyPaintBox;
    Label16: TLabel;
    Label18: TLabel;
    cySimpleGauge2: TcySimpleGauge;
    cySimpleGauge1: TcySimpleGauge;
    cySimpleGauge3: TcySimpleGauge;
    Label76: TLabel;
    cySimpleGauge5: TcySimpleGauge;
    cySimpleGauge6: TcySimpleGauge;
    cySimpleGauge7: TcySimpleGauge;
    cySimpleGauge8: TcySimpleGauge;
    cySimpleGauge9: TcySimpleGauge;
    Image3: TImage;
    cySimpleGauge10: TcySimpleGauge;
    Label17: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure cySimpleGauge8CustomDrawItem(Sender: TObject; aRect: TRect;
      Value: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure SBReadOnlyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCySimpleGauge: TFrmCySimpleGauge;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCySimpleGauge.cySimpleGauge8CustomDrawItem(Sender: TObject;
  aRect: TRect; Value: Double);
begin
  if Value <= cySimpleGauge8.Position
  then begin
    if Value < 30
    then
      cyGradientFill(cySimpleGauge8.Canvas, aRect, clWhite, clRed, dgdVertical, 50,0, bmReverse, 255, 100)
    else
      if Value < 50
      then
        cyGradientFill(cySimpleGauge8.Canvas, aRect, clWhite, clYellow, dgdVertical, 50,0, bmReverse, 255, 100)
      else
        cyGradientFill(cySimpleGauge8.Canvas, aRect, clWhite, clGreen, dgdVertical, 50,0, bmReverse, 255, 100);
  end
  else
    cyGradientFill(cySimpleGauge8.Canvas, aRect, clWhite, clSilver, dgdVertical, 50,0, bmNormal, 255, 100);
end;

procedure TFrmCySimpleGauge.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCySimpleGauge.FormCreate(Sender: TObject);
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

procedure TFrmCySimpleGauge.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCySimpleGauge.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCySimpleGauge.SBReadOnlyClick(Sender: TObject);
var i: Integer;
begin
  for i := 0 to ComponentCount-1 do
    if Components[i] is TcySimpleGauge
    then TcySimpleGauge(Components[i]).ReadOnly := SBReadOnly.Down;
end;

procedure TFrmCySimpleGauge.Timer1Timer(Sender: TObject);
begin
  if not Application.Terminated
  then
    if cySimpleGauge10.Position <> 100
    then cySimpleGauge10.StepIt
    else cySimpleGauge10.Position := 0;
end;

end.
