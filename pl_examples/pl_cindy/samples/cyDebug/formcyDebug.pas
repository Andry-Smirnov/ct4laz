unit formcyDebug;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons, Math,
  cyGraphics, cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyColorMatrix, cyBaseSpeedButton, cyBaseColorMatrix, cyBaseButton, cyBitBtn, Grids, cyDebug;

type
  TFrmCyDebug = class(TForm)
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
    CBRandomDemo: TCheckBox;
    cyColorMatrix1: TcyColorMatrix;
    cyDebug1: TcyDebug;
    StringGrid1: TStringGrid;
    CBShowStatistics: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure CBRandomDemoClick(Sender: TObject);
    procedure cySpeedButton1Click(Sender: TObject);
    procedure cyColorMatrix1Paint(Sender: TObject);
    procedure CBShowStatisticsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyDebug: TFrmCyDebug;

  Statistics: String;

implementation

{$R *.lfm}
const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyDebug.CBShowStatisticsClick(Sender: TObject);
begin
  if CBShowStatistics.Checked
  then cyDebug1.ProcessGrid := StringGrid1
  else cyDebug1.ProcessGrid := Nil;

  StringGrid1.Visible := CBShowStatistics.Checked;
end;

procedure TFrmCyDebug.cyColorMatrix1Paint(Sender: TObject);
begin
  // You can only change cyColorMatrix1.Canvas here!
  cyDebug1.ProcessEnter('Paint');
                              Sleep(10);
  if CBRandomDemo.Checked
  then begin
    cyColorMatrix1.Canvas.Brush.Color := clBlack;
    cyColorMatrix1.Canvas.Font.Color := clWhite;
    cyColorMatrix1.Canvas.TextOut(10, cyColorMatrix1.Height-25, Statistics);
  end;

  cyDebug1.ProcessExit('Paint');
end;

procedure TFrmCyDebug.cySpeedButton1Click(Sender: TObject);
var aRow: Integer;
begin
  aRow := cyColorMatrix1.ValueToRow(cyColorMatrix1.TopRowValue);
  cyColorMatrix1.SetColorGridRange(aRow, aRow, 0, 10, clRed);

  aRow := cyColorMatrix1.ValueToRow(cyColorMatrix1.BottomRowValue);
  cyColorMatrix1.SetColorGridRange(aRow, aRow, 0, 10, clLime);
end;

procedure TFrmCyDebug.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyDebug.FormCreate(Sender: TObject);
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

  Randomize;
  Statistics := 'Calculating ...';
end;

procedure TFrmCyDebug.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyDebug.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCyDebug.CBRandomDemoClick(Sender: TObject);
var
  c, r, _r, _g, _b: integer;
  DrawCount: Integer;
  SaveTime: Cardinal;
begin
  // Activate TcyDebug:
  cyDebug1.Active := CBRandomDemo.Checked;

  if cyDebug1.Active then
    cyDebug1.InitializeProcesses;

  DrawCount := 0;
  SaveTime := GetTickCount64;
  while (CBRandomDemo.Checked) and (not Application.Terminated) do
  begin
    cyColorMatrix1.BeginUpdate;

    for r := 0 to cyColorMatrix1.RowCount - 1 do
    begin
      cyDebug1.ProcessEnter('Set line color');

      for c := 0 to cyColorMatrix1.ColCount -1 do
      begin
        _r := RandomRange(0, 254);
        _g := RandomRange(0, 254);
        _b := RandomRange(0, 254);

        cyColorMatrix1.SetColorGrid(r, c, RGB(_r, _g, _b));

        Inc(DrawCount, 1);
        if GetTickCount64 - SaveTime > 1000
        then begin
          Statistics := intToStr(DrawCount) + ' boxes painted per second.';
          // Frames per second:
          Statistics := Statistics + '   '
            + intToStr(DrawCount div (cyColorMatrix1.RowCount*cyColorMatrix1.ColCount)) + ' frames per second.';
          DrawCount := 0;
          SaveTime := GetTickCount64;
        end;
      end;

      cyDebug1.ProcessExit('Set line color');
    end;

    cyColorMatrix1.Canvas.Brush.Style := bsSolid;
    cyColorMatrix1.Canvas.Brush.Color := clWhite;
    cyColorMatrix1.Canvas.Font.Color := clBlack;
    cyColorMatrix1.DrawText('Random demo', 0, 0, cyColorMatrix1.RowCount-1, cyColorMatrix1.ColCount-1, DT_WORDBREAK);
    cyColorMatrix1.EndUpdate;

    Application.ProcessMessages;
  end;
end;

end.
