unit formcyGridPanel;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBaseSpeedButton, cyBaseGridPanel, cyGridPanel;

type

  { TFrmCyGridPanel }

  TFrmCyGridPanel = class(TForm)
    CyPanel1: TCyPanel;
    CyPanel3: TCyPanel;
    CyPanel4: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    StatusBar1: TStatusBar;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    cyLabel1: TcyLabel;
    LblWebSite: TcyHotLabel;
    Panel1: TPanel;
    SBAllowRuntimeDesign: TcySpeedButton;
    CyPanel6: TCyPanel;
    CyPanel5: TCyPanel;
    CyGridPanel4: TCyGridPanel;
    CyGridPanel3: TCyGridPanel;
    Panel4: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure SBAllowRuntimeDesignClick(Sender: TObject);
    procedure CyGridPanel4StartRunTimeDesign(Sender: TObject; X, Y: Integer);
    procedure CyGridPanel4DoRunTimeDesign(Sender: TObject; X, Y: Integer);
    procedure CyGridPanel4EndRunTimeDesign(Sender: TObject; X, Y: Integer);
    procedure CyGridPanel3MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure CyGridPanel3MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure CyGridPanel4Click(Sender: TObject);
    procedure CyGridPanel3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyGridPanel: TFrmCyGridPanel;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyGridPanel.CyGridPanel3Click(Sender: TObject);
begin
  CyGridPanel3.SetFocus;
end;

procedure TFrmCyGridPanel.CyGridPanel3MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  TControl(Sender).Top := TControl(Sender).Top + 1;
end;

procedure TFrmCyGridPanel.CyGridPanel3MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  TControl(Sender).Top := TControl(Sender).Top - 1;
end;

procedure TFrmCyGridPanel.CyGridPanel4Click(Sender: TObject);
begin
  CyGridPanel4.SetFocus;
end;

procedure TFrmCyGridPanel.CyGridPanel4DoRunTimeDesign(Sender: TObject; X,
  Y: Integer);
begin
  TPanel(Sender).Caption := 'Runtime design continue';
end;

procedure TFrmCyGridPanel.CyGridPanel4EndRunTimeDesign(Sender: TObject; X,
  Y: Integer);
begin
  TPanel(Sender).Caption := 'Runtime design ended';
end;

procedure TFrmCyGridPanel.CyGridPanel4StartRunTimeDesign(Sender: TObject; X,
  Y: Integer);
begin
  TPanel(Sender).Caption := 'Runtime design started';
end;

procedure TFrmCyGridPanel.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyGridPanel.FormCreate(Sender: TObject);
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

procedure TFrmCyGridPanel.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyGridPanel.SBAllowRuntimeDesignClick(Sender: TObject);

    procedure ApplyRuntime(toPanel: TcyGridPanel);
    begin
      toPanel.RunTimeDesign.AllowMove := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeTop := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeLeft := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeRight := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeBottom := SBAllowRuntimeDesign.Down;
    end;

begin
  ApplyRuntime(CyGridPanel3);
  ApplyRuntime(CyGridPanel4);
end;

procedure TFrmCyGridPanel.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
