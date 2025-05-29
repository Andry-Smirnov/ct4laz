unit formcyFlowPanel;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBaseSpeedButton, cyBaseFlowPanel, cyFlowPanel, cyBevel, cyBaseLed, cyLed, cyBaseButton, cyBitBtn;

type
  TFrmCyFlowPanel = class(TForm)
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
    SBAllowRuntimeDesign: TcySpeedButton;
    CyPanel6: TCyPanel;
    CyPanel5: TCyPanel;
    CyFlowPanel4: TCyFlowPanel;
    CyFlowPanel3: TCyFlowPanel;
    Edit1: TEdit;
    cyBitBtn1: TcyBitBtn;
    cyLed1: TcyLed;
    cyBevel1: TcyBevel;
    Edit2: TEdit;
    cyBitBtn2: TcyBitBtn;
    cyLed2: TcyLed;
    cyBevel2: TcyBevel;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure SBAllowRuntimeDesignClick(Sender: TObject);
    procedure CyFlowPanel4StartRunTimeDesign(Sender: TObject; X, Y: Integer);
    procedure CyFlowPanel4DoRunTimeDesign(Sender: TObject; X, Y: Integer);
    procedure CyFlowPanel4EndRunTimeDesign(Sender: TObject; X, Y: Integer);
    procedure CyFlowPanel3MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure CyFlowPanel3MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure CyFlowPanel4Click(Sender: TObject);
    procedure CyFlowPanel3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyFlowPanel: TFrmCyFlowPanel;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyFlowPanel.CyFlowPanel3Click(Sender: TObject);
begin
  CyFlowPanel3.SetFocus;
end;

procedure TFrmCyFlowPanel.CyFlowPanel3MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  TControl(Sender).Top := TControl(Sender).Top + 1;
end;

procedure TFrmCyFlowPanel.CyFlowPanel3MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  TControl(Sender).Top := TControl(Sender).Top - 1;
end;

procedure TFrmCyFlowPanel.CyFlowPanel4Click(Sender: TObject);
begin
  CyFlowPanel4.SetFocus;
end;

procedure TFrmCyFlowPanel.CyFlowPanel4DoRunTimeDesign(Sender: TObject; X,
  Y: Integer);
begin
  TPanel(Sender).Caption := 'Runtime design continue';
end;

procedure TFrmCyFlowPanel.CyFlowPanel4EndRunTimeDesign(Sender: TObject; X,
  Y: Integer);
begin
  TPanel(Sender).Caption := 'Runtime design ended';
end;

procedure TFrmCyFlowPanel.CyFlowPanel4StartRunTimeDesign(Sender: TObject; X,
  Y: Integer);
begin
  TPanel(Sender).Caption := 'Runtime design started';
end;

procedure TFrmCyFlowPanel.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyFlowPanel.FormCreate(Sender: TObject);
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

procedure TFrmCyFlowPanel.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyFlowPanel.SBAllowRuntimeDesignClick(Sender: TObject);

    procedure ApplyRuntime(toPanel: TcyFlowPanel);
    begin
      toPanel.RunTimeDesign.AllowMove := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeTop := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeLeft := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeRight := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeBottom := SBAllowRuntimeDesign.Down;
    end;

begin
  ApplyRuntime(CyFlowPanel3);
  ApplyRuntime(CyFlowPanel4);
end;

procedure TFrmCyFlowPanel.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
