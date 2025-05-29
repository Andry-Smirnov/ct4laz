unit formcyAdvFlowPanel;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyAdvPanel, cyBaseSpeedButton, cyBaseFlowPanel, cyFlowPanel, cyAdvFlowPanel, cyBaseButton, cyBitBtn, cyBaseLed, cyLed, cyBevel;

type
  TFrmCyAdvFlowPanel = class(TForm)
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
    cyAdvFlowPanel2: TcyAdvFlowPanel;
    cyAdvFlowPanel3: TcyAdvFlowPanel;
    cyAdvFlowPanel4: TcyAdvFlowPanel;
    SBAllowRuntimeDesign: TcySpeedButton;
    cyLed1: TcyLed;
    cyBitBtn1: TcyBitBtn;
    Edit1: TEdit;
    cyLed2: TcyLed;
    cyBitBtn2: TcyBitBtn;
    Edit2: TEdit;
    cyLed3: TcyLed;
    cyBitBtn3: TcyBitBtn;
    Edit3: TEdit;
    cyBevel1: TcyBevel;
    cyBevel2: TcyBevel;
    cyBevel3: TcyBevel;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure SBAllowRuntimeDesignClick(Sender: TObject);
    procedure cyAdvFlowPanel4StartRunTimeDesign(Sender: TObject; X, Y: Integer);
    procedure cyAdvFlowPanel4EndRunTimeDesign(Sender: TObject; X, Y: Integer);
    procedure cyAdvFlowPanel4DoRunTimeDesign(Sender: TObject; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyAdvFlowPanel: TFrmCyAdvFlowPanel;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyAdvFlowPanel.cyAdvFlowPanel4DoRunTimeDesign(Sender: TObject; X, Y: Integer);
begin
  TcyAdvPanel(Sender).Caption := 'Runtime design continue';
end;

procedure TFrmCyAdvFlowPanel.cyAdvFlowPanel4EndRunTimeDesign(Sender: TObject; X, Y: Integer);
begin
  TcyAdvPanel(Sender).Caption := 'Runtime design ended';
end;

procedure TFrmCyAdvFlowPanel.cyAdvFlowPanel4StartRunTimeDesign(Sender: TObject; X, Y: Integer);
begin
  TcyAdvPanel(Sender).Caption := 'Runtime design started';
end;

procedure TFrmCyAdvFlowPanel.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyAdvFlowPanel.FormCreate(Sender: TObject);
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

procedure TFrmCyAdvFlowPanel.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyAdvFlowPanel.SBAllowRuntimeDesignClick(Sender: TObject);

    procedure ApplyRuntime(toPanel: TcyAdvFlowPanel);
    begin
      toPanel.RunTimeDesign.AllowMove := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeTop := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeLeft := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeRight := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeBottom := SBAllowRuntimeDesign.Down;
    end;

begin
  ApplyRuntime(cyAdvFlowPanel2);
  ApplyRuntime(cyAdvFlowPanel3);
  ApplyRuntime(cyAdvFlowPanel4);
end;

procedure TFrmCyAdvFlowPanel.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
