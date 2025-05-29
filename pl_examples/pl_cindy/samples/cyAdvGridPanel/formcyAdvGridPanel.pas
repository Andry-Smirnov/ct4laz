unit formcyAdvGridPanel;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyAdvPanel, cyBaseSpeedButton, cyBaseGridPanel, cyGridPanel, cyAdvGridPanel;

type
  TFrmCyAdvGridPanel = class(TForm)
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
    cyAdvGridPanel2: TcyAdvGridPanel;
    cyAdvGridPanel3: TcyAdvGridPanel;
    cyAdvGridPanel4: TcyAdvGridPanel;
    SBAllowRuntimeDesign: TcySpeedButton;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure SBAllowRuntimeDesignClick(Sender: TObject);
    procedure cyAdvGridPanel4StartRunTimeDesign(Sender: TObject; X, Y: Integer);
    procedure cyAdvGridPanel4EndRunTimeDesign(Sender: TObject; X, Y: Integer);
    procedure cyAdvGridPanel4DoRunTimeDesign(Sender: TObject; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyAdvGridPanel: TFrmCyAdvGridPanel;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyAdvGridPanel.cyAdvGridPanel4DoRunTimeDesign(Sender: TObject; X, Y: Integer);
begin
  TcyAdvPanel(Sender).Caption := 'Runtime design continue';
end;

procedure TFrmCyAdvGridPanel.cyAdvGridPanel4EndRunTimeDesign(Sender: TObject; X, Y: Integer);
begin
  TcyAdvPanel(Sender).Caption := 'Runtime design ended';
end;

procedure TFrmCyAdvGridPanel.cyAdvGridPanel4StartRunTimeDesign(Sender: TObject; X, Y: Integer);
begin
  TcyAdvPanel(Sender).Caption := 'Runtime design started';
end;

procedure TFrmCyAdvGridPanel.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyAdvGridPanel.FormCreate(Sender: TObject);
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

procedure TFrmCyAdvGridPanel.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyAdvGridPanel.SBAllowRuntimeDesignClick(Sender: TObject);

    procedure ApplyRuntime(toPanel: TcyAdvGridPanel);
    begin
      toPanel.RunTimeDesign.AllowMove := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeTop := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeLeft := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeRight := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeBottom := SBAllowRuntimeDesign.Down;
    end;

begin
  ApplyRuntime(cyAdvGridPanel2);
  ApplyRuntime(cyAdvGridPanel3);
  ApplyRuntime(cyAdvGridPanel4);
end;

procedure TFrmCyAdvGridPanel.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
