unit formcyAdvPanel;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyAdvPanel, cyBaseSpeedButton;

type
  TFrmCyAdvPanel = class(TForm)
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
    cyAdvPanel2: TcyAdvPanel;
    cyAdvPanel3: TcyAdvPanel;
    cyAdvPanel4: TcyAdvPanel;
    SBAllowRuntimeDesign: TcySpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure SBAllowRuntimeDesignClick(Sender: TObject);
    procedure cyAdvPanel4StartRunTimeDesign(Sender: TObject; X, Y: Integer);
    procedure cyAdvPanel4EndRunTimeDesign(Sender: TObject; X, Y: Integer);
    procedure cyAdvPanel4DoRunTimeDesign(Sender: TObject; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyAdvPanel: TFrmCyAdvPanel;

implementation

{$R *.lfm}
const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyAdvPanel.cyAdvPanel4DoRunTimeDesign(Sender: TObject; X, Y: Integer);
begin
  TcyAdvPanel(Sender).Caption := 'Runtime design continue';
end;

procedure TFrmCyAdvPanel.cyAdvPanel4EndRunTimeDesign(Sender: TObject; X, Y: Integer);
begin
  TcyAdvPanel(Sender).Caption := 'Runtime design ended';
end;

procedure TFrmCyAdvPanel.cyAdvPanel4StartRunTimeDesign(Sender: TObject; X, Y: Integer);
begin
  TcyAdvPanel(Sender).Caption := 'Runtime design started';
end;

procedure TFrmCyAdvPanel.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyAdvPanel.FormCreate(Sender: TObject);
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

procedure TFrmCyAdvPanel.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyAdvPanel.SBAllowRuntimeDesignClick(Sender: TObject);

    procedure ApplyRuntime(toPanel: TcyAdvPanel);
    begin
      toPanel.RunTimeDesign.AllowMove := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeTop := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeLeft := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeRight := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeBottom := SBAllowRuntimeDesign.Down;
    end;

begin
  ApplyRuntime(cyAdvPanel2);
  ApplyRuntime(cyAdvPanel3);
  ApplyRuntime(cyAdvPanel4);
end;

procedure TFrmCyAdvPanel.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
