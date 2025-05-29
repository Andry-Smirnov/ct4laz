unit formcyPanel;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBaseSpeedButton;

type
  TFrmCyPanel = class(TForm)
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
    CyPanel4: TCyPanel;
    CyPanel3: TCyPanel;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure SBAllowRuntimeDesignClick(Sender: TObject);
    procedure CyPanel4StartRunTimeDesign(Sender: TObject; X, Y: Integer);
    procedure CyPanel4DoRunTimeDesign(Sender: TObject; X, Y: Integer);
    procedure CyPanel4EndRunTimeDesign(Sender: TObject; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyPanel: TFrmCyPanel;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyPanel.CyPanel4DoRunTimeDesign(Sender: TObject; X,
  Y: Integer);
begin
  TPanel(Sender).Caption := 'Runtime design continue';
end;

procedure TFrmCyPanel.CyPanel4EndRunTimeDesign(Sender: TObject; X,
  Y: Integer);
begin
  TPanel(Sender).Caption := 'Runtime design ended';
end;

procedure TFrmCyPanel.CyPanel4StartRunTimeDesign(Sender: TObject; X,
  Y: Integer);
begin
  TPanel(Sender).Caption := 'Runtime design started';
end;

procedure TFrmCyPanel.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyPanel.FormCreate(Sender: TObject);
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

procedure TFrmCyPanel.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyPanel.SBAllowRuntimeDesignClick(Sender: TObject);

    procedure ApplyRuntime(toPanel: TcyPanel);
    begin
      toPanel.RunTimeDesign.AllowMove := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeTop := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeLeft := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeRight := SBAllowRuntimeDesign.Down;
      toPanel.RunTimeDesign.AllowResizeBottom := SBAllowRuntimeDesign.Down;
    end;

begin
  ApplyRuntime(CyPanel3);
  ApplyRuntime(CyPanel4);
end;

procedure TFrmCyPanel.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
