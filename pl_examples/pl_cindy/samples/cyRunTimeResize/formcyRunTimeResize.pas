unit formcyRunTimeResize;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyRunTimeResize, cyBaseSpeedButton;

type
  TFrmCyRunTimeResize = class(TForm)
    CyPanel1: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    StatusBar1: TStatusBar;
    SBAllowMove: TcySpeedButton;
    SBAllowResize: TcySpeedButton;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    cyLabel1: TcyLabel;
    LblWebSite: TcyHotLabel;
    Panel1: TPanel;
    cyRunTimeResize1: TcyRunTimeResize;
    Label1: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    ListBox1: TListBox;
    SpeedButton1: TSpeedButton;
    cySpeedButton3: TcySpeedButton;
    SBAllowOutside: TcySpeedButton;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure Label1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Label1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Label1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBAllowMoveClick(Sender: TObject);
    procedure SBAllowResizeClick(Sender: TObject);
    procedure SBAllowOutsideClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyRunTimeResize: TFrmCyRunTimeResize;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyRunTimeResize.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyRunTimeResize.FormCreate(Sender: TObject);
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

procedure TFrmCyRunTimeResize.Label1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  cyRunTimeResize1.StartJob(X, Y);
end;

procedure TFrmCyRunTimeResize.Label1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  // Change handled control:
  cyRunTimeResize1.Control := TControl(Sender);
  cyRunTimeResize1.DoJob(X, Y);
end;

procedure TFrmCyRunTimeResize.Label1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  cyRunTimeResize1.EndJob(X, Y);
end;

procedure TFrmCyRunTimeResize.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyRunTimeResize.SBAllowMoveClick(Sender: TObject);
begin
  cyRunTimeResize1.Options.AllowMove := SBAllowMove.Down;
end;

procedure TFrmCyRunTimeResize.SBAllowOutsideClick(Sender: TObject);
begin
  cyRunTimeResize1.Options.OutsideParentRect := SBAllowOutside.Down;
end;

procedure TFrmCyRunTimeResize.SBAllowResizeClick(Sender: TObject);
begin
  cyRunTimeResize1.Options.AllowResizeTop := SBAllowResize.Down;
  cyRunTimeResize1.Options.AllowResizeLeft := SBAllowResize.Down;
  cyRunTimeResize1.Options.AllowResizeRight := SBAllowResize.Down;
  cyRunTimeResize1.Options.AllowResizeBottom := SBAllowResize.Down;
end;

procedure TFrmCyRunTimeResize.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
