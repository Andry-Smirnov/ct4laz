unit formcyIniForm;

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyIniForm, IniFiles, cyBaseSpeedButton;

type
  TFrmCyIniForm = class(TForm)
    CyPanel1: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    StatusBar1: TStatusBar;
    SBLoad: TcySpeedButton;
    SBSave: TcySpeedButton;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    cyLabel1: TcyLabel;
    LblWebSite: TcyHotLabel;
    Panel1: TPanel;
    cyIniForm1: TcyIniForm;
    Label1: TLabel;
    ECustom: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure SBLoadClick(Sender: TObject);
    procedure SBSaveClick(Sender: TObject);
    procedure cyIniForm1CustomSaveToFile(Sender: TObject;
      IniFile: TIniFile);
    procedure cyIniForm1CustomLoadFromFile(Sender: TObject;
      IniFile: TIniFile; FileVersion: string);
  private

  public

  end;

var
  FrmCyIniForm: TFrmCyIniForm;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ENDIF}
{$IFDEF UNIX}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyIniForm.cyIniForm1CustomLoadFromFile(Sender: TObject; IniFile: TIniFile; FileVersion: string);
begin
  ECustom.Text := IniFile.ReadString('MY CUSTOM SECTION', 'MY_CUSTOM_STRING1', '');
end;

procedure TFrmCyIniForm.cyIniForm1CustomSaveToFile(Sender: TObject; IniFile: TIniFile);
begin
  IniFile.WriteString('MY CUSTOM SECTION', 'MY_CUSTOM_STRING1', ECustom.Text);
end;

procedure TFrmCyIniForm.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyIniForm.FormCreate(Sender: TObject);
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

procedure TFrmCyIniForm.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyIniForm.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCyIniForm.SBLoadClick(Sender: TObject);
begin
  cyIniForm1.LoadDefinitions;
end;

procedure TFrmCyIniForm.SBSaveClick(Sender: TObject);
begin
  cyIniForm1.SaveDefinitions;
end;

end.
