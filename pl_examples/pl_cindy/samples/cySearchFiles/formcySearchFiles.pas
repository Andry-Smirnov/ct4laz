unit formcySearchFiles;

{$MODE Delphi}

interface

uses
  Windows,  ShellAPI,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons, EditBtn,
  Calendar, cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel, cyLabel,  LCLIntf,
   cySearchFiles, cyBaseSpeedButton;

type

  { TFrmCySearchFiles }

  TFrmCySearchFiles = class(TForm)
    EScanPath: TDirectoryEdit;
    DTPModifiedDate: TCalendar;
    CyPanel1: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    SBExecute: TSpeedButton;
    SBPause: TSpeedButton;
    SBClose: TSpeedButton;
    SBResume: TSpeedButton;
    SBAbort: TSpeedButton;
    cySplitter1: TcySplitter;
    Panel1: TPanel;
    Label63: TLabel;
    Label83: TLabel;
    Label84: TLabel;
    Label85: TLabel;
    MemScanIncMask: TMemo;
    MemScanExMask: TMemo;
    CBScanArchive: TCheckBox;
    CBScanHidden: TCheckBox;
    CBScanReadOnly: TCheckBox;
    CBScanSystem: TCheckBox;
    CBScanSubDirs: TCheckBox;
    CBScanTemporary: TCheckBox;
    GroupBox1: TGroupBox;
    Label88: TLabel;
    LblCurrentFile: TLabel;
    LBScanFilesMatch: TListBox;
   // SBAbort: TcySpeedButton;
   // SBResume: TcySpeedButton;
    cySearchFiles1: TcySearchFiles;
    CyPanel3: TCyPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure SBExecuteClick(Sender: TObject);
    procedure SBPauseClick(Sender: TObject);
    procedure SBResumeClick(Sender: TObject);
    procedure SBAbortClick(Sender: TObject);
    procedure cySearchFiles1Pause(Sender: TObject);
    procedure cySearchFiles1Resume(Sender: TObject);
    procedure cySearchFiles1Terminate(Sender: TObject);
    procedure cySearchFiles1ValidateFile(Sender: TObject; ValidMaskInclude,
      ValidMaskExclude, ValidAttributes: Boolean; var Accept: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCySearchFiles: TFrmCySearchFiles;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCySearchFiles.SBPauseClick(Sender: TObject);
begin
  cySearchFiles1.Pause;
end;

procedure TFrmCySearchFiles.SBResumeClick(Sender: TObject);
begin
  cySearchFiles1.Resume;
end;

procedure TFrmCySearchFiles.cySearchFiles1Pause(Sender: TObject);
begin
  SBResume.Enabled := true;
  SBPause.Enabled := false;
end;

procedure TFrmCySearchFiles.cySearchFiles1Resume(Sender: TObject);
begin
  SBResume.Enabled := false;
  SBPause.Enabled := true;
end;

procedure TFrmCySearchFiles.cySearchFiles1Terminate(Sender: TObject);
begin
  SBPause.Enabled := false;
  SBResume.Enabled := false;
  SBAbort.Enabled := false;
  SBExecute.Enabled := true;
  Screen.Cursor := crDefault;

  if cySearchFiles1.Aborted
  then LblCurrentFile.Caption := 'Aborted.  '
  else LblCurrentFile.Caption := 'Terminated.  ';

  LblCurrentFile.Caption := LblCurrentFile.Caption + IntToStr(cySearchFiles1.MatchedFiles) + ' files matched!';
end;

procedure TFrmCySearchFiles.cySearchFiles1ValidateFile(Sender: TObject;
  ValidMaskInclude, ValidMaskExclude, ValidAttributes: Boolean;
  var Accept: Boolean);

    function FileTimeToDate(_FT: TFileTime): TDateTime;
    var _ST: SYSTEMTIME;
    begin
      FileTimeToSystemTime(_FT, _ST);
      RESULT := SystemTimeToDateTime(_ST);
    end;

begin
  if Accept
  then
    if FileTimeToDate(cySearchFiles1.ActiveSearchRec.SearchRec.FindData.ftLastWriteTime) > DTPModifiedDate.DateTime
    then begin
      LblCurrentFile.Caption := cySearchFiles1.CurrentDirectory + cySearchFiles1.CurrentFileName;
      LBScanFilesMatch.Items.Add(LblCurrentFile.Caption);
      LBScanFilesMatch.ItemIndex := LBScanFilesMatch.Items.Count-1;
    end
    else Accept := false;

  // Let user pause/resume and abort :
  Application.ProcessMessages;
end;

procedure TFrmCySearchFiles.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCySearchFiles.FormCreate(Sender: TObject);
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

procedure TFrmCySearchFiles.FormShow(Sender: TObject);
begin
  EScanPath.Text:=ExtractFileDir(ParamStr(0));
end;

procedure TFrmCySearchFiles.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCySearchFiles.SBAbortClick(Sender: TObject);
begin
  cySearchFiles1.Abort;
end;

procedure TFrmCySearchFiles.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCySearchFiles.SBExecuteClick(Sender: TObject);

    function CheckBoxStateToAttrMode(aCheckBox: TCheckBox): TcyFileAttributeMode;
    begin
      case aCheckBox.State of
        cbGrayed: RESULT := faBoth;
        cbChecked: RESULT := faYes;
        cbUnchecked: RESULT := faNo;
      end;
    end;

begin
  LBScanFilesMatch.Items.Clear;
  cySearchFiles1.FromPath := EScanPath.Text;
  cySearchFiles1.SubDirectories := CBScanSubDirs.Checked;
  cySearchFiles1.MaskInclude.Text := MemScanIncMask.Text;
  cySearchFiles1.MaskExclude.Text := MemScanExMask.Text;
  cySearchFiles1.FileAttributes.Archive   := CheckBoxStateToAttrMode(CBScanArchive);
  cySearchFiles1.FileAttributes.Hidden    := CheckBoxStateToAttrMode(CBScanHidden);
  cySearchFiles1.FileAttributes.ReadOnly  := CheckBoxStateToAttrMode(CBScanReadOnly);
  cySearchFiles1.FileAttributes.System    := CheckBoxStateToAttrMode(CBScanSystem);
  {$IFDEF VER200}
  cySearchFiles1.FileAttributes.Temporary := CheckBoxStateToAttrMode(CBScanTemporary);
  {$ENDIF}

  Screen.Cursor := crHourGlass;
  SBPause.Enabled := true;
  SBAbort.Enabled := true;
  SBResume.Enabled := false;
  SBExecute.Enabled := false;

  if not cySearchFiles1.Execute
  then cySearchFiles1Terminate(cySearchFiles1);
end;

end.
