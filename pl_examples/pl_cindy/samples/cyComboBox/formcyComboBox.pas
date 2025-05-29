unit formcyComboBox;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyBevel, cyBaseSpeedButton, cyBaseCombobox, cyBaseFilterComboBox;

type
(*  TListItemObject = class
  private
    Id: Integer;
  public
    constructor TListItemObject.Create(const withID: string);
end;    *)

  TDynControl_Edit = TEdit;
  TDynControl_ComboBox = TcyFilterCombobox;


  TFrmCyComboBox = class(TForm)
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
    PanSelectCity: TCyPanel;
    Label11: TLabel;
    Label12: TLabel;
    cyCombobox1: TcyCombobox;
    cyCombobox3: TcyCombobox;
    cyFilterCombobox1: TcyFilterCombobox;
    cyFilterCombobox2: TcyFilterCombobox;
    Label7: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    cyFilterCombobox4: TcyFilterCombobox;
    Button1: TButton;
    CBCols: TComboBox;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure cyCombobox3DropDown(Sender: TObject);
    procedure ListBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ListBox1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyComboBox: TFrmCyComboBox;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
 pathMedia = '..\xmedia\';
{$ELSE}
 pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyComboBox.Button1Click(Sender: TObject);
var
  i: Integer;
  acyFilterCombobox: TcyFilterCombobox;
begin
  acyFilterCombobox := cyFilterCombobox1;


  acyFilterCombobox.Items.BeginUpdate;
  acyFilterCombobox.Items.Clear;


  for i := 1 to 1000 do
    acyFilterCombobox.Items.Add('Inserted item nΊ ' + intToStr(i) );

  acyFilterCombobox.Items.EndUpdate;

  if acyFilterCombobox.CanFocus then
    acyFilterCombobox.SetFocus;
end;

procedure TFrmCyComboBox.Button2Click(Sender: TObject);
begin
  CBCols.Items.Add('X' + #1#1#1#1#1#1#1#1#1#1#1#1 + '111');
  CBCols.Items.Add('YYY' + #1#1#1#1#1#1#1#1#1#1#1#1 + '222');

  CBCols.ItemIndex := CBCols.Items.Count - 1;

  if CBCols.Text <> CBCols.Items[CBCols.ItemIndex] then
    Beep;

end;

procedure TFrmCyComboBox.cyCombobox3DropDown(Sender: TObject);
begin
  ListBox1.ItemIndex := cyCombobox3.ItemIndex;
end;

procedure TFrmCyComboBox.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyComboBox.FormCreate(Sender: TObject);
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

procedure TFrmCyComboBox.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute* }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyComboBox.ListBox1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 13 then
  begin
    Key := 0;
    cyCombobox3.ItemIndex := ListBox1.ItemIndex;
    cyCombobox3.GetDropDownControlDefs.Close;
  end;
end;

procedure TFrmCyComboBox.ListBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  cyCombobox3.ItemIndex := ListBox1.ItemIndex;
  cyCombobox3.GetDropDownControlDefs.Close;
end;

procedure TFrmCyComboBox.SBCloseClick(Sender: TObject);
begin
  Close;
end;

end.
