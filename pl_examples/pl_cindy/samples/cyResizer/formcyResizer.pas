unit formcyResizer;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyResizer, cyBaseSpeedButton, Menus, cyBevel;

type
  TFrmCyResizer = class(TForm)
    CyPanel1: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TMemo;
    StatusBar1: TStatusBar;
    SBActivate: TcySpeedButton;
    SBClearSelection: TcySpeedButton;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    cyLabel1: TcyLabel;
    LblWebSite: TcyHotLabel;
    cyResizer1: TcyResizer;
    Panel1: TPanel;
    PanDesigning: TPanel;
    Shape10: TShape;
    Label95: TLabel;
    Panel11: TPanel;
    Panel12: TPanel;
    BitBtn3: TBitBtn;
    Edit6: TEdit;
    BitBtn2: TBitBtn;
    Edit5: TEdit;
    Panel10: TPanel;
    Edit4: TEdit;
    Panel13: TPanel;
    Panel14: TPanel;
    CyPanel3: TCyPanel;
    Shape4: TShape;
    Label94: TLabel;
    BitBtn1: TBitBtn;
    ComboBox1: TComboBox;
    ListBox1: TListBox;
    Panel3: TPanel;
    SBResizerUp: TSpeedButton;
    SBResizerDown: TSpeedButton;
    SBResizerLeft: TSpeedButton;
    SBResizerRight: TSpeedButton;
    Bevel2: TBevel;
    Label96: TLabel;
    CBIgnoreSnapToGrid: TCheckBox;
    CBSnapToGrid: TCheckBox;
    Bevel1: TBevel;
    SBRemoveFromSelection: TcySpeedButton;
    SBAddToSelection: TcySpeedButton;
    RBMoveMode: TRadioButton;
    RadioButton2: TRadioButton;
    GroupBox1: TGroupBox;
    CheckBoxMouseSelect: TCheckBox;
    CheckBoxMouseMove: TCheckBox;
    CheckBoxMouseResize: TCheckBox;
    CheckBoxMouseMultiSelect: TCheckBox;
    CheckBoxMouseMultiMove: TCheckBox;
    CheckBoxMouseMultiResize: TCheckBox;
    GroupBox2: TGroupBox;
    CheckBoxKeySelect: TCheckBox;
    CheckBoxKeyMove: TCheckBox;
    CheckBoxKeyResize: TCheckBox;
    CheckBoxKeyMultiSelect: TCheckBox;
    CheckBoxKeyMultiMove: TCheckBox;
    CheckBoxKeyMultiResize: TCheckBox;
    CheckBoxKeyUnselectAll: TCheckBox;
    CheckBoxOnlyMultiSelectIfSameParent: TCheckBox;
    CheckBoxOnlyMultiMoveIfSameParent: TCheckBox;
    CheckBoxOnlyMultiResizeIfSameParent: TCheckBox;
    CheckBoxGuidelines: TCheckBox;
    PopupMenu1: TPopupMenu;
    aaa1: TMenuItem;
    bbb1: TMenuItem;
    SBAlignLeft: TcySpeedButton;
    SBAlignRight: TcySpeedButton;
    SBAlignVertCenters: TcySpeedButton;
    SBAlignBottom: TcySpeedButton;
    SBAlignTop: TcySpeedButton;
    SBAlignHorzCenters: TcySpeedButton;
    SBAlignToGrid: TcySpeedButton;
    SBMakeSameHeight: TcySpeedButton;
    SBMakeSameWidth: TcySpeedButton;
    cyBevel1: TcyBevel;
    SBSpaceEqualHor: TcySpeedButton;
    SBIncHorzSpace: TcySpeedButton;
    SBMakeSameSize: TcySpeedButton;
    cyBevel2: TcyBevel;
    SBCenterVertically: TcySpeedButton;
    SBCenterHorizontally: TcySpeedButton;
    cyBevel3: TcyBevel;
    SBDecHorzSpace: TcySpeedButton;
    SBRemovHorzSpace: TcySpeedButton;
    SBSpaceEqualVert: TcySpeedButton;
    SBIncVertSpace: TcySpeedButton;
    SBDecVertSpace: TcySpeedButton;
    SBRemovVertSpace: TcySpeedButton;
    CheckBoxMouseUnselectAll: TCheckBox;
    Image1: TImage;
    CheckBoxDrawGrid: TCheckBox;
    CheckBoxDrawControlsFrame: TCheckBox;
    CheckBoxDrawSelectedControlsFrame: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure SBActivateClick(Sender: TObject);
    procedure CBSnapToGridClick(Sender: TObject);
    procedure SBAddToSelectionClick(Sender: TObject);
    procedure SBRemoveFromSelectionClick(Sender: TObject);
    procedure SBClearSelectionClick(Sender: TObject);
    procedure SBResizerUpClick(Sender: TObject);
    procedure SBResizerDownClick(Sender: TObject);
    procedure SBResizerLeftClick(Sender: TObject);
    procedure SBResizerRightClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure CheckBoxMouseSelectClick(Sender: TObject);
    procedure CheckBoxMouseMoveClick(Sender: TObject);
    procedure CheckBoxMouseResizeClick(Sender: TObject);
    procedure CheckBoxMouseMultiSelectClick(Sender: TObject);
    procedure CheckBoxMouseMultiMoveClick(Sender: TObject);
    procedure CheckBoxMouseMultiResizeClick(Sender: TObject);
    procedure CheckBoxKeySelectClick(Sender: TObject);
    procedure CheckBoxKeyMoveClick(Sender: TObject);
    procedure CheckBoxKeyResizeClick(Sender: TObject);
    procedure CheckBoxKeyMultiSelectClick(Sender: TObject);
    procedure CheckBoxKeyMultiMoveClick(Sender: TObject);
    procedure CheckBoxKeyMultiResizeClick(Sender: TObject);
    procedure CheckBoxKeyUnselectAllClick(Sender: TObject);
    procedure CheckBoxGuidelinesClick(Sender: TObject);
    procedure CheckBoxOnlyMultiSelectIfSameParentClick(Sender: TObject);
    procedure CheckBoxOnlyMultiMoveIfSameParentClick(Sender: TObject);
    procedure CheckBoxOnlyMultiResizeIfSameParentClick(Sender: TObject);
    procedure SBAlignLeftClick(Sender: TObject);
    procedure SBAlignRightClick(Sender: TObject);
    procedure SBAlignVertCentersClick(Sender: TObject);
    procedure SBAlignTopClick(Sender: TObject);
    procedure SBAlignBottomClick(Sender: TObject);
    procedure SBAlignHorzCentersClick(Sender: TObject);
    procedure SBAlignToGridClick(Sender: TObject);
    procedure SBMakeSameWidthClick(Sender: TObject);
    procedure SBMakeSameHeightClick(Sender: TObject);
    procedure SBMakeSameSizeClick(Sender: TObject);
    procedure SBCenterVerticallyClick(Sender: TObject);
    procedure SBCenterHorizontallyClick(Sender: TObject);
    procedure SBSpaceEqualHorClick(Sender: TObject);
    procedure SBIncHorzSpaceClick(Sender: TObject);
    procedure SBDecHorzSpaceClick(Sender: TObject);
    procedure SBRemovHorzSpaceClick(Sender: TObject);
    procedure SBSpaceEqualVertClick(Sender: TObject);
    procedure SBIncVertSpaceClick(Sender: TObject);
    procedure SBDecVertSpaceClick(Sender: TObject);
    procedure SBRemovVertSpaceClick(Sender: TObject);
    procedure CheckBoxMouseUnselectAllClick(Sender: TObject);
    procedure CheckBoxDrawGridClick(Sender: TObject);
    procedure CheckBoxDrawControlsFrameClick(Sender: TObject);
    procedure CheckBoxDrawSelectedControlsFrameClick(Sender: TObject);
  private
    { Private declarations }
    procedure ChangeOption(AddOption: Boolean; Option: TResizerOption);
  public
    { Public declarations }
  end;

var
  FrmCyResizer: TFrmCyResizer;

implementation

{$R *.lfm}
const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}

procedure TFrmCyResizer.ChangeOption(AddOption: Boolean; Option: TResizerOption);
begin
  if AddOption
  then cyResizer1.Options := cyResizer1.Options + [Option]
  else cyResizer1.Options := cyResizer1.Options - [Option];
end;

procedure TFrmCyResizer.BitBtn1Click(Sender: TObject);
begin
  ShowMessage('clicked!');
end;

procedure TFrmCyResizer.CBSnapToGridClick(Sender: TObject);
begin
  cyResizer1.Grid.SnapToGrid := CBSnapToGrid.Checked;
end;

procedure TFrmCyResizer.CheckBoxKeyMultiMoveClick(Sender: TObject);
begin
  ChangeOption(CheckBoxKeyMultiMove.Checked, roKeyMultiMove);
end;

procedure TFrmCyResizer.CheckBoxDrawControlsFrameClick(Sender: TObject);
begin
  cyResizer1.ControlsFrame.Visible := CheckBoxDrawControlsFrame.Checked;
end;

procedure TFrmCyResizer.CheckBoxDrawGridClick(Sender: TObject);
begin
  cyResizer1.Grid.Visible := CheckBoxDrawGrid.Checked;
end;

procedure TFrmCyResizer.CheckBoxDrawSelectedControlsFrameClick(Sender: TObject);
begin
  cyResizer1.HandlingSingleControl.Frame.Visible := CheckBoxDrawSelectedControlsFrame.Checked;
  cyResizer1.HandlingMultipleControls.Frame.Visible := CheckBoxDrawSelectedControlsFrame.Checked;
end;

procedure TFrmCyResizer.CheckBoxGuidelinesClick(Sender: TObject);
begin
  if CheckBoxGuidelines.Checked
  then cyResizer1.GuideLines.Controls := gmAllSelected
  else cyResizer1.GuideLines.Controls := gmNone;
end;

procedure TFrmCyResizer.CheckBoxKeyMoveClick(Sender: TObject);
begin
  ChangeOption(CheckBoxKeyMove.Checked, roKeyMove);
end;

procedure TFrmCyResizer.CheckBoxKeyMultiResizeClick(Sender: TObject);
begin
  ChangeOption(CheckBoxKeyMultiResize.Checked, roKeyMultiResize);
end;

procedure TFrmCyResizer.CheckBoxKeyMultiSelectClick(Sender: TObject);
begin
  ChangeOption(CheckBoxKeyMultiSelect.Checked, roKeyMultiSelect);
end;

procedure TFrmCyResizer.CheckBoxKeyResizeClick(Sender: TObject);
begin
  ChangeOption(CheckBoxKeyResize.Checked, roKeyResize);
end;

procedure TFrmCyResizer.CheckBoxKeySelectClick(Sender: TObject);
begin
  ChangeOption(CheckBoxKeySelect.Checked, roKeySelect);
end;

procedure TFrmCyResizer.CheckBoxKeyUnselectAllClick(Sender: TObject);
begin
  ChangeOption(CheckBoxKeyUnselectAll.Checked, roKeyUnselectAll);
end;

procedure TFrmCyResizer.CheckBoxMouseMoveClick(Sender: TObject);
begin
  ChangeOption(CheckBoxMouseMove.Checked, roMouseMove);
end;

procedure TFrmCyResizer.CheckBoxMouseMultiMoveClick(Sender: TObject);
begin
  ChangeOption(CheckBoxMouseMultiMove.Checked, roMouseMultiMove);
end;

procedure TFrmCyResizer.CheckBoxMouseMultiResizeClick(Sender: TObject);
begin
  ChangeOption(CheckBoxMouseMultiResize.Checked, roMouseMultiResize);
end;

procedure TFrmCyResizer.CheckBoxMouseMultiSelectClick(Sender: TObject);
begin
  ChangeOption(CheckBoxMouseMultiSelect.Checked, roMouseMultiSelect);
end;

procedure TFrmCyResizer.CheckBoxMouseResizeClick(Sender: TObject);
begin
  ChangeOption(CheckBoxMouseResize.Checked, roMouseResize);
end;

procedure TFrmCyResizer.CheckBoxMouseSelectClick(Sender: TObject);
begin
  ChangeOption(CheckBoxMouseSelect.Checked, roMouseSelect);
end;

procedure TFrmCyResizer.CheckBoxMouseUnselectAllClick(Sender: TObject);
begin
  ChangeOption(CheckBoxMouseUnselectAll.Checked, roMouseUnselectAll);
end;

procedure TFrmCyResizer.CheckBoxOnlyMultiMoveIfSameParentClick(Sender: TObject);
begin
  ChangeOption(CheckBoxOnlyMultiMoveIfSameParent.Checked, roOnlyMultiMoveIfSameParent);
end;

procedure TFrmCyResizer.CheckBoxOnlyMultiResizeIfSameParentClick(Sender: TObject);
begin
  ChangeOption(CheckBoxOnlyMultiResizeIfSameParent.Checked, roOnlyMultiResizeIfSameParent);
end;

procedure TFrmCyResizer.CheckBoxOnlyMultiSelectIfSameParentClick(Sender: TObject);
begin
  ChangeOption(CheckBoxOnlyMultiSelectIfSameParent.Checked, roOnlyMultiSelectIfSameParent);
end;

procedure TFrmCyResizer.SBSpaceEqualHorClick(Sender: TObject);
begin
  cyResizer1.SpaceSelectionEquallyHorizontally(0, false);
end;

procedure TFrmCyResizer.SBSpaceEqualVertClick(Sender: TObject);
begin
  cyResizer1.SpaceSelectionEquallyVertically(0, false);
end;

procedure TFrmCyResizer.SBRemovHorzSpaceClick(Sender: TObject);
begin
  cyResizer1.SpaceSelectionEquallyHorizontally(0, true);
end;

procedure TFrmCyResizer.SBRemovVertSpaceClick(Sender: TObject);
begin
  cyResizer1.SpaceSelectionEquallyVertically(0, true);
end;

procedure TFrmCyResizer.SBMakeSameHeightClick(Sender: TObject);
begin
  cyResizer1.MakeSelectionSameSize(false, true);
end;

procedure TFrmCyResizer.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyResizer.FormCreate(Sender: TObject);
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

procedure TFrmCyResizer.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyResizer.SBActivateClick(Sender: TObject);
begin
  if not cyResizer1.Active
  then begin
    cyResizer1.Activate(PanDesigning);
    SBActivate.Caption := 'Deactivate designing mode';
  end
  else begin
    cyResizer1.Deactivate;
    SBActivate.Caption := 'Activate designing mode';
  end;
end;

procedure TFrmCyResizer.SBAddToSelectionClick(Sender: TObject);
begin
  cyResizer1.HandlingControlList.InsertControl(CyPanel3);
end;

procedure TFrmCyResizer.SBAlignBottomClick(Sender: TObject);
begin
  cyResizer1.AlignSelection(acBottomEdges, true);
end;

procedure TFrmCyResizer.SBAlignHorzCentersClick(Sender: TObject);
begin
  cyResizer1.AlignSelection(acHorizontalsCenters, false);
end;

procedure TFrmCyResizer.SBAlignLeftClick(Sender: TObject);
begin
  cyResizer1.AlignSelection(acLeftEdges, true);
end;

procedure TFrmCyResizer.SBAlignRightClick(Sender: TObject);
begin
  cyResizer1.AlignSelection(acRightEdges, true);
end;

procedure TFrmCyResizer.SBAlignToGridClick(Sender: TObject);
begin
  cyResizer1.AlignSelectionToGrid;
end;

procedure TFrmCyResizer.SBAlignTopClick(Sender: TObject);
begin
  cyResizer1.AlignSelection(acTopEdges, true);
end;

procedure TFrmCyResizer.SBAlignVertCentersClick(Sender: TObject);
begin
  cyResizer1.AlignSelection(acVerticalCenters, true);
end;

procedure TFrmCyResizer.SBCenterHorizontallyClick(Sender: TObject);
begin
  cyResizer1.CenterSelection(true, false);
end;

procedure TFrmCyResizer.SBCenterVerticallyClick(Sender: TObject);
begin
  cyResizer1.CenterSelection(false, true);
end;

procedure TFrmCyResizer.SBClearSelectionClick(Sender: TObject);
begin
  cyResizer1.HandlingControlList.Clear;
end;

procedure TFrmCyResizer.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCyResizer.SBDecHorzSpaceClick(Sender: TObject);
begin
  cyResizer1.SpaceSelectionEquallyHorizontally(-5, false);
end;

procedure TFrmCyResizer.SBDecVertSpaceClick(Sender: TObject);
begin
  cyResizer1.SpaceSelectionEquallyVertically(-5, false);
end;

procedure TFrmCyResizer.SBIncHorzSpaceClick(Sender: TObject);
begin
  cyResizer1.SpaceSelectionEquallyHorizontally(5, false);
end;

procedure TFrmCyResizer.SBIncVertSpaceClick(Sender: TObject);
begin
  cyResizer1.SpaceSelectionEquallyVertically(5, false);
end;

procedure TFrmCyResizer.SBMakeSameSizeClick(Sender: TObject);
begin
  cyResizer1.MakeSelectionSameSize(true, true);
end;

procedure TFrmCyResizer.SBMakeSameWidthClick(Sender: TObject);
begin
  cyResizer1.MakeSelectionSameSize(true, false);
end;

procedure TFrmCyResizer.SBRemoveFromSelectionClick(Sender: TObject);
begin
  cyResizer1.HandlingControlList.RemoveControl(CyPanel3);
end;

procedure TFrmCyResizer.SBResizerDownClick(Sender: TObject);
begin
  if RBMoveMode.Checked
  then cyResizer1.MoveSelection(0, 10, CBIgnoreSnapToGrid.Checked)
  else cyResizer1.ResizeSelection([reBottom], 0, 10, CBIgnoreSnapToGrid.Checked);
end;

procedure TFrmCyResizer.SBResizerLeftClick(Sender: TObject);
begin
  if RBMoveMode.Checked
  then cyResizer1.MoveSelection(-10, 0, CBIgnoreSnapToGrid.Checked)
  else cyResizer1.ResizeSelection([reLeft], -10, 0, CBIgnoreSnapToGrid.Checked);
end;

procedure TFrmCyResizer.SBResizerRightClick(Sender: TObject);
begin
  if RBMoveMode.Checked
  then cyResizer1.MoveSelection(10, 0, CBIgnoreSnapToGrid.Checked)
  else cyResizer1.ResizeSelection([reRight], 10, 0, CBIgnoreSnapToGrid.Checked);
end;

procedure TFrmCyResizer.SBResizerUpClick(Sender: TObject);
begin
  if RBMoveMode.Checked
  then cyResizer1.MoveSelection(0, -10, CBIgnoreSnapToGrid.Checked)
  else cyResizer1.ResizeSelection([reTop], 0, -10, CBIgnoreSnapToGrid.Checked);
end;

end.
