unit formcyEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons,
  cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, ShellAPI, cyBaseSpeedButton, cyEdit, cyEditInteger, cyEditFloat, cyBaseButton,
  cyBitBtn, Spin, cyEditTime, cyEditDate;

type
  TFrmCyEdit = class(TForm)
    CyPanel1: TCyPanel;
    Panel2: TPanel;
    CyPanel2: TCyPanel;
    RichEditInfo: TRichEdit;
    StatusBar1: TStatusBar;
    SBClose: TcySpeedButton;
    cySplitter1: TcySplitter;
    cyLabel1: TcyLabel;
    LblWebSite: TcyHotLabel;
    Panel1: TPanel;
    SBIgnoreRules: TcySpeedButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    EDitDate: TcyEdit;
    EditPostalCode: TcyEdit;
    EditNegNumber: TcyEdit;
    EditNegFloat: TcyEdit;
    EditTime: TcyEdit;
    Button1: TButton;
    EditBinary: TcyEdit;
    EditHexa: TcyEdit;
    Button2: TButton;
    MemLog: TMemo;
    EditPhone: TcyEdit;
    EditIP: TcyEdit;
    cyEdit1: TcyEdit;
    cyEdit2: TcyEdit;
    TabSheet2: TTabSheet;
    cyEditInteger1: TcyEditInteger;
    BtnGetInteger: TButton;
    EditRsltInteger: TEdit;
    cyEditFloat1: TcyEditFloat;
    BtnGetFloat: TButton;
    EditRsltFloat: TEdit;
    Label11: TLabel;
    Label14: TLabel;
    CBIntegerAllowEmpty: TCheckBox;
    CBIntegerAllowNegative: TCheckBox;
    GroupBox1: TGroupBox;
    CBErrorHandlingBeep: TCheckBox;
    CBErrorHandlingKeepFocus: TCheckBox;
    CBErrorHandlingClearText: TCheckBox;
    CBErrorHandlingRaiseMsgError: TCheckBox;
    CBFloatAllowEmpty: TCheckBox;
    CBFloatAllowNegative: TCheckBox;
    Label15: TLabel;
    Label16: TLabel;
    EIntegerMinValue: TcyEditInteger;
    EIntegerMaxValue: TcyEditInteger;
    Label17: TLabel;
    Label18: TLabel;
    EFloatMinValue: TcyEditFloat;
    EFloatMaxValue: TcyEditFloat;
    Label19: TLabel;
    EMsgInvalidValue: TEdit;
    Label20: TLabel;
    EMsgOutOfBoundValue: TEdit;
    LblEditIntegerInfo: TLabel;
    Label21: TLabel;
    cyEditTime1: TcyEditTime;
    CBTimeAllowEmpty: TCheckBox;
    CBTimeWithSeconds: TCheckBox;
    BtnGetTime: TButton;
    EditRsltTime: TEdit;
    cyEditDate1: TcyEditDate;
    Label22: TLabel;
    BtnGetDate: TButton;
    EditRsltDate: TEdit;
    CBDateAllowEmpty: TCheckBox;
    CBTimeAutoComplete: TCheckBox;
    CBDateAutoComplete: TCheckBox;
    Label23: TLabel;
    CBDateFormat: TComboBox;
    Label24: TLabel;
    Label25: TLabel;
    ETimeHourPrefix: TEdit;
    Label26: TLabel;
    ETimeMinutePrefix: TEdit;
    Label27: TLabel;
    ETimeSecondsPrefix: TEdit;
    Label28: TLabel;
    Label29: TLabel;
    EDateSeparator: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SBIgnoreRulesClick(Sender: TObject);
    procedure EDitDateChange(Sender: TObject);
    procedure BtnGetIntegerClick(Sender: TObject);
    procedure BtnGetFloatClick(Sender: TObject);
    procedure CBErrorHandlingBeepClick(Sender: TObject);
    procedure CBIntegerAllowEmptyClick(Sender: TObject);
    procedure CBIntegerAllowNegativeClick(Sender: TObject);
    procedure CBFloatAllowEmptyClick(Sender: TObject);
    procedure CBFloatAllowNegativeClick(Sender: TObject);
    procedure EIntegerMinValueChange(Sender: TObject);
    procedure EIntegerMaxValueChange(Sender: TObject);
    procedure EFloatMinValueChange(Sender: TObject);
    procedure EFloatMaxValueChange(Sender: TObject);
    procedure cyEditInteger1ValidateError(Sender: TObject; var Handled: Boolean);
    procedure cyEditInteger1Change(Sender: TObject);
    procedure CBTimeAllowEmptyClick(Sender: TObject);
    procedure CBTimeWithSecondsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnGetTimeClick(Sender: TObject);
    procedure CBDateAllowEmptyClick(Sender: TObject);
    procedure BtnGetDateClick(Sender: TObject);
    procedure CBTimeAutoCompleteClick(Sender: TObject);
    procedure CBDateAutoCompleteClick(Sender: TObject);
    procedure ETimeHourPrefixChange(Sender: TObject);
    procedure EDateSeparatorChange(Sender: TObject);
    procedure CBDateFormatClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyEdit: TFrmCyEdit;

implementation

{$R *.dfm}

procedure TFrmCyEdit.BtnGetDateClick(Sender: TObject);
begin
  if cyEditDate1.IsValidValue
  then EditRsltDate.Text := DateToStr(cyEditDate1.Value)
  else EditRsltDate.Text := 'Invalid value!';
end;

procedure TFrmCyEdit.BtnGetFloatClick(Sender: TObject);
begin
  if cyEditFloat1.IsValidValue
  then EditRsltFloat.Text := FloatToStr(cyEditFloat1.Value)
  else EditRsltFloat.Text := 'Invalid value!';
end;

procedure TFrmCyEdit.BtnGetIntegerClick(Sender: TObject);
begin
  if cyEditInteger1.IsValidValue
  then EditRsltInteger.Text := intToStr(cyEditInteger1.Value)
  else EditRsltInteger.Text := 'Invalid value!';
end;

procedure TFrmCyEdit.BtnGetTimeClick(Sender: TObject);
begin
  if cyEditTime1.IsValidValue
  then EditRsltTime.Text := TimeToStr(cyEditTime1.Value)
  else EditRsltTime.Text := 'Invalid value!';
end;

procedure TFrmCyEdit.Button1Click(Sender: TObject);
begin
  EDitDate.Text := '15/01/1977';
end;

procedure TFrmCyEdit.Button2Click(Sender: TObject);
begin
  EditPostalCode.IgnoreRules := true;
  EditPostalCode.Text := 'Hello World!';
  EditPostalCode.IgnoreRules := SBIgnoreRules.Down;
end;

procedure TFrmCyEdit.cyEditInteger1Change(Sender: TObject);
begin
  LblEditIntegerInfo.Caption := '';
end;

procedure TFrmCyEdit.cyEditInteger1ValidateError(Sender: TObject; var Handled: Boolean);
begin
  case cyEditInteger1.ValidateText(cyEditInteger1.Text) of
    evInvalidValue: LblEditIntegerInfo.Caption := 'The value is not an integer value.';
    evOutOfMinRange:  LblEditIntegerInfo.Caption := 'The value is out of bound.';
    evOutOfMaxRange:  LblEditIntegerInfo.Caption := 'The value is out of bound.';
  end;
end;

procedure TFrmCyEdit.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyEdit.EDitDateChange(Sender: TObject);
begin
  with TcyEdit(Sender) Do
  begin
    MemLog.Lines.Insert(0, Name + '.Text = ' + Text);
  end;
end;

procedure TFrmCyEdit.FormCreate(Sender: TObject);
var
  L: Integer;
  cyCompName: String;
  RtfFile: TFilename;
begin
  L := Length(Name);
  cyCompName := Copy(Name, 4, L-3);
  Caption := cyCompName + ' demo';
  RtfFile := ExtractFileDir(ParamStr(0)) + '\' + cyCompName + '.rtf';

  try
    RichEditInfo.Lines.LoadFromFile(RtfFile);
  finally

  end;
end;

procedure TFrmCyEdit.FormShow(Sender: TObject);
begin
  cyEditTime1.Value := Now;
  cyEditDate1.Value := Date;
  PageControl1.ActivePageIndex := 0;
end;

procedure TFrmCyEdit.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  ShellExecute(handle, 'open', 'http://sourceforge.net/projects/tcycomponents/', '', '', SW_NORMAL);
  Screen.Cursor := crDefault;
end;

procedure TFrmCyEdit.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCyEdit.SBIgnoreRulesClick(Sender: TObject);
var i: Integer;
begin
  for i := 0 to Panel1.ControlCount-1 do
    if Panel1.Controls[i] is TcyEdit then
      TcyEdit(Panel1.Controls[i]).IgnoreRules := SBIgnoreRules.Down;
end;

procedure TFrmCyEdit.CBDateAllowEmptyClick(Sender: TObject);
begin
  cyEditDate1.AllowEmpty := CBDateAllowEmpty.Checked;
end;

procedure TFrmCyEdit.CBDateAutoCompleteClick(Sender: TObject);
begin
  cyEditDate1.AutoComplete := CBDateAutoComplete.Checked;
end;

procedure TFrmCyEdit.CBDateFormatClick(Sender: TObject);
begin
  cyEditDate1.DateArrangement := TEditDateArrangement( CBDateFormat.ItemIndex );
end;

procedure TFrmCyEdit.CBErrorHandlingBeepClick(Sender: TObject);

      function GetErrorHandlingOptions: TEditErrorOptions;
      begin
        Result := [];
        if CBErrorHandlingBeep.Checked then
          include(Result, eoBeepSound);
        if CBErrorHandlingKeepFocus.Checked then
          include(Result, eoKeepFocus);
        if CBErrorHandlingClearText.Checked then
          include(Result, eoClearText);
        if CBErrorHandlingRaiseMsgError.Checked then
          include(Result, eoRaiseMsgError);
      end;

begin
  cyEditInteger1.ErrorHandling.Options := GetErrorHandlingOptions;
  cyEditInteger1.ErrorHandling.MsgInvalidValue := EMsgInvalidValue.Text;
  cyEditInteger1.ErrorHandling.MsgOutOfMinRange := EMsgOutOfBoundValue.Text;
  cyEditInteger1.ErrorHandling.MsgOutOfMaxRange := EMsgOutOfBoundValue.Text;

  cyEditFloat1.ErrorHandling.Options := GetErrorHandlingOptions;
  cyEditFloat1.ErrorHandling.MsgInvalidValue := EMsgInvalidValue.Text;
  cyEditFloat1.ErrorHandling.MsgOutOfMinRange := EMsgOutOfBoundValue.Text;
  cyEditFloat1.ErrorHandling.MsgOutOfMaxRange := EMsgOutOfBoundValue.Text;

  cyEditTime1.ErrorHandling.Options := GetErrorHandlingOptions;
  cyEditTime1.ErrorHandling.MsgInvalidValue := EMsgInvalidValue.Text;
  cyEditTime1.ErrorHandling.MsgOutOfMinRange := EMsgOutOfBoundValue.Text;
  cyEditTime1.ErrorHandling.MsgOutOfMaxRange := EMsgOutOfBoundValue.Text;
end;

procedure TFrmCyEdit.CBFloatAllowEmptyClick(Sender: TObject);
begin
  cyEditFloat1.AllowEmpty := CBFloatAllowEmpty.Checked;
end;

procedure TFrmCyEdit.CBFloatAllowNegativeClick(Sender: TObject);
begin
  cyEditFloat1.AllowNegative := CBFloatAllowNegative.Checked;
end;

procedure TFrmCyEdit.CBIntegerAllowEmptyClick(Sender: TObject);
begin
  cyEditInteger1.AllowEmpty := CBIntegerAllowEmpty.Checked;
end;

procedure TFrmCyEdit.CBIntegerAllowNegativeClick(Sender: TObject);
begin
  cyEditInteger1.AllowNegative := CBIntegerAllowNegative.Checked;
end;

procedure TFrmCyEdit.CBTimeAllowEmptyClick(Sender: TObject);
begin
  cyEditTime1.AllowEmpty := CBTimeAllowEmpty.Checked;
end;

procedure TFrmCyEdit.CBTimeAutoCompleteClick(Sender: TObject);
begin
  cyEditTime1.AutoComplete := CBTimeAutoComplete.Checked;
end;

procedure TFrmCyEdit.CBTimeWithSecondsClick(Sender: TObject);
begin
  cyEditTime1.Seconds := CBTimeWithSeconds.Checked;
end;

procedure TFrmCyEdit.EIntegerMinValueChange(Sender: TObject);
begin
  cyEditInteger1.MinValue := EIntegerMinValue.Value;
end;

procedure TFrmCyEdit.EIntegerMaxValueChange(Sender: TObject);
begin
  cyEditInteger1.MaxValue := EIntegerMaxValue.Value;
end;

procedure TFrmCyEdit.EFloatMaxValueChange(Sender: TObject);
begin
  cyEditFloat1.MaxValue := EFloatMaxValue.Value;
end;

procedure TFrmCyEdit.EFloatMinValueChange(Sender: TObject);
begin
  cyEditFloat1.MinValue := EFloatMinValue.Value;
end;

procedure TFrmCyEdit.EDateSeparatorChange(Sender: TObject);
begin
  cyEditDate1.CustomDateSeparator := EDateSeparator.Text;
end;

procedure TFrmCyEdit.ETimeHourPrefixChange(Sender: TObject);
begin
  cyEditTime1.CustomHoursSymbol   := ETimeHourPrefix.Text;
  cyEditTime1.CustomMinutesSymbol := ETimeMinutePrefix.Text;
  cyEditTime1.CustomSecondsSymbol := ETimeSecondsPrefix.Text;
end;

end.
