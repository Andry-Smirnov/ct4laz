unit f_navPanel;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, cyBasePanel, cyPanel, cyAdvPanel, cyNavPanel, StdCtrls, Buttons,
  cyBaseSpeedButton, cySpeedButton, cyBaseLabel, cyHotLabel,
  cyPaintBox, cyLabel{, GIFImg};

type

  { TFrmNavPanel }

  TFrmNavPanel = class(TForm)
    cyNavPanel1: TcyNavPanel;
    SBMainOption: TcySpeedButton;
    SBHotmail: TcySpeedButton;
    SByahoo: TcySpeedButton;
    SBGMail: TcySpeedButton;
    PanDelphiXE5: TCyPanel;
    Image1: TImage;
    cyHotLabel1: TcyHotLabel;
    Label3: TLabel;
    PanNav: TPanel;
    Image3: TImage;
    PanTools: TPanel;
    Image4: TImage;
    cySpeedButton34: TcySpeedButton;
    ImgNext: TImage;
    ImgFirst: TImage;
    ImgPrior: TImage;
    ImgLast: TImage;
    PanSourceForge: TPanel;
    Image9: TImage;
    cyHotLabel3: TcyHotLabel;
    LblEmbarcadero: TLabel;
    LblCindyComponents: TLabel;
    LblDownloads: TLabel;
    LblAbout: TLabel;
    SBItem1: TcySpeedButton;
    SBItem5: TcySpeedButton;
    SBItem3: TcySpeedButton;
    SBItem4: TcySpeedButton;
    SBItem2: TcySpeedButton;
    Shape1: TShape;
    Label1: TcyLabel;
    Label2: TLabel;
    Label6: TcyLabel;
    ImgClose: TImage;
    SpeedButton1: TSpeedButton;
    Shape3: TShape;
    cySpeedButton39: TcySpeedButton;
    cySpeedButton40: TcySpeedButton;
    cySpeedButton41: TcySpeedButton;
    cySpeedButton42: TcySpeedButton;
    cySpeedButton43: TcySpeedButton;
    cySpeedButton44: TcySpeedButton;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    SpeedButton2: TSpeedButton;
    LBInfo: TListBox;
    procedure cyNavPanel1CanNavigateToControl(Sender: TObject; ToControl: TControl; const KeyPressed: Word; var Accept: Boolean);
    procedure cyNavPanel1ActivateControl(Sender: TObject);
    procedure cyNavPanel1KeyPress(Sender: TObject; var Key: Char);
    procedure ImgCloseClick(Sender: TObject);
    procedure cyNavPanel1AfterDrawBackground(Sender: TObject);
    procedure cyNavPanel1DeactivateControl(Sender: TObject);
    procedure cyNavPanel1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure cyNavPanel1ActiveControlFocused(Sender: TObject; FocusedControl: TControl);
    procedure cyNavPanel1ActiveControlMouseActivate(Sender: TObject; MouseActivatedControl: TControl);
    procedure cyNavPanel1Exit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmNavPanel: TFrmNavPanel;

implementation

{$R *.lfm}

procedure TFrmNavPanel.cyNavPanel1ActivateControl(Sender: TObject);
begin
  if cyNavPanel1.ActiveControl <> Nil then
    if cyNavPanel1.ActiveControl is TLabel then
      with TLabel(cyNavPanel1.ActiveControl) do
        Font.Style := Font.Style + [fsUnderline];

  if cyNavPanel1.ActiveControl <> Nil then
    LBInfo.Items.Insert(0, 'Navigated to ' + cyNavPanel1.ActiveControl.Name);
end;

procedure TFrmNavPanel.cyNavPanel1DeactivateControl(Sender: TObject);
begin
  if cyNavPanel1.ActiveControl <> Nil then
    if cyNavPanel1.ActiveControl is TLabel then
      with TLabel(cyNavPanel1.ActiveControl) do
        Font.Style := Font.Style - [fsUnderline];
end;

procedure TFrmNavPanel.cyNavPanel1Exit(Sender: TObject);
begin
  if cyNavPanel1.CanFocus then
    cyNavPanel1.SetFocus;
end;

procedure TFrmNavPanel.cyNavPanel1CanNavigateToControl(Sender: TObject; ToControl: TControl; const KeyPressed: Word; var Accept: Boolean);
begin
  Accept := ToControl.Tag <> 1;
end;

procedure TFrmNavPanel.cyNavPanel1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Correct bad navigation :
  if Key = VK_LEFT then
    if cyNavPanel1.ActiveControl = LblAbout then
    begin
      Key := 0;
      cyNavPanel1.ActiveControl :=LblDownloads;
    end;

end;

procedure TFrmNavPanel.cyNavPanel1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then // Enter key
  begin
    Key := #13;
    if cyNavPanel1.ActiveControl <> Nil then
    begin
      LbInfo.Items.Insert(0, 'User have pressed EnterKey on ' + cyNavPanel1.ActiveControl.Name);

      if cyNavPanel1.ActiveControl is TImage then
        TImage(cyNavPanel1.ActiveControl).OnClick(nil);
    end;
  end;
end;

procedure TFrmNavPanel.FormShow(Sender: TObject);
begin
 { Label2.Caption := intToStr((ImgGif.Picture.Graphic as TGIFImage).AnimationSpeed);

  (ImgGif.Picture.Graphic as TGIFImage).AnimationSpeed := 800;
  (ImgGif.Picture.Graphic as TGIFImage).Animate := True;  }
end;

procedure TFrmNavPanel.cyNavPanel1ActiveControlFocused(Sender: TObject; FocusedControl: TControl);
begin
  LbInfo.Items.Insert(0, 'Activate ' + cyNavPanel1.ActiveControl.Name + ' by focusing ' + FocusedControl.Name);
end;

procedure TFrmNavPanel.cyNavPanel1ActiveControlMouseActivate(Sender: TObject; MouseActivatedControl: TControl);
begin
  LbInfo.Items.Insert(0, 'Activate ' + cyNavPanel1.ActiveControl.Name + ' by mouse activate ' + MouseActivatedControl.Name);
end;

procedure TFrmNavPanel.cyNavPanel1AfterDrawBackground(Sender: TObject);
begin
  //
end;

procedure TFrmNavPanel.ImgCloseClick(Sender: TObject);
begin
  Close;
end;

end.
