unit formcyBook;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, cyBasePanel, cyPanel, StdCtrls, Buttons, Math,
  cyGraphics, cySpeedButton, cySplitter, cyBaseLabel, cyHotLabel,
  cyLabel, cyColorMatrix, cyBaseSpeedButton, cyBaseColorMatrix, cyBaseButton, cyBitBtn, Grids, cyDebug, cyBook;

type
  TFrmCyBook = class(TForm)
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
    Label1: TLabel;
    cyBook1: TcyBook;
    Label2: TLabel;
    LblInfo1: TLabel;
    cyBook2: TcyBook;
    LblInfo2: TLabel;
    CBReadOnly: TCheckBox;
    CBEnabledTurnPageAnimation: TCheckBox;
    BtnPriorPages: TButton;
    BtnNextPages: TButton;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    CBQuality: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    Image1: TImage;
    CBShowPageNumber: TCheckBox;
    CBResizeSmallImages: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure LblWebSiteClick(Sender: TObject);
    procedure cySplitter1Moved(Sender: TObject);
    procedure cyBook1RightPageChange(Sender: TObject);
    procedure cyBook2RightPageChange(Sender: TObject);
    procedure CBReadOnlyClick(Sender: TObject);
    procedure CBEnabledTurnPageAnimationClick(Sender: TObject);
    procedure BtnPriorPagesClick(Sender: TObject);
    procedure BtnNextPagesClick(Sender: TObject);
    procedure CBQualityClick(Sender: TObject);
    procedure cyBook2NeedPage(Sender: TObject; PageNumber: Integer; PageMode: TPageMode);
    procedure cyBook1Click(Sender: TObject);
    procedure cyBook2BeforePreparePage(Sender: TObject; PageNumber: Integer; PageMode: TPageMode; PageRect: TRect; var RenderMode: TPageRender;
      var Handled: Boolean);
    procedure cyBook2DblClick(Sender: TObject);
    procedure cyBook2Paint(Sender: TObject);
    procedure CBShowPageNumberClick(Sender: TObject);
    procedure CBResizeSmallImagesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCyBook: TFrmCyBook;

implementation

{$R *.lfm}

const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}


procedure TFrmCyBook.cySplitter1Moved(Sender: TObject);
begin
  RichEditInfo.Invalidate;
end;

procedure TFrmCyBook.FormCreate(Sender: TObject);
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

  cyBook1RightPageChange(Nil);
  cyBook2RightPageChange(Nil);
end;

procedure TFrmCyBook.LblWebSiteClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OpenURL('http://sourceforge.net/projects/tcycomponents/'); { *Converted from ShellExecute*  }
  Screen.Cursor := crDefault;
end;

procedure TFrmCyBook.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCyBook.BtnNextPagesClick(Sender: TObject);
begin
  cyBook2.ViewNext2Pages(20, 300);
end;

procedure TFrmCyBook.BtnPriorPagesClick(Sender: TObject);
begin
  cyBook2.ViewPrior2Pages(20, 300);
end;

procedure TFrmCyBook.CBEnabledTurnPageAnimationClick(Sender: TObject);
begin
  cyBook1.TurnPageAnimation.Enabled := CBEnabledTurnPageAnimation.Checked;
end;

procedure TFrmCyBook.CBQualityClick(Sender: TObject);
begin
  case CBQuality.ItemIndex of
    0: cyBook2.PageRender := prFitLowQuality;
    1: cyBook2.PageRender := prFitHighQuality;
    2: cyBook2.PageRender := prStretchLowQuality;
    3: cyBook2.PageRender := prStretchHighQuality;
    4: cyBook2.PageRender := prCentered; // Images must be more little than book page !
  end;

  cyBook2.LeftPageView.InvalidatePage;
  cyBook2.LeftPageView.InvalidatePageBehind;
  cyBook2.RightPageView.InvalidatePage;
  cyBook2.RightPageView.InvalidatePageBehind;

  cyBook2.Invalidate;
end;

procedure TFrmCyBook.CBReadOnlyClick(Sender: TObject);
begin
  cyBook1.ReadOnly := CBReadOnly.Checked;
end;

procedure TFrmCyBook.CBResizeSmallImagesClick(Sender: TObject);
begin
  if CBResizeSmallImages.Checked
  then cyBook2.Options := cyBook2.Options + [boFitSmallImages, boStretchSmallImages]
  else cyBook2.Options := cyBook2.Options - [boFitSmallImages, boStretchSmallImages];

  cyBook2.LeftPageView.InvalidatePage;
  cyBook2.RightPageView.InvalidatePage;
  cyBook2.Invalidate;
end;

procedure TFrmCyBook.CBShowPageNumberClick(Sender: TObject);
begin
  cyBook2.Invalidate;
end;

procedure TFrmCyBook.cyBook1Click(Sender: TObject);
begin
  Caption := Caption + ' *';
end;

procedure TFrmCyBook.cyBook1RightPageChange(Sender: TObject);
begin
  LblInfo1.Caption := 'Left page=' + intToStr(cyBook1.CurrentLeftPage) + '  Right page=' + intToStr(cyBook1.CurrentRightPage)
                         + ' of ' + intToStr(cyBook1.Pages) + ' pages';
end;

procedure TFrmCyBook.cyBook2BeforePreparePage(Sender: TObject; PageNumber: Integer; PageMode: TPageMode; PageRect: TRect; var RenderMode: TPageRender;
  var Handled: Boolean);
var
  SourceBmp, DestinationBmp: TBitmap;
begin
  // You can use your own code for page rendering!
  if CBQuality.ItemIndex = 5 then
  begin
    case PageMode of
      pmLeftPage:        begin SourceBmp := cyBook2.LeftPageView.OriginalPage;        DestinationBmp := cyBook2.LeftPageView.Page; end;
      pmRightPage:       begin SourceBmp := cyBook2.RightPageView.OriginalPage;       DestinationBmp := cyBook2.RightPageView.Page; end;
      pmBehindLeftPage:  begin SourceBmp := cyBook2.LeftPageView.OriginalPageBehind;  DestinationBmp := cyBook2.LeftPageView.PageBehind; end;
      pmBehindRightPage: begin SourceBmp := cyBook2.RightPageView.OriginalPageBehind; DestinationBmp := cyBook2.RightPageView.PageBehind; end;
    end;

    if (SourceBmp.Height < PageRect.Bottom - PageRect.Top) and (SourceBmp.Width < PageRect.Right - PageRect.Left) then
    begin
      RenderMode := prCentered;
      Exit;
    end;

    // Custom page rendering exemple :
    Handled := true;

    DestinationBmp.Width := PageRect.Right - PageRect.Left;
    DestinationBmp.Height := PageRect.Bottom - PageRect.Top;
    DestinationBmp.PixelFormat := pf24bit;
    DestinationBmp.Transparent := false;

    // Windows GDI stretch function :
    SetStretchBltMode(DestinationBmp.Canvas.Handle, HALFTONE); //STRETCH_HALFTONE);   9999
    StretchBlt(DestinationBmp.Canvas.Handle,
               0,
               0,
               DestinationBmp.Width,
               DestinationBmp.Height,
               SourceBmp.Canvas.Handle,
               0,
               0,
               SourceBmp.Width,
               SourceBmp.Height,
               SRCCOPY);
  end;
end;

procedure TFrmCyBook.cyBook2DblClick(Sender: TObject);
begin
  if cyBook2.TurnPageAnimation.AnimatedPage = pmLeftPage
  then cyBook2.ViewPrior2Pages(20, 300)
  else cyBook2.ViewNext2Pages(20, 300);
end;

procedure TFrmCyBook.cyBook2NeedPage(Sender: TObject; PageNumber: Integer; PageMode: TPageMode);
var aImage: TImage;
begin
  aImage := TImage(FindComponent('Image' + intToStr(PageNumber)));

{ // Method 1 :
  case PageMode of
    pmLeftPage:
    begin
      cyBook2.LeftPageView.OriginalPage.Assign(aImage.Picture.Graphic);
      cyBook2.LeftPageView.InvalidOriginalPage := false;
    end;

    pmBehindLeftPage:
    begin
      cyBook2.LeftPageView.OriginalPageBehind.Assign(aImage.Picture.Graphic);
      cyBook2.LeftPageView.InvalidOriginalPageBehind := false;
    end;

    pmBehindRightPage:
    begin
      cyBook2.RightPageView.OriginalPageBehind.Assign(aImage.Picture.Graphic);
      cyBook2.RightPageView.InvalidOriginalPageBehind := false;
    end;

    pmRightPage:
    begin
      cyBook2.RightPageView.OriginalPage.Assign(aImage.Picture.Graphic);
      cyBook2.RightPageView.InvalidOriginalPage := false;
    end;
  end;  }

// Method 2 :
  case PageMode of
    pmLeftPage:        cyBook2.LeftPageView.SetPage(aImage.Picture.Graphic, clWhite);
    pmBehindLeftPage:  cyBook2.LeftPageView.SetPageBehind(aImage.Picture.Graphic, clWhite);
    pmBehindRightPage: cyBook2.RightPageView.SetPageBehind(aImage.Picture.Graphic, clWhite);
    pmRightPage:       cyBook2.RightPageView.SetPage(aImage.Picture.Graphic, clWhite);
  end;

end;

procedure TFrmCyBook.cyBook2Paint(Sender: TObject);
begin
  if not CBShowPageNumber.Checked then Exit;

  cyBook2.Canvas.Font.Name   := 'Arial';
  cyBook2.Canvas.Font.Size   := 20;
  cyBook2.Canvas.Font.Color  := clBlack;
  cyBook2.Canvas.Brush.Style := bsClear;

  if cyBook2.CurrentLeftPage <> 0 then
    cyBook2.Canvas.TextOut(30, cyBook2.Height - 60, intToStr(cyBook2.CurrentLeftPage));

  if cyBook2.CurrentRightPage <> 0 then
    cyBook2.Canvas.TextOut(cyBook2.Width - 60, cyBook2.Height - 60, intToStr(cyBook2.CurrentRightPage));
end;

procedure TFrmCyBook.cyBook2RightPageChange(Sender: TObject);
begin
  LblInfo2.Caption := 'Left page=' + intToStr(cyBook2.CurrentLeftPage) + '  Right page=' + intToStr(cyBook2.CurrentRightPage)
                         + ' of ' + intToStr(cyBook2.Pages) + ' pages';
end;

end.
