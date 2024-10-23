unit ButtonsPanelmw;


interface

uses
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ExtCtrls, StdCtrls, TplButtonsPanelUnit;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    plButtonsPanel1: TplButtonsPanel;
    Label1: TLabel;
    plButtonsPanel2: TplButtonsPanel;
    ImageList2: TImageList;
    procedure plButtonsPanel1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Bar: TplButtonsPanel;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.plButtonsPanel1Change(Sender: TObject);
begin
  Label1.Caption := 'Button ' + IntToStr((Sender as TplButtonsPanel).ItemIndex) + ' clicked.';
end;

end.
