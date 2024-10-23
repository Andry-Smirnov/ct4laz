unit sidebar2mw;

interface

uses
  Messages, SysUtils, Variants,LCLtype,LMessages, LCLIntf, LResources,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, TplSideBarUnit, ImgList;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Shape1: TShape;
    Shape2: TShape;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    NiceSideBar1: TplSideBar;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure NiceSideBar1Select(Sender: TObject; Index, SubIndex: Integer; aCaption: String);
  private
  protected
  public

  end;

var
  Form1: TForm1;

implementation

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.NiceSideBar1Select(Sender: TObject; Index, SubIndex: Integer; aCaption: String);
begin
  Label4.Caption := 'Selected Item: ' + inttostr(Index)+'/'+inttostr(SubIndex)+' '+ aCaption;
end;


initialization
  {$I sidebar2mw.lrs}

end.
