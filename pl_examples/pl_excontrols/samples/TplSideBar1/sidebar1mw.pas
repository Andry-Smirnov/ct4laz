unit sidebar1mw;

interface

uses
  Messages,LResources, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TplSideBarUnit, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SideBar: TplSideBar;
  end;

var
  Form1: TForm1;

implementation

procedure TForm1.FormCreate(Sender: TObject);
var
  Item: TSideBarItem;
begin
  SideBar := TplSideBar.Create(Self);
  with SideBar do
  begin
    BeginUpdate;
    Parent := Self;

    Item := Items.Add;
    Item.Caption := 'Test Item 1';
    Item.Items.CommaText := '"Sub Item 1","Sub Item 2","Sub Item 3","Sub Item 4"';

    Item := Items.Add;
    Item.Caption := 'Test Item 2';
    Item.Items.CommaText := '"Sub Item 1","Sub Item 2","Sub Item 3","Sub Item 4"';

    ItemIndex := Item.Index;
    SubItemIndex := 2;

    Item := Items.Add;
    Item.Caption := 'Test Item 3';
    Item.Items.CommaText := '"Sub Item 1","Sub Item 2","Sub Item 3","Sub Item 4"';

    Item := Items.Add;
    Item.Caption := 'Test Item 4';
    Item.Items.CommaText := '"Sub Item 1","Sub Item 2","Sub Item 3","Sub Item 4"';

    EndUpdate;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SideBar.ItemIndex := 0;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SideBar.SubItemIndex := 2;
end;

initialization
  {$I sidebar1mw.lrs}

end.
