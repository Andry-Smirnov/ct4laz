unit TreeListViewSmplmw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, TplTreeListViewUnit, Forms, Controls,
  Graphics, Dialogs, Menus, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList: TImageList;
    TreeListView1: TplTreeListView;
    pmenu: TPopupMenu;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TreeListView1ClickAtItem(sender: TObject; item: TplTreeListItem);
  private

    procedure TLClickRecordItem(sender: TObject; item: TplTreeListRecordItem);
    procedure TLCollapsed(sender: TObject; item: TplTreeListItem);
    procedure TLExpanded(sender: TObject; item: TplTreeListItem);
    procedure TLSelect(sender: TObject; item: TplTreeListItem);

  public

  end;

var
  Form1: TForm1; 
   running:boolean=true;

implementation

{$R *.lfm}


const
// Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\TplTreeListView\';
{$ENDIF}
{$IFDEF UNIX}
  pathMedia = '../TplTreeListView/';
{$ENDIF}



procedure createTree(plist: TplTreeListItems; deep: longint);
var i:longint;
begin
  if deep<=0 then exit;
  for i:=1 to 5 do
    createTree(plist.Add('tree:'+IntToStr(deep)+','+IntToStr(i)).SubItems,deep-1)
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var i:longint;
    bmp:TBitmap;
    mi: TMenuItem;
    grid: TTIPropertyGrid;
begin
  inherited  ;

  ImageList.Width:=14;
  ImageList.Height:=14;
  if FileExists(pathMedia+'tv1.bmp') then
  begin
    bmp:=TBitmap.Create;
    bmp.LoadFromFile(pathMedia+'tv1.bmp');
    ImageList.Add(bmp,nil);
    bmp:=TBitmap.Create;
    bmp.LoadFromFile(pathMedia+'tv2.bmp');
    ImageList.Add(bmp,nil);
    bmp:=TBitmap.Create;
    bmp.LoadFromFile(pathMedia+'tv3.bmp');
    ImageList.Add(bmp,nil);
    TreeListView1.Images:=ImageList;
  end;

  TreeListView1.OnItemExpanded:=@TLExpanded;
  TreeListView1.OnItemCollapsed:=@TLCollapsed;
  TreeListView1.OnSelect:=@TLSelect;
  TreeListView1.OnClickAtRecordItem:=@TLClickRecordItem;
  TreeListView1.OnClickAtItem:=@TreeListView1ClickAtItem;

  TreeListView1.Columns.clear;
  TreeListView1.Columns.add.Text:='Column 1';
  TreeListView1.Columns[0].Width:=170;
  with TreeListView1.Columns.Add do begin
    text:='Column 2';
    Width:=100;
  end;
  with TreeListView1.Columns.Add do begin
    text:='3';
    Width:=50;
  end;
  with TreeListView1.Columns.Add do begin
    text:='right';
    Width:=100;
    Alignment:=taRightJustify;
  end;
  with TreeListView1.Columns.Add do begin
    text:='center';
    Width:=100;
    Alignment:=taCenter;
  end;
  with TreeListView1.Columns.Add do begin
    text:='clicked';
    Width:=45;
  end;
  TreeListView1.Options:=TreeListView1.Options + [tlvoMultiSelect,tlvoHotTrackRecordTextItems,tlvoSorted];

  TreeListView1.Options:=TreeListView1.Options + [tlvoColumnsDragable];
  TreeListView1.createUserColumnVisibilityPopupMenu();

  TreeListView1.HorizontalLineMode:=lmSolid;
  TreeListView1.VerticalLineMode:=lmDot;
  TreeListView1.BeginUpdate;
  with TreeListView1.Items.Add do begin
    Text:='Hello';
    RecordItems.Add('World');
    RecordItems.Add('123456789');
    SubItems.Add('World 1').RecordItems.Add.Text:='first World';
    SubItems.Add('World 2').RecordItems.Add.Text:='second World';
    with SubItems.Add('World 3') do begin
      RecordItems.Add.Text:='dritte World';
      RecordItems.Add.Text:='alpha';
      RecordItems.Add.Text:='beta';
      RecordItems.Add.Text:='gamma';
      SubItems.Add('There are giraffes').SubItems.Add('and penguins');
      SubItems.Add('There Suns');

    end;
    SubItems.Add('World 4').RecordItems.Add.Text:='fourth World';
  end;
  with TreeListView1.Items.Add do begin
    Text:='Good bye';
    SubItems.Add('World 1').RecordItems.Add.Text:='first World';
    SubItems.Add('World 2').RecordItems.Add.Text:='second World';
    with SubItems.Add('World 3') do begin
      RecordItems.Add.Text:='third World';
      RecordItems.Add.Text:='alpha';
      RecordItems.Add.Text:='beta';
      RecordItems.Add.Text:='gamma';
      SubItems.Add('There are giraffes').SubItems.Add('and penguins');
      SubItems.Add('There Suns');
    end;
    SubItems.Add('World 4').RecordItems.Add.Text:='fourth World';
  end;
  for i := 0 to 20 do
    with TreeListView1.Items.Add(inttostr(i)) do begin
      RecordItemsText[1]:='10 - '+inttostr(i)+' = '+inttostr(10-i);
      RecordItemsText[2]:='20 - '+inttostr(i)+' = '+inttostr(20-i);
      RecordItemsText[3]:='30 - '+inttostr(i)+' = '+inttostr(30-i);
      RecordItemsText[4]:='40 - '+inttostr(i)+' = '+inttostr(40-i);
      //ImageTyp:=itListIndex;
      ImageIndex:=(i+1) mod 2 +1;
    end;

   with TreeListView1.Items.Add('LARGE TREE') do
  begin
     if FileExists(pathMedia+'tvExt1.bmp') then
     begin
       ImageBitmap:=TBitmap.Create;
       imagebitmap.LoadFromFile(pathMedia+'tvExt1.bmp');
       createTree(SubItems,3);
     end;
   end;
   TreeListView1.EndUpdate;
   pmenu:=TPopupMenu.create(self);
   mi:=TMenuItem.create(self);mi.Caption:='does nothing';
   pmenu.items.add(mi);
   TreeListView1.popupmenu:=pmenu;

   visible:=true;


   with TSplitter.Create(self) do begin
     Parent:=self;
     align:=alLeft;
   end;

   grid:=TTIPropertyGrid.Create(self);
   grid.Parent:=self;
   grid.Align:=alLeft;
   grid.TIObject:=TreeListView1;

end;

procedure TForm1.TreeListView1ClickAtItem(sender: TObject; item: TplTreeListItem
  );
begin  
  if item = nil then exit;
  //if item.Parent = nil then TTreeListView(sender).Items.RemoveObject(item)
  //else item.Parent.SubItems.RemoveObject(item)
  item.RecordItemsText[5]:=inttostr(StrToIntDef(item.RecordItemsText[5], 0) + 1);

end;

procedure TForm1.TLClickRecordItem(sender: TObject; item: TplTreeListRecordItem
  );
var
  i: TplTreeListItem;
begin
  Caption:=item.parent.Text + ' clicked on: '+item.Text;
end;

procedure TForm1.TLCollapsed(sender: TObject; item: TplTreeListItem);
begin  
  Caption:=item.Text + ' collapsed';
end;

procedure TForm1.TLExpanded(sender: TObject; item: TplTreeListItem);
begin  
  Caption:=item.Text + ' expanded';
end;

procedure TForm1.TLSelect(sender: TObject; item: TplTreeListItem);
begin  
  Caption:=item.Text + ' selected';
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  running:=false;
end;

end.

