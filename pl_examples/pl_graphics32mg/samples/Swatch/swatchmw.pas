unit Swatchmw;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GR32_Image, igCore_Viewer, igGrid_ListView,
  igSwatch_ListView, igCore_Items, igGrid, igSwatch, bivGrid,
  igSwatch_rwACO, igSwatch_rwASE;

type
  TForm1 = class(TForm)
    swatch1: TigSwatchList;
    btn1: TButton;
    dlgOpen1: TOpenDialog;
    swgrid1: TigSwatchGrid;
    btnClear: TButton;
    swgrid2: TigSwatchGrid;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
// CodeTyphon: Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia = '..\xmedia\';
{$ELSE}
  pathMedia = '../xmedia/';
{$ENDIF}


type
  TigGridAccess = class(TigGrid);


procedure TForm1.btn1Click(Sender: TObject);
begin
  if dlgOpen1.FileName='' then  dlgOpen1.InitialDir:=SetDirSeparators(pathMedia);  // 9999
  if dlgOpen1.Execute then
    swatch1.LoadFromFile(dlgOpen1.FileName);
end;

procedure TForm1.FormCreate(Sender: TObject);
var g : TigGrid;
begin
  dlgOpen1.Filter := TigSwatchList.ReadersFilter;
  {g := TigGrid.Create(self);
  g.Parent := self;
  TigGridAccess(g).ItemList := swatch1;}
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  swatch1.Clear;
end;

end.
