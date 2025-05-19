unit compresszip1mw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TplZipUnit, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonCompress: TButton;
    ButtonUnCompress: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    plZipCompress1: TplZipCompress;
    plZipUnCompress1: TplZipUnCompress;
    procedure ButtonCompressClick(Sender: TObject);
    procedure ButtonUnCompressClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
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

xTestFile='test2.txt';
xOutFile ='test2.zip';

{ TForm1 }

procedure TForm1.ButtonCompressClick(Sender: TObject);
 var ass:TZipFileEntry;
begin
  plZipCompress1.FileName:=ExtractFilePath(Application.ExeName)+xOutFile;
  ass:=plZipCompress1.Entries.AddFileEntry(ExpandFileName(SetDirSeparators(pathMedia+xTestFile)));

  //Set only the file name NOT and the path of file
  if ass<>nil then ass.ArchiveFileName:=xTestFile;

  plZipCompress1.ZipAllFiles;   

  Memo1.Lines.Add('Compress --- OK');
  Memo1.Lines.Add('Hit UnCompress ...');
end;

procedure TForm1.ButtonUnCompressClick(Sender: TObject);
begin
 plZipUnCompress1.FileName  :=ExtractFilePath(Application.ExeName)+xOutFile;
 plZipUnCompress1.OutputPath:=ExtractFilePath(Application.ExeName);
 plZipUnCompress1.UnZipAllFiles;   

  Memo1.Lines.LoadFromFile(ExtractFilePath(Application.ExeName)+xTestFile);

end;


end.

