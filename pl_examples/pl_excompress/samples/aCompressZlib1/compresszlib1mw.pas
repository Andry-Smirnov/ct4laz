unit CompressZlib1mw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TplZlibUnit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonCompress: TButton;
    ButtonUnCompress: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    plZlibCompress1: TplZlibCompress;
    plZLibUnCompress1: TplZLibUnCompress;
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

xTestFile='test1.txt';
xOutFile ='test1.zlib';

{ TForm1 }

procedure TForm1.ButtonCompressClick(Sender: TObject);
 var FSOut:TfileStream;
begin
  FSOut:=TfileStream.Create(ExtractFilePath(Application.ExeName)+xOutFile,fmcreate);
  FSOut.Position:=0;
  plZlibCompress1.OutStream:=FSOut;
  plZlibCompress1.InputFiles.Clear;
  plZlibCompress1.InputFiles.Add(ExpandFileName(SetDirSeparators(pathMedia+xTestFile)));
  plZlibCompress1.CreateArchive;
  FSOut.Free; 

  Memo1.Lines.Add('Compress --- OK');
  Memo1.Lines.Add('Hit UnCompress ...');
end;

procedure TForm1.ButtonUnCompressClick(Sender: TObject);
 var FSIn,FSOut:TfileStream;
begin
  FSIn:=TfileStream.Create(ExtractFilePath(Application.ExeName)+xOutFile,fmOpenRead);
  FSIn.Position:=0;
  plZLibUnCompress1.InStream:=FSIn;

  FSOut:=TfileStream.Create(ExtractFilePath(Application.ExeName)+xTestFile,fmcreate);
  FSOut.Position:=0;
  plZLibUnCompress1.ExtractFileToStream(xTestFile,FSOut);

  FSIn.Free;
  FSOut.Free;


  Memo1.Lines.LoadFromFile(ExtractFilePath(Application.ExeName)+xTestFile);
end;


end.

