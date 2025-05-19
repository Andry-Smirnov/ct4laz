unit compressLZMA1mw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TplLzmaUnit, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonCompress: TButton;
    ButtonUnCompress: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    plLzmaCompress1: TplLzmaCompress;
    plLzmaUnCompress1: TplLzmaUnCompress;
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

xTestFile='test3.txt';
xOutFile ='test3.Lzma';

{ TForm1 }

procedure TForm1.ButtonCompressClick(Sender: TObject);
 var FSOut:TfileStream;
begin
  FSOut:=TfileStream.Create(ExtractFilePath(Application.ExeName)+xOutFile,fmcreate);
  FSOut.Position:=0;
  plLzmaCompress1.OutStream:=FSOut;
  plLzmaCompress1.InputFiles.Clear;
  plLzmaCompress1.InputFiles.Add(ExpandFileName(SetDirSeparators(pathMedia+xTestFile)));
  plLzmaCompress1.CreateArchive;
  FSOut.Free;

  Memo1.Lines.Add('Compress --- OK');
  Memo1.Lines.Add('Hit UnCompress ...');
end;

procedure TForm1.ButtonUnCompressClick(Sender: TObject);
 var FSIn,FSOut:TfileStream;
begin
  FSIn:=TfileStream.Create(ExtractFilePath(Application.ExeName)+xOutFile,fmOpenRead);
  FSIn.Position:=0;
  plLzmaUnCompress1.InStream:=FSIn;

  FSOut:=TfileStream.Create(ExtractFilePath(Application.ExeName)+xTestFile,fmcreate);
  FSOut.Position:=0;
  plLzmaUnCompress1.ExtractFileToStream(xTestFile,FSOut);

  FSIn.Free;
  FSOut.Free;

  Memo1.Lines.LoadFromFile(ExtractFilePath(Application.ExeName)+xTestFile);
end;


end.

