unit plGifAnimatormw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TplGifAnimatorUnit, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    plGifAnimator1: TplGifAnimator;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public

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

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  plGifAnimator1.FileName:=pathMedia+'agif1.gif';
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  plGifAnimator1.FileName:=pathMedia+'giphy1.gif';
end;

end.

