unit appMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LbClass, Forms, Controls, Graphics, Dialogs,
  StdCtrls, lbCipher, Converters;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Lb3DES1: TLb3DES;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  aKey: TKey128;
  I: Integer;
begin
  //lb3DES1.
  lb3DES1.GenerateRandomKey;
  lb3DES1.GetKey(aKey);
  Edit1.Text:= '';
  for I := 0 to Length(aKey) - 1 do
      Edit1.Text:= Edit1.Text + '0x'+IntToHex(aKey[I], 2) + ' ';
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  aKey: TKey128;
  tmpArr: TDataBytes;
  I: Integer;
begin
  tmpArr := StringToBytes(Edit2.Text);
  for I := 0 to Length(TmpArr) - 1 do
      aKey[I] := tmpArr[I];
  lb3DES1.SetKey(aKey);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  inBuffer: TDataBytes;
  outBuffer: TDataBytes;
  I: Integer;
  dataLen: Integer;
  S: String;
begin
  inBuffer := StringToBytes(Edit1.Text);
  SetLength(outBuffer, Length(inBuffer) *4);
  dataLen := lb3DES1.EncryptBuffer(inBuffer[0], Length(InBuffer), outBuffer[0]);
  SetLength(outBuffer, dataLen);
  //Setlength(S, Length(outBuffer));
  for I := 0 to Length(outBuffer) - 1 do
      S := S + '0x' + IntToHex(outBuffer[I],2) + ' ';

  edit3.Text:= S;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  inBuffer: TDataBytes;
  outBuffer: TDataBytes;
  I, dataLen: Integer;
  S: String;
begin
  inBuffer := StringToBytes(Edit3.Text);
  SetLength(outBuffer, Length(inBuffer) *2);
  dataLen := lb3DES1.DecryptBuffer(inBuffer[0], Length(InBuffer), outBuffer[0]);
  //Setlength(S, Length(outBuffer));
  SetLength(outBuffer, dataLen);
  for I := 0 to Length(outBuffer) - 1 do
      S := S + IntToHex(outBuffer[I],2) + ' ';

  edit4.Text:= S;

end;

end.

