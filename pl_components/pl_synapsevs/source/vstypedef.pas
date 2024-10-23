
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsTypeDef;


interface

uses Classes, SysUtils, vsFileLogger;

const
  PathSep =
  {$IFDEF UNIX}
  '/'
  {$ELSE} //Assume windows
  '\'
  {$ENDIF}
  ;

type

  TIPInfo = record
    ConnectionHandle: Integer;
    RemoteIP,
    RemotePort:String;
  end;



  TString = class
    Value: String;
  end;

function  IsIP(aString:String):Boolean;
function  IsIP6(aString:String):Boolean;
function  StrToObj(Value: String): TString;
procedure FreeWithObj (S: TStrings);

const

// originally taken from synapse' mimepart.pas
// for a more complete list, look here:
// http://oregonstate.edu/cws/tutorials/faq/mime-types.html

  MaxMimeType = 37;
  MimeType: array[0..MaxMimeType, 0..2] of string =
  (
    ('AU', 'audio', 'basic'),
    ('AVI', 'video', 'x-msvideo'),
    ('BMP', 'image', 'bmp'),
    ('DOC', 'application', 'msword'),
    ('EPS', 'application', 'Postscript'),
    ('GIF', 'image', 'gif'),
    ('GZ', 'application', 'gzip'),
    ('HTM', 'text', 'html'),
    ('HTML', 'text', 'html'),
    ('JAR', 'application', 'x-java-archive'),
    ('JPEG', 'image', 'jpeg'),
    ('JPG', 'image', 'jpeg'),
    ('JS', 'application', 'x-javascript'),
    ('LOG', 'text', 'plain'),
    ('MID', 'audio', 'midi'),
    ('MOV', 'video', 'quicktime'),
    ('MPEG', 'video', 'mpeg'),
    ('MPG', 'video', 'mpeg'),
    ('MP2', 'audio', 'mpeg'),
    ('MP3', 'audio', 'mpeg'),
    ('OGG', 'application', 'ogg'),
    ('PDF', 'application', 'pdf'),
    ('PNG', 'image', 'png'),
    ('PS', 'application', 'Postscript'),
    ('QT', 'video', 'quicktime'),
    ('RA', 'audio', 'x-realaudio'),
    ('RAR', 'application', 'x-rar'),
    ('RPM', 'application', 'octetstream'),
    ('RTF', 'application', 'rtf'),
    ('SND', 'audio', 'basic'),
    ('TIF', 'image', 'tiff'),
    ('TIFF', 'image', 'tiff'),
    ('TXT', 'text', 'plain'),
    ('WAV', 'audio', 'x-wav'),
    ('WPD', 'application', 'Wordperfect5.1'),
    ('XHTML', 'text', 'html'),
    ('XML', 'text', 'xml'),
    ('ZIP', 'application', 'zip')
    );

function MimeTypeFromExtension (Extension: String): String;

implementation

function IsIP(aString:String):Boolean;
begin
  result:=true;
end;

function IsIP6(aString:String):Boolean;
begin
  result:=true;
end;

function StrToObj(Value: String): TString;
begin
  Result := TString.Create;
  Result.Value := Value;
end;

procedure FreeWithObj (S: TStrings);
var i: Integer;
begin
  for i:= 0 to S.Count - 1 do
    if Assigned (S.Objects[i]) then
      S.Objects[i].Free;
  S.Free;
end;


function MimeTypeFromExtension (Extension: String): String;
var i: Integer;
begin
  if Extension = '' then
    exit;
  if Extension[1]='.' then
    Delete (Extension, 1, 1);
  for i:=0 to high (MimeType) do
    begin
      if AnsiCompareText (MimeType[i][0], Extension) = 0 then
        begin
          Result := MimeType[i][1]+'/'+MimeType[i][2];
          break;
        end;
    end;
end;

end.
