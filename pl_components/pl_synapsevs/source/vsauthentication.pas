
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsAuthentication;

//this unit is not thread-safe yet..

interface

uses
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, IniFiles, synacode,
  vsTypeDef;

type
  TAuthEncoding = (aePlain, aeBase64, aeMD5);
  TAuthMethod = (amDenyAll, amAcceptAll, amAnonymous, amInifile, amSystem, amCallback{, amModAuth, amMySQL});

  TOnAuthenticate = procedure(Sender: TComponent; User, Pass: string; IPInfo: TIPInfo; var Authenticated: boolean) of object;

  TvsAuthentication = class(TComponent)
  private
    FCaseSensitive: boolean;
    FPasswordFile: string;
    FEncoding: TAuthEncoding;
    FMethod: TAuthMethod;
    FIPInfo: TIPInfo;
  protected
    FPassDecoded: string;
    FPassEncoded: string;
    FDummyBool: boolean;
    FUser: string;
    FPass: string;
    FOnAuthenticate: TOnAuthenticate;
    FIni: TIniFile;
    procedure EncodeDecodePass; //decode using appropiate method (=base64)
    function AuthenticateAnonymous: boolean;
    function AuthenticateWin32: boolean;
    function AuthenticateUnix: boolean;
    function AuthenticateSystem: boolean;
    function AuthenticateIniFile: boolean;
    function AuthenticateCallBack: boolean;
  public
    function Authenticate(User, Pass: string; Encoding: TAuthEncoding = aePlain): boolean;
    function GetAuthenticated: boolean;
    function AddUser(User, Pass: string): boolean;
    //    procedure ModifyUser - identical to AddUser
    property IPInfo: TIPInfo read FIPInfo write FIPInfo;
  published
    property Encoding: TAuthEncoding read FEncoding write FEncoding;
    property Method: TAuthMethod read FMethod write FMethod;
    property User: string read FUser write FUser;
    property Pass: string read FPass write FPass;
    property PasswordFile: string read FPasswordFile write FPasswordFile;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
    property IsAuthenticated: boolean read GetAuthenticated write FDummyBool;
    property OnAuthenticate: TOnAuthenticate read FOnAuthenticate write FOnAuthenticate;
  end;

implementation

{ TvsAuthentication }

function TvsAuthentication.AddUser(User, Pass: string): boolean;
begin
  Result := False;
  try
    try
      FIni := TIniFile.Create(FPasswordFile);
      FIni.WriteString('Authenticate', User, EncodeBase64(MD5(Pass)));
      Result := True;
    finally
      FIni.Free;
    end;
  except
    Result := False;
  end;
end;

function TvsAuthentication.Authenticate(User, Pass: string; Encoding: TAuthEncoding): boolean;
begin
  FUser := User;
  FPass := Pass;
  FEncoding := Encoding;
  EncodeDecodePass;
  case Method of
    amDenyAll: Result := False;
    amAcceptAll: Result := True;
    amAnonymous: Result := AuthenticateAnonymous;
    amIniFile: Result := AuthenticateIniFile;
    amSystem: Result := AuthenticateSystem;
    amCallback: Result := AuthenticateCallback;
    else
      Result := False;
  end;
end;

function TvsAuthentication.AuthenticateAnonymous: boolean;
var
  U: string;
begin
  if FCaseSensitive then
    U := FUser
  else
    U := lowercase(FUser);
  Result := (U = 'anonymous');
end;

function TvsAuthentication.AuthenticateCallBack: boolean;
var
  a: boolean;
begin
  Result := False;
  if Assigned(FOnAuthenticate) then
    try
      a := False;
      FOnAuthenticate(Self, FUser, FPass, FIPInfo, a);
      Result := a;
    except
      Result := False;
    end;
end;

function TvsAuthentication.AuthenticateIniFile: boolean;
begin
  try
    try
      FIni := TIniFile.Create(FPasswordFile);
      Result := EncodeBase64(FPassEncoded) = FIni.ReadString('Authenticate', User, '');
    finally
      FIni.Free;
    end;
  except
    Result := False;
  end;
end;

function TvsAuthentication.AuthenticateSystem: boolean;
begin
{$IFDEF WINDOWS}
  Result := AuthenticateWin32;
{$ELSE}
  Result := AuthenticateUnix;
{$ENDIF}
end;

function TvsAuthentication.AuthenticateUnix: boolean;
begin
{$IFNDEF UNIX}
  Result := False;
{$ELSE}
  //Match against password file in /etc
  (* //see http://www.experts-exchange.com/Programming/Programming_Languages/Cplusplus/Q_21104620.html#11897622
 You either want to authenticate them using PAM; or if you want to make assumptions
about the authentication method, use the shadow functions

getspnam(user)  to read their entry from the shadow file
then compare it using crypt.

For details about using pam

See the Linux-Pam Application Developer's Guide, section 2
 http://www.krasline.ru/Library/pam-doc/pam_appl-2.html

And the example code: http://www.krasline.ru/Library/pam-doc/pam_appl-6.html
  *)
{$ENDIF}
end;

function TvsAuthentication.AuthenticateWin32: boolean;
var
  Token: THandle;
begin
{$IFNDEF MSWINDOWS}
  Result := False;
{$ELSE}
  Result := LogonUser(PChar(User), nil, //PChar('.'), //nil, //domain
    PChar(Pass), LOGON32_LOGON_INTERACTIVE,  //NETWORK, //BATCH, //
    LOGON32_PROVIDER_DEFAULT, Token);
  //Release token:
  CloseHandle(Token);
{$ENDIF}
end;

procedure TvsAuthentication.EncodeDecodePass;
begin
  case FEncoding of
    aePlain: FPassDecoded := FPass;
    aeBase64: FPassDecoded := DecodeBase64(FPass);
    aeMD5: FPassDecoded := FPass; //athentication routine should check
  end;
  case FEncoding of
    aePlain, aeBase64: FPassEncoded := MD5(FPassDecoded); //beware, this is not hexadecimal!
    aeMD5: FPassEncoded := FPassDecoded;
  end;
end;

function TvsAuthentication.GetAuthenticated: boolean;
begin
  Result := Authenticate(FUser, FPass, Encoding);


end;

end.
