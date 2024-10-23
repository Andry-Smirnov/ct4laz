
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vswebclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpwebclient, httpsend;

Type

TvsWebClient = Class(TAbstractWebClient)
  Protected
    Function DoCreateRequest: TWebClientRequest; override;
    Function DoHTTPMethod(Const AMethod,AURL : String; ARequest : TWebClientRequest) : TWebClientResponse; override;
  end;

TvsSynRequest = Class(TWebClientRequest)
  Private
    FHTTP : THTTPSend;
  Protected
    function GetHeaders: TStrings;override;
    function GetStream: TStream;override;
  Public
    Constructor Create(AHTTP : THTTPSend);
    Destructor Destroy; override;
  end;

TvsSynResponse = Class(TWebClientResponse)
  Private
    FHTTP : THTTPSend;
  Protected
    Function GetStatusCode : Integer; override;
    Function GetStatusText : String; override;
    function GetHeaders: TStrings;override;
    function GetStream: TStream;override;
  Public
    Constructor Create(ARequest : TWebClientRequest);  override;
    Destructor Destroy; override;
  end;


implementation

//=================== TvsSynRequest ======================================

Constructor TvsSynRequest.Create(AHTTP: THTTPSend);
begin
  FHTTP:=AHTTP;
end;

Destructor TvsSynRequest.Destroy;
begin
  FreeAndNil(FHTTP);
  inherited Destroy;
end;

function TvsSynRequest.GetHeaders: TStrings;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.Headers else
    Result:=Inherited GetHeaders;
end;

function TvsSynRequest.GetStream: TStream;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.Document else
    Result:=Inherited GetStream;
end;

//====================== TvsSynResponse ====================================

Constructor TvsSynResponse.Create(ARequest : TWebClientRequest);
begin
  Inherited Create(ARequest);
  FHTTP:=(ARequest as TvsSynRequest).FHTTP;
end;

Destructor TvsSynResponse.Destroy;
begin
  FreeAndNil(FHTTP);
  inherited Destroy;
end;

Function TvsSynResponse.GetStatusCode: Integer;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.ResultCode else
    Result:=0;
end;

Function TvsSynResponse.GetStatusText: String;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.ResultString else
    Result:='';
end;

function TvsSynResponse.GetHeaders: TStrings;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.Headers else
    Result:=Inherited GetHeaders;
end;

function TvsSynResponse.GetStream: TStream;
begin
  if Assigned(FHTTP) then
    Result:=FHTTP.Document else
    Result:=Inherited GetStream;
end;

//=================== TvsWebClient ==========================================

Function TvsWebClient.DoCreateRequest: TWebClientRequest;
begin
  Result:=TvsSynRequest.Create(THTTPSend.Create);
end;

Function TvsWebClient.DoHTTPMethod(Const AMethod, AURL: String; ARequest: TWebClientRequest): TWebClientResponse;
Var
  U,S : String;
  I : Integer;
  h : THTTPSend;

begin
  U:=AURL;
  H:=TvsSynRequest(ARequest).FHTTP;
  S:=ARequest.ParamsAsQuery;

  if (S<>'') then
    begin
      if Pos('?',U)=0 then U:=U+'?';
      U:=U+S;
    end;

  I:=H.Headers.IndexOfName('Content-type');

  if I<>-1 then
    begin
       H.MimeType:=H.Headers.Values['Content-type'];
       H.Headers.Delete(I);
    end;

  if Not H.HTTPMethod(AMethod,U) then
    begin
       H.Document.Clear;
       H.Headers.Clear;
       H.Cookies.Clear;
       With H.Sock do
          Raise EFPWebClient.CreateFmt('HTTP Request failed (%d : %s)',[LastError,LastErrorDesc]);
    end else
    begin
       Result:=TvsSynResponse.Create(ARequest);
       if Assigned(ARequest.ResponseContent) then ARequest.ResponseContent.CopyFrom(ARequest.Content,0);
       TvsSynRequest(ARequest).FHTTP:=Nil;
    end;
end;

end.

