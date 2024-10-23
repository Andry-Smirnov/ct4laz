
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsSmtpServer;

interface

uses Classes, vsVisualServerBase, blcksock;

type

  TSMTPServer = class(TVisualServer)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSMTPHandler = class(TServerHandler)
    procedure Handler; override;
  end;


implementation

{ TSMTPServer }

constructor TSMTPServer.Create(AOwner: TComponent);
begin
  inherited;
  FClientType := TSMTPHandler;
  ListenPort := '25';
end;

//SMTP Handler
procedure TSMTPHandler.Handler;
var
  FData: string;
begin
  //Handle a smtp transfer
  FSock.SendString('220+' + FSettings.FServerName + ' SMTP' + CRLF);
  FSock.SendString('220 Be welcome ' + FIPInfo.RemoteIP + CRLF);

  //example loop, smtp not implemented yet.
  FData := '';
  while not Terminated and (FSock.LastError = 0) do
  begin
    FSock.RecvString(60000);
    if FSock.LastError = 0 then
    begin

    end
    else
    begin
      FSock.SendString('000 TimeOut, terminating connection');
      FSock.CloseSocket;
      Break;
    end;

  end;
end;


end.
