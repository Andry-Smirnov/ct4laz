{**********************************************************************
                PilotLogic Software House.
  
 Package pl_SynapseVS
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

unit allsynapsevsregister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo,
  lresources, lclintf,

  vsComPort,
  vswebclient,
  vsping,
  {$IFDEF WINDOWS}
  vsRawIP,
  vsVisualSynapse,
  {$ENDIF}

  vsPastella,
  vsTcpServer,
  vsFtpServer,
  vsauthentication,
  {smtpserver,}
  vsHttpServer;

procedure Register;

implementation

 {$R allsynapsevsregister.res}

procedure Register;
begin

  RegisterComponents('SynapseVS', [
                                   TvsComPort,
                                   TvsWebClient,
                  {$IFDEF WINDOWS} TvsSniffer,  {$ENDIF}
                                   TvsHTTPServer,
                                   TvsFTPServer,
                                   TvsAuthentication,
                                   { TPastella,}{ TSMTPServer,}
                  {$IFDEF WINDOWS} TvsVisualDNS,
                                   TvsVisualHTTP,
                                   TvsVisualUDP,
                                   TvsVisualTCP,
                                   TvsVisualICMP,
                                   TvsSocksProxyInfo,
                                   TvsIPHelper,
                                   TvsSendMail, {$ENDIF}
                                   TvsSynPing
                                   ]);


end;


end.

