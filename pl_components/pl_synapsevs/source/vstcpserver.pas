
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsTcpServer;

interface

uses Classes, SysUtils, vsVisualServerBase;

type
  TTCPServer = class(TVisualServer) //basic TCP server
    constructor Create(AOwner: TComponent); override;
  end;

  TTCPHandler = class(TServerHandler)
    procedure Handler; override;
  end;

implementation

{ TTCPServer }

constructor TTCPServer.Create(AOwner: TComponent);
begin
  inherited;
  FClientType := TTCPHandler;
end;

{ TTCPHandler }

procedure TTCPHandler.Handler;
var
  Buf: string;
begin
  //basic server example
  while not Terminated and (FSock.LastError <> 0) do
  begin
    if FSock.WaitingData > 0 then
    begin
      SetLength(Buf, FSock.WaitingData);
      FSock.RecvBuffer(Pointer(Buf), Length(Buf));
      FRequest.RawRequest := Buf;
      CallBack(@OnRequest);
      if FResponse.ResponseText <> '' then
        FSock.SendString(FResponse.ResponseText);
      if FResponse.Data <> '' then
        FSock.SendString(FResponse.Data);
      if Assigned(FResponse.DataStream) then
        try
          FSock.SendStream(FResponse.DataStream);
          FreeAndNil(FResponse.DataStream);
        except
        end;
    end;
  end;
end;



end.
