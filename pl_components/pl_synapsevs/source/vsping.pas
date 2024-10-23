
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsping;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  blcksock, PINGsend, extctrls;


type
  TOnStatusEvent = procedure(Sender:TObject; status:string; time:integer) of object;

  TvsSynPing = class(TComponent)
  private
  fhost         :       string;
  fpacketsize   :       integer;
  fpingtime     :       integer;
  ftimeout      :       integer;
  fstatus       :       string;
  finterval     :       integer;        //interval pro vnitrni timer
  fOnStatus     :       TOnStatusEvent; //definice udalosti pro status
  synping       :       TPINGSend;      //puvodni unita
  timer         :       TTimer;         //timer
  procedure SetHost(value:string);
  procedure SetPacketSize(value:integer);
  procedure SetTimeOut(value:integer);
  function  ReadPingTime:integer;
  procedure OnTimer(Sender: TObject);
  protected
  public
  published
  Sock : TICMPBlockSocket;
  property OnStatus: TOnStatusEvent read fOnStatus write fOnStatus;
  property Host: string read fhost write SetHost;
  property PacketSize: integer read fpacketsize write SetPacketSize;
  property PingTime: integer read ReadPingTime;
  property TimeOut: integer read ftimeout write SetTimeOut;
  property Status: string read fstatus write fstatus;
  property TimerInterval: integer read finterval write finterval;
  function Ping(Host:string):boolean;
  procedure StartCheck;
  procedure StopCheck;
  constructor Create(AOwner:TComponent); override;
  destructor Destroy; override;
  end;


implementation

procedure TvsSynPing.SetHost(value:string);
begin
if fhost<>value then
 begin
  fhost:=value;
 end;
end;

function TvsSynPing.Ping(Host:string):boolean;
begin
if Host<>'' then
 begin
 if ftimeout=0 then
        self.synping.Timeout:=5000 else
        self.synping.Timeout:=ftimeout;
 if fpacketsize=0 then
        self.synping.PacketSize:=512 else
        self.synping.PacketSize:=fpacketsize;
        result:=self.synping.Ping(fhost);
        fpingtime:=Self.synping.PingTime;
 end;
end;

procedure TvsSynPing.SetPacketSize(value:integer);
begin
  if fpacketsize<>value then
  begin
    fpacketsize:=value;
    Self.synping.PacketSize:=value;
  end;
end;

procedure TvsSynPing.SetTimeOut(value:integer);
begin
  if ftimeout<>value then
  begin
    ftimeout:=value;
    Self.synping.Timeout:=value;
  end;
end;

function TvsSynPing.ReadPingTime;
begin
    fpingtime:=Self.synping.PingTime;
    result:=fpingtime;
end;

procedure TvsSynPing.StartCheck;
begin
  if finterval=0 then timer.interval:=1000 else timer.Interval:=finterval;
  timer.Enabled:=true;
end;

procedure TvsSynPing.StopCheck;
begin
  timer.Enabled:=false;
end;

constructor TvsSynPing.create(AOwner:TComponent);
begin
inherited;
  synping:=TPINGSend.Create;
  timer:=TTimer.Create(self);
  timer.Interval:=finterval;
  timer.OnTimer:=OnTimer;
  Self.fhost:='127.0.0.1';
  Self.fpacketsize:=512;
  Self.ftimeout:=5000;
  Self.finterval:=1000;
end;

procedure TvsSynPing.OnTimer(Sender: TObject);
begin
  if not Ping(fhost) then
  fstatus:='BAD'
  else
  fstatus:='OK';
  
  Self.OnStatus(self,fstatus,fpingtime);  
end;

destructor TvsSynPing.destroy;
begin
  if timer<>nil then
    begin
      timer.Enabled:=false;
      timer.Free;
    end;
  if synping<>nil then synping.Free;
inherited;
end;

end.
