
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsPastella;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, LMessages,
  Classes, SysUtils, blcksock, syncobjs,
  vsVisualServerBase, synacode, synautil;

type

  THashString = string; //md5sum is 128-bit.. maybe we need switch to 64-bit or even 32-bit
  //for performance reasons. although 32-bit gets tricky.

  THash = array[0..7] of char; //make it Int64?

  //commands:
  // push hashlist
  // push packets
  // get packets

  TPastellaCmd = packed record
    Cmd: array[0..3] of char;
    Count: word;  //number of logical sequences
    Param: word;
  end;

  TP2PPacket = record
    Packet: string;
    Checksum: string;
  end;

  TPacketType = (ptNull, ptIPPort, ptBroadcast, ptRoute, ptNode, ptAgent, ptWalker, ptPersistant, ptStream);

  TNodeAddr = array[0..7] of char;

  TPacketHeader = packed record
    Hash: THash;                   //16 byte
    DataLength: DWord;             //4 byte
    packettype: TPacketType;       //1 byte
    reserved: byte;                //1 byte
    TTL: word;                     //2 byte
    AddrFrom: TNodeAddr;           //8 byte
    AddrTo: TNodeAddr;             //8 byte
    timestamp: double;             //8 byte
  end;
  //48 byte

  TPastellaPacket = record
    header: TPacketHeader;
    Data: string; //to be sent and received in two steps
  end;

  Util = class
    class function MakeHash(Data: string): THash;
    class function HeaderValid(Header: TPacketHeader): boolean;
  end;

  TPacketOrigin = (poClient, poNetwork, poUnknown);

  //internal structure, also used for application interaction
  TPastellaMessage = class
    //MsgFrom: String;
    //MsgTo: String;
    //MsgType: TPacketType;
    Data: TPastellaPacket;
    //Hash: THash;
    RefCount: integer;
    //constructor not needed
    tickcount: integer;
    origin: TPacketOrigin;
    procedure CreateHash;
    function Valid: boolean;
    function HeaderValid: boolean;
  end;

  THashList = class;
  TPacketList = class;

  THashObj = class
    Hash: THash;
    TimeStamp: double;
    Packet: Pointer;
    UserData: Pointer;
    constructor Create(Value: THash);
  end;

  THashListIndex = integer;

  THashList = class
  private
    FItems: TList;
    FForEachIndex: integer;
    function GetItems(Index: integer): THash;
    procedure SetItems(Index: integer; const Value: THash);
    function GetUserData(Hash: THash): Pointer;
    procedure SetUserData(Hash: THash; const Value: Pointer);
  public
    CS: TCriticalSection;
    PacketList: TPacketList; //used for reference counting
    constructor Create(Packets: TPacketList);
    destructor Destroy; override;
    function Add(Hash: THash; DoRefCount: boolean = True): boolean;
    function AddUnique(Hash: THash): boolean; //adds only if unique
    function Exists(Hash: THash): boolean;
    procedure Clear(DoRefCount: boolean = True);
    function Delete(Hash: THash; DoRefCount: boolean = True): boolean; overload;
    function Delete(i: integer; DoRefCount: boolean = True): boolean; overload;
    function Delete(HashList: THashList; DoRefCount: boolean = True): boolean; overload;
    function RemoveOld(TimeStamp: double): integer;
    function Count: integer;
    function IndexOf(Hash: THash): integer;
    procedure CopyExclusive(HashList: THashList);
    procedure MatchExclusive(Source: THashList; Match: THashList; DoRefCount: boolean = True);
    procedure Inclusive(HashList: THashList; var Strings: TStrings);
    procedure CopyFrom(HashList: THashList; DoRefCount: boolean = True);
    procedure CopyClearFrom(HashList: THashList);
    procedure MoveFrom(HashList: THashList);
    procedure MoveTo(HashList: THashList);
    procedure Truncate(Number: integer; DoRefCount: boolean = True);
    function ForInit: boolean;
    function ForEach(var Hash: THash): boolean;
    procedure Lock;
    procedure UnLock;
    property Items[Index: integer]: THash read GetItems write SetItems; default;
    property UserData[Hash: THash]: Pointer read GetUserData write SetUserData;
  end;

  TPacketListIndex = record
    table: byte;
    Index: integer;
  end;

  TPacketList = class
    //in future we might optimize performance
    //by having or: indexed list
    //or multiple lists of convenient size.
    //but up to few thousand packets it may work.
    //although search algorythm is not efficient yet.
    //however, i focus on getting things working first.
  protected
    function AddPacket(Packet: TPastellaMessage): boolean;
  private
    //these function with no critical section set
    procedure DeletePacket(Index: TPacketListIndex); overload;
    function FindPacket(Hash: THash): TPacketListIndex;
  public
    PacketList: array [0.. 255] of TList;
    CS: TCriticalSection;
    constructor Create;
    destructor Destroy; override;
    function Add(Packet: TPastellaMessage): boolean; overload;
    function Add(Packet: TPastellaPacket): boolean; overload;
    procedure DeletePacket(Hash: THash); overload;
    function GetPacket(Hash: THash): TPastellaMessage; overload;
    function GetPacket(Index: TPacketListIndex): TPastellaMessage; overload;
    function Exists(Hash: THash): boolean;
    function IncRef(Hash: THash): boolean;
    function DecRef(Hash: THash): boolean;
    procedure Clean; //cleans packets with refcount 0
    procedure Clear;
    procedure Lock;
    procedure UnLock;
  end;

  TQueue = class
    GlobalList: TPacketList;
    IncomingNet: THashList;
    IncomingClient: THashList;
    Incoming: THashList;
    ToClient: THashList;  //network layer queues up for client
    ClientBuf: THashList; //client assigns and calls back without distrubing network.
    ClientOut: THashList;
    CommonOutList: THashList;
    Fetching: THashList; //clients updates this to notice fetching in progress
    Persistant: THashList;
    Agents: THashList;
    BroadCast: THashList;
    Route: THashList;
    NetCache: THashList;
    IPList: TStrings; //list of known ip's
    Connected: TStrings; //list of IP's that is connected or connecting
    NodeList: TStrings; //list of known nodes
    Multiplex: TList;
    CS: TCriticalSection; //used to sync IPList and multiplex list
    //Stream
    constructor Create;
    destructor Destroy; override;
    procedure RegisterMultiplex(HashList: THashList);
    procedure UnregisterMultiplex(Hashlist: THashList);
    procedure AddHost(IPPort: string);
  end;


  TPastellaVersion = record
    case integer of
      0: (
        Raw: array[0..7] of byte;
      );
      1: (
        reserved1,
        reserved2,
        reserved3: word;
        MinorVersion,
        MajorVersion: byte;
      );
  end;

  TP2PSettings = class
    ListenPort: string;
    ListenIP: string;
    UID: string;
    DoListen: boolean;
    MinConnections: integer;
    MaxConnections: integer;
    NumConnections: integer;
    RejectSend: boolean;
    CS: TCriticalSection; //use this to share resources among threads
    constructor Create;
    destructor Destroy; override;
  end;

  TDelegator = class(TThread)
    Queue: TQueue;
    procedure Execute; override;
    procedure FilterPackets(HL: THashList);
  end;

  TConnector = class(TThread) //manages connections
    P2PSettings: TP2PSettings;
    Settings: TSettings;
    //CallBack: TCallBacks;
    Queue: TQueue;
    procedure Execute; override;
  end;

  TPastella = class;

  TClientCallBack = class(TThread)
    Queue: TQueue;
    Pastella: TPastella;
    procedure SyncCallBack;
    procedure Execute; override;
  end;

  TOnPastellaPacket = procedure(Sender: TObject; Packet: TPastellaMessage) of object;

  TPastella = class(TVisualServer)
  private
    FUID: string;
    FOnBroadcast: TOnPastellaPacket;
    FOnStreamPacket: TOnPastellaPacket;
    FOnRoutedPacket: TOnPastellaPacket;
    FOnPersistantPacket: TOnPastellaPacket;
    FOnAnyPacket: TOnPastellaPacket;
    FOnAgentPacket: TOnPastellaPacket;
    procedure SetOnAgentPacket(const Value: TOnPastellaPacket);
    procedure SetOnAnyPacket(const Value: TOnPastellaPacket);
    procedure SetOnBroadcast(const Value: TOnPastellaPacket);
    procedure SetOnPersistantPacket(const Value: TOnPastellaPacket);
    procedure SetOnRoutedPacket(const Value: TOnPastellaPacket);
    procedure SetOnStreamPacket(const Value: TOnPastellaPacket);
  protected
    Queue: TQueue;
    Delegator: TDelegator;
    Connector: TConnector;
    ClientCallBack: TClientCallBack;
    function SendPacket(Packet: TPastellaMessage; MsgType: TPacketType): boolean;
    function SendBroadcast(Packet: TPastellaMessage): boolean;
    function RoutePacket(Packet: TPastellaMessage): boolean;
    function SetPersistant(Packet: TPastellaMessage): boolean;
    function InsertAgent(Packet: TPastellaMessage): boolean;
    function InsertWalker(Packet: TPastellaMessage): boolean;
  public
    P2PSettings: TP2PSettings;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateRandomUID;
    procedure CreateUIDFromString(Value: string);
    function Send(Data: string; _To: string = ''; MsgType: TPacketType = ptBroadcast; Param: DWord = 0; Timestamp: double = -1): boolean;
    function BroadCast(Data: string): boolean;
    function Route(Data: string; _To: string): boolean;
    function Walker(Data: string; TTL: integer): boolean;
    function Connect(IP, Port: string): boolean; overload;
    function Connect(IPPort: string): boolean; overload;
    procedure SetUID(const Value: string);
  published
    property UID: string read FUID write SetUID;
    property OnAnyPacket: TOnPastellaPacket read FOnAnyPacket write SetOnAnyPacket;
    property OnBroadcast: TOnPastellaPacket read FOnBroadcast write SetOnBroadcast;
    property OnRoutedPacket: TOnPastellaPacket read FOnRoutedPacket write SetOnRoutedPacket;
    property OnPersistantPacket: TOnPastellaPacket read FOnPersistantPacket write SetOnPersistantPacket;
    property OnAgentPacket: TOnPastellaPacket read FOnAgentPacket write SetOnAgentPacket;
    property OnStreamPacket: TOnPastellaPacket read FOnStreamPacket write SetOnStreamPacket;
  end;

  TPastellaHandler = class(TServerHandler)
  public
    P2PSettings: TP2PSettings;
    Queue: TQueue;
    FOutgoing: boolean;
    FRemoteHost: string; //used for outgoing connections
    FRemotePort: string; // dito
    //local and remote hash lists:
    HLLocal, HLRemote: THashList;
    //temporary lists:
    InQueue, OutQueue, NewQueue: THashList;
    RejectQueue: THashList;
    Offered: THashList;
    //list that marks items that are fetched by this thread:
    Fetching: THashList;
    FOutput: string;
    FRoute: TStringList;
    procedure Handler; override;
    procedure CopyCustomVars; override;
    procedure ProcessIncoming;
    procedure ProcessOutgoing;
    procedure InitialOutgoing;
    procedure Output(Data: Pointer; Len: integer); overload;
    procedure Output(Data: string); overload;
    procedure SendOutputBuffer;
    function ReadCmd(var Cmd: TPastellaCmd): boolean;
    function SendCmd(Cmd: string; Count: integer; Param: integer = 0): boolean; overload;
    function SendCmd(Cmd: TPastellaCmd): boolean; overload;
    function ReadHashes(HashList: THashList; Count: integer): boolean;
    function ReadPacket(var PastellaPacket: TPastellaPacket): boolean;
    function WriteHashes(HashList: THashList): boolean;
    function WritePacket(PastellaPacket: TPastellaPacket): boolean;
    function WritePackets(HashList: THashList): boolean;
    function IsNewHash(Hash: THash): boolean;
    function Fetch(HashList: THashList): boolean;
    function Reject(HashList: THashList): boolean;
    function CheckAndFetch(HashList: THashList): boolean;
    function RejectPackets(HashList: THashList): boolean;
    function ReadDataPackets(Count: integer): boolean;
    function CreateData(Data: string; _To: string = ''; MsgType: TPacketType = ptBroadCast; Param: DWord = 0): boolean;
  end;

implementation

var
  PasVersion: TPastellaVersion = (reserved1: 0;
  reserved2: 0;
  reserved3: 0;
  MinorVersion: 0;
  MajorVersion: 1;
  );

const
  nilHash: THash = (#0, #0, #0, #0, #0, #0, #0, #0);

{ TListener }

{ TPastellaHandler }

procedure TPastellaHandler.CopyCustomVars;
begin

  Queue := TPastella(FSettings.Owner).Queue;
  P2PSettings := TPastella(FSettings.Owner).P2PSettings;
  FCallBacks := TPastella(FSettings.Owner).FCallBacks;
end;

function TPastellaHandler.Fetch(HashList: THashList): boolean;
var
  v: string;
  i: integer;
  h: THash;
begin
  Result := False;
  //send out fetch requests
  if HashList.Count = 0 then
    exit;
  v := '';
  //this doesn't work properly.
  //  for i:=0 to HashList.Count - 1 do
  //    v := v + HashList [i];
  //this does.
  SetLength(v, Hashlist.Count * SizeOf(THash));
  for i := 0 to HashList.Count - 1 do
  begin
    h := HashList[i];
    Move(h, v[1 + i * SizeOf(THash)], SizeOf(THash));
  end;
  Result := SendCmd('FTCH', HashList.Count);
  if Result then
  begin
    Output(v);
  end;
end;

procedure TPastellaHandler.Handler;
var
  connected: boolean;
  RemoteVersion: TPastellaVersion;
  Buf, v: string;
  HostIP: string;
  i: integer;
begin
  //Handle request
  //if FOutgoing is set, we must try to connect ourselves
  //elsewise, connection is accepted by listener.
  P2PSettings.CS.Enter;
  Inc(P2PSettings.NumConnections);
  P2PSettings.CS.Leave;

  HLLocal := THashList.Create(Queue.GlobalList); //incoming packets from client and other network
  NewQueue := THashList.Create(Queue.GlobalList); //temporary inqueue to avoid locking hllocal
  Offered := THashList.Create(Queue.GlobalList); //packets that are offered remotely, waiting for fetching or rejecting
  OutQueue := THashList.Create(Queue.GlobalList); //temp queue

  InQueue := THashList.Create(nil); //temporary inqueue to avoid locking hllocal
  HLRemote := THashList.Create(nil); //list of remotely known hashes
  Fetching := THashList.Create(nil); //local queue of hashes we are fetching
  RejectQueue := THashList.Create(nil); //queue of hashes remote site has rejected

  FRoute := TStringList.Create;

  //subscribing to global incoming new packets
  Queue.RegisterMultiplex(HLLocal);


  //connecting to remote site
  //or accepting connections
  //once connection is established, both hosts are fully equal,
  //that is: no 'server/client' relationship.

  Connected := False;
  //do interaction with remote host
  if FOutGoing then
  begin
    //try to connect with remote host
    HostIP := FRemoteHost + ':' + FRemotePort;
    Queue.CS.Enter;
    Queue.Connected.Add(HostIP);
    Queue.CS.Leave;
    FSock.Connect(FRemoteHost, FRemotePort);
    connected := FSock.LastError = 0;
    if connected then //broadcast this outgoing connection
    begin
      CreateData(HostIP, '', ptIPPort);
      FIPInfo.RemoteIP := FRemoteHost;
      FIPInfo.RemotePort := FRemotePort;
      //CallBackThreadMethod (OnConnect);
    end;
  end;

  if (not FOutGoing) or Connected then
  begin
    //send out 'PASTELLA' header
    //send out pastella version
    //receive those from remote
    //if match, continue.
    v := 'PASTELLA';
    FSock.SendString(v);
    FSock.SendBuffer(@PasVersion, SizeOf(TPastellaVersion));
    Buf := FSock.RecvBufferStr(8, 30000);
    if Buf = v then //remote also sais 'PASTELLA'
    begin
      FSock.RecvBufferEx(@RemoteVersion, SizeOf(TPastellaVersion), 30000);
      if RemoteVersion.MajorVersion = PasVersion.MajorVersion then
      begin
        //successfull handshake.
        //remember, both parties are equal
        //and the transmission is stateless.
        //however it is nice to first read all data from remote size.
        //also, take care that remote part may have a window size that may block transport.

        //Offer persistant variables to remote host
        InitialOutGoing;

        //Loop
        i := 0;
        while (not Terminated) and ((FSock.LastError = 0) or (FSock.LastError = 10060)) do
        begin
          //Main loop. processing all requests here

          ProcessIncoming;
          ProcessOutgoing;
          SendOutputBuffer;
          Inc(i);
          if (i mod 100) = 0 then
            //remove remote hashcache older than 1 minute
            HLRemote.RemoveOld(now - 1 / (24 * 60)); //1 minute
          if (i mod 101) = 0 then
            Offered.RemoveOld(now - 1 / (24 * 30)); //2 minutes
          sleep(60);
        end;

      end;
    end;
  end;

  Queue.UnregisterMultiplex(HLLocal);

  if FOutgoing then
    CallBackThreadMethod(OnDisConnect);
  HLLocal.Free;
  HLRemote.Free;
  NewQueue.Free;
  Offered.Free;
  OutQueue.Free;
  InQueue.Free;
  Fetching.Free;
  RejectQueue.Free;
  try
    P2PSettings.CS.Enter;
    Dec(P2PSettings.NumConnections);
    P2PSettings.CS.Leave;
  except
  end;
  Queue.CS.Enter;
  i := Queue.Connected.IndexOf(HostIP);
  if i >= 0 then
    Queue.Connected.Delete(i);
  Queue.CS.Leave;
end;

function TPastellaHandler.IsNewHash(Hash: THash): boolean;
var
  n: boolean;
begin
  //look at queues (Fetching, commonknown)
  Result := False;
  //are we fetching ourselves (should not happen)
  if Fetching.Exists(Hash) then
    exit;
  //is some other client fetching it at the moment?
  Queue.Fetching.Lock;
  n := Queue.Fetching.Exists(Hash);
  Queue.Fetching.UnLock;
  if n then
    exit;
  //do we already know it?
  Queue.GlobalList.Lock;
  n := Queue.GlobalList.Exists(Hash);
  Queue.GlobalList.Unlock;
  if n then
    exit;

  Result := True;
end;

procedure TPastellaHandler.ProcessIncoming;
var
  Cmd: TPastellaCmd;
begin
  if not ReadCmd(Cmd) then
  begin
    //Terminate;
    exit;
  end;
  if Cmd.Cmd = 'HASH' then //read remote hashlist
  begin
    ReadHashes(InQueue, Cmd.Count);
    CheckAndFetch(InQueue);
    HLRemote.CopyFrom(InQueue, False);
    InQueue.Clear;
  end
  else
  if Cmd.Cmd = 'REJE' then //read remote hashlist
  begin
    ReadHashes(InQueue, Cmd.Count);
    if P2PSettings.RejectSend then
      RejectPackets(InQueue);
    InQueue.Clear;
  end
  else
  if Cmd.Cmd = 'DATA' then //read remote data packets
  begin
    ReadDatapackets(Cmd.Count);
  end
  else
  if Cmd.Cmd = 'PING' then //ment for keep-alive
  begin
    SendCmd('PONG', 0);
  end
  else
  if Cmd.Cmd = 'PONG' then //silently ignore, ment for keep-alive
  begin
    //just ignore
  end
  else
  if Cmd.Cmd = 'FTCH' then //remote hosts likes to get packets
  begin
    InQueue.Clear;
    ReadHashes(InQueue, Cmd.Count);
    WritePackets(InQueue);
    HLRemote.CopyFrom(InQueue, False);
    Offered.Delete(InQueue);
    InQueue.Clear;
  end
  else
  begin //This is not valid data. break connection
    raise Exception.Create('debug - invalid command');
    Terminate;
  end;
end;

procedure TPastellaHandler.ProcessOutgoing;
begin
  //see if there are packets in persistant vars that are unknown remotely
  //see if there are packets in outgoing queue
  //match against list of remote known

  //HLLocal is our list of new packets
  //Match them against remote
  HLLocal.Lock;
  NewQueue.MoveFrom(HLLocal); //fetch from global sync inlist
  HLLocal.UnLock;

  OutQueue.Clear;
  OutQueue.MatchExclusive(NewQueue, HLRemote, True); //take only those that don't match
  NewQueue.Clear; //clears list, decrease refcount (outqueue is also refcounted)
  //  HLLocal.Clear;
  //limit max number of packets
  //OutQueue.Truncate (60, False);
  //delete from the list of offered hashes:

  if OutQueue.Count > 0 then
  begin
    //Offer remotely
    WriteHashes(OutQueue);
    HLRemote.CopyFrom(OutQueue, False);
    Offered.MoveFrom(OutQueue); //clears outqueue
  end;
end;

function TPastellaHandler.ReadCmd(var Cmd: TPastellaCmd): boolean;
var
  i: integer;
begin
  FillChar(Cmd, SizeOf(Cmd), #0);
  i := FSock.RecvBufferEx(@Cmd, SizeOf(Cmd), 0);
  if (i > 0) and (i <> SizeOf(Cmd)) then
    raise Exception.Create('debug');
  //we expect a timeout here
  Result := FSock.LastError = 0;
end;

function TPastellaHandler.ReadHashes(HashList: THashList; Count: integer): boolean;
var
  i, j: integer;
  Hash: THash;
begin
  HashList.Clear;
  Result := True;
  for i := 1 to Count do
  begin
    j := FSock.RecvBufferEx(@Hash, SizeOf(THash), 30000);
    if j <> SizeOf(Hash) then
      raise Exception.Create('debug');
    Result := FSock.LastError = 0;
    if not Result then
      break
    else
      HashList.Add(Hash);
  end;
end;

function TPastellaHandler.ReadPacket(var PastellaPacket: TPastellaPacket): boolean;
var
  i: integer;
begin
  Result := False;
  //first read header
  //set data to appropiate length if header is valid
  //read data
  i := FSock.RecvBufferEx(@PastellaPacket.header, SizeOf(TPacketHeader), 30000);
  if i <> SizeOf(TPacketHeader) then
    raise Exception.Create('debug');
  //    Terminate;
  if FSock.LastError <> 0 then
    raise Exception.Create('debug');
  //header must be reasonable sized
  if Util.HeaderValid(PastellaPacket.header) then
  begin
    //looks reasonable.
    //see if data is not of extraordinary size.
    if PastellaPacket.header.DataLength > 1024 * 1024 then
      raise Exception.Create('debug');
    //limit to 1Mb.
    SetLength(PastellaPacket.Data, PastellaPacket.header.DataLength);
    //      PastellaPacket.Data := FSock.RecvBufferStr (PastellaPacket.header.DataLength, 120000);
    FSock.RecvBufferEx(@PastellaPacket.Data[1], PastellaPacket.header.DataLength, 30000);
    if length(PastellaPacket.Data) <> PastellaPacket.header.DataLength then
      raise Exception.Create('debug');
    Result := FSock.LastError = 0;
  end
  else
    Terminate;
end;

function TPastellaHandler.SendCmd(Cmd: TPastellaCmd): boolean;
begin
  Output(@Cmd, SizeOf(TPastellaCmd));
  Result := True;
end;

function TPastellaHandler.SendCmd(Cmd: string; Count, Param: integer): boolean;
var
  Command: TPastellaCmd;
begin
  Result := False;
  if length(Cmd) <> SizeOf(Command.Cmd) then
    exit;
  if (Count < 0) or (Count > 65536) then
    exit;
  //Command.Cmd := Cmd;
  Move(Cmd[1], Command.Cmd[0], SizeOf(Command.Cmd));
  Command.Count := Count;
  Command.Param := Param;
  Result := SendCmd(Command);
end;

function TPastellaHandler.WritePackets(HashList: THashList): boolean;
var
  i, j: integer;
  pm: TPastellaMessage;
  Data: array of TPastellaPacket;
begin
  //loop all items in hashlist
  //send them to client
  Result := False;
  if HashList.Count = 0 then
    exit;
  //  Result := SendCmd ('DATA', HashList.Count);
  SetLength(Data, HashList.Count);
  Queue.GlobalList.Lock;
  j := 0;
  for i := 0 to HashList.Count - 1 do
  begin
    pm := Queue.GlobalList.GetPacket(HashList[i]);
    if Assigned(pm) then
    begin
      Data[j] := pm.Data;
      Inc(j);
    end
    else
      SetLength(Data, high(Data));
  end;
  Queue.GlobalList.UnLock;
  if high(Data) >= 0 then
  begin
    SendCmd('DATA', high(Data) + 1);
    for i := 0 to high(Data) do
      WritePacket(Data[i]);
  end;
end;

function TPastellaHandler.WriteHashes(HashList: THashList): boolean;
var
  i: integer;
  v: string;
  h: THash;
begin
  v := '';
  Result := False;
  HashList.Lock;
  if HashList.Count = 0 then
  begin
    HashList.Unlock;
    exit;
  end;
  SetLength(v, HashList.Count * SizeOf(THash));
  for i := 0 to HashList.Count - 1 do
  begin
    h := HashList[i];
    Move(h, v[1 + i * SizeOf(THash)], SizeOf(THash));
  end;
  SendCmd('HASH', HashList.Count);
  Output(v);
  Result := True;
  HashList.UnLock;
end;

function TPastellaHandler.WritePacket(PastellaPacket: TPastellaPacket): boolean;
begin
  if PastellaPacket.Header.DataLength <> Length(PastellaPacket.Data) then
    raise Exception.Create('debug');
  Output(@PastellaPacket.Header, SizeOf(TPacketHeader));
  Output(PastellaPacket.Data);
  Result := True;
end;

function TPastellaHandler.CheckAndFetch(HashList: THashList): boolean;
var
  i: integer;
  h: THash;
begin
  Result := True;
  //match list against known packets
  //fetch if not known.
  OutQueue.Clear;
  RejectQueue.Clear;
  //lock global fetching queue to avoid doubles.

  for i := 0 to HashList.Count - 1 do
  begin
    h := HashList[i];
    if IsNewHash(h) then
    begin
      //add to local fetchqueue
      Fetching.Add(h);
      //add to global fetchqueue
      Queue.Fetching.Lock;
      Queue.Fetching.Add(h);
      Queue.Fetching.UnLock;
      //add to outqueue
      OutQueue.Add(h);
    end
    else
      RejectQueue.Add(h);
  end;
  if OutQueue.Count > 0 then
    Fetch(OutQueue);
  if RejectQueue.Count > 0 then
    Reject(RejectQueue);
end;

function TPastellaHandler.ReadDataPackets(Count: integer): boolean;
var
  i: integer;
  pp: array of TPastellaPacket;
begin
  //read count datapackets
  Result := False;
  if (Count < 0) or (Count > 65530) then
    exit;
  SetLength(pp, Count);
  for i := 0 to Count - 1 do
  begin
    Result := ReadPacket(pp[i]);
    if not Result then
    begin
      //Roger. we got problem. remote host sending invalid stuff.
      raise Exception.Create('debug');
      Terminate;
      break;
    end;
  end;
  if Result then
  begin
    //add packets to global incoming list
    Queue.GlobalList.Lock;
    for i := 0 to high(pp) do
      //autolock, but we locked the whole array
    begin
      Queue.GlobalList.Add(pp[i]);
      Queue.GlobalList.IncRef(pp[i].Header.Hash);
    end;
    Queue.GlobalList.UnLock;
    //notify there is a new packet available:
    Queue.IncomingNet.Lock;
    for i := 0 to high(pp) do
      Queue.IncomingNet.Add(pp[i].header.hash);
    Queue.IncomingNet.Unlock;
    Queue.Fetching.Lock;
    //Remove packet from local fetch list
    for i := 0 to high(pp) do
      Fetching.Delete(pp[i].header.hash);
    //remove packet from global fetch list
    for i := 0 to high(pp) do
      Queue.Fetching.Delete(pp[i].header.hash, False);
    Queue.Fetching.UnLock;
  end;
end;

procedure TPastellaHandler.InitialOutgoing;
begin
  //fetch list of persistant variables
  OutQueue.CopyFrom(Queue.Persistant, False);
  //and offer them remotely
  if OutQueue.Count > 0 then
    WriteHashes(OutQueue);
end;

procedure TPastellaHandler.Output(Data: Pointer; Len: integer);
var
  l: integer;
begin
  if Len <= 0 then
    exit;
  //Add data to end of output buffer
  l := Length(FOutput);
  SetLength(FOutput, l + Len);
  Move(Data^, FOutput[l + 1], Len);
  //SendOutputBuffer;
end;

procedure TPastellaHandler.Output(Data: string);
begin
  FOutput := FOutput + Data;
  SendOutputBuffer;
end;

procedure TPastellaHandler.SendOutputBuffer;
var
  l: integer;
begin
  if FOutput <> '' then
  begin
    l := FSock.SendBuffer(@FOutput[1], length(FOutput));
    if l > 0 then
      Delete(FOutput, 1, l);
  end;
end;

function TPastellaHandler.Reject(HashList: THashList): boolean;
var
  v: string;
  i: integer;
  h: THash;
begin
  Result := False;
  //send out fetch requests
  if HashList.Count = 0 then
    exit;
  v := '';
  //this doesn't work properly.
  //  for i:=0 to HashList.Count - 1 do
  //    v := v + HashList [i];
  //this does.
  SetLength(v, Hashlist.Count * SizeOf(THash));
  for i := 0 to HashList.Count - 1 do
  begin
    h := HashList[i];
    Move(h, v[1 + i * SizeOf(THash)], SizeOf(THash));
  end;
  Result := SendCmd('REJE', HashList.Count);
  if Result then
  begin
    Output(v);
  end;
end;

function TPastellaHandler.RejectPackets(HashList: THashList): boolean;
begin
  Result := True;
  Offered.Delete(HashList, True);
end;

function TPastellaHandler.CreateData(Data, _To: string; MsgType: TPacketType; Param: DWord): boolean;
var
  Packet: TPastellaMessage;
begin
  Result := False;

  if (MsgType = ptRoute) and (length(_To) <> SizeOf(TNodeAddr)) then
    exit;
  if (MsgType <> ptRoute) and (_To <> '') then
    exit;
  if length(P2PSettings.UID) <> SizeOf(TNodeAddr) then
    exit;
  while length(_To) < SizeOf(TNodeAddr) do
    _To := _To + #0;
  Packet := TPastellaMessage.Create;
  Packet.origin := poNetwork;
  Packet.Data.Data := Data;
  Packet.CreateHash;
  Packet.Data.header.TTL := 0;
  if MsgType = ptWalker then
    Packet.Data.header.TTL := Param;
  Packet.Data.header.timestamp := now;
  Packet.Data.header.packettype := MsgType;
  Packet.Data.header.DataLength := Length(Data);
  Packet.Data.header.reserved := 0;
  move(_To[1], Packet.Data.header.AddrTo, SizeOf(TNodeAddr));
  move(P2PSettings.UID[1], Packet.Data.header.AddrFrom, SizeOf(TNodeAddr));

  Packet.CreateHash;
  //add packet to known queue:
  Queue.GlobalList.Lock;
  Queue.GlobalList.AddPacket(Packet);
  Queue.GlobalList.UnLock;

  Queue.IncomingClient.Lock;
  Queue.IncomingClient.Add(Packet.Data.Header.Hash);
  Queue.IncomingClient.UnLock;
end;

{ TPastella }

function TPastella.BroadCast(Data: string): boolean;
begin
  Send(Data, '', ptBroadCast);
  Result := True;
end;

function TPastella.Connect(IP, Port: string): boolean;
begin
  Result := Connect(IP + ':' + Port);
end;

function TPastella.Connect(IPPort: string): boolean;
begin
  Queue.AddHost(IPPort);
  Result := True;
end;

constructor TPastella.Create(AOwner: TComponent);
begin
  inherited;
  FSettings.FHasCustomVars := True;
  Queue := TQueue.Create;
  P2PSettings := TP2PSettings.Create;
  P2PSettings.MinConnections := 2;
  P2PSettings.MaxConnections := 16;
  P2PSettings.RejectSend := True;
  P2PSettings.UID := FUID;

  if not (csDesigning in ComponentState) then
  begin
    Delegator := TDelegator.Create(True);
    Connector := TConnector.Create(True);
    ClientCallBack := TClientCallBack.Create(True);
    Delegator.Queue := Queue;
    Connector.P2PSettings := P2PSettings;
    Connector.Settings := FSettings;
    Connector.Queue := Queue;
    ClientCallback.Pastella := Self;
    ClientCallBack.Queue := Queue;
    FClientType := TPastellaHandler;
    ListenPort := '8070';
    CreateRandomUID;
    Delegator.Resume;
    Connector.Resume;
    ClientCallBack.Resume;
    //launch the Delegator thread
    //launch the client handler thread
    //launch the connecter thread
  end;
end;

procedure TPastella.CreateRandomUID;
var
  v: string;
begin
  v := 'gpu' + IntToStr(gettickcount) + IntToStr(trunc(now));
  FUID := Util.MakeHash(v);
  P2PSettings.UID := FUID;
end;

procedure TPastella.CreateUIDFromString(Value: string);
var
  v: string;
  i: integer;
begin
  v := MD5(Value);
  if length(v) = 16 then //should be
  begin
    //make 8-byte out of 16-byte using simple xor
    SetLength(FUID, 8);
    for i := 1 to length(FUID) do
      FUID[i] := char(byte(v[i]) xor byte(v[i + 8]));
  end
  else
    FUID := '';
  P2PSettings.UID := FUID;
end;

destructor TPastella.Destroy;
var
  i: integer;
begin
  Active := False;
  //signal all clients to terminate
  with FSettings.FClients.LockList do
  begin
    for i := 0 to Count - 1 do
      TServerHandler(Items[i]).Terminate;
  end;
  if not (csDesigning in ComponentState) then
  begin
    Delegator.Terminate;
    Connector.Terminate;
    ClientCallBack.Terminate;
    Delegator.WaitFor;
    Connector.WaitFor;
    ClientCallBack.WaitFor;
    Delegator.Free;
    Connector.Free;
    ClientCallBack.Free;
  end;
  Queue.Free;
  P2PSettings.Free;
  inherited;
end;

function TPastella.InsertAgent(Packet: TPastellaMessage): boolean;
begin
  Result := SendPacket(Packet, ptAgent);
end;

function TPastella.InsertWalker(Packet: TPastellaMessage): boolean;
begin
  Result := SendPacket(Packet, ptWalker);
end;

function TPastella.Route(Data, _To: string): boolean;
begin
  Result := Send(Data, _To, ptRoute);
end;

function TPastella.RoutePacket(Packet: TPastellaMessage): boolean;
begin
  Result := SendPacket(Packet, ptRoute);
end;

function TPastella.Send(Data: string; _To: string = ''; MsgType: TPacketType = ptBroadCast; Param: DWord = 0; Timestamp: double = -1): boolean;
var
  pm: TPastellaMessage;
begin
  Result := False;
  if FUID = '' then
    CreateRandomUID;
  if (MsgType = ptRoute) and (length(_To) <> SizeOf(TNodeAddr)) then
    exit;
  if (MsgType <> ptRoute) and (_To <> '') then
    exit;
  if length(FUID) <> SizeOf(TNodeAddr) then
    exit;
  while length(_To) < SizeOf(TNodeAddr) do
    _To := _To + #0;
  pm := TPastellaMessage.Create;
  pm.origin := poClient;
  pm.Data.Data := Data;
  pm.CreateHash;
  pm.Data.header.TTL := 0;
  if MsgType = ptWalker then
    pm.Data.header.TTL := Param;
  if Timestamp = -1 then
    pm.Data.header.timestamp := now
  else
    pm.Data.header.timestamp := Timestamp;
  pm.Data.header.packettype := MsgType;
  pm.Data.header.DataLength := Length(Data);
  pm.Data.header.reserved := 0;
  move(_To[1], pm.Data.header.AddrTo, SizeOf(TNodeAddr));
  move(FUID[1], pm.Data.header.AddrFrom, SizeOf(TNodeAddr));
  SendPacket(pm, MsgType);
end;

function TPastella.SendBroadcast(Packet: TPastellaMessage): boolean;
begin
  Result := SendPacket(Packet, ptBroadcast);
end;

function TPastella.SendPacket(Packet: TPastellaMessage; MsgType: TPacketType): boolean;
begin
  //add this message to the global message queue
  if Assigned(Packet) and Packet.Valid then
  begin
    Packet.CreateHash;
    //add packet to known queue:
    Queue.GlobalList.Lock;
    Queue.GlobalList.AddPacket(Packet);
    Queue.GlobalList.UnLock;

    Queue.IncomingClient.Lock;
    Queue.IncomingClient.Add(Packet.Data.Header.Hash);
    Queue.IncomingClient.UnLock;

  end;
  Result := True;
end;

procedure TPastella.SetOnAgentPacket(const Value: TOnPastellaPacket);
begin
  FOnAgentPacket := Value;
end;

procedure TPastella.SetOnAnyPacket(const Value: TOnPastellaPacket);
begin
  FOnAnyPacket := Value;
end;

procedure TPastella.SetOnBroadcast(const Value: TOnPastellaPacket);
begin
  FOnBroadcast := Value;
end;

procedure TPastella.SetOnPersistantPacket(const Value: TOnPastellaPacket);
begin
  FOnPersistantPacket := Value;
end;

procedure TPastella.SetOnRoutedPacket(const Value: TOnPastellaPacket);
begin
  FOnRoutedPacket := Value;
end;

procedure TPastella.SetOnStreamPacket(const Value: TOnPastellaPacket);
begin
  FOnStreamPacket := Value;
end;

function TPastella.SetPersistant(Packet: TPastellaMessage): boolean;
begin
  Result := SendPacket(Packet, ptPersistant);
end;

procedure TPastella.SetUID(const Value: string);
begin
  FUID := Value;
  P2PSettings.UID := FUID;
end;

function TPastella.Walker(Data: string; TTL: integer): boolean;
begin
  Result := Send(Data, '', ptWalker, TTL);
end;

{ TPacketList }

function TPacketList.Add(Packet: TPastellaMessage): boolean;
begin
  Lock;
  Result := AddPacket(Packet);
  UnLock;
end;

function TPacketList.Add(Packet: TPastellaPacket): boolean;
var
  pm: TPastellaMessage;
begin
  pm := TPastellaMessage.Create;
  pm.origin := poUnknown;
  pm.Data := Packet;
  Result := Add(pm);
end;

function TPacketList.AddPacket(Packet: TPastellaMessage): boolean;
begin
  //assume we are locked.
  Packet.tickcount := GetTickCount;
  PacketList[byte(Packet.Data.header.Hash[0])].Add(Packet);
  Result := True;
end;

procedure TPacketList.Clean;
var
  i, j: integer;
  pm: TPastellaMessage;
  tc: integer;
begin
  //clear all packets with refcount of zero
  CS.Enter;
  tc := GetTickCount;
  for i := 0 to high(PacketList) do
  begin
    for j := PacketList[i].Count - 1 downto 0 do
    begin
      pm := TPastellaMessage(PacketList[i][j]);
      if (pm.RefCount = 0) or (not (pm.Data.header.packettype in [ptPersistant]) and //non-persistant packets live
        ((tc - pm.tickcount) > 5 * 60 * 1000)) or //5 minutes max.
        ((pm.Data.header.packettype in [ptPersistant]) and //peristant packets live
        ((tc - pm.tickcount) > 24 * 60 * 60 * 1000)) //24 hour
      then
      begin
        pm.Free;
        PacketList[i].Delete(j);
      end;
    end;
  end;
  CS.Leave;
end;

procedure TPacketList.Clear;
var
  i, j: integer;
begin
  //clear all packets
  CS.Enter;
  for i := 0 to high(PacketList) do
  begin
    for j := 0 to PacketList[i].Count - 1 do
      TPastellaMessage(PacketList[i][j]).Free;
    PacketList[i].Clear;
  end;
  CS.Leave;
end;

constructor TPacketList.Create;
var
  i: integer;
begin
  for i := 0 to high(PacketList) do
    PacketList[i] := TList.Create;
  CS := TCriticalSection.Create;
end;

function TPacketList.DecRef(Hash: THash): boolean;
var
  pm: TPastellaMessage;
  i: TPacketListIndex;
begin
  Lock;
  i := FindPacket(Hash);
  pm := GetPacket(i);
  if Assigned(pm) then
  begin
    if pm.RefCount > 0 then
    begin
      Dec(pm.RefCount);
      //if refcount is zero we may delete it
      if pm.RefCount = 0 then
        DeletePacket(i);
      Result := True;
    end
    else
    begin
      //In fact we have serious problem here,
      //because this should never happen.
      //good place for breakpoint.
      Result := False;
    end;
  end
  else
    Result := False;
  UnLock;
end;

procedure TPacketList.DeletePacket(Index: TPacketListIndex);
begin
  if (Index.Index >= 0) and (Index.Index < PacketList[Index.Table].Count) then
  begin
    TPastellaMessage(PacketList[Index.Table][Index.Index]).Free;
    PacketList[Index.Table].Delete(Index.Index);
  end;
end;

procedure TPacketList.DeletePacket(Hash: THash);
begin
  CS.Enter;
  DeletePacket(FindPacket(Hash));
  CS.Leave;
end;



destructor TPacketList.Destroy;
var
  i: integer;
begin
  for i := 0 to high(PacketList) do
  begin
    PacketList[i].Clear;
    PacketList[i].Free;
  end;
  CS.Free;
end;

function TPacketList.Exists(Hash: THash): boolean;
begin
  Result := FindPacket(Hash).Index >= 0;
end;

function TPacketList.FindPacket(Hash: THash): TPacketListIndex;
var
  i: integer;
begin
  //loop all packets
  //if hash matches, break
  Lock;
  Result.Table := byte(Hash[0]);
  Result.Index := -1;
  for i := 0 to PacketList[Result.Table].Count - 1 do
    if TPastellaMessage(PacketList[Result.Table][i]).Data.Header.Hash = Hash then
    begin
      Result.Index := i;
      break;
    end;
  UnLock;
end;

function TPacketList.GetPacket(Hash: THash): TPastellaMessage;
begin
  //returns message
  Lock;
  Result := GetPacket(FindPacket(Hash));
  UnLock;
end;

function TPacketList.GetPacket(Index: TPacketListIndex): TPastellaMessage;
begin
  if (Index.Index >= 0) and (Index.Index < PacketList[Index.Table].Count) then
    Result := PacketList[Index.Table][Index.Index]
  else
    Result := nil;
end;

function TPacketList.IncRef(Hash: THash): boolean;
var
  pm: TPastellaMessage;
begin
  Lock;
  pm := GetPacket(Hash);
  if Assigned(pm) then
  begin
    Inc(pm.RefCount);
    Result := True;
  end
  else
    Result := False;
  Unlock;
end;

procedure TPacketList.Lock;
begin
  CS.Enter;
end;

procedure TPacketList.UnLock;
begin
  CS.Leave;
end;

{ TConnecter }

procedure TConnector.Execute;
var
  i, l: integer;
  ip: string;
  h: TPastellahandler;
  Tried: TStrings;
begin
  i := 0;
  Tried := TStringList.Create;
  while not Terminated do
  begin
    sleep(500);
    Inc(i);
    if i mod 4 = 0 then
      with P2PSettings do
      begin
        //atomic, read only (outside scope of CS)
        if TPastella(Settings.Owner).Active and (NumConnections < MinConnections) then
        begin
          Queue.CS.Enter;
          //Check if enough threads are running
          if Queue.IPList.Count > 0 then
            ip := Queue.IPList[random(Queue.IPList.Count)];
          if Queue.Connected.Indexof(ip) >= 0 then
            ip := '';
          Queue.CS.Leave;

          if Tried.Indexof(ip) >= 0 then
            ip := '';

          if ip <> '' then //add to connect queue
          begin
            //launch connect thread
            h := TPastellaHandler.Create(True);
            //h.FSettings.Owner := Pastella;
            h.P2PSettings := P2PSettings;
            h.FSettings := Settings;
            h.CopyCustomVars;
            h.FOutgoing := True;
            h.FRemoteHost := SeparateLeft(ip, ':');
            h.FRemotePort := SeparateRight(ip, ':');
            h.FSock := TTCPBlockSocket.Create;
            h.FSock.SetRemoteSin(h.FRemoteHost, h.FRemotePort);
            h.Resume;
            Tried.Add(ip);
            ip := '';
          end;
        end;
      end;
    if (i mod 900 = 0) and (Tried.Count > 0) then //each 7 minutes
      for l := 0 to Tried.Count div 2 do //delete first half of list
        Tried.Delete(0);
  end;
  Tried.Clear;
end;

{ THashList }

constructor THashList.Create(Packets: TPacketList);
begin
  inherited Create;
  PacketList := Packets;
  FItems := TList.Create;
  CS := TCriticalSection.Create;
end;

destructor THashList.Destroy;
begin
  Clear;
  FItems.Free;
  CS.Free;
  inherited;
end;


function THashList.Add(Hash: THash; DoRefCount: boolean = True): boolean;
var
  i, r: integer;
  hi, lo, mi: integer;
begin
  r := -1;
  lo := 0;
  if FItems.Count = 0 then
    FItems.Add(THashObj.Create(Hash))
  else
  begin
    hi := FItems.Count - 1;
    while hi > lo do
    begin
        {
          if THashObj(FItems[lo]).Hash = Hash then
            begin
              r := lo;
              break;
            end;
          if THashObj(FItems[hi]).Hash = Hash then
            begin
              r := hi;
              break;
            end;
         }
      mi := lo + (hi - lo) div 2;
      if THashObj(FItems[mi]).Hash < Hash then
        lo := mi + 1
      else
        hi := mi;
    end;
    FItems.Insert(hi, THashObj.Create(Hash));
  end;

  if DoRefCount and Assigned(PacketList) then
    PacketList.IncRef(Hash); //ignore result (?)
  Result := True;
end;

procedure THashList.Clear(DoRefCount: boolean = True);
var
  i: integer;
begin
  if DoRefCount and Assigned(PacketList) then
    for i := 0 to Count - 1 do
      PacketList.DecRef(Items[i]);
  for i := 0 to Count - 1 do
    THashObj(FItems[i]).Free;
  FItems.Clear;
end;

procedure THashList.CopyClearFrom(HashList: THashList);
var
  i: integer;
begin
  //This eliminates the need for reference counting
  //So should be a lot faster
  //TList does not have addmultiple function
  FItems.Capacity := FItems.Count + HashList.FItems.Count;
  for i := 0 to HashList.FItems.Count - 1 do
  begin
    //      FItems.Add (HashList.FItems[i]);
    Add(HashList[i], False);
  end;
  HashList.Clear(False);
  //  HashList.FItems.Clear;
end;

procedure THashList.CopyFrom(HashList: THashList; DoRefCount: boolean = True);
var
  i: integer;
begin
  for i := 0 to HashList.Count - 1 do
    Add(HashList[i], DoRefCount);
end;


function THashList.Delete(Hash: THash; DoRefCount: boolean = True): boolean;
var
  i: integer;
begin
  try
    i := IndexOf(Hash);
    if i >= 0 then
      Delete(i, DoRefCount);
  except
  end;
  Result := True;
end;

procedure THashList.CopyExclusive(HashList: THashList);
var
  i: integer;
begin
  //Find items from this hashlist that are not listed in Self.
  //copy them
  for i := 0 to HashList.Count - 1 do
    if not Exists(HashList[i]) then
      Add(HashList[i]);
end;

function THashList.Exists(Hash: THash): boolean;
begin
  Result := IndexOf(Hash) >= 0;
end;

procedure THashList.Inclusive(HashList: THashList; var Strings: TStrings);
begin
  //Find items in this hashlist that are shared with HashList
end;

procedure THashList.Lock;
begin
  CS.Enter;
end;

function THashList.RemoveOld(TimeStamp: double): integer;
var
  i: integer;
begin
  //delete all items older then timestamp
  Result := Count;
  Lock;
  for i := Count - 1 downto 0 do
  begin
    if THashObj(FItems[i]).TimeStamp < TimeStamp then
      Delete(i);
  end;
  Unlock;
end;

procedure THashList.UnLock;
begin
  CS.Leave;
end;

function THashList.Count: integer;
begin
  Result := FItems.Count;
end;

procedure THashList.MoveFrom(HashList: THashList);
begin
  //copyclear ignores refernce counting, speeding things up
  //it is internal alias for movefrom
  CopyClearFrom(HashList);
end;

procedure THashList.MoveTo(HashList: THashList);
begin
  hashlist.CopyClearFrom(Self);
end;

function THashList.GetItems(Index: integer): THash;
begin
  if (Index >= 0) and (Index < Count) then
    Result := THashObj(FItems[Index]).Hash
  else
    Result := nilHash;
end;

procedure THashList.SetItems(Index: integer; const Value: THash);
begin
  // we don't allow this. try adding an item.
end;

function THashList.IndexOf(Hash: THash): integer;
var
  i: integer;
  hi, lo, mi: integer;
begin
  Result := -1;
  {
  for i:=0 to Count - 1 do
    if THashObj(FItems[i]).Hash = Hash then
      begin
        Result := i;
        break;
      end;
  }

  lo := 0;
  hi := FItems.Count - 1;
  while hi > lo do
  begin
    if THashObj(FItems[hi]).Hash = Hash then
    begin
      Result := hi;
      break;
    end;
    if THashObj(FItems[lo]).Hash = Hash then
    begin
      Result := lo;
      break;
    end;
    mi := lo + (hi - lo) div 2;
    if THashObj(FItems[mi]).Hash < Hash then
      lo := mi + 1
    else
      hi := mi;
  end;
end;

function THashList.Delete(i: integer; DoRefCount: boolean): boolean;
begin
  Result := False;
  if (i < 0) or (i >= Count) then
    exit;
  if DoRefCount and Assigned(PacketList) then
    PacketList.DecRef(THashObj(FItems[i]).Hash);
  THashObj(FItems[i]).Free;
  FItems.Delete(i);
end;

function THashList.AddUnique(Hash: THash): boolean;
begin
  Result := not Exists(Hash);
  if not Result then
    Result := Add(Hash);
end;

procedure THashList.MatchExclusive(Source, Match: THashList; DoRefCount: boolean = True);
var
  i: integer;
  hash: THash;
begin
  //copy only packets that are available in Source and not in Match
  Clear(DoRefCount);
{  for i:=0 to Source.Count - 1 do
    if not Match.Exists(Source[i]) then
      Add (Source[i], DoRefCount);
}
  Source.ForInit;
  while Source.ForEach(hash) do
  begin
    if not Match.Exists(hash) then
      Add(hash, DoRefCount);
  end;
end;

procedure THashList.Truncate(Number: integer; DoRefCount: boolean = True);
var
  i: integer;
begin
  if Number < 0 then
    exit;

  //Truncate list to count items
  for i := Count - 1 downto Number do
    Delete(i, DoRefcount);
end;

function THashList.Delete(HashList: THashList; DoRefCount: boolean): boolean;
var
  i: integer;
begin
  for i := 0 to HashList.Count - 1 do
    Delete(HashList[i], DoRefCount);
  //returns always true (...)
  Result := True;
end;

function THashList.ForEach(var Hash: THash): boolean;
  //this method gives an universal method to loop all items
  //even if internal list structure may change.
begin
  Result := FForEachIndex < Count;
  if Result then
    Hash := Items[FForEachIndex]
  else
    Hash := nilHash;
  Inc(FForEachIndex);
end;

function THashList.ForInit: boolean;
begin
  FForEachIndex := 0;
  Result := True;
end;

function THashList.GetUserData(Hash: THash): Pointer;
var
  i: integer;
begin
  Result := nil;
  i := IndexOf(Hash);
  if i >= 0 then
    Result := THashObj(FItems[i]).UserData;
end;

procedure THashList.SetUserData(Hash: THash; const Value: Pointer);
var
  i: integer;
begin
  i := IndexOf(Hash);
  if i >= 0 then
    THashObj(FItems[i]).UserData := Value;
end;

{ TQueue }

procedure TQueue.AddHost(IPPort: string);
begin
  CS.Enter;
  try
    if IPList.IndexOf(IPPort) < 0 then
      IPList.Add(IPPort);
  finally
    CS.Leave;
  end;
end;

constructor TQueue.Create;
begin
  inherited Create;
  GlobalList := TPacketList.Create;
  IncomingNet := THashList.Create(GlobalList);
  IncomingClient := THashList.Create(GlobalList);
  Incoming := THashList.Create(GlobalList);
  ToClient := THashList.Create(GlobalList);
  ClientBuf := THashList.Create(GlobalList);
  ClientOut := THashList.Create(GlobalList);
  CommonOutList := THashList.Create(GlobalList);
  Fetching := THashList.Create(GlobalList);
  Persistant := THashList.Create(GlobalList);
  Agents := THashList.Create(GlobalList);
  BroadCast := THashList.Create(GlobalList);
  Route := THashList.Create(GlobalList);
  NetCache := THashList.Create(GlobalList);
  IPList := TStringList.Create;
  Connected := TStringList.Create;
  NodeList := TStringList.Create;
  MultiPlex := TList.Create;
  CS := TCriticalSection.Create;
end;

destructor TQueue.Destroy;
begin
  IncomingNet.Free;
  IncomingClient.Free;
  Incoming.Free;
  ToClient.Free;
  ClientBuf.Free;
  ClientOut.Free;
  CommonOutList.Free;
  Fetching.Free;
  Persistant.Free;
  Agents.Free;
  BroadCast.Free;
  Route.Free;
  NetCache.Free;
  IPList.Free;
  Connected.Free;
  Multiplex.Free;
  GlobalList.Free;
  CS.Free;
  inherited;
end;

procedure TQueue.RegisterMultiplex(HashList: THashList);
begin
  CS.Enter;
  if Multiplex.IndexOf(HashList) < 0 then
    Multiplex.Add(HashList);
  CS.Leave;
end;

procedure TQueue.UnregisterMultiplex(Hashlist: THashList);
var
  i: integer;
begin
  CS.Enter;
  i := Multiplex.IndexOf(HashList);
  if i >= 0 then
    Multiplex.Delete(i);
  CS.Leave;
end;

{ TDelegator }

procedure TDelegator.Execute;
var
  i: integer;
begin
  //loops the global packet out queue
  //and distributes it to all connected nodes
  //and to the client.
  //also imports request from the client
  //and adds them to the global queue.
  while not Terminated do
    with Queue do
    begin

      IncomingNet.Lock;
      Incoming.MoveFrom(IncomingNet);
      IncomingNet.UnLock;

      IncomingClient.Lock;
      Incoming.MoveFrom(IncomingClient);
      IncomingClient.UnLock;

      if Incoming.Count > 0 then
      begin
        //Assign these among all net handlers and among the client
        //for our application:

        ToClient.Lock;
        ToClient.CopyFrom(Incoming);
        ToClient.UnLock;

        //client gets everything returned
        //but we also need fetch special data
        //and verify TTL before sending to network

        FilterPackets(Incoming);

        //loop all network handlers
        CS.Enter;
        try
          for i := 0 to MultiPlex.Count - 1 do
            with THashList(MultiPlex[i]) do
            begin
              Lock;
              CopyFrom(Incoming);
              UnLock;
            end;
        finally
          CS.Leave;
        end;
        Incoming.Clear;
      end;
      sleep(60);
    end;
end;

procedure TDelegator.FilterPackets(HL: THashList);
var
  Hash: THash;
  p: TPastellaMessage;
  r: boolean;
  i: integer;
begin
  for i := HL.Count - 1 downto 0 do
  begin
    Hash := HL[i];
    p := HL.PacketList.GetPacket(Hash);
    if Assigned(p) then
    begin
      //Also, increase TTL
      Inc(p.Data.header.TTL);
      r := False;
      if p.Data.header.DataLength > 512 * 1024 then
        //1/2 MB packet, we are not going to send this.
        r := True
      else
      if p.Data.header.TTL >= 64 then
        r := True
      else
      //TPacketType = (ptNull, ptIPPort, ptBroadcast, ptRoute, ptAgent, ptWalker, ptPersistant, ptStream);
      if p.Data.header.packettype = ptNull then
        r := True
      else
      if p.Data.header.packettype = ptAgent then
      //AgentsIn.Add ()..
      else
      if p.Data.header.packettype = ptIpPort then
      begin
        //add to global list
        Queue.CS.Enter;
        if (length(p.Data.Data) < 50) and (Queue.IPList.IndexOf(p.Data.Data) < 0) then
          Queue.IPList.Add(p.Data.Data);
        Queue.CS.Leave;
      end
      else
      if p.Data.header.packettype = ptPersistant then
      begin
        Queue.CS.Enter;
        //some scheduler should clear persistant data after certain interval
        Queue.Persistant.Add(p.Data.header.Hash);
        Queue.CS.Leave;
      end
      else
      if p.Data.header.packettype = ptRoute then
      begin
        //find connection with shortest path

      end
      else
      if p.Data.header.packettype = ptNode then
      begin
        //Node exist, expected path length
        if length(p.Data.Data) = SizeOf(THash) then
        begin
          //                  Queue.CS.Enter;
          //                  Queue.NodeList.Add  (p.Data.Data);
          //                  Queue.CS.Leave;
          //Queue.Route.
        end;
      end;
      if r then //packet needs to be removed
        HL.Delete(Hash);
    end;
  end;
end;

{ TPastellaMessage }

procedure TPastellaMessage.CreateHash;
begin
  //create a hash from the data
  Data.header.Hash := Util.MakeHash(Data.header.AddrFrom + Data.header.AddrTo +
    StringReplace(FloatToStr(Data.header.TimeStamp), DecimalSeparator, '.', []) +
    Data.Data);
end;

function TPastellaMessage.HeaderValid: boolean;
begin
  Result := Util.HeaderValid(Data.Header);
end;

function TPastellaMessage.Valid: boolean;
begin
  //checks header and matches data size
  Result := HeaderValid and (Data.header.DataLength = length(Data.Data));
end;

{ TClientCallBack }

procedure TClientCallBack.Execute;
begin
  //loop queue
  //if any events for client, parse them

  while not Terminated do
  begin
    //if queue.count>0
    //synchronize synccallback
    sleep(60);
    Queue.ToClient.Lock;
    if Queue.ToClient.Count > 0 then
      Queue.ClientBuf.MoveFrom(Queue.ToClient);
    Queue.ToClient.UnLock;
    if Queue.ClientBuf.Count > 0 then
      synchronize(SyncCallBack);
  end;
end;

procedure TClientCallBack.SyncCallBack;
var
  pm: TPastellaMessage;
  hash: THash;
begin
  //synchronize with parent
  try
    with Queue.ClientBuf do
    begin
      ForInit;
      //      if Assigned (Pastella.OnAnyPacket) then
      while ForEach(hash) do
      begin
        try
          Queue.GlobalList.Lock;
          pm := Queue.GlobalList.GetPacket(Hash);
          Queue.GlobalList.UnLock;
          if Assigned(pm) then
          begin
            if Assigned(Pastella.OnAnyPacket) and not (csDestroying in Pastella.ComponentState) then
              Pastella.OnAnyPacket(Pastella, pm);
            //case pm.header.packettype of
            //broadcast, agent etc.
          end;
        except
        end;
      end;
      Clear; //decreases refcount
    end;
  except
  end;
end;

{ Util }

class function Util.HeaderValid(Header: TPacketHeader): boolean;
begin
  //checks header for abnormalities.

  //this can be optimized, but now for max readability.
  Result := False;
  if (header.TTL > 256) and (header.packettype <> ptWalker) then
    exit;
  //your clocks should be about synchrone.
  if (header.timestamp > now + 2) or (header.timestamp < now - 5) then
    exit;
  if header.packettype = ptNull then
    exit;
  if byte(header.packettype) > byte(high(TPacketType)) then
    exit;
  if header.reserved <> 0 then
    exit;
  Result := True;

end;

class function Util.MakeHash(Data: string): THash;
var
  v: string;
  i: integer;
begin
  Result := nilHash;
  v := MD5(Data);
  if length(V) <> 16 then
    exit;
  for i := 1 to 8 do
    v[i] := char(byte(v[i]) xor byte(v[i + 8]));
  Move(v[1], Result, SizeOf(Result));
  if Result = nilHash then //should be very rare, but just in case
    Result := MakeHash('_' + Data);
end;

{ THashObj }

constructor THashObj.Create(Value: THash);
begin
  inherited Create;
  Hash := Value;
  TimeStamp := now;
end;

{ TP2PSettings }

constructor TP2PSettings.Create;
begin
  inherited;
  CS := TCriticalSection.Create;
end;

destructor TP2PSettings.Destroy;
begin
  CS.Free;
  inherited;
end;

end.
(*
Proposed protocol

//all connection is done based on 4-bytes (or 8-byte) block size.

after conenction, 'PASTELLA' is send.
after that, 8 bytes follow with protocol version.

//if all is set up, clients are immediately 'equal' to eachother.
//also meaning: messages are sent and received asynchronous.
//a confirmation is never sent.
//if some peer sents invalid packets
//the other peer should just disconnect
//and remains the rights no longer connect that client.
//this enforces a strict protocol.

//The idea:
content of packets is entirely the responsibility of the application.
the communications layer simply does not care what is in the packets.
a reasonable size for maximum packet size may be set.





*)
