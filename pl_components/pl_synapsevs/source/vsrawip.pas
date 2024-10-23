
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsRawIP;


interface

uses
 {$IFDEF WINDOWS}
  Windows,
 {$ENDIF}
  Classes, SysUtils, synautil,
  blcksock, synsock, Dialogs;

//winsock 2 declarations
const
  SIO_RCVALL = $98000001;

//raw IP packet related constants:
const //Ethernet header flags:
  EthernetType2 = $0800;
  EthernetTypeARP = $0806;

  //IP header flages
  TCP_Flag_Unused = 1 shl 15;
  TCP_Flag_Dont_Fragment = 1 shl 14;
  TCP_Flag_More = 1 shl 13;

  //ICMP predefined protocol constants:
  ICMP_Echo_Reply = 0;
  ICMP_Dest_Unreachable = 3;
  ICMP_Source_Quench = 4;
  ICMP_Redirect = 5;
  ICMP_Echo_Request = 8;
  ICMP_Router_Advertisement = 9;
  ICMP_Router_Sollicitation = 10;
  ICMP_Time_Exceeded = 11;
  ICMP_Parameter_Problem = 12;
  ICMP_Timestamp_Request = 13;
  ICMP_Timestamp_Reply = 14;
  ICMP_Information_Request = 15;
  ICMP_Information_Reply = 16;
  ICMP_Address_Mask_Request = 17;
  ICMP_Address_Mask_Reply = 18;
  ICMP_TraceRoute = 30;

  //TCP header flags:
  TCP_FIN = 1;  //Connection control
  TCP_SYN = 2;  //Synchronize, connection control, syncing sequence numbers
  TCP_RST = 4;  //RESET, end of connection
  TCP_PSH = 8;  //PUSH
  TCP_ACK = 16; //Acknowledgement number is valid
  TCP_URG = 32; //Urgent pointer is valid

  //TCP OPTION FIELD VALUES
  TCPOPT_END_OF_OPTIONS = 0;
  TCPOPT_NO_OPERATION = 1;
  TCPOPT_MAX_SEQMENT_SIZE = 2;
  TCPOPT_WINDOW_SCALE = 3;
  TCPOPT_SELECTIVE_ACK = 4;
  TCPOPT_TIMESPAMP = 8;

  //IP HEADER OPTION VALUES
  IPOPT_END_OF_OPTIONS = 0;
  IPOPT_NO_OPERATION = 1;
  IPOPT_RECORD_ROUTE = 7;
  IPOPT_TIMESTAMP = 68;
  IPOPT_LOOSE_SOURCE_ROUTE = 131;
  IPOPT_STRICT_SOURCE_ROUTE = 137;



// IP / TCP / UDP / ICMP / ARP header structures:
//only IP and UDP headers are tested.
//for other structures i cannot guarantee they are correct.

type
  Byte6 = array[0..5] of byte;
  Byte3 = array[0..2] of byte;

  TIPHeader = packed record
    case integer of
      0: (
        VerLen: byte;
        TOS: byte;
        TotalLen: word;
        Identifier: word;
        FragOffsets: word;
        TTL: byte;
        Protocol: byte;
        CheckSum: word;
        SourceIp: DWORD;
        DestIp: DWORD;
        //          Options: DWORD; //no options by default, header size 5 DWords
      );
      1: (Raw: array[0..9] of word);
  end;
  PIPHeader = ^TIPHeader;

  TArpHeader = packed record
    //Ethernet type typically $0806
    Hardware: word;
    Protocol: word;
    HardwareAddressLength: byte;
    ProtocolLength: byte;
    Operation: word;
    SenderHWAddress: Byte6;
    SenderIPAddress: DWord;
    TargetHWAddress: Byte6;
    TargetIPAddress: DWord;
  end;

  TEthernetHeader = packed record
    DestMacAddress: Byte6; //leave $FF FFFF for broadcasts
    SourceMacAddress: Byte6;
    ProtocolType: word; //mostly is EthernetType2
  end;
{   An EthernetPacket typically looks as follows:
      * TEthernetHeader
      * TIPHeader
      * TCP or UDP header
      * User data (TCP/UDP)
      * CRC checksum, DWord. wrapped by the ethernet protocol.
    so there is no real use for checksumming inside the user data.
}
  TICMPHeader = packed record
    ICMPtype: byte;
    code: byte;
    Checksum: word;
  end;
  PICMPHeader = ^TICMPHeader;

  TIPICMPHeader = packed record
    IPHeader: TIPHeader;
    ICMPHeader: TICMPHeader;
  end;
  PIPICMPHeader = ^TIPICMPHeader;

  TICMPPacket = packed record //??
    IPHeader: TIPHeader;
    ICMPHeader: TICMPHeader;
    Data: array[0..1499] of byte;
  end;

  TTCPHeader = packed record
    SourcePort: word;
    DestPort: word;
    SequenceNumber: DWord;
    AcknowledgementNumber: DWord;
    Offset: byte; //only left 4 bits. Header length in 32-bit segments
    Flags: byte;
    Window: word;
    Checksum: word;  //includes speudo header instead of TCP header.
    UrgentPointer: word;
       {Optionally:
       Options:byte3; //MSS (Maximum Segment Size) at connection startup
       Padding:byte;
       }
  end;

  TIPTCPHeader = packed record
    IP: TIPHeader;
    TCP: TTCPHeader;
  end;
  PIPTCPHeader = ^ TIPTCPHeader;

  TTCPPacket = packed record
    IPHeader: TIPHeader;
    TCPHeader: TTCPHeader;
    Data: array[0..32767] of byte;
  end;

  TUDPHeader = packed record
    case integer of
      0: (
        SourcePort,
        DestPort: word; //why why why a Dword ???
        Length,
        Checksum: word;
      );
      1: (
        Raw: array[0..3] of word;
      );
  end;
  PUDPHeader = ^TUDPHeader;

  TIPUDPHeader = packed record
    IP: TIPHeader;
    UDP: TUDPHeader;
  end;



// Helper functions from the ip helper api (iphlpapi.dll)

// Next types extracted from whirwater:
// http://www.whirlwater.com/frames.php?http://www.whirlwater.com/information/2001/windows2000/usingtheiphelperapi.html
// thanx for coverting type definitions fellows
const
  MAX_HOSTNAME_LEN = 128; { from IPTYPES.H }
  MAX_DOMAIN_NAME_LEN = 128;
  MAX_SCOPE_ID_LEN = 256;
  MAX_ADAPTER_NAME_LENGTH = 256;
  MAX_ADAPTER_DESCRIPTION_LENGTH = 128;
  MAX_ADAPTER_ADDRESS_LENGTH = 8;

type
  TIPAddressString = array[0..4 * 4 - 1] of char;

  PIPAddrString = ^TIPAddrString;

  TIPAddrString = record
    Next: PIPAddrString;
    IPAddress: TIPAddressString;
    IPMask: TIPAddressString;
    Context: integer;
  end;

  PFixedInfo = ^TFixedInfo;
  TFixedInfo = record { FIXED_INFO }
    case integer of
      0: (
        HostName: array[0..MAX_HOSTNAME_LEN + 3] of char;
        DomainName: array[0..MAX_DOMAIN_NAME_LEN + 3] of char;
        CurrentDNSServer: PIPAddrString;
        DNSServerList: TIPAddrString;
        NodeType: integer;
        ScopeId: array[0..MAX_SCOPE_ID_LEN + 3] of char;
        EnableRouting: integer;
        EnableProxy: integer;
        EnableDNS: integer;
      );
      1: (A: array[0..2047] of byte);
  end;

  PIPAdapterInfo = ^TIPAdapterInfo;

  TIPAdapterInfo = record { IP_ADAPTER_INFO }
    Next: PIPAdapterInfo;
    ComboIndex: integer;
    AdapterName: array[0..MAX_ADAPTER_NAME_LENGTH + 3] of char;
    Description: array[0..MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of char;
    AddressLength: integer;
    Address: array[1..MAX_ADAPTER_ADDRESS_LENGTH] of byte;
    Index: integer;
    _Type: integer;
    DHCPEnabled: integer;
    CurrentIPAddress: PIPAddrString;
    IPAddressList: TIPAddrString;
    GatewayList: TIPAddrString;
    DHCPServer: TIPAddrString;
    HaveWINS: Boolean;
    PrimaryWINSServer: TIPAddrString;
    SecondaryWINSServer: TIPAddrString;
    LeaseObtained: integer;
    LeaseExpires: integer;
  end;



//some support functions:

//get primary IP of each adapter on the system:
function GetAdapters(aStrings: TStrings): boolean;

//calculate IP, TCP and UDP checksums
function IPChecksum(Data: Pointer; Size: integer): word;

//Classes that implement protocols on top of raw IP:
type
  TRawIPSocket = class(TBlockSocket)
  public
    procedure CreateSocket;
    function SendBuffer(const Buffer: TMemory; Length: integer): integer; override;
  end;

  TRawUDPBlockSocket = class(TBlockSocket)
  public
    IPHeader: TIPHeader;
    UDPHeader: TUDPHeader;
    FRemoteSin: TVarSin;
    Data: array[0..2047] of byte;
    procedure CreateSocket;
    procedure CalcUDPChecksum;
    procedure Connect(IP, Port: string); override;
    procedure SetFrom(IP, Port: string);
    function SendBuffer(const Buffer: TMemory; Length: integer): integer; override;
  end;

  TSniffingSocket = class(TBlockSocket)
  public
    FAdapterIP: string;
    procedure CreateSocket;
  end;

  TOnPacketSniffed = procedure(Sender: TObject; Data: string) of object;

  TvsSnifferThread = class(TThread)
    FData: string;
    FOwner: TObject;
    FSocket: TSniffingSocket;
    procedure SyncPacket;
    procedure Execute; override;
  end;

  TvsSniffer = class(TComponent) //make it component if you like
  private
    FOnPacket: TOnPacketSniffed;
  protected
    FActive: boolean;
  public
    FSniffer: TvsSnifferThread;
    FAdapter: string;
    procedure SetActive(Value: boolean);
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Adapter: string read FAdapter write FAdapter;
    property Active: boolean read FActive write SetActive;
    property OnPacketSniffed: TOnPacketSniffed read FOnPacket write FOnPacket;
  end;



  //ip helper interface
  TIPHelperInfo = class(TObject) //make that component if you like
    //After construction, these strings will be created and filled
    //system wide settings:
    HostName: string;
    DomainName: string;
    CurrentDNSServer: string;
    DNSServerList: TStrings;
    NodeType: integer;
    ScopeId: string;
    EnableRouting: boolean;
    EnableProxy: boolean;
    EnableDNS: boolean;
    //Filled per adapter:
    DNSServers: TStrings;
    AdapterIPs: TStrings;
    AdapterNames: TStrings;
    AdapterDescriptions: TStrings;
    AdapterMACs: TStrings;
    DHCPServers: TStrings;
    GateWays: TStrings;
    CurrentIPs: TStrings;
    CurrentMasks: TStrings;
    //      PrimaryIPs:TStrings;
    //      PrimaryMasks:TStrings;
    //LeaseObtained:TList
    //LeaseExpired:TList
    //multiples filled per adapter
    AllIPS: TStrings;
    AllMasks: TStrings;
    constructor Create;
    destructor Destroy; override;
  end;


//externals:
function GetNetworkParams(FI: PFixedInfo; var BufLen: integer): integer;
  stdcall; external 'iphlpapi.dll' Name 'GetNetworkParams';

function GetAdaptersInfo(AI: PIPAdapterInfo; var BufLen: integer): integer;
  stdcall; external 'iphlpapi.dll' Name 'GetAdaptersInfo';


//////////////////////////////////////////////////////////////////

implementation

//support functions:
function IPChecksum(Data: Pointer; Size: integer): word;
var
  Checksum: DWord;
  D: string;
  i, l: integer;
begin
  Checksum := 0;
  if size = 0 then
    exit;
  l := Size;
  if l mod 2 <> 0 then
    Inc(l, 1);
  SetLength(D, l);
  D[l] := #0;
  move(Data, D[1], Size);
  for i := 1 to (l div 2) do
  begin
    Checksum := Checksum + word(@D[l * 2 - 1]);
  end;
  Checksum := (Checksum shr 16) + (Checksum and $FFFF);
  Checksum := Checksum + (Checksum shr 16);
  Result := word(-Checksum - 1);
end;


function getAdapters(aStrings: TStrings): boolean;
var
  Data: string;
  l: integer;
  PInfo: PIPAdapterInfo;
  PIP: PIPAddrString;
begin
  //Fill Strings with an array of adapters
  Result := False;
  if (aStrings = nil) or not (aStrings is TStrings) then
    exit;
  aStrings.Clear;
  SetLength(Data, 8192); //arbritrary, increase if you expect loads of adapters.
  PInfo := @Data[1];
  l := length(Data);
  if 0 <> GetAdaptersInfo(PInfo, l) then
    exit;
  //now PInfo contains list of adapters:
  while (PInfo <> nil) and (ptrint(PInfo) <= ptrint(@Data[Length(Data)]) - SizeOf(TIPAdapterInfo)) do
  begin
    PIP := @PInfo^.IPAddressList;
    while PIP <> nil do
    begin
      aStrings.Add(PIP^.IPAddress);
      PIP := PIP^.Next;
      Result := True;
    end;
    PInfo := PInfo^.Next;
  end;
end;


constructor TIPHelperInfo.Create;
var
  Data: string;
  l: integer;
  PInfo: PIPAdapterInfo;
  PIP: PIPAddrString;
  NWInfo: PFixedInfo;
  M: string;
  i: integer;

  procedure AddrToStrings(P: PIPAddrString; IP: TStrings; Mask: TStrings);
  begin
    while P <> nil do
    begin
      if Assigned(IP) then
        IP.Add(P^.IPAddress);
      if Assigned(Mask) then
        Mask.Add(P^.IPMask);
      P := P^.Next;
    end;
  end;

begin
  inherited;
  DNSServerList := TStringList.Create;
  DNSServers := TStringList.Create;
  AdapterIPs := TStringList.Create;
  AdapterNames := TStringList.Create;
  AdapterDescriptions := TStringList.Create;
  AdapterMACs := TStringList.Create;
  DHCPServers := TStringList.Create;
  GateWays := TStringList.Create;
  CurrentIPs := TStringList.Create;
  CurrentMasks := TStringList.Create;
  //  PrimaryIPs:=TStringList.Create;
  //  PrimaryMasks:=TStringList.Create;
  //LeaseObtained:TList
  //LeaseExpired:TList
  //multiples filled per adapter
  AllIPS := TStringList.Create;
  AllMasks := TStringList.Create;
  //Now fill structures

  //Fill Strings with an array of adapters
  SetLength(Data, 8192); //arbritrary, increase if you expect loads of adapters.
  PInfo := @Data[1];
  l := length(Data);
  if 0 = GetAdaptersInfo(PInfo, l) then
    //now PInfo contains list of adapters:
    while (PInfo <> nil) and (integer(PInfo) <= integer(@Data[Length(Data)]) - SizeOf(TIPAdapterInfo)) do
    begin
      AdapterNames.Add(PInfo^.AdapterName);
      AdapterDescriptions.Add(PInfo^.Description);
      M := '';
      for i := 1 to PInfo^.AddressLength do
        M := M + IntToHex(byte(PInfo^.Address[i]), 2);
      AdapterMacs.Add(M);
      if Assigned(PInfo^.CurrentIPAddress) then
      begin
        CurrentIPs.Add(string(PInfo^.CurrentIPAddress^.IPAddress));
        CurrentMasks.Add(PInfo^.CurrentIPAddress^.IPMask);
      end;
      AddrToStrings(@PInfo^.GatewayList, GateWays, nil);
      AddrToStrings(@PInfo^.DHCPServer, DHCPServers, nil);
      AddrToStrings(@PInfo^.IPAddressList, AllIPs, AllMasks);
      PInfo := PInfo^.Next;
    end;
  //Now fill system-wide settigs:
  NWInfo := @Data[1];
  if 0 = GetNetworkParams(NWInfo, l) then
  begin
    Hostname := NWInfo^.HostName;
    DomainName := NWInfo^.DomainName;
    if Assigned(NWInfo^.CurrentDNSServer) then
      CurrentDNSServer := NWInfo^.CurrentDNSServer^.IPAddress;
    AddrToStrings(@NWINfo^.DNSServerList, DNSServers, nil);
    EnableRouting := boolean(NWInfo^.EnableRouting);
    EnableProxy := boolean(NWInfo^.EnableProxy);
    EnableDNS := boolean(NWInfo^.EnableDNS);
    ScopeID := NWInfo^.ScopeId;
    NodeType := NWInfo^.NodeType;
  end;
end;

destructor TIPHelperInfo.Destroy;
begin
  DNSServerList.Free;
  DNSServers.Free;
  AdapterIPs.Free;
  AdapterNames.Free;
  AdapterDescriptions.Free;
  AdapterMACs.Free;
  DHCPServers.Free;
  GateWays.Free;
  CurrentIPs.Free;
  CurrentMasks.Free;
  //  PrimaryIPs.Free;
  //  PrimaryMasks.Free;
  //LeaseObtained.Free
  //LeaseExpired.Free
  AllIPS.Free;
  AllMasks.Free;
  inherited;
end;

procedure TRawIPSocket.CreateSocket;
var
  c: integer;
  Sin: TVarSin;
  i: integer;
begin
  FSocket := synsock.Socket(AF_INET, SOCK_RAW, IPPROTO_IP);

  c := 1;
  i := setsockopt(FSocket, 0{SOL_SOCKET}, IP_HDRINCL, @c, sizeof(c));
  //  showmessage(inttostr(i));
  inherited CreateSocket;
  //  Bind ('0.0.0.0', '0'); //Any
  //  FProtocol:=IPPROTO_RAW;
  //  SetSin(Sin, '192.168.0.77','0');
  //  SockCheck(synsock.Bind(FSocket, @Sin, SizeOfVarSin(Sin)));
end;

function TRawIPSocket.SendBuffer(const Buffer: TMemory; Length: integer): integer;
var
  P: PIPTCPHeader;
  l: integer;
  S: TVarSin;
  Port: string;
  Host: string;
  sai: TVarSin;
begin
  P := Buffer;
  Port := IntToStr(ntohs(P^.TCP.DestPort));
  Host := PChar(synsock.inet_ntoa(TInAddr(P^.IP.DestIp)));
  //  inherited Connect (Host, Port);
  SetSin(sai, Host, Port);
  sai.sin_addr.S_addr := inet_addr(PChar(Host));
  //  inherited connect (Host, Port);
  //  GetSins;
  l := Length;
  Result :=
    sendto(FSocket, Buffer, l, 0, //flags
    Sai);//, //we need this filled for the kernel to send.
  //            SizeOfVarSin(Sai));
end;

procedure TRawUDPBlockSocket.CreateSocket;
var
  c: integer;
  Sin: TVarSin;
begin
  FSocket := synsock.Socket(PF_INET, SOCK_RAW, IPPROTO_IP);
  c := 1;
  inherited CreateSocket;
  setsockopt(FSocket, 0, IP_HDRINCL, @c, sizeof(c));
  //fill header info
  with IPHeader do
  begin
    Protocol := 17; //UDP
    TTL := 128;
    VerLen := (4 shl 4) or 5;
  end;
end;

procedure TRawUDPBlockSocket.CalcUDPChecksum;
//calc UDP checksum
var
  checksum: integer;
  i, l, m: integer;
begin
  //  see rfc 768;
  //  http://www.faqs.org/rfcs/rfc768.html
  checksum := 0;
  l := ntohs(UDPHeader.Length) - SizeOf(TUDPHeader);  //data length
  m := l div 2; //see if padding is needed
  if (l mod 2) <> 0 then
  begin
    Data[l] := 0; //add padding zero
    Inc(m);
  end;
  //checksum the data:
  for i := 0 to m - 1 do
    Checksum := Checksum - ((Data[i * 2] shl 8) + Data[i * 2 + 1]) - 2;
  //pseudo headers, source+dest:
  for i := 8 to 9 do
    Checksum := Checksum - IPHeader.Raw[i] - 1;
  //pseudo headers: proto + udplength
  Checksum := Checksum - IPHeader.Protocol - UDPHeader.Length - 2;
  //take the one's complement:
  Checksum := -Checksum - 1;

  //now make 16 bits from 32 bits:
  while (Checksum shr 16) > 0 do
    Checksum := ((Checksum shr 16) and $FFFF) or (Checksum and $FFFF);
  UDPHeader.Checksum := 0; //Checksum; it aint working yet.
end;

procedure TRawUDPBlockSocket.Connect(IP, Port: string);
type
  pu_long = ^u_long;
var
  HostEnt: PHostEnt;
begin
  //inherited connect is of no use here, since we deal with raw sockets.
  //fill the IP header structure
  inherited Connect(IP, Port); //fill sins
  if IP = cBroadCast then
    IPHeader.DestIP := INADDR_BROADCAST
  else
  begin
    IPHeader.DestIP := synsock.inet_addr(PChar(IP));
    if IPHeader.DestIP = INADDR_NONE then
    begin
      HostEnt := synsock.GetHostByName(PChar(IP));
      if HostEnt <> nil then
        IPHeader.DestIP := u_long(Pu_long(HostEnt^.h_addr_list^)^);
    end;
  end;
  UDPHeader.DestPort := htons(StrToIntDef(Port, 0));
end;

procedure TRawUDPBlockSocket.SetFrom(IP, Port: string);
type
  pu_long = ^ulong;
var
  HostEnt: PHostEnt;
begin
  if IP = cBroadCast then
    IPHeader.SourceIP := INADDR_BROADCAST
  else
  begin
    IPHeader.SourceIP := synsock.inet_addr(PChar(IP));
    if IPHeader.SourceIP = INADDR_NONE then
    begin
      HostEnt := synsock.GetHostByName(PChar(IP));
      if HostEnt <> nil then
        IPHeader.SourceIP := u_long(Pu_long(HostEnt^.h_addr_list^)^);
    end;
  end;
  UDPHeader.SourcePort := htons(StrToIntDef(Port, 0));
end;


function TRawUDPBlockSocket.SendBuffer(const Buffer: TMemory; Length: integer): integer;
var
  P: string; //the actual packet
  d: TSockAddr;
  l: integer;
begin
  if Length >= high(Data) then
  begin
    Result := -1;
    exit;
  end;
  //set header checksum
  IPHeader.TotalLen := htons(SizeOf(TIPHeader) + SizeOf(TUDPHeader) + Length);
  IPHeader.Identifier := gettickcount mod $FFFF; //0 also allowed, then kernel fills it.
  IPHeader.FragOffsets := $00; //htons(FlagDontFragment);

  //  CalcHeaderChecksum; //don't, kernel does this!
  //move data
  move(Buffer^, Data[0], Length);
  //set udp checksum
  UDPHeader.Length := htons(SizeOf(TUDPHeader) + Length);
  CalcUDPChecksum; //you can leave it zero if you like: UDPHeader.Checksum := 0;

  //move to buffer,
  //setlength of total IP packet:
  SetLength(P, SizeOf(TIPHeader) + SizeOf(TUDPHeader) + Length);
  //move IP header:
  move(IPHeader.Raw[0], P[1]{Pointer(P)^}, SizeOf(TIPHeader));
  //move IP data, in this case: UDP header:
  move(UDPHeader.Raw[0], P[1 + SizeOf(TIPHeader)], SizeOf(TUDPHeader));
  //move UDP data:
  move(Data[0], P[1 + SizeOf(TIPHeader) + SizeOf(TUDPHeader)], Length);
  //send data
  l := system.Length(P);
  //  Connect (IPHeader.DestIP, IPHeader.Port);

  Result :=
    sendto(FSocket, @P[1], l, 0, //flags
    FRemoteSin);
  //            PSockAddr(@FRemoteSin), //we need this filled for the kernel to send.
  //            SizeOf(FRemoteSin));
end;


procedure TSniffingSocket.CreateSocket;
var
  c, d, l: integer;
  F: TStrings;
  Sin: TVarSin;
begin
  //take your pig:
  FSocket := synsock.Socket(AF_INET, SOCK_RAW, IPPROTO_RAW{IP});
  //FSocket := synsock.Socket(AF_UNSPEC{INET}, SOCK_RAW, IPPROTO_RAW{IP});
  // no inherited CreateSocket here.
  c := 1;
  if FAdapterIP = '' then
  begin
    //fetch adapterIP
    //get default (first listed) adapter:
    F := TStringList.Create;
    if (not GetAdapters(F)) or (F.Count = 0) then
      exit; //don't try further, no use without IP.
    FAdapterIP := F[0];
  end;
  SetSin(Sin, FAdapterIP, '0');
  //  Sin.sin_family := IPPROTO_RAW; //AF_UNSPEC;
  SockCheck(synsock.Bind(FSocket, Sin{, SizeOfVarSin(Sin)}));
  c := 1;
  setsockopt(FSocket, 0{SOL_SOCKET}, IP_HDRINCL, @c, sizeof(c)); //not necessary
  c := 500000;
  setsockopt(FSocket, SOL_SOCKET, SO_RCVBUF, @c, sizeof(c)); //not necessary
  c := 1;
  d := 0;
  FLastError := WSAIoctl(FSocket, SIO_RCVALL, @c, SizeOf(c), @d, SizeOf(d), @l, nil, nil);
end;

procedure TvsSnifferThread.SyncPacket;
begin
  try
    if Assigned(TvsSniffer(FOwner).FOnPacket) then
      TvsSniffer(FOwner).FOnPacket(FOwner, FData);
  except //build-in safety, stop sniffing if anything fails:
    FOwner := nil;
    Terminate;
  end;
end;

procedure TvsSnifferThread.Execute;
begin
  FSocket.CreateSocket;
  while not Terminated do
  begin
    if (FSocket.WaitingData > 0) and (FSocket.WaitingData < 1000000) then
    begin
      SetLength(FData, FSocket.WaitingData);
      SetLength(FData,
        FSocket.RecvBuffer(@FData[1], length(FData)));
      if FData <> '' then
        Synchronize(@SyncPacket);
    end
    else
      sleep(2);
  end;

end;

constructor TvsSniffer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TvsSniffer.Destroy;
begin
  Active := False;
  inherited;
end;

procedure TvsSniffer.Loaded;
begin
  //component loaded, test active
  if FActive then
  begin
    FActive := False;
    SetActive(True);
  end;
end;

procedure TvsSniffer.SetActive(Value: boolean);
var
  s: TStrings;
  i: integer;
begin
  if Value = FActive then
    exit;
  if Value and ((FAdapter = '') or (FAdapter = '0.0.0.0')) then
  begin
    s := TStringList.Create;
    GetAdapters(s);
    for i := 0 to s.Count - 1 do
    begin
      if (s[i] <> '') and (s[i] <> '0.0.0.0') then
      begin
        FAdapter := s[i];
        break;
      end;
    end;
    s.Free;
  end;
  if (FAdapter = '') or (FAdapter = '0.0.0.0') then
    FActive := False;
  if not ((csDesigning in ComponentState) or (csReading in ComponentState)) then
  begin
    if Value then
    begin
      FSniffer := TvsSnifferThread.Create(True);
      FSniffer.FSocket := TSniffingSocket.Create;
      FSniffer.FSocket.FAdapterIP := FAdapter;
      FSniffer.FOwner := Self;
      FSniffer.Resume;
    end
    else
    begin
      FSniffer.Terminate;
      FSniffer.WaitFor;
      FSniffer.Free;
    end;
  end;
  FActive := Value;
end;

end.
