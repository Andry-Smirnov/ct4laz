
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsVisualSynapse;

interface

uses
     {$IF DEFINED(LINUX) or DEFINED(FREEBSD)}
  Libc,
     {$ENDIF}

    {$IFDEF WINDOWS}
  Windows {sleep function},
     {$ENDIF}
  Classes, SysUtils,
     {$IFDEF OCX}
  ExtCtrls,
     {$ENDIF}
  syncobjs,
  dnssend, httpsend, pingsend, slogsend, synautil, blcksock, synsock,
  synamisc, smtpsend, mimemess, mimepart, ftpsend, vsTypeDef;

const
  MAX_REDIRECT_COUNT = 12;

type //we have to re-type some stuff for the callback methods, else clients
  //have to include corresponding units:



{$IFDEF FPC}
{$DEFINE VS_SAFE_TYPE}
{$ENDIF}

{$IFDEF VS_SAFE_TYPE}
  //FPC compatability and delphi 6 'bug' work around.
  THookReason = THookSocketReason;
  TSynapseSocket = TSocksBlockSocket;
{$ELSE}
  //FPC incompatible
  THookReason = type THookSocketReason;
  TSynapseSocket = type TSocksBlockSocket;
{$ENDIF}
  //explanation of this re-typing:
  //if installed component is doubleclicked for some OnEvent
  //and this event has a THookSocketReason
  //user would have to _manually_ add the blcksock unit
  //to his source, which is inconvenient.
  //delphi 6 does not support this. 5 & 7 do.


{
hookreasons are:
    HR_ResolvingBegin,
    HR_ResolvingEnd,
    HR_SocketCreate,
    HR_SocketClose,
    HR_Bind,
    HR_Connect,
    HR_CanRead,
    HR_CanWrite,
    HR_Listen,
    HR_Accept,
    HR_ReadCount,
    HR_WriteCount,
    HR_Wait,
    HR_Error
}
  THostInfo = record
    Host: string;
    Port: string;
    MetaText: string; // may contain data dependent on protocol
    MetaData: Pointer; //we leave this nil normally
    ResultCode: integer;
  end;

  TSocksInfo = record
    IP,
    Port,
    Username,
    Password: string;
    Timeout: integer;
    Resolver: boolean;
    SocksType: TSocksType;
  end;

  TVisualSynapse = class;
  TVisualThread = class;
  TvsSocksProxyInfo = class;

  TOnVisualData = procedure(Sender: TVisualSynapse; VSHandle: integer; Data: string; Query: string; From: THostInfo) of object;
  TOnDataStrings = procedure(Sender: TVisualSynapse; VSHandle: integer; Data: TStrings; Query: string; From: THostInfo) of object;
  TOnError = procedure(Sender: TVisualSynapse; VSHandle: integer; Query: string; ErrorCode: integer; ErrorMessage: string) of object;
  TOnProgress = procedure(Sender: TVisualSynapse; VSHandle: integer; Reason: THookReason; Value: string; Sock: TSynapseSocket;
    var Continue: boolean) of object;
  TOnSockStatus = procedure(Sender: TVisualSynapse; VSHandle: integer; Value: string) of object;
  TOnSockInteger = procedure(Sender: TVisualSynapse; VSHandle: integer; Value: integer) of object;

  TJobType = (jtCreateNew, jtControl);

  TJob = class(TObject)
    Handle: integer;
    SendBandwidth: integer;
    RecvBandWidth: integer;
    JobType: TJobType; //defaults to createnew
  end;


  TVisualSynapse = class(TComponent)
    (*
      {$IFDEF OCX}
        (TPanel)
      {$ELSE}
        {$IFDEF BAREOBJECT}
          (TObject)
        {$ELSE} //no switch is default delphi component
         (TComponent)
        {$ENDIF}
      {$ENDIF}
    *)
  private
    FJobs: TList; //always encapsulate access by critical section FCS
  protected
    FInfo: THostInfo;
    FSocksProxyInfo: TvsSocksProxyInfo;
    FQuery: string;
    FData: string;
    FThreads: TList; //TVisualThread;
    FAutoTLS: boolean;
    FMaxThreads: integer;
    FSendBandwidth: integer;
    FRecvBandwidth: integer;
    FCS: TCriticalSection;
    FJobCount: integer;
    FDummyStrings: TStrings;
    FOnData: TOnVisualData;
    FOnDataStrings: TOnDataStrings;
    FOnError: TOnError;
    FOnResolvingBegin: TOnSockStatus;
    FOnResolvingEnd: TOnSockStatus;
    FOnSocketCreate: TOnSockStatus;
    FOnSocketClose: TOnSockStatus;
    FOnBind: TOnSockStatus;
    FOnConnect: TOnSockStatus;
    FOnCanRead: TOnSockStatus;
    FOnCanWrite: TOnSockStatus;
    FOnListen: TOnSockStatus;
    FOnAccept: TOnSockStatus;
    FOnReadCount: TOnSockInteger;
    FOnWriteCount: TOnSockInteger;
    FOnWait: TOnSockStatus;
    FOnSockError: TOnSockStatus;
    FOnProgress: TOnProgress;
    procedure CreateThread;
    procedure SetDummyStrings(Value: TStrings); //support function for published TStrings properties
    function Enqueue(Value: TJob): integer; //returns jobID
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Info: THostInfo read FInfo write FInfo;
    property LastJob: integer read FJobCount;
  published
    property SendBandwidth: integer read FSendBandwidth write FSendBandwidth;
    property RecvBandwidth: integer read FRecvBandwidth write FRecvBandwidth;
    property MaxThreads: integer read FMaxThreads write FMaxThreads;
    property SocksProxyInfo: TvsSocksProxyInfo read FSocksProxyInfo write FSocksProxyInfo;
    property OnData: TOnVisualData read FOnData write FOnData;
    property OnDataStrings: TOnDataStrings read FOnDataStrings write FOnDataStrings;
    property OnError: TOnError read FOnError write FOnError;
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
    property OnResolvingBegin: TOnSockStatus read FOnResolvingBegin write FOnResolvingBegin;
    property OnResolvingEnd: TOnSockStatus read FOnResolvingEnd write FOnResolvingEnd;
    property OnSocketCreate: TOnSockStatus read FOnSocketCreate write FOnSocketCreate;
    property OnSocketClose: TOnSockStatus read FOnSocketClose write FOnSocketClose;
    property OnBind: TOnSockStatus read FOnBind write FOnBind;
    property OnConnect: TOnSockStatus read FOnConnect write FOnConnect;
    property OnCanRead: TOnSockStatus read FOnCanRead write FOnCanRead;
    property OnCanWrite: TOnSockStatus read FOnCanWrite write FOnCanWrite;
    property OnListen: TOnSockStatus read FOnListen write FOnListen;
    property OnAccept: TOnSockStatus read FOnAccept write FOnAccept;
    property OnReadCount: TOnSockInteger read FOnReadCount write FOnReadCount;
    property OnWriteCount: TOnSockInteger read FOnWriteCount write FOnWriteCount;
    property OnWait: TOnSockStatus read FOnWait write FOnWait;
    property OnSockError: TOnSockStatus read FOnSockError write FOnSockError;
  end;

  TVisualClient = class(TVisualSynapse)
  published //Hide some properties:
    property OnListen: TOnSockStatus read FOnListen;
    property OnAccept: TOnSockStatus read FOnAccept;
  end;


  TVisualThread = class(TThread)
    Owner: TVisualSynapse;
    FData: string;
    FDataStrings: TStrings;
    FQuery: string;
    FInfo: THostInfo;
    FLastError: integer;
    FErrorMsg: string;
    FSocksInfo: TSocksInfo;
    FCurrentJob: TJob;
    FCurrentSock: TSynapseSocket;
    FContinue: boolean;
    //callback event
    FHookReason: THookSocketReason;
    FHookValue: string;
    function GetQueued(aHandle: integer): TObject; //get first command in queue or nil
    procedure CopySocksInfo(Socket: TSocksBlockSocket);

    //hook, do an onstatus:
    procedure SockCallBack(Sender: TObject; Reason: THookSocketReason; const Value: string);
    procedure SyncOnData;
    procedure SyncOnError;
    procedure SyncOnProgress;
  end;

  //Support methods:

  TvsSocksProxyInfo = class(TComponent)
  private
  protected
  public
    FSocksInfo: TSocksInfo;
  published
    property SocksIP: string read FSocksInfo.IP write FSocksInfo.IP;
    property SocksPort: string read FSocksInfo.Port write FSocksInfo.Port;
    property SocksUsername: string read FSocksInfo.Username write FSocksInfo.Username;
    property SocksPassword: string read FSocksInfo.Password write FSocksInfo.Password;
    property SocksTimeout: integer read FSocksInfo.Timeout write FSocksInfo.Timeout;
    property SocksResolver: boolean read FSocksInfo.Resolver write FSocksInfo.Resolver;
    property SocksType: TSocksType read FSocksInfo.SocksType write FSocksInfo.SocksType;
  end;

  // HTTP
  THTTPInfo = record
    UserName,
    UserPass,
    ProxyHost,
    ProxyPort,
    ProxyUser,
    ProxyPass,
    UserAgent,
    IPInterface: string;
    TimeOut: integer;
    KeepAlive: boolean;
    FollowRedirect: boolean;
  end;

  THTTPMethod = (hmGet, hmHead, hmPost);

  THTTPRequest = class(TJob)
    Method: THTTPMethod;
    URL: string;
    PostData: string;
    HTTPInfo: THTTPInfo;
  end;

  THTTPThread = class;

  TvsVisualHTTP = class(TVisualClient)
  protected
    //       FThread:THTTPThread;
    FMethod: THTTPMethod;
    FPostData: string;
    FURL: string;
    FOnHeader: TOnDataStrings;
    FHTTPInfo: THTTPInfo;
    function  DoHTTP(URL: string): integer;
    procedure getURL(URL: string); //calls doHTTP
  public
    function Get(URL: string): integer; //calls doHTTP
    function Head(URL: string): integer;
    function Post(URL, PostData: string): integer;
  published
    property Method: THTTPMethod read FMethod write FMethod;
    property URL: string read FURL write GetURL;
    property UserName: string read FHTTPInfo.UserName write FHTTPInfo.UserName;
    property UserPass: string read FHTTPInfo.UserPass write FHTTPInfo.UserPass;
    property ProxyHost: string read FHTTPInfo.ProxyHost write FHTTPInfo.ProxyHost;
    property ProxyPort: string read FHTTPInfo.ProxyPort write FHTTPInfo.ProxyPort;
    property ProxyUser: string read FHTTPInfo.ProxyUser write FHTTPInfo.ProxyUser;
    property ProxyPass: string read FHTTPInfo.ProxyPass write FHTTPInfo.ProxyPass;
    property UserAgent: string read FHTTPInfo.UserAgent write FHTTPInfo.UserAgent;
    property IPInterface: string read FHTTPInfo.IPInterface write FHTTPInfo.IPInterface;
    property TimeOut: integer read FHTTPInfo.TimeOut write FHTTPInfo.TimeOut;
    property KeepAlive: boolean read FHTTPInfo.KeepAlive write FHTTPInfo.KeepAlive;
    property FollowRedirect: boolean read FHTTPInfo.FollowRedirect write FHTTPInfo.FollowRedirect;
    property PostData: string read FPostData write FPostData;

    property OnHeader: TOnDataStrings read FOnHeader write FOnHeader;
  end;

  THTTPThread = class(TVisualThread)
    HTTP: THTTPSend;
    Req: THTTPRequest;
    procedure SyncOnHeader;
    procedure Execute; override;
  end;

  //TCP & UDP

  TUDPRequest = class(TJob)
    Host: string;
    Port: string;
    BindPort: string;
    BindAdapter: string;
    Data: string;
    CloseSocket: boolean;
  end;

  TUDPResponse = class(TJob)
    Info: THostInfo;
    Data: string;
  end;


  TvsVisualUDP = class(TVisualClient)
  protected
    FActive: boolean;
    FDualThreaded: boolean;
    FRemoteHost: string;
    FRemotePort: string;
    FBindAdapter: string;
    FBindPort: string;
    FSyncThread: TVisualThread;
  public
    procedure Connect(Host, Port: string);
    procedure SetActive(Value: boolean);
    procedure Send(Data: string);
    procedure SendTo(Host, Port, Data: string);
    procedure Loaded; override;
    procedure SetDualThreaded(Value: boolean);
  published
    property Active: boolean read FActive write SetActive;
    property Host: string read FRemoteHost write FRemoteHost;
    property Port: string read FRemotePort write FRemotePort;
    property BindPort: string read FBindPort write FBindPort;
    property BindAdapter: string read FBindAdapter write FBindAdapter;
    property DualThreaded: boolean read FDualThreaded write SetDualThreaded;
  end;

  TUDPThread = class(TVisualThread)
    FSock: TUDPBlockSocket;
    FBindPort: string;
    FBindAdapter: string;
    procedure Execute; override;
  end;

  TUDPSyncThread = class(TVisualThread)
    CS: TCriticalSection;
    fQueue: TList;
    procedure Execute; override;
  end;

  // TCP
  TTCPStatus = (tsConnect, tsDisconnect);
  TTCPRequest = class(TJob);

  TTCPData = class(TTCPRequest)
    Data: string;
  end;

  TTCPControl = class(TTCPRequest)
    Info: THostInfo;
    Status: TTCPStatus;
  end;

  TvsVisualTCP = class(TVisualSynapse)
  protected
    FActive: boolean;
    //FInfo: THostInfo;
  public
    function Connect(Host, Port: string): integer;
    procedure Disconnect(Handle: integer);
    procedure DisconnectAll;
    procedure SetActive(Value: boolean); //disconnect all?
    procedure Send(Data: string; Handle: integer);
    procedure SendAll(Data: string);  //send to all/first/?
  published
    property Active: boolean read FActive write SetActive;
    property Host: string read FInfo.Host write FInfo.Host;
    property Port: string read FInfo.Port write FInfo.Port;
  end;

  TTCPThread = class(TVisualThread)
    procedure Execute; override;
  end;

{
     TTCPServer = class (TVisualSynapse)
     end;

     TVisualSMTP = class (TVisualSynapse)
     protected
       FAttachments:TStrings;
       FFrom:String;
       FTo: TStrings;
     public
       function Attach (Value:TFileName);
       function AddTo;
       ClearAttachments;
       function Send;
       function SendMessage (cTo, cFrom, cSubject, FAttachments;
     published
     end;
}

  /// DNS
  TDNSMethod = (DNS_AUTO, DNS_LOOKUP, DNS_REVERSE, DNS_MX,
    DNS_TXT, DNS_ALL);

  TDNSRequest = class(TJob)
    Method: TDNSMethod;
    UseNetBios: boolean;
    DNSServer: string;
    Query: string;
  end;

  TDNSThread = class;

  TvsVisualDNS = class(TVisualClient)
  protected
    //       FThread:TDNSThread;
    FMethod: TDNSMethod;
    FUseNetbios: boolean;
    FDNSServer: string;
    //       property Thread:TDNSThread read FThread write FThread;
  public
    function  QueryDNS(Query: string): integer;
    procedure SetDNS(Value: string);
  published
    property DNSQuery: string read FQuery write SetDNS;
    property DNSResult: string read FData;
    property DNSMethod: TDNSMethod read FMethod write FMethod;
    property UseNetbios: boolean read FUseNetBios write FUseNetBios;
    property DNSServer: string read FDNSServer write FDNSServer;
  end;

  TDNSThread = class(TVisualThread)
    DNS: TDNSSend;
    FDoNetBios: boolean;
    procedure Execute; override;
  end;

  TPingType = (ptPing, ptTraceRoute, ptTraceResolveHosts);

  TICMPRequest = class(TJob)
    pingtype: TPingType;
    Host: string;
  end;

  TvsVisualICMP = class(TVisualClient)
  protected
    FHost: string;
    FPingType: TPingType;
    FActive: boolean;
  public
    function Ping(Host: string): integer;
    function traceroute(Host: string; ResolveHostNames: boolean): integer;
    function PingRequest(Host: string; PingType: TPingType): integer;
    procedure DoIt(Value: boolean);
  published
    property SocksProxyInfo: TvsSocksProxyInfo read FSocksProxyInfo; //hide
    property PingType: TPingType read FPingType write FPingType;
    property Activate: boolean read FActive write DoIt;
    property Host: string read FHost write FHost;
  end;

  TICMPThread = class(TVisualThread)
    procedure Execute; override;
  end;


  TAttachment = class(TObject)
    Data: string;
    Primary: string;
    Secondary: string;
    Filename: TFileName;
  end;

  TvsSendMailRequest = class(TJob)
    From: string;
    ReplyTo: string;
    _To: TStrings;
    Subject: string;
    Mailer: string;
    TextMessage: string;
    HTMLMessage: string;
    AttachedFiles: TStrings;
    Attachments: TList;
    AutoHTML: boolean;
    SMTP: string;
    Headers: TStrings;
  end;

  TvsSendMail = class(TVisualSynapse)
  protected
    FFrom: string;
    FReplyTo: string;
    FTo: TStrings;
    FSubject: string;
    FMailer: string;
    FMessage: string;
    FHTML: string;
    FAttachedFiles: TStrings;
    FAttachments: TList;
    FAutoHTML: boolean;
    FSMTP: string;
  public
    FHeaders: TStrings;
    function getToOne: string;
    procedure setToOne(Value: string);
    procedure SetToList(Value: TStrings);
    procedure setAttachedFiles(Value: TStrings);
    procedure Attach(Data, Primary, Secondary: string; Filename: TFileName);
    procedure AttachBinary(Data: string; FileName: TFileName);
    procedure AttachHTML(Data: string);
    procedure AttachImage(Data: string; FileName: TFileName);
    procedure AttachFile(Filename: TFileName);
    procedure Send;
    procedure SendTo(From, _To, Subject, TextMessage: string);
    procedure Clear;
    //       procedure SendToOne (From:String; _To:String; Subject:String; _Message:String);
    property Attachments: TList read FAttachments;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoHTML: boolean read FAutoHTML write FAutoHTML;
    property From: string read FFrom write FFrom;
    property ReplyTo: string read FReplyTo write FReplyTo;
    property ToOne: string read getToOne write setToOne;
    property ToList: TStrings read FTo write setToList;
    property Subject: string read FSubject write FSubject;
    property Mailer: string read FMailer write FMailer;
    property Text: string read FMessage write FMessage;
    property HTML: string read FHTML write FHTML;
    property AttachedFiles: TStrings read FAttachedFiles write setAttachedFiles;
    property SMTPServer: string read FSMTP write FSMTP;
  end;

  TvsSendMailThread = class(TVisualThread)
    SMTP: TSMTPSend;
    procedure Execute; override;
  end;


//support function
function ResolveHostName(IP: string): string;


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
    HaveWINS: longbool;
    PrimaryWINSServer: TIPAddrString;
    SecondaryWINSServer: TIPAddrString;
    LeaseObtained: integer;
    LeaseExpires: integer;
  end;

  {$IFDEF WINDOWS}//ip helper api only supported on windows

  TGetNetworkParams = function(FI: PFixedInfo; var BufLen: integer): integer;
    stdcall;

  TGetAdaptersInfo = function(AI: PIPAdapterInfo; var BufLen: integer): integer;
    stdcall;
{$ENDIF}

type
  //ip helper interface
  TvsIPHelper = class(TComponent)
    //After construction, these strings will be created and filled
    //system wide settings:
  protected
    FIPHelperDLL: THandle;
      {$IFDEF WINDOWS}
    FGetNetworkParams: TGetNetworkParams;
    FGetAdaptersInfo: TGetAdaptersInfo;
      {$ENDIF}
    FHostName: string;
    FDomainName: string;
    FCurrentDNSServer: string;
    FDNSServerList: TStrings;
    FNodeType: integer;
    FScopeId: string;
    FEnableRouting: boolean;
    FEnableProxy: boolean;
    FEnableDNS: boolean;
    //Filled per adapter:
    FDNSServers: TStrings;
    FAdapterIPs: TStrings;
    FAdapterNames: TStrings;
    FAdapterDescriptions: TStrings;
    FAdapterMACs: TStrings;
    FDHCPServers: TStrings;
    FGateWays: TStrings;
    FCurrentIPs: TStrings;
    FCurrentMasks: TStrings;
    //LeaseObtained:TList
    //LeaseExpired:TList
    //multiples filled per adapter
    FAllIPS: TStrings;
    FAllMasks: TStrings;
    FDummyStrings: TStrings;
    FDummyString: string;
    FDummyInt: integer;
    FDummyBool: boolean;
  public
    procedure Refresh;
    procedure SetString(Value: string); //dummy calls to help the object inspector
    procedure SetStrings(Value: TStrings); //that don't like read-only properties.
    procedure SetInt(Value: integer);
    procedure SetBool(Value: boolean);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    //the property interfaces
    property HostName: string read FHostName write SetString;
    property DomainName: string read FDomainName write SetString;
    property CurrentDNSServer: string read FCurrentDNSServer write SetString;
    property DNSServerList: TStrings read FDNSServerList write SetStrings;
    property NodeType: integer read FNodeType write SetInt;
    property ScopeId: string read FScopeId write SetString;
    property EnableRouting: boolean read FEnableRouting write SetBool;
    property EnableProxy: boolean read FEnableProxy write SetBool;
    property EnableDNS: boolean read FEnableDNS write SetBool;
    //Filled per adapter:
    property DNSServers: TStrings read FDNSServers write SetStrings;
    property AdapterIPs: TStrings read FAdapterIPs write SetStrings;
    property AdapterNames: TStrings read FAdapterNames write SetStrings;
    property AdapterDescriptions: TStrings read FAdapterDescriptions write SetStrings;
    property AdapterMACs: TStrings read FAdapterMACs write SetStrings;
    property DHCPServers: TStrings read FDHCPServers write SetStrings;
    property GateWays: TStrings read FGateWays write SetStrings;
    property CurrentIPs: TStrings read FCurrentIPs write SetStrings;
    property CurrentMasks: TStrings read FCurrentMasks write SetStrings;
    property AllIPS: TStrings read FAllIPS write SetStrings;
    property AllMasks: TStrings read FAllMasks write SetStrings;
  end;

//support functions
function TextToHTML(Value: string): string;
function ResolveIP(HostName: string): string;
//function ResolveIPS (HostName:String):TStringList;


implementation

//{ $ R VisualSynapse.dcr}
//{$R *.dcr}


function ResolveHostName(IP: string): string;
var
  HE: PHostEnt;
  P: integer;
begin
  P := synsock.inet_addr(PChar(IP));
  HE := synsock.GetHostByAddr(@P, SizeOf(P), AF_INET);
  if Assigned(HE) then
    Result := HE^.h_name
  else
  begin
    if p <> 0 then
      Result := IP
    else
      Result := ''; //invalid anything
  end;
end;

function ResolveIP(HostName: string): string;
var
  HE: synsock.PHostEnt;
  TI: synsock.TInAddr;
  P: PChar;
begin
  HE := synsock.GetHostByName(PChar(HostName));
  if Assigned(HE) then
  begin
      {$IFDEF WIN32}
    TI := synsock.TInAddr(HE^.h_addr^^);
      {$ELSE}
    Move(HE^.h_addr^^, TI, SizeOf(TI));
      {$ENDIF}
    P := synsock.inet_ntoa(TI);
    Result := P;
  end
  else
    Result := '0.0.0.0';
end;

(*
function ResolveIPS (HostName:String):TStringList;
var HE: synsock.PHostEnt;
    TI: synsock.TInAddr;
    P: PChar;
    T: synsock.PInAddr;
begin
  Result := TStringList.Create;
  HE := synsock.GetHostByName(PChar(HostName));
  if Assigned(HE) then
    begin
      T:=HE^.h_addr^;
      while Assigned (Pointer(T^)) do
        begin
          {$IFDEF WIN32}
          TI := synsock.TInAddr(T^);
          {$ELSE}
          //Move (HE^.h_addr^^, TI, SizeOf(TI));
          Move (T^, TI, SizeOf(TI));

          {$ENDIF}
          P := synsock.inet_ntoa(TI);
          Result.Add(P);

          inc (T);
        end;
      Result.Add(HE^.h_name);
    end;
end;
*)

function TextToHTML(Value: string): string;
  //inserts <br> tags and adds hyperlinks to http://, www. and email addresses
var
  s: string;
begin
  //optimizer issue:
  Result := '';

  Value := stringReplace(Value, '<', '&lt;', [rfReplaceAll]);
  Value := stringReplace(Value, '>', '&gt;', [rfReplaceAll]);

  Value := stringReplace(Value, #13#10, #10, [rfReplaceAll]);
  Value := stringreplace(Value, #13, #10, [rfReplaceAll]);
  Value := stringreplace(Value, #10, '<br/> ' + #13#10, [rfReplaceAll]);
  Value := Trim(Value) + ' ';
  while pos(' ', Value) > 0 do
  begin
    s := Copy(Value, 1, pos(' ', Value) - 1);
    Value := copy(Value, pos(' ', Value) + 1, maxint);
    if (pos('http://', lowercase(s)) = 1) then
      s := '<A HREF="' + s + '">' + s + '</A>';
    if (pos('www.', lowercase(s)) = 1) then
      s := '<A HREF="http://' + s + '">' + s + '</A>';
    if (pos('@', s) > 0) then
      s := '<A HREF="mailto:' + s + '">' + s + '</A>';
    Result := Result + s + ' ';
  end;
  Result := stringreplace(Result, '<br/> ', #13#10'<br/>', [rfReplaceAll]);
  Result := '<body>'#13#10 + Result + #13#10'</body>';
end;

function ExtractMail(V: string): string;
var
  i, j: integer;
begin
  Result := '';
  if pos('@', V) < 0 then
    exit;
  i := pos('@', V);
  while (i > 1) and not (V[i - 1] in ['<', '"', ' ']) do
    Dec(i);
  j := i;
  i := pos('@', V);
  while (i < length(V)) and not (V[i + 1] in ['>', '"', ' ']) do
    Inc(i);
  Result := Copy(V, j, i - j + 1);
end;

//synchronized methods:
procedure TVisualThread.SyncOnData;
var
  E: TStrings;
begin
  if csDestroying in Owner.ComponentState then
    exit;
  if Assigned(Owner.FOnData) then
    try
      Owner.FOnData(Owner, FCurrentJob.Handle, FData, FQuery, FInfo);
    except
    end;
  if Assigned(Owner.FOnDataStrings) then
    try
      E := TStringList.Create;
      //See if there is any data in FDataStrings
      if FDataStrings.Count > 0 then
        E.Assign(FDataStrings) //note that we do an extra assign here
      //if client somehow or another frees E,
      //thread will not be affected.
      //it costs some performance, but for safety it is better
      else
        // if Data is of reasonable size, fit into FDataStrings
        // this is a auto conversion that only takes place if there was
        // no data in FDataStrings
      begin
        //            if length (FData)<=1024*1024 then // 1Mb max ?
        E.Text := FData;
      end;

      Owner.FOnDataStrings(Owner, FCurrentJob.Handle, E, FQuery, FInfo);
      E.Free;
    except
    end;
end;

procedure TVisualThread.SyncOnError;
begin
  if Assigned(Owner.FOnError) and not (csDestroying in Owner.ComponentState) then
    try
      //to-do: set up some visual error structure
      if FErrorMsg = '' then
      begin
        if FLastError > 0 then
          FErrorMsg := TBlockSocket.GetErrorDesc(FLastError) //try to decode winsock error
        else
        begin //fetch from visual synapse error array
          FErrorMsg := IntToStr(FLastError);
        end;
      end;
      Owner.FOnError(Owner, FCurrentJob.Handle, FQuery, FLastError, FErrorMsg);
    except
    end;
end;

procedure TVisualThread.CopySocksInfo(Socket: TSocksBlockSocket);
begin
  Socket.SocksIP := FSocksInfo.IP;
  Socket.SocksPort := FSocksInfo.Port;
  Socket.SocksUsername := FSocksInfo.Username;
  Socket.SocksTimeout := FSocksInfo.TimeOut;
  Socket.SocksResolver := FSocksInfo.Resolver;
  Socket.SocksType := FSocksInfo.SocksType;
end;

procedure TVisualThread.SockCallBack(Sender: TObject; Reason: THookSocketReason; const Value: string);
begin
  FHookReason := Reason;
  FHookValue := Value;
  FContinue := True;
  if Sender is TSocksBlockSocket then
    FCurrentSock := TSynapseSocket(Sender)
  else
    FCurrentSock := nil;
  synchronize(@SyncOnProgress);
  if not FContinue then
  begin
    FCurrentSock.AbortSocket; //generates new callback, be aware
    FContinue := False;
  end;
end;


procedure TVisualThread.SyncOnProgress;
begin
  if not terminated then
    try
      if Assigned(Owner.FOnProgress) then
        Owner.FOnProgress(Owner, FCurrentJob.Handle, FHookReason, FHookValue, FCurrentSock, FContinue);
      case FHookReason of
        HR_ResolvingBegin:
          if Assigned(Owner.FOnResolvingBegin) then
            Owner.FOnResolvingBegin(Owner, FCurrentJob.Handle, FHookValue);
        HR_ResolvingEnd:
          if Assigned(Owner.FOnResolvingEnd) then
            Owner.FOnResolvingEnd(Owner, FCurrentJob.Handle, FHookValue);
        HR_SocketCreate:
          if Assigned(Owner.FOnSocketCreate) then
            Owner.FOnSocketCreate(Owner, FCurrentJob.Handle, FHookValue);
        HR_SocketClose:
          if Assigned(Owner.FOnSocketClose) then
            Owner.FOnSocketClose(Owner, FCurrentJob.Handle, FHookValue);
        HR_Bind:
          if Assigned(Owner.FOnBind) then
            Owner.FOnBind(Owner, FCurrentJob.Handle, FHookValue);
        HR_Connect:
          if Assigned(Owner.FOnConnect) then
            Owner.FOnConnect(Owner, FCurrentJob.Handle, FHookValue);
        HR_CanRead:
          if Assigned(Owner.FOnCanRead) then
            Owner.FOnCanRead(Owner, FCurrentJob.Handle, FHookValue);
        HR_CanWrite:
          if Assigned(Owner.FOnCanWrite) then
            Owner.FOnCanWrite(Owner, FCurrentJob.Handle, FHookValue);
        HR_Listen:
          if Assigned(Owner.FOnListen) then
            Owner.FOnListen(Owner, FCurrentJob.Handle, FHookValue);
        HR_Accept:
          if Assigned(Owner.FOnAccept) then
            Owner.FOnAccept(Owner, FCurrentJob.Handle, FHookValue);
        HR_ReadCount:
          if Assigned(Owner.FOnReadCount) then
            Owner.FOnReadCount(Owner, FCurrentJob.Handle, StrToIntDef(FHookValue, 0));
        HR_WriteCount:
          if Assigned(Owner.FOnWriteCount) then
            Owner.FOnWriteCount(Owner, FCurrentJob.Handle, StrToIntDef(FHookValue, 0));
        HR_Wait:
          if Assigned(Owner.FOnWait) then
            Owner.FOnWait(Owner, FCurrentJob.Handle, FHookValue);
        HR_Error:
          if Assigned(Owner.FOnSockError) then
            Owner.FOnSockError(Owner, FCurrentJob.Handle, FHookValue);
      end; //case
    except
    end;
end;


procedure TVisualSynapse.CreateThread;
var
  FThread: TVisualThread;
begin
  //convenience for further programming
  //same pointer, but code gets shorter and no overrides needed.
  //this allows more uniform component access, since it shortens between three classes.
  //overriding still allowed
  FThread := nil;
  if Self is TvsVisualDNS then
    FThread := TDNSThread.Create(True);
  if Self is TvsVisualHTTP then
    FThread := THTTPThread.Create(True);
  if Self is TvsVisualUDP then
    FThread := TUDPThread.Create(True);
  if Self is TvsVisualTCP then
    FThread := TTCPThread.Create(True);
  if Self is TvsVisualICMP then
    FThread := TICMPThread.Create(True);
  if Self is TvsSendMail then
    FThread := TvsSendMailThread.Create(True);

  if FThread <> nil then
  begin
    FThread.Owner := Self;
    FThread.FDataStrings := TStringList.Create;
    if Assigned(FSocksProxyInfo) then
      FThread.FSocksInfo := FSocksProxyInfo.FSocksInfo;
    FThread.Resume;
    FThreads.Add(FThread);
  end;
end;

function TVisualSynapse.Enqueue(Value: TJob): integer;
var
  FIdle: boolean;
  i: integer;
begin
  if (csDesigning in ComponentState) then
  begin
    Value.Free;
    exit;
  end;
  if Value.Handle = 0 then //new or unassigned request
  begin
    FIdle := False;
    //see if there are more threads needed:
    //do outside critical section since it is read-only atomic what we access here:
    //this is small overhead for single-threaded (udp/tcp like etc) connections.
    for i := 0 to FThreads.Count - 1 do
    begin
      FIdle := TVisualthread(FThreads[i]).FCurrentJob = nil;
      if FIdle then
        break;
    end;
    //do this outside critical section as well:
    if (not FIdle) and (FThreads.Count < FMaxThreads) then //create new thread
      CreateThread;
  end;
  //enqueue the job:
  FCS.Enter;
  if Value.Handle = 0 then
  begin
    Inc(FJobCount);
    Result := FJobCount;
    Value.Handle := FJobCount;
    Value.SendBandwidth := FSendBandwidth;
    Value.RecvBandWidth := FRecvBandwidth;
  end
  else
    Result := Value.Handle;
  FJobs.Add(Value);
  FCS.Leave;
end;


function TVisualThread.GetQueued(aHandle: integer): TObject;
  //this is where a thread polls for jobs:
var
  i: integer;
begin
  Result := nil;
  if csLoading in Owner.ComponentState then
    exit;
  Owner.FCS.Enter;
  with Owner do
  begin
    for i := 0 to FJobs.Count - 1 do
      if ((aHandle = 0) and (TJob(FJobs[i]).JobType = jtCreateNew)) or (TJob(FJobs[i]).Handle = aHandle) then
      begin
        Result := TObject(FJobs[i]);
        FJobs.Delete(i);
        FCurrentJob := Tjob(Result);
        break;
      end;
  end;
  Owner.FCS.Leave;
end;


constructor TVisualSynapse.Create(AOwner: TComponent);
begin
  inherited;
  FThreads := TList.Create;
  FCS := TCriticalSection.Create;
  FJobs := TList.Create;
  FMaxThreads := 1;
  //adjust for any protocol; user can adjust anyhow:
  if (Self is TvsVisualHTTP) or (Self is TvsVisualDNS) then
    FMaxThreads := 16;
  {$IFDEF OCX}
  //make invisible at runtime
  Visible := False;
  {$ENDIF}
end;


destructor TVisualSynapse.Destroy;
var
  i: integer;
begin
  for i := 0 to FThreads.Count - 1 do
    with TVisualThread(FThreads[i]) do
      try //it's a component, so be safe
        Terminate;
        WaitFor;
        FDataStrings.Free;
        Free;
      except
      end;
  FThreads.Free;
  for i := 0 to FJobs.Count - 1 do
    TJob(FJobs[i]).Free;
  FJobs.Free;
  FCS.Free;
  inherited;
end;

procedure TVisualSynapse.SetDummyStrings(Value: TStrings);
begin
  FDummyStrings := Value;
end;

function TvsVisualHTTP.DoHTTP(URL: string): integer;
var
  d: THTTPRequest;
begin
  d := THTTPRequest.Create;
  //copy actual properties:
  d.Method := FMethod;
  if pos('://', URL) <= 0 then
    URL := 'http://' + URL;
  d.URL := URL;
  if FMethod = hmPost then
    d.PostData := FPostData;
  D.HTTPInfo := FHTTPInfo;
  //and queue:
  Result := Enqueue(D);
end;

procedure TvsVisualHTTP.getURL(URL: string);
begin
  DoHTTP(URL);
end;

function TvsVisualHTTP.Get(URL: string): integer;
begin
  FMethod := hmGet;
  Result := DoHTTP(url);
end;

function TvsVisualHTTP.Head(URL: string): integer;
begin
  FMethod := hmHead;
  Result := DoHTTP(url);
end;

function TvsVisualHTTP.Post(URL, PostData: string): integer;
begin
  FMethod := hmPost;
  FPostData := PostData;
  Result := DoHTTP(url);
end;

procedure THTTPThread.SyncOnHeader;
var
  E: TStringList;
begin
  if Assigned(TvsVisualHTTP(Owner).FOnHeader) then
    try
      //adjust:
      E := TStringList.Create;
      E.Assign(HTTP.Headers);
      TvsVisualHTTP(Owner).FOnHeader(Owner, FCurrentJob.Handle, E, Req.URL, FInfo);
      E.Free;
    except
    end;
end;

procedure THTTPThread.Execute;
var
  M: string;
  SL: TStringList;
  RedirectCount: integer;
  Ok: boolean;
begin
  HTTP := THTTPSend.Create;
  while not Terminated do
  begin
    Req := THTTPRequest(GetQueued(0));
    if Assigned(Req) then
    begin
      //do a nice http request
      case Req.Method of
        hmGet: M := 'GET';
        hmHead: M := 'HEAD';
        hmPost: M := 'POST';
      end;
      HTTP.Document.Size := 0;
      HTTP.Headers.Clear;
      if (Req.Method = hmPost) and (Req.PostData <> '') then
      begin
        HTTP.Document.Write(Req.PostData[1], length(Req.PostData));
        HTTP.MimeType := 'application/x-www-form-urlencoded';
        HTTP.Protocol := '1.1';
        //HTTP.UserAgent := 'Mozilla/5.0 (Windows; U; Windows NT 5.0; en-US; rv:1.7) Gecko/20040614 Firefox/0.8';
        HTTP.Headers.Add('Referer: http://www.google.com/translate_t');
        //              HTTP.Headers.Add ('Content-Length: '+IntToStr(HTTP.Document.Size))
      end;

      CopySocksInfo(HTTP.Sock);
      HTTP.Sock.MaxSendBandwidth := Req.SendBandwidth;
      HTTP.Sock.MaxRecvBandwidth := Req.RecvBandwidth;

      //copy other stuff, like proxy etc.
      HTTP.ProxyHost := Req.HTTPInfo.ProxyHost;
      HTTP.ProxyPort := Req.HTTPInfo.ProxyPort;
      HTTP.ProxyUser := Req.HTTPInfo.ProxyUser;
      HTTP.ProxyPass := Req.HTTPInfo.ProxyPass;
      HTTP.IPInterface := Req.HTTPInfo.IPInterface;
          {$IFDEF SYNAPSE_VER33}
      HTTP.Username := Req.HTTPInfo.UserName;
      HTTP.Password := Req.HTTPInfo.UserPass;
          {$ELSE}//Version 32 ?
      //synapse 32 does not support HTTP authentication.
      //          HTTP.Username := Req.HTTPInfo.UserName;
      //          HTTP.Password := Req.HTTPInfo.UserPass;
          {$ENDIF}
      HTTP.UserAgent := Req.HTTPInfo.UserAgent;
      HTTP.KeepAlive := Req.HTTPInfo.KeepAlive;
      FInfo.Host := Req.URL;
      FQuery := Req.URL;
      //          HTTP.Sock.OnStatus
      HTTP.Sock.OnStatus := @SockCallBack;
      if HTTP.HTTPMethod(M, Req.URL) then
      begin
        Ok := True;
        if Req.HTTPInfo.FollowRedirect then
          //this does not follow redirects like "meta-equiv" in html documents.
          //only headers are examined.
        begin
          //see if there is a redirect
          RedirectCount := 0;
          SL := TStringList.Create;

          //this is safe, because it leaves lines intact and urls
          //are not supposed to have ': ' (whitespace) in it.
          SL.Text := StringReplace(HTTP.Headers.Text, ': ', '=', [rfReplaceAll]);
          while (SL.IndexOfName('location') >= 0) and (RedirectCount < MAX_REDIRECT_COUNT) do
          begin
            //ok then, fetch new document
            HTTP.Clear;
            if not (HTTP.HTTPMethod(M, SL.Values['location'])) then
            begin
              break; //sorry//
              Ok := False;
            end
            else
            begin
              SL.Text := StringReplace(HTTP.Headers.Text, ': ', '=', [rfReplaceAll]);
              Inc(RedirectCount);
            end;
          end;
          SL.Free;
        end;
        if Ok then
        begin
          FInfo.Host := HTTP.TargetHost;
          FInfo.Port := HTTP.TargetPort;
          FInfo.MetaText := HTTP.Headers.Text;
          FInfo.ResultCode := HTTP.ResultCode;
          SetLength(FData, HTTP.Document.Size);
          if FData <> '' then
            HTTP.Document.Read(FData[1], length(FData));
          synchronize(@SyncOnHeader);
          synchronize(@SyncOnData);
        end
        else
        begin
          FLastError := HTTP.ResultCode;
          FErrorMsg := 'Redirected, but failed to fetch document';
          synchronize(@SyncOnError);
        end;
      end
      else
      begin
        FLastError := HTTP.ResultCode;
        FErrorMsg := HTTP.ResultString;
        synchronize(@SyncOnError);
      end;
      Req.Free;
    end
    else
      sleep(200);
  end;
  HTTP.Free;
end;

function TvsVisualTCP.Connect(Host, Port: string): integer;
var
  C: TTCPControl;
begin
  C := TTCPControl.Create;
  C.Info.Host := Host;
  C.Info.Port := Port;
  C.Status := tsConnect;
  Result := Enqueue(C);
end;

procedure TvsVisualTCP.Disconnect(Handle: integer);
var
  C: TTCPControl;
begin
  C := TTCPControl.Create;
  C.Handle := Handle;
  C.Status := tsDisConnect;
  Enqueue(C);
end;

procedure TvsVisualTCP.SetActive(Value: boolean);
begin
  if Value then
    Connect(FInfo.Host, FInfo.Port)
  else
    DisconnectAll;
  FActive := Value;
end;


procedure TvsVisualTCP.Send(Data: string; Handle: integer);
var
  J: TTCPData;
begin
  J := TTCPData.Create;
  J.Handle := Handle;
  J.JobType := jtControl;
  J.Data := Data;
  Enqueue(J);
end;

procedure TvsVisualTCP.SendAll(Data: string);  //send to all/first/?
begin
  //todo:
  //loop all
  //for i:=0 to connected.count -1 do
  //send (data, connected[i].handle)
  Send(Data, 0);
end;

procedure TvsVisualTCP.DisconnectAll;
var
  i: integer;
  J: TTCPControl;
begin
  FCS.Enter;
  for i := 0 to FThreads.Count - 1 do
    if Assigned(TVisualThread(FThreads[i]).FCurrentJob) then
    begin
      J := TTCPControl.Create;
      J.Handle := TVisualThread(FThreads[i]).FCurrentJob.Handle;
      J.Status := tsDisconnect;
      Enqueue(J); //same thread, nested Critical section.
    end;
  FCS.Leave;
end;

procedure TTCPThread.Execute;
var
  FSock: TTCPBlockSocket;
  J: TTCPRequest;
  C: TTCPControl;
  D: TTCPData;
begin
  FSock := TTCPBlockSocket.Create;
  FSock.OnStatus := @SockCallBack;
  CopySocksInfo(FSock);
  while not Terminated do
  begin
    J := TTCPRequest(GetQueued(0)); //get new job
    if Assigned(J) then
    begin
      if (J is TTCPControl){should be} then
      begin
        C := TTCPControl(J);
        FSock.Connect(C.Info.Host, C.Info.Port);
        while (FSock.LastError = 0) and (not Terminated) do
        begin
          J := TTCPRequest(GetQueued(C.Handle));
          if Assigned(J) then
          begin
            if (J is TTCPControl) and (TTCPControl(J).Status = tsDisconnect) then
            begin
              FSock.CloseSocket;
              //todo: clean up eventual remaining stuff
              break; //break loop
            end;
            //outgoing traffic:
            if (J is TTCPData) then
            begin
              FSock.SendString(TTCPData(J).Data);
            end;
            J.Free;
          end;
          //incoming traffic:
          if FSock.CanRead(20) then
          begin
            FData := FSock.RecvPacket(0);
            synchronize(@syncOnData);
          end;
        end;
        FSock.CloseSocket;
        C.Free;
      end
      else //just ignore, invalid packet
        J.Free;
    end
    else
      sleep(200);
  end;
end;


procedure TvsVisualUDP.Connect(Host, Port: string);
begin
  FRemoteHost := Host;
  FRemotePort := Port;
  Active := True;
end;

procedure TvsVisualUDP.SetActive(Value: boolean);
var
  U: TUDPRequest;
begin
  if Value = FActive then
    exit;
  FActive := Value;
  if (csLoading in ComponentState) then
    Exit;
  U := TUDPRequest.Create;
  if Value then
  begin
    if FBindPort = '' then
      FBindPort := '0';
    if FBindAdapter = '' then
      FBindAdapter := '0.0.0.0';
    U.BindPort := FBindPort;
    U.BindAdapter := FBindAdapter;
  end
  else
  begin
    U.CloseSocket := True;
  end;
  Enqueue(U);
end;

procedure TvsVisualUDP.Loaded;
begin
  inherited;
  if FActive then
  begin
    FActive := False;
    SetActive(True);
  end;
  if FDualThreaded then
  begin
    FDualThreaded := False;
    SetDualThreaded(True);
  end;
end;


procedure TvsVisualUDP.Send(Data: string);
begin
  SendTo(FRemoteHost, FRemotePort, Data);
end;

procedure TvsVisualUDP.SendTo(Host, Port, Data: string);
var
  U: TUDPRequest;
begin
  if not FActive then
    exit;
  U := TUDPRequest.Create;
  U.Host := Host;
  U.Port := Port;
  U.Data := Data;
  Enqueue(U);
end;

procedure TUDPThread.Execute;
var
  U: TUDPRequest;
  F: TUDPRequest;
  Packet: string;
  J: TUDPResponse;
begin

  FSock := TUDPBlockSocket.Create;
  FSock.OnStatus := @SockCallBack;
  CopySocksInfo(FSock);

  F := TUDPRequest.Create;

  while not Terminated do
  begin
    U := TUDPRequest(GetQueued(0));
    if Assigned(U) then
    begin
      if U.CloseSocket then
        FSock.CloseSocket;
      if (U.BindAdapter <> '') and (U.BindPort <> '') then
      begin
        FSock.CloseSocket;
        FSock.Bind(U.BindAdapter, U.BindPort);
        F.Handle := U.Handle;
      end
      else
      begin
        FSock.Connect(U.Host, U.Port);
        FSock.SendString(U.Data);
        //if flasterror = 0 then sync onwritedata else sync onerror
      end;
      FreeAndNil(U);
    end;
    //      else
    begin
      if FSock.CanRead(0) then
      begin
        Packet := FSock.RecvPacket(0);
        if TvsVisualUDP(Owner).FDualThreaded then
        begin
          //put in queue
          J := TUDPResponse.Create;
          J.Data := Packet;
          J.Info.Host := FSock.GetRemoteSinIP;
          J.Info.Port := IntToStr(FSock.GetRemoteSinPort);
          TUDPSyncThread(TvsVisualUDP(Owner).FSyncThread).CS.Enter;
          TUDPSyncThread(TvsVisualUDP(Owner).FSyncThread).fQueue.Add(J);
          TUDPSyncThread(TvsVisualUDP(Owner).FSyncThread).CS.Leave;
        end
        else
        begin //do it now
          FData := Packet;
          FInfo.Host := FSock.GetRemoteSinIP;
          FInfo.Port := IntToStr(FSock.GetRemoteSinPort);
          FCurrentJob := F;
          synchronize(@SyncOnData);
        end;
      end
      else //probably not connected
        sleep(50);
    end;
  end;
  FSock.CloseSocket;
  FSock.Free;
end;

procedure TUDPSyncThread.Execute;
var
  Q: TList;
  i: integer;
  J: TUDPResponse;
begin
  //if dual-threaded
  //this thread provides application callback
  //while the other receives and sends data.
  Q := TList.Create;
  while not Terminated do
  begin
    CS.Enter;
    if fQueue.Count > 0 then
    begin
      for i := 0 to fQueue.Count - 1 do
        Q.Add(fQueue[i]);
      fQueue.Clear;
    end;
    CS.Leave;
    if Q.Count = 0 then
      sleep(50)
    else
    begin
      //call client
      for i := 0 to Q.Count - 1 do
      begin
        J := TUDPResponse(Q[i]);
        FInfo := J.Info;
        FCurrentJob := J;
        FData := J.Data;
        synchronize(@SyncOnData);
        J.Free;
      end;
      Q.Clear;
    end;
  end;
end;

function TvsVisualDNS.QueryDNS(Query: string): integer;
var
  D: TDNSRequest;
begin
  D := TDNSRequest.Create;
  D.Method := FMethod;
  D.UseNetBios := FUseNetBIOS;
  D.Query := Query;
  D.DNSServer := FDNSServer;
  Enqueue(D);
end;

procedure TvsVisualDNS.SetDNS(Value: string);
begin
  QueryDNS(Value);
end;

procedure TDNSThread.Execute;
var
  IPH: TvsIPHelper;
  i, l: integer;
  Ffound: boolean;
  qtype: byte;
  qt: TDNSMethod;
    {$IFDEF WINDOWS}
  //    HE:PHostEnt;
  P: string;
    {$ENDIF}
  D: TDNSRequest;
begin
  DNS := TDNSSend.Create;
  DNS.Sock.AbortSocket;
  CopySocksInfo(DNS.Sock);
  CopySocksInfo(DNS.TCPSock);
  //  Data:=TStringList.Create;
  IPH := TvsIPHelper.Create(Self.Owner);
  while not Terminated do
  begin
    D := TDNSRequest(GetQueued(0));
    if Assigned(D) then
    begin
      IPH.Refresh; //refresh the helper (?? but may be needed on modem connections)
      FQuery := D.Query;
      Ffound := False;
      for i := 0 to IPH.DNSServers.Count - 1 do //break if found
      begin
        if D.DNSServer = '' then
          DNS.TargetHost := IPH.DNSServers[i]
        else
          DNS.TargetHost := D.DNSServer;
        qt := D.Method;
        if qt = DNS_AUTO then
        begin
          if IsIP(FQuery) then
            qtype := QTYPE_PTR //reverse lookup
          else
            qtype := QTYPE_A; //normal lookup
        end
        else
          case qt of
            DNS_LOOKUP: qtype := QTYPE_A;
            DNS_REVERSE: qtype := QTYPE_PTR;
            DNS_MX: qtype := QTYPE_MX;
            DNS_TXT: qtype := QTYPE_TXT;
            DNS_ALL: qtype := QTYPE_ALL;
          end;

        if (DNS.DNSQuery(FQuery, QTYPE, FDataStrings)) and (FDataStrings.Count > 0) then
        begin
          FData := FDataStrings[0];
          //                  if FDataStrings.Count >= 2 then
          //                    FData := FDataStrings.Text;
          synchronize(@SyncOnData);

          FFound := True;
          break;
        end;
        if D.DNSServer <> '' then
          break;
      end;
          {$IFDEF WINDOWS}
      if not FFound and D.UseNetBios then
      begin
        //try netbios:
              {
              P:=synsock.inet_addr(PChar(FQuery));
              HE := synsock.GetHostByAddr(@P, Length(FQuery), AF_INET);
              if Assigned(HE) then}
        P := ResolveHostName(FQuery);
        if P <> '' then
        begin
          FDataStrings.Clear;
          FDataStrings.Add(P);
          FData := P;
          synchronize(@SyncOnData);
          FFound := True;
        end;
      end;
          {$ENDIF}
      //alternatively, on can use the getnameinfo function, on both linux and windows i think.

      if not Ffound then
      begin
        synchronize(@SyncOnError);
      end;
      D.Free;
    end
    else
      sleep(20);
  end;
  IPH.Free;
  DNS.Free;
end;

function TvsVisualICMP.Ping(Host: string): integer;
begin
  Result := PingRequest(Host, ptPing);
end;

function TvsVisualICMP.traceroute(Host: string; ResolveHostNames: boolean): integer;
begin
  if ResolveHostNames then
    Result := PingRequest(Host, ptTraceResolveHosts)
  else
    Result := PingRequest(Host, ptTraceRoute);
end;

function TvsVisualICMP.PingRequest(Host: string; PingType: TPingType): integer;
var
  P: TICMPRequest;
begin
  FHost := Host;
  FPingType := PingType;
  P := TICMPRequest.Create;
  P.Host := FHost;
  P.pingtype := FPingType;
  Enqueue(P);
end;

procedure TvsVisualICMP.DoIt(Value: boolean);
begin
  if Value then
    PingRequest(FHost, FPingType);
end;

procedure TICMPThread.Execute;
var
  P: TICMPRequest;
  i: integer;
  Ping: TPingSend;
  SomeHost: string;
  ttl: byte;
  //copied the ping and traceroute routine from pingsend.pas
begin
  //i'm not too sure if socks would work and/or implementations are uniform.
  // i quote from http://www.socks.permeo.com/TechnicalResources/SOCKSFAQ/SOCKSGeneralFAQ/index.asp
{23. Can I use ping/traceroute with SOCKS?
SOCKS works with TCP and UDP applications. ping and traceroute are ICMP applications,
so strictly speaking, they cannot.
Some implementations provide SOCKS implementations of ping and traceroute,
but they use vendor-specific protocol extensions.}
  //anyhow, i skip that for now.
  while not Terminated do
  begin
    P := TICMPRequest(GetQueued(0));
    if Assigned(P) then
    begin
      if P.PingType = ptPing then
      begin  //straight from pingsend support functions:
        with TPINGSend.Create do
          try
            i := -1;
            Sock.OnStatus := @SockCallBack; //added this
            //don't know if this is supported by socks server
            if Ping(P.Host) then
              if ReplyError = IE_NoError then
                i := PingTime;
          finally
            Free;
          end;
        FData := IntToStr(i);
        if i >= 0 then
          synchronize(@SyncOnData)
        else
          synchronize(@SyncOnError);
      end

      else //tracert

      begin
        FData := '';
        Ping := TPINGSend.Create;
        Ping.Sock.OnStatus := @SockCallBack;
        try
          ttl := 1;
          repeat
            ping.Sock.TTL := ttl;
            Inc(ttl);
            if ttl > 30 then
              Break;
            if not ping.Ping(P.Host) then
            begin
              FData := FData + cAnyHost + ' Timeout' + CRLF;
              continue;
            end;
            if (ping.ReplyError <> IE_NoError) and (ping.ReplyError <> IE_TTLExceed) then
            begin
              SomeHost := Ping.ReplyFrom;
              if P.PingType = ptTraceResolveHosts then
                SomeHost := ResolveHostName(SomeHost);
              FData := FData + SomeHost + ' ' + Ping.ReplyErrorDesc + CRLF;
            end;
            SomeHost := Ping.ReplyFrom;
            if P.PingType = ptTraceResolveHosts then
              SomeHost := ResolveHostName(SomeHost);
            FData := FData + SomeHost + ' ' + IntToStr(Ping.PingTime) + CRLF;
          until ping.ReplyError = IE_NoError;
        finally
          Ping.Free;
        end;
        if FData <> '' then
          synchronize(@SyncOnData)
        else
          synchronize(@SyncOnError);
      end;
      P.Free;
    end
    else
      sleep(20);
  end;
end;

//TvsSendMail//
constructor TvsSendMail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTo := TStringList.Create;
  FAttachedFiles := TStringList.Create;
  FAttachments := TList.Create;
  FMailer := 'Visual Synapse';
  FHeaders := TStringList.Create;
  FMaxThreads := 16;
end;

destructor TvsSendMail.Destroy;
begin
  FTo.Free;
  FAttachedFiles.Free;
  FAttachments.Free;
end;

procedure TvsSendMail.setToOne(Value: string);
begin
  FTo.Clear;
  FTo.Add(Value);
end;

function TvsSendMail.getToOne: string;
begin
  if FTo.Count > 0 then
    Result := FTo[0]
  else
    Result := '';
end;

procedure TvsSendMail.SetToList(Value: TStrings);
var
  i: integer;
begin
  FTo.Clear;
  for i := 0 to Value.Count - 1 do
    if pos('@', Value[i]) > 1 then
      FTo.Add(Value[i]);
end;

procedure TvsSendMail.setAttachedFiles(Value: TStrings);
begin
  FAttachedFiles.Assign(Value);
end;

procedure TvsSendMail.Send;
var
  Job: TvsSendMailRequest;
  i: integer;
begin
  //Enqueue job
  Job := TvsSendMailRequest.Create;
  Job.From := FFrom;
  Job.ReplyTo := FReplyTo;
  Job.Subject := FSubject;
  Job.TextMessage := FMessage;
  Job.HTMLMessage := FHTML;
  Job.Mailer := FMailer;
  Job._To := TStringList.Create;
  Job._To.Assign(FTo);
  Job.AttachedFiles := TStringList.Create;
  Job.AttachedFiles.Assign(FAttachedFiles);
  //we need to clear FAttachments, since the thread will clean up:
  Job.Attachments := FAttachments;
  FAttachments := TList.Create;
  Job.AutoHTML := FAutoHTML;
  Job.SMTP := FSMTP;
  Job.Headers := TStringList.Create;
  Job.Headers.Assign(FHeaders);
  Enqueue(Job);
end;

procedure TvsSendMail.Clear;
var
  i: integer;
begin
  FTo.Clear;
  FAttachments.Clear;
  for i := 0 to FAttachments.Count - 1 do
    TAttachment(FAttachments[i]).Free;
  FAttachedFiles.Clear;
  FFrom := '';
  FSubject := '';
  FMessage := '';
  FHTML := '';
end;

procedure TvsSendMail.Attach(Data, Primary, Secondary: string; Filename: TFileName);
var
  Attachment: TAttachment;
begin
  Attachment := TAttachMent.Create;
  Attachment.Data := Data;
  Attachment.Primary := Primary;
  Attachment.Secondary := Secondary;
  Attachment.Filename := FileName;
  FAttachments.Add(Attachment);
end;

procedure TvsSendMail.AttachBinary(Data: string; FileName: TFileName);
var
  Primary, Secondary, Ext: string;
  i: integer;
begin
  Primary := '';
  Secondary := '';
  Ext := '';
  i := 0;
  Attach(Data, Primary, Secondary, FileName);
end;

procedure TvsSendMail.AttachHTML(Data: string);
begin
  Attach(Data, 'text', 'html', '');
end;

procedure TvsSendMail.AttachImage(Data: string; FileName: TFileName);
begin
  Attach(Data, '', '', FileName);
end;

procedure TvsSendMail.AttachFile(Filename: TFileName);
begin
  if FileExists(FileName) and (FAttachedFiles.IndexOf(FileName) < 0) then
    FAttachedFiles.Add(FileName);
end;

procedure TvsSendMail.SendTo(From, _To, Subject, TextMessage: string);
begin
  FFrom := From;
  FTo.Clear;
  FTo.Add(_To);
  FSubject := Subject;
  FMessage := TextMessage;
  Send;
end;


procedure TvsSendMailThread.Execute;
var
  xSMTP: TSMTPSend;
  DNSServer: string;
  DNS: TStrings;
  sTo: string;
  i, j, N: integer;
  P, S: string;
  L: integer;
  DNSEntry: PIPAddrString;
  SMTPRelay: TStringList;
  Success: boolean;
  V: string;
  Job: TvsSendMailRequest;
  Mime: TMimeMess;
  MimeBody: TMimePart;
  MimeText: TMimePart;
  M: TStrings;
  MS: TMemoryStream;
  A: TAttachment;

  //procedure SendToRaw as copied and adjusted from SMTPSend.pas
  function SendMail(const MailFrom, MailTo, SMTPHost: string; const MailData: TStrings; const Username, Password: string): boolean;
  var
    cSMTP: TSMTPSend;
    s, t: string;
  begin
    Result := False;
    cSMTP := TSMTPSend.Create;
    try
      CopySocksInfo(cSMTP.Sock);
      // if you need support for upgrade session to TSL/SSL, uncomment next lines:
      //       SMTP.AutoTLS := True;
      // if you need support for TSL/SSL tunnel, uncomment next lines:
      // SMTP.FullSSL := True;
      cSMTP.Sock.MaxBandwidth := Job.SendBandWidth;
      cSMTP.Sock.SetLinger(True, 25);
      cSMTP.Sock.OnStatus := @SockCallBack;

      cSMTP.TargetHost := SeparateLeft(SMTPHost, ':');
      s := SeparateRight(SMTPHost, ':');
      if (s <> '') and (s <> SMTPHost) then
        cSMTP.TargetPort := s;
      cSMTP.Username := Username;
      cSMTP.Password := Password;
      if cSMTP.Login then
      begin
        if cSMTP.MailFrom(GetEmailAddr(MailFrom), Length(MailData.Text)) then
        begin
          s := MailTo;
          repeat
            t := GetEmailAddr(FetchEx(s, ',', '"'));
            if t <> '' then
              Result := cSMTP.MailTo(t);
            if not Result then
              Break;
          until s = '';
          if Result then
            Result := SMTP.MailData(MailData);
        end;
        cSMTP.Logout;
      end;
    finally
      cSMTP.Free;
    end;
  end;

begin
  while not Terminated do
  begin
    Job := TvsSendMailRequest(GetQueued(0));
    if Assigned(Job) then
    begin
      try
        Mime := TMimeMess.Create;
        M := TStringList.Create;
        MS := TMemoryStream.Create;

        //no support for inline images yet...

        if (Job.AttachedFiles.Count + Job.Attachments.Count > 0) then //Multipart
          MimeBody := Mime.AddPartMultiPart('mixed', nil)
        else
          MimeBody := nil;

        //Add text + optional html part:
        if Job.AutoHTML or (Job.HTMLMessage <> '') then
        begin
          MimeText := Mime.AddPartMultipart('alternative', MimeBody);
          M.Text := Job.TextMessage;
          Mime.AddPartText(M, MimeText);
          if Job.HTMLMessage <> '' then
            M.Text := Job.HTMLMessage
          else
            M.Text := TextToHTML(Job.TextMessage);
          Mime.AddPartHTML(M, MimeText);
        end
        else
        begin
          M.Text := Job.TextMessage;
          Mime.AddPartText(M, MimeBody);
        end;

        //add attachments:
        if Assigned(MimeBody) then //multipart/mixed
        begin
          for i := 0 to Job.AttachedFiles.Count - 1 do
            try
              Mime.AddPartbinaryFromFile(Job.AttachedFiles[i], MimeBody);
            except
            end;
          for i := 0 to Job.Attachments.Count - 1 do
            try
              A := TAttachment(Job.Attachments[i]);
              if (A.Primary = 'text') and (A.Secondary = 'html') then
              begin
                M.Text := A.Data;
                Mime.AddPartHTML(M, MimeBody);
              end
              else
              begin
                MS.Size := 0;
                MS.Write(A.Data[1], length(A.Data));
                Mime.AddPartBinary(MS, A.FileName, MimeBody);
              end;
            except
            end;
        end;

        M.Free;
        MS.Free;

        //set some header info:
        mime.Header.From := Job.From;
        mime.Header.ToList.Assign(Job._To);
        mime.Header.Subject := Job.Subject;
        mime.Header.XMailer := Job.Mailer;
        mime.Header.CustomHeaders.AddStrings(Job.Headers);
        if Job.ReplyTo <> '' then
          mime.Header.CustomHeaders.Add('Reply-To: ' + Job.ReplyTo);


        Mime.EncodeMessage; //messagepart => mime.lines

        SMTPRelay := TStringList.Create; //list of smtp servers we try
        DNSServer := GetDNS;
        if (DNSServer = '') and (Job.SMTP = '') then
        begin
          FErrorMsg := 'unable to retrieve dns server';
          SyncOnError;
        end
        else
        begin
          DNS := TStringList.Create;
          while pos(',', DNSServer) > 0 do
          begin
            DNS.Add(Copy(DNSServer, 1, pos(',', DNSServer) - 1));
            DNSServer := Copy(DNSServer, pos(',', DNSServer) + 1, maxint);
          end;
          if DNSServer <> '' then
            DNS.Add(DNSServer);

          for l := 0 to Job._To.Count - 1 do
          begin
            V := ExtractMail(Job._To[l]);
            sTo := copy(V, pos('@', V) + 1, maxint);
            //sTo contains the server name from which we want to retrieve the MX record:
            for i := 0 to DNS.Count - 1 do
              if GetMailServers(DNS[i], sTo, SMTPRelay) then
                break;

            //some people forget to specify MX records
            //in that case, add plain domain name:
            if SMTPRelay.Count = 0 then
              SMTPRelay.Add(sTo);

            if Job.SMTP <> '' then
              SMTPRelay.Insert(0, Job.SMTP);

            if SMTPRelay.Count = 0 then
            begin
              FErrorMsg := 'Cannot deliver, no smtp host available';
              synchronize(@syncOnError);
            end
            else
            begin
              //Now SMTPRelay contains a list of SMTP servers. This can be the host itself,
              //or a relaying server. We don't care, we simply try sending the message:

              Success := False;
              FData := Mime.Lines.Text;
              //                FDataStrings.Assign (Mime.Header.Lines);
              FQuery := Job.From + ':' + Job._To[l] + ':' + Job.Subject;
              for i := 0 to SMTPRelay.Count - 1 do
              begin
                if SendMail(Job.From, V, SMTPRelay[i],
                  Mime.Lines, '', '') then
                begin
                  success := True;
                  synchronize(@syncOnData);
                  break;
                end;
              end;
              if not Success then
              begin
                FErrorMsg := 'Failed to send';
                synchronize(@SyncOnError);
              end;
            end;
          end;
          DNS.Free;
        end;
        SMTPRelay.Free;
        Mime.Free;
        Job._To.Free;
        Job.AttachedFiles.Free;
        for i := 0 to Job.Attachments.Count - 1 do
          TAttachment(Job.Attachments[i]).Free;
        Job.Attachments.Free;
        Job.Free;
      except
        on E: Exception do
        begin
          FData := E.Message;
          synchronize(@syncOnError);
        end;
      end;

    end
    else
      sleep(200);
  end; //thread terminated
  //  FreeOnTerminate := True; //free self
end;


//TvsIPHelper//

constructor TvsIPHelper.Create(AOwner: TComponent);
begin
  inherited;
  FDNSServerList := TStringList.Create;
  FDNSServers := TStringList.Create;
  FAdapterIPs := TStringList.Create;
  FAdapterNames := TStringList.Create;
  FAdapterDescriptions := TStringList.Create;
  FAdapterMACs := TStringList.Create;
  FDHCPServers := TStringList.Create;
  FGateWays := TStringList.Create;
  FCurrentIPs := TStringList.Create;
  FCurrentMasks := TStringList.Create;
  //  PrimaryIPs:=TStringList.Create;
  //  PrimaryMasks:=TStringList.Create;
  //LeaseObtained:TList
  //LeaseExpired:TList
  //multiples filled per adapter
  FAllIPS := TStringList.Create;
  FAllMasks := TStringList.Create;

  {$IFDEF WINDOWS}
  //load libraries
  FIPHelperDLL := LoadLibrary('iphlpapi.dll');
  if FIPHelperDLL <> 0 then
  begin
    FGetNetworkParams := TGetNetworkParams(getProcAddress(FIPHelperDLL, 'GetNetworkParams'));
    FGetAdaptersInfo  := TGetAdaptersInfo(getProcAddress(FIPHelperDLL, 'GetAdaptersInfo'));
    //Now fill structures
  end;
  {$ENDIF}
  Refresh;
end;

procedure TvsIPHelper.SetString(Value: string);
begin
  FDummyString := Value;
end;

procedure TvsIPHelper.SetStrings(Value: TStrings);
begin
  FDummyStrings := Value;
end;

procedure TvsIPHelper.SetInt(Value: integer);
begin
  FDummyInt := Value;
end;

procedure TvsIPHelper.SetBool(Value: boolean);
begin
  FDummyBool := Value;
end;

procedure TvsIPHelper.Refresh;
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
  DNSServerList.Clear;
  DNSServers.Clear;
  AdapterIPs.Clear;
  AdapterNames.Clear;
  AdapterDescriptions.Clear;
  AdapterMACs.Clear;
  DHCPServers.Clear;
  GateWays.Clear;
  CurrentIPs.Clear;
  CurrentMasks.Clear;
  //  PrimaryIPs:=TStringList.Create;
  //  PrimaryMasks:=TStringList.Create;
  //LeaseObtained:TList
  //LeaseExpired:TList
  //multiples filled per adapter
  AllIPS.Clear;
  AllMasks.Clear;
  {$IFDEF WIN32}
  if not Assigned(FGetNetworkParams) or not Assigned(FGetAdaptersInfo) then
  begin //no w2k/xp platform
    DNSServers.Add(GetDNS); //cross-platform function declared in synamisc
    exit; //we have no further info to add
  end;
  {$ELSE}
  exit;
  {$ENDIF}

  {$IFDEF WIN32}
  //Fill Strings with an array of adapters
  SetLength(Data, 8192); //arbritrary, increase if you expect loads of adapters.
  PInfo := @Data[1];
  l := length(Data);
  if 0 = FGetAdaptersInfo(PInfo, l) then
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
  if 0 = FGetNetworkParams(NWInfo, l) then
  begin
    FHostname := NWInfo^.HostName;
    FDomainName := NWInfo^.DomainName;
    if Assigned(NWInfo^.CurrentDNSServer) then
      FCurrentDNSServer := NWInfo^.CurrentDNSServer^.IPAddress;
    AddrToStrings(@NWINfo^.DNSServerList, FDNSServers, nil);
    if (FCurrentDNSServer = '') and (FDNSServers.Count > 0) then
      FCurrentDNSServer := FDNSServers[0];
    FEnableRouting := boolean(NWInfo^.EnableRouting);
    FEnableProxy := boolean(NWInfo^.EnableProxy);
    FEnableDNS := boolean(NWInfo^.EnableDNS);
    FScopeID := NWInfo^.ScopeId;
    FNodeType := NWInfo^.NodeType;
  end;
  {$ENDIF}
end;

destructor TvsIPHelper.Destroy;
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
  AllIPS.Free;
  AllMasks.Free;
  inherited;
end;



procedure TvsVisualUDP.SetDualThreaded(Value: boolean);
begin
  if Value <> FDualThreaded then
  begin
    if (csLoading in ComponentState) or (csDesigning in ComponentState) then
    begin
      FDualThreaded := Value;
      Exit;
    end;
    if Value then
    begin
      FSyncThread := TUDPSyncThread.Create(True);
      FSyncThread.Owner := Self;
      TUDPSyncThread(FSyncThread).CS := TCriticalSection.Create;
      TUDPSyncThread(FSyncThread).fQueue := TList.Create;
      FSyncThread.Resume;
    end
    else
    begin
      FDualThreaded := False; //signal other threads in advance
      FSyncThread.Terminate;
      FSyncThread.WaitFor;
      TUDPSyncThread(FSyncThread).CS.Free;
      TUDPSyncThread(FSyncThread).fQueue.Free;
      FSyncThread.Free;
    end;
    FDualThreaded := Value;
  end;
end;

end.
