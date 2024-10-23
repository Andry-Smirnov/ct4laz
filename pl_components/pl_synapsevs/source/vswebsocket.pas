
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vswebsocket;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF WINDOWS} Windows,{$ENDIF}
  Classes, SysUtils, blcksock, syncobjs, Math,
  synautil, synacode, synsock, synachar;

const
  //Constants section defining what kind of data are sent from one pont to another. Continuation frame
  wsCodeContinuation = $0;
  //Text frame
  wsCodeText = $1;
  //Binary frame
  wsCodeBinary = $2;
  //Close frame
  wsCodeClose = $8;
  //Ping frame
  wsCodePing = $9;
  //Frame frame
  wsCodePong = $A;


  //Constants section defining close codes
  //Normal valid closure, connection purpose was fulfilled
  wsCloseNormal = 1000;
  //Endpoint is going away (like server shutdown)
  wsCloseShutdown = 1001;
  //Protocol error
  wsCloseErrorProtocol = 1002;
  //Unknown frame data type or data type application cannot handle
  wsCloseErrorData = 1003;
  //Reserved
  wsCloseReserved1 = 1004;
  //Close received by peer but without any close code. This close code MUST NOT be sent by application.
  wsCloseNoStatus = 1005;
  //Abnotmal connection shutdown close code. This close code MUST NOT be sent by application.
  wsCloseErrorClose = 1006;
  //Received text data are not valid UTF-8.
  wsCloseErrorUTF8 = 1007;
  //Endpoint is terminating the connection because it has received a message that violates its policy. Generic error.
  wsCloseErrorPolicy = 1008;
  //Too large message received
  wsCloseTooLargeMessage = 1009;
  //Client is terminating the connection because it has expected the server to negotiate one or more extension,
  //but the server didn't return them in the response message of the WebSocket handshake
  wsCloseClientExtensionError = 1010;
  //Server is terminating the connection because it encountered an unexpected condition that prevented it from fulfilling the request
  wsCloseErrorServerRequest = 1011;
  //Connection was closed due to a failure to perform a TLS handshake. This close code MUST NOT be sent by application.
  wsCloseErrorTLS = 1015;

type

  TswsCustomServer = class;
  TswsCustomConnection = class;

  TswsWebSockeCustomConnection = class;

  //Event procedural type to hook OnOpen events on connection
  TWebSocketConnectionEvent = procedure(aSender: TswsWebSockeCustomConnection) of object;

  //Event procedural type to hook OnPing, OnPong events on connection
  //TWebSocketConnectionPingPongEvent = procedure (aSender: TswsWebSockeCustomConnection; aData: string) of object;

  //Event procedural type to hook OnClose event on connection
  TWebSocketConnectionClose = procedure(aSender: TswsWebSockeCustomConnection; aCloseCode: integer; aCloseReason: string;
    aClosedByPeer: boolean) of object;

  //Event procedural type to hook OnRead on OnWrite event on connection
  TWebSocketConnectionData = procedure(aSender: TswsWebSockeCustomConnection; aFinal, aRes1, aRes2, aRes3: boolean;
    aCode: integer; aData: TMemoryStream) of object;

  //Event procedural type to hook OnReadFull
  TWebSocketConnectionDataFull = procedure(aSender: TswsWebSockeCustomConnection; aCode: integer; aData: TMemoryStream) of object;


  TSwsBaseThread = class(TThread)
  protected
    fSyncLock: TCriticalSection;
    procedure Synchronize(AMethod: TThreadMethod);
  public
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
  end;


  TswsTCPCustomConnectionSocket = class(TTCPBlockSocket)
  protected
    fConnection: TswsCustomConnection;
    fCurrentStatusReason: THookSocketReason;
    fCurrentStatusValue: string;
    fOnSyncStatus: THookSocketStatus;
    procedure DoOnStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
    procedure SyncOnStatus;
  public
    constructor Create;
    destructor Destroy; override;
    property Connection: TswsCustomConnection read fConnection;
    property OnSyncStatus: THookSocketStatus read fOnSyncStatus write fOnSyncStatus;
  end;

  {abstract(Basic connection thread)
    This object is used from server and client as working thread.
    When object is server connection: object is created automatically by @link(Parent) server.
    Thread can be terminated from outside. If server is terminated, all remaining
    connections are closed. This object is used to communicate with client.
    Object should not be created directly.
  }

  TswsCustomConnection = class(TSwsBaseThread)
  protected
    fIndex: integer;
    fParent: TswsCustomServer;
    fSocket: TswsTCPCustomConnectionSocket;
    fSSL: boolean;
    procedure AfterConnectionExecute; virtual;
    function BeforeExecuteConnection: boolean; virtual;
    procedure ExecuteConnection; virtual;
    function GetIsTerminated: boolean;
  public
    constructor Create(aSocket: TswsTCPCustomConnectionSocket); virtual;
    destructor Destroy; override;
    procedure Execute; override;
    procedure Start;
    procedure Stop;
    procedure TerminateThread; virtual;
    property Index: integer read fIndex;
    property IsTerminated: boolean read GetIsTerminated;
    property Parent: TswsCustomServer read fParent;
    property Socket: TswsTCPCustomConnectionSocket read fSocket;
    property SSL: boolean read fSSL write fSSL;
  end;


  //Event procedural type to hook OnAfterAddConnection in server
  //Use this hook to get informations about connection accepted server that was added

  TServerAfterAddConnection = procedure(Server: TswsCustomServer; aConnection: TswsCustomConnection) of object;
  //Event procedural type to hook OnBeforeAddConnection in server
  //   Use this hook to be informed that connection is about to be accepred by server.
  //   Use CanAdd parameter (@false) to refuse connection

  TServerBeforeAddConnection = procedure(Server: TswsCustomServer; aConnection: TswsCustomConnection; var CanAdd: boolean) of object;
  //Event procedural type to hook OnAfterRemoveConnection in server
  //  Use this hook to get informations about connection removed from server (connection is closed)

  TServerAfterRemoveConnection = procedure(Server: TswsCustomServer; aConnection: TswsCustomConnection) of object;
  //Event procedural type to hook OnAfterRemoveConnection in server
  //Use this hook to get informations about connection removed from server (connection is closed)

  TServerBeforeRemoveConnection = procedure(Server: TswsCustomServer; aConnection: TswsCustomConnection) of object;
  //Event procedural type to hook OnSockedError in server
  //  Use this hook to get informations about error on server binding

  TServerSocketError = procedure(Server: TswsCustomServer; Socket: TTCPBlockSocket) of object;


  TswsCustomServer = class(TSwsBaseThread)
  protected
    fBind: string;
    fPort: string;
    fCanAddConnection: boolean;
    fConnections: TList;
    fConnectionTermLock: TCriticalSection;
    fCurrentAddConnection: TswsCustomConnection;
    fCurrentRemoveConnection: TswsCustomConnection;
    fCurrentSocket: TTCPBlockSocket;
    fIndex: integer;
    fMaxConnectionsCount: integer;
    fOnAfterAddConnection: TServerAfterAddConnection;
    fOnAfterRemoveConnection: TServerAfterRemoveConnection;
    fOnBeforeAddConnection: TServerBeforeAddConnection;
    fOnBeforeRemoveConnection: TServerBeforeRemoveConnection;
    fOnSocketErrot: TServerSocketError;
    fSSL: boolean;
    fSSLCertificateFile: string;
    fSSLKeyPassword: string;
    fSSLPrivateKeyFile: string;
    function AddConnection(var aSocket: TswsTCPCustomConnectionSocket): TswsCustomConnection; virtual;
    function CreateServerConnection(aSocket: TswsTCPCustomConnectionSocket): TswsCustomConnection; virtual;
    procedure DoAfterAddConnection; virtual;
    procedure DoBeforeAddConnection;
    procedure DoAfterRemoveConnection;
    procedure DoBeforeRemoveConnection;
    procedure DoSocketError;
    function GetConnection(index: integer): TswsCustomConnection;
    function GetConnectionByIndex(index: integer): TswsCustomConnection;
    function GetCount: integer;
    procedure OnConnectionTerminate(Sender: TObject);
    procedure RemoveConnection(aConnection: TswsCustomConnection);
    procedure SyncAfterAddConnection;
    procedure SyncBeforeAddConnection;
    procedure SyncAfterRemoveConnection;
    procedure SyncBeforeRemoveConnection;
    procedure SyncSocketError;
  public
    constructor Create(aBind: string; aPort: string); virtual;
    destructor Destroy; override;
    procedure Execute; override;
    procedure TerminateThread; virtual;
    procedure LockTermination;
    procedure Start;
    procedure Stop;
    procedure UnLockTermination;
    property Connection[index: integer]: TswsCustomConnection read GetConnection; default;
    property ConnectionByIndex[index: integer]: TswsCustomConnection read GetConnectionByIndex;
    property Count: integer read GetCount;
    property Host: string read fBind;
    property Index: integer read fIndex;
    property MaxConnectionsCount: integer read fMaxConnectionsCount write fMaxConnectionsCount;
    property Port: string read fPort;
    property SSL: boolean read fSSL write fSSL;
    property SSLCertificateFile: string read fSSLCertificateFile write fSSLCertificateFile;
    property SSLKeyPassword: string read fSSLKeyPassword write fSSLKeyPassword;
    property SSLPrivateKeyFile: string read fSSLPrivateKeyFile write fSSLPrivateKeyFile;
    property OnAfterAddConnection: TServerAfterAddConnection read fOnAfterAddConnection write fOnAfterAddConnection;
    property OnBeforeAddConnection: TServerBeforeAddConnection read fOnBeforeAddConnection write fOnBeforeAddConnection;
    property OnAfterRemoveConnection: TServerAfterRemoveConnection read fOnAfterRemoveConnection write fOnAfterRemoveConnection;
    property OnBeforeRemoveConnection: TServerBeforeRemoveConnection read fOnBeforeRemoveConnection write fOnBeforeRemoveConnection;
    property OnSocketError: TServerSocketError read fOnSocketErrot write fOnSocketErrot;
  end;

  TswsWebSockeCustomConnection = class(TswsCustomConnection)
  protected
    fOnRead: TWebSocketConnectionData;
    fOnReadFull: TWebSocketConnectionDataFull;
    fOnWrite: TWebSocketConnectionData;
    fOnClose: TWebSocketConnectionClose;
    fOnOpen: TWebSocketConnectionEvent;
    fCookie: string;
    fVersion: integer;
    fProtocol: string;
    fResourceName: string;
    fOrigin: string;
    fExtension: string;
    fPort: string;
    fHost: string;
    fHeaders: TStringList;
    fClosedByMe: boolean;
    fClosedByPeer: boolean;
    fMasking: boolean;
    fRequireMasking: boolean;
    fHandshake: boolean;
    fCloseCode: integer;
    fCloseReason: string;
    fClosingByPeer: boolean;
    fReadFinal: boolean;
    fReadRes1: boolean;
    fReadRes2: boolean;
    fReadRes3: boolean;
    fReadCode: integer;
    fReadStream: TMemoryStream;
    fWriteFinal: boolean;
    fWriteRes1: boolean;
    fWriteRes2: boolean;
    fWriteRes3: boolean;
    fWriteCode: integer;
    fWriteStream: TMemoryStream;
    fSendCriticalSection: SyncObjs.TCriticalSection;
    fFullDataProcess: boolean;
    fFullDataStream: TMemoryStream;
    function GetClosed: boolean;
    function GetClosing: boolean;
    procedure ExecuteConnection; override;
    function ReadData(var aFinal, aRes1, aRes2, aRes3: boolean; var aCode: integer; aData: TMemoryStream): integer; virtual;
    function ValidConnection: boolean;
    procedure DoSyncClose;
    procedure DoSyncOpen;
    procedure DoSyncRead;
    procedure DoSyncReadFull;
    procedure DoSyncWrite;
    procedure SyncClose;
    procedure SyncOpen;
    procedure SyncRead;
    procedure SyncReadFull;
    procedure SyncWrite;

    {Overload this function to process connection close (not at socket level, but as an actual WebSocket frame)
      aCloseCode represents close code (see wsClose constants)
      aCloseReason represents textual information transfered with frame (there is no specified format or meaning)
      aClosedByPeer whether connection has been closed by this connection object or by peer endpoint }
    procedure ProcessClose(aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean); virtual;


    {Overload this function to process data as soon as they are read before other Process<data> function is called
      this function should be used by extensions to modify incomming data before the are process based on code }
    procedure ProcessData(var aFinal: boolean; var aRes1: boolean; var aRes2: boolean;
      var aRes3: boolean; var aCode: integer; aData: TMemoryStream); virtual;


    {Overload this function to process ping frame)
      aData represents textual information transfered with frame (there is no specified format or meaning) }
    procedure ProcessPing(aData: string); virtual;

    {Overload this function to process pong frame)
      aData represents textual information transfered with frame (there is no specified format or meaning) }
    procedure ProcessPong(aData: string); virtual;

    {Overload this function to process binary frame)
      aFinal whether frame is final frame or continuing
      aRes1 whether 1st extension bit is set up
      aRes2 whether 2nd extension bit is set up
      aRes3 whether 3rd extension bit is set up
      aData data stream

      second version is for contuniation frames }
    procedure ProcessStream(aFinal, aRes1, aRes2, aRes3: boolean; aData: TMemoryStream); virtual;
    procedure ProcessStreamContinuation(aFinal, aRes1, aRes2, aRes3: boolean; aData: TMemoryStream); virtual;
    procedure ProcessStreamFull(aData: TMemoryStream); virtual;

    {Overload this function to process text frame)
      aFinal whether frame is final frame or continuing
      aRes1 whether 1st extension bit is set up
      aRes2 whether 2nd extension bit is set up
      aRes3 whether 3rd extension bit is set up
      aData textual data

      second version is for contuniation frames }
    procedure ProcessText(aFinal, aRes1, aRes2, aRes3: boolean; aData: string); virtual;
    procedure ProcessTextContinuation(aFinal, aRes1, aRes2, aRes3: boolean; aData: string); virtual;
    procedure ProcessTextFull(aData: string); virtual;

  public
    constructor Create(aSocket: TswsTCPCustomConnectionSocket); override;
    destructor Destroy; override;


    //  Whether connection is in active state (not closed, closing, socket, exists, i/o  threads not terminated..)
    function CanReceiveOrSend: boolean;

    //Procedure to close connection
    //  aCloseCode represents close code (see wsClose constants)
    //  aCloseReason represents textual information transfered with frame (there is no specified format or meaning) the string can only be 123 bytes length
    procedure Close(aCode: integer; aCloseReason: string); virtual; abstract;

    {Send binary frame
      aData data stream
      aFinal whether frame is final frame or continuing
      aRes1 1st extension bit
      aRes2 2nd extension bit
      aRes3 3rd extension bit }
    procedure SendBinary(aData: TStream; aFinal: boolean = True; aRes1: boolean = False; aRes2: boolean = False; aRes3: boolean = False);

    {Send binary continuation frame
      aData data stream
      aFinal whether frame is final frame or continuing
      aRes1 1st extension bit
      aRes2 2nd extension bit
      aRes3 3rd extension bit }
    procedure SendBinaryContinuation(aData: TStream; aFinal: boolean = True; aRes1: boolean = False; aRes2: boolean = False;
      aRes3: boolean = False);

    {Send generic frame
      aFinal whether frame is final frame or continuing
      aRes1 1st extension bit
      aRes2 2nd extension bit
      aRes3 3rd extension bit
      aCode frame code
      aData data stream or string }
    function SendData(aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TStream): integer; overload; virtual;
    function SendData(aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: string): integer; overload; virtual;


    {Send textual frame
      aData data string (MUST be UTF-8)
      aFinal whether frame is final frame or continuing
      aRes1 1st extension bit
      aRes2 2nd extension bit
      aRes3 3rd extension bit}
    procedure SendText(aData: string; aFinal: boolean = True; aRes1: boolean = False; aRes2: boolean = False; aRes3: boolean = False); virtual;

    {Send textual continuation frame
      aData data string (MUST be UTF-8)
      aFinal whether frame is final frame or continuing
      aRes1 1st extension bit
      aRes2 2nd extension bit
      aRes3 3rd extension bit  }
    procedure SendTextContinuation(aData: string; aFinal: boolean = True; aRes1: boolean = False; aRes2: boolean = False; aRes3: boolean = False);

    {Send Ping
      aData ping informations
    }
    procedure Ping(aData: string);

    {Send Pong
      aData pong informations }
    procedure Pong(aData: string);

    {Temination procedure
      This method should be called instead of Terminate to terminate thread,
      it internally calls Terminate, but can be overloaded,
      and can be used for data clean up }
    procedure TerminateThread; override;

    { Whether connection has been closed
      (either socket has been closed or thread has been terminated or WebSocket has been closed by this and peer connection) }
    property Closed: boolean read GetClosed;

    { Whether WebSocket has been closed by this and peer connection }
    property Closing: boolean read GetClosing;

    { WebSocket connection cookies
      Property is regular unparsed Cookie header string
      e.g. cookie1=value1;cookie2=value2

      empty string represents that no cookies are present }
    property Cookie: string read fCookie;

    { WebSocket connection extensions
      Property is regular unparsed Sec-WebSocket-Extensions header string
      e.g. foo, bar; baz=2

      On both client and server connection this value represents the extension(s) selected by server to be used
      as a result of extension negotioation

      value - represents that no extension was negotiated and no header will be sent to client
      it is the default value }
    property Extension: string read fExtension;

    {Whether to register for full data processing
    (callink @link(ProcessFullText), @link(ProcessFullStream) @link(OnFullRead)
    those methods/events are called if FullDataProcess is @true and whole message is read (after final frame) }
    property FullDataProcess: boolean read fFullDataProcess write fFullDataProcess;



    // Whether WebSocket handshake was succecfull (and connection is afer WS handshake)
    property Handshake: boolean read fHandshake;

    { WebSocket connection host
      Property is regular unparsed Host header string
      e.g. server.example.com }
    property Host: string read fHost;

    { WebSocket connection origin
      Property is regular unparsed Sec-WebSocket-Origin header string
      e.g. http://example.com }
    property Origin: string read fOrigin;

    { WebSocket connection protocol
      Property is regular unparsed Sec-WebSocket-Protocol header string
      e.g. chat, superchat

      On both client and server connection this value represents the protocol(s) selected by server to be used
      as a result of protocol negotioation

      value - represents that no protocol was negotiated and no header will be sent to client
      it is the default value  }
    property Protocol: string read fProtocol;

    // Connection port
    property Port: string read fPort;

    { Connection resource
      e.g. /path1/path2/path3/file.ext }
    property ResourceName: string read fResourceName;

    // WebSocket version (either 7 or 8 or 13)
    property Version: integer read fVersion;

    // WebSocket Close frame event
    property OnClose: TWebSocketConnectionClose read fOnClose write fOnClose;

    // WebSocket connection successfully
    property OnOpen: TWebSocketConnectionEvent read fOnOpen write fOnOpen;

    { : WebSocket ping
    property OnPing: TWebSocketConnectionPingPongEvent read fOnPing write fOnPing;
    }

    { : WebSocket pong
    property OnPong: TWebSocketConnectionPingPongEvent read fOnPong write fOnPong;
    }

    // WebSocket frame read
    property OnRead: TWebSocketConnectionData read fOnRead write fOnRead;

    // WebSocket read full data
    property OnReadFull: TWebSocketConnectionDataFull read fOnReadFull write fOnReadFull;

    // WebSocket frame written
    property OnWrite: TWebSocketConnectionData read fOnWrite write fOnWrite;
  end;

  TswsWebSockeCustomConnections = class of TswsWebSockeCustomConnection;


  // WebSocket server connection automatically created by server on incoming connection
  TswsWebSocketServerConnection = class(TswsWebSockeCustomConnection)
  public
    constructor Create(aSocket: TswsTCPCustomConnectionSocket); override;
    procedure Close(aCode: integer; aCloseReason: string); override;
    procedure TerminateThread; override;

    { List of all headers
      keys are lowercased header name
      e.g host, connection, sec-websocket-key }
    property Header: TStringList read fHeaders;
  end;

  TswsWebSocketServerConnections = class of TswsWebSocketServerConnection;

  { WebSocket client connection, this object shoud be created to establish client to server connection  }
  TswsWebSocketClientConnection = class(TswsWebSockeCustomConnection)
  protected
    function BeforeExecuteConnection: boolean; override;
  public
    { construstor to create connection,
      parameters has the same meaning as corresponging connection properties (see 2 differences below) and
      should be formated according to headers values

      aProtocol and aExtension in constructor represents protocol(s) and extension(s)
      client is trying to negotiate, obejst properties then represents
      protocol(s) and extension(s) the server is supporting (the negotiation result)

      Version must be >= 8 }
    constructor Create(aHost, aPort, aResourceName: string; aOrigin: string = '-';
      aProtocol: string = '-'; aExtension: string = '-';
      aCookie: string = '-'; aVersion: integer = 13); reintroduce; virtual;

    procedure Close(aCode: integer; aCloseReason: string); override;
    procedure Execute; override;
  end;


  TswsWebSocketServer = class;

  {Event procedural type to hook OnReceiveConnection events on server
    every time new server connection is about to be created (client is connecting to server)
    this event is called

    properties are representing connection properties as defined in @link(TswsWebSocketServerConnection)

    Protocol and Extension represents corresponding headers sent by client, as their out value
    server must define what kind of protocol(s) and extension(s) server is supporting, if event
    is not implemented, both values are considered as - (no value at all)

    HttpResult represents the HTTP result to be send in response, if connection is about to be
    accepted, the value MUST BE 101, any other value meand that the client will be informed about the
    result (using the HTTP code meaning) and connection will be closed, if event is not implemented
    101 is used as a default value }
  TswsWebSocketServerReceiveConnection = procedure(Server: TswsWebSocketServer;
    Socket: TswsTCPCustomConnectionSocket;
    Header: TStringList;
    aResourceName, aHost, aPort, aOrigin, aCookie: string;
    HttpResult: integer;
    Protocol, Extensions: string) of object;



  TswsWebSocketServer = class(TswsCustomServer)
  protected
    {CreateServerConnection sync variables}
    fncSocket: TswsTCPCustomConnectionSocket;
    fncResourceName: string;
    fncHost: string;
    fncPort: string;
    fncOrigin: string;
    fncProtocol: string;
    fncExtensions: string;
    fncCookie: string;
    fncHeaders: string;
    fncResultHttp: integer;

    fOnReceiveConnection: TswsWebSocketServerReceiveConnection;
  protected
    function CreateServerConnection(aSocket: TswsTCPCustomConnectionSocket): TswsCustomConnection; override;
    procedure DoSyncReceiveConnection;
    procedure SyncReceiveConnection;
    property Terminated;

    {This function defines what kind of TswsWebSocketServerConnection implementation should be used as
      a connection object.
      The servers default return value is TswsWebSocketServerConnection.

      If new connection class based on TswsWebSocketServerConnection is implemented,
      new server should be implemented as well with this method overloaded

      properties are representing connection properties as defined in @link(TswsWebSocketServerConnection)

      Protocol and Extension represents corresponding headers sent by client, as their out value
      server must define what kind of protocol(s) and extension(s) server is supporting, if event
      is not implemented, both values are cosidered as - (no value at all)

      HttpResult represents the HTTP result to be send in response, if connection is about to be
      accepted, the value MUST BE 101, any other value meand that the client will be informed about the
      result (using the HTTP code meaning) and connection will be closed, if event is not implemented
      101 is used as a default value }
    function GetWebSocketConnectionClass(Socket: TswsTCPCustomConnectionSocket; Header: TStringList;
      aResourceName, aHost, aPort, aOrigin, aCookie: string;
      out HttpResult: integer;
      var Protocol, Extensions: string): TswsWebSocketServerConnections; virtual;

  public
    // WebSocket connection received
    property OnReceiveConnection: TswsWebSocketServerReceiveConnection read fOnReceiveConnection write fOnReceiveConnection;

    { close all connections
    for parameters see connection Close method }
    procedure CloseAllConnections(aCloseCode: integer; aReason: string);


    {Temination procedure
      This method should be called instead of Terminate to terminate thread,
      it internally calls Terminate, but can be overloaded,
      and can be used for data clean up }
    procedure TerminateThread; override;

    { Method to send binary data to all connected clients
      see @link(TswsWebSocketServerConnection.SendBinary) for parameters description }
    procedure BroadcastBinary(aData: TStream; aFinal: boolean = True; aRes1: boolean = False; aRes2: boolean = False; aRes3: boolean = False);

    { Method to send text data to all connected clients
      see @link(TswsWebSocketServerConnection.SendText) for parameters description }
    procedure BroadcastText(aData: string; aFinal: boolean = True; aRes1: boolean = False; aRes2: boolean = False; aRes3: boolean = False);

  end;

var
  VarVsSynchronizeThreads: boolean = False;

implementation

{$IFDEF WINDOWS} {$O-} {$ENDIF}

var
  fConnectionsIndex: integer = 0;

function httpCode(code: integer): string;
begin
  case (code) of
    100: Result := 'Continue';
    101: Result := 'Switching Protocols';
    200: Result := 'OK';
    201: Result := 'Created';
    202: Result := 'Accepted';
    203: Result := 'Non-Authoritative Information';
    204: Result := 'No Content';
    205: Result := 'Reset Content';
    206: Result := 'Partial Content';
    300: Result := 'Multiple Choices';
    301: Result := 'Moved Permanently';
    302: Result := 'Found';
    303: Result := 'See Other';
    304: Result := 'Not Modified';
    305: Result := 'Use Proxy';
    307: Result := 'Temporary Redirect';
    400: Result := 'Bad Request';
    401: Result := 'Unauthorized';
    402: Result := 'Payment Required';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    406: Result := 'Not Acceptable';
    407: Result := 'Proxy Authentication Required';
    408: Result := 'Request Time-out';
    409: Result := 'Conflict';
    410: Result := 'Gone';
    411: Result := 'Length Required';
    412: Result := 'Precondition Failed';
    413: Result := 'Request Entity Too Large';
    414: Result := 'Request-URI Too Large';
    415: Result := 'Unsupported Media Type';
    416: Result := 'Requested range not satisfiable';
    417: Result := 'Expectation Failed';
    500: Result := 'Internal Server Error';
    501: Result := 'Not Implemented';
    502: Result := 'Bad Gateway';
    503: Result := 'Service Unavailable';
    504: Result := 'Gateway Time-out';
    else
      Result := 'unknown code: $code';
  end;
end;


function ReadHttpHeaders(aSocket: TswsTCPCustomConnectionSocket; var aGet: string; aHeaders: TStrings): boolean;
var
  s, Name: string;
begin
  aGet := '';
  aHeaders.Clear;
  Result := True;
  repeat
    aSocket.MaxLineLength := 1024 * 1024; // not to attack memory on server
    s := aSocket.RecvString(30 * 1000); // not to hang up connection
    if (aSocket.LastError <> 0) then
    begin
      Result := False;
      break;
    end;
    if (s = '') then
      break;
    if (aGet = '') then
      aGet := s
    else
    begin
      Name := LowerCase(trim(SeparateLeft(s, ':')));
      if (aHeaders.Values[Name] = '') then
        aHeaders.Values[Name] := trim(SeparateRight(s, ':'))
      else
        aHeaders.Values[Name] := aHeaders.Values[Name] + ',' + trim(SeparateRight(s, ':'));
    end;
  until {IsTerminated} False;
  aSocket.MaxLineLength := 0;
end;

procedure ODS(aStr: string); overload;
begin
  {$IFDEF WINDOWS}
  OutputDebugString(PChar(FormatDateTime('yyyy-mm-dd hh:nn:ss', now) + ': ' + aStr));
  {$ENDIF}
end;

procedure ODS(aStr: string; aData: array of const); overload;
begin
  {$IFDEF WINDOWS}
  ODS(Format(aStr, aData));
  {$ENDIF}
end;

function getConnectionIndex: integer;
begin
  Result := fConnectionsIndex;
  Inc(fConnectionsIndex);
end;

function GetByte(aSocket: TswsTCPCustomConnectionSocket; var aByte: byte; var aTimeout: integer): integer;
begin
  aByte := aSocket.RecvByte(aTimeout);
  Result := aSocket.LastError;
end;

function hexToStr(aDec: integer; aLength: integer): string;
var
  tmp: string;
  i: integer;
begin
  tmp := IntToHex(aDec, aLength);
  Result := '';
  for i := 1 to (Length(tmp) + 1) div 2 do
  begin
    Result := Result + ansichar(StrToInt('$' + Copy(tmp, i * 2 - 1, 2)));
  end;
end;

function StrToHexstr2(str: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(str) do
    Result := Result + IntToHex(Ord(str[i]), 2) + ' ';
end;

//============== TSwsBaseThread ==============================================

constructor TSwsBaseThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  fSyncLock := TCriticalSection.Create;
end;

destructor TSwsBaseThread.Destroy;
begin
  fSyncLock.Free;
  inherited;
end;

procedure TSwsBaseThread.Synchronize(AMethod: TThreadMethod);
begin
  if (VarVsSynchronizeThreads) or (GetCurrentThreadID = MainThreadID) then
    aMethod
  else
    inherited Synchronize(aMethod);
end;

//================= TswsCustomServer =====================

constructor TswsCustomServer.Create(aBind: string; aPort: string);
begin
  fBind := aBind;
  fPort := aPort;

  FreeOnTerminate := True;
  fConnections := TList.Create;
  fConnectionTermLock := SyncObjs.TCriticalSection.Create;   //--- ct9999 ----
  fMaxConnectionsCount := -1;
  fCanAddConnection := True;
  fCurrentAddConnection := nil;
  fCurrentRemoveConnection := nil;
  fCurrentSocket := nil;
  fIndex := getConnectionIndex;
  inherited Create(True);
end;

destructor TswsCustomServer.Destroy;
begin
  fConnectionTermLock.Free;
  fConnections.Free;
  inherited Destroy;
end;

procedure TswsCustomServer.OnConnectionTerminate(Sender: TObject);
begin
  try
    RemoveConnection(TswsCustomConnection(Sender));
  finally
  end;
end;

procedure TswsCustomServer.RemoveConnection(aConnection: TswsCustomConnection);
var
  xindex: integer;
begin
  xindex := fConnections.IndexOf(aConnection);
  if (xindex <> -1) then
  begin
    fCurrentRemoveConnection := aConnection;
    DoBeforeRemoveConnection;
    fConnectionTermLock.Enter;
    fConnections.Extract(aConnection);
    fConnectionTermLock.Leave;
    DoAfterRemoveConnection;
  end;
end;

procedure TswsCustomServer.DoAfterAddConnection;
begin
  if (assigned(fOnAfterAddConnection)) then
    Synchronize(@SyncAfterAddConnection);
end;

procedure TswsCustomServer.DoBeforeAddConnection;
begin
  if (assigned(fOnBeforeAddConnection)) then
    Synchronize(@SyncBeforeAddConnection);
end;

procedure TswsCustomServer.DoAfterRemoveConnection;
begin
  if (assigned(fOnAfterRemoveConnection)) then
    Synchronize(@SyncAfterRemoveConnection);
end;

procedure TswsCustomServer.DoBeforeRemoveConnection;
begin
  if (assigned(fOnBeforeRemoveConnection)) then
    Synchronize(@SyncBeforeRemoveConnection);
end;

procedure TswsCustomServer.DoSocketError;
begin
  if (assigned(fOnSocketErrot)) then
    Synchronize(@SyncSocketError);
end;

procedure TswsCustomServer.SyncAfterAddConnection;
begin
  if (assigned(fOnAfterAddConnection)) then
    fOnAfterAddConnection(self, fCurrentAddConnection);
end;

procedure TswsCustomServer.SyncBeforeAddConnection;
begin
  if (assigned(fOnBeforeAddConnection)) then
    fOnBeforeAddConnection(self, fCurrentAddConnection, fCanAddConnection);
end;

procedure TswsCustomServer.SyncAfterRemoveConnection;
begin
  if (assigned(fOnAfterRemoveConnection)) then
    fOnAfterRemoveConnection(self, fCurrentRemoveConnection);
end;

procedure TswsCustomServer.SyncBeforeRemoveConnection;
begin
  if (assigned(fOnBeforeRemoveConnection)) then
    fOnBeforeRemoveConnection(self, fCurrentRemoveConnection);
end;

procedure TswsCustomServer.SyncSocketError;
begin
  if (assigned(fOnSocketErrot)) then
    fOnSocketErrot(self, fCurrentSocket);
end;

procedure TswsCustomServer.TerminateThread;
begin
  if (terminated) then
    exit;
  Terminate;
end;

function TswsCustomServer.GetCount: integer;
begin
  Result := fConnections.Count;
end;

function TswsCustomServer.GetConnection(index: integer): TswsCustomConnection;
begin
  fConnectionTermLock.Enter;
  Result := TswsCustomConnection(fConnections[index]);
  fConnectionTermLock.Leave;
end;

function TswsCustomServer.GetConnectionByIndex(index: integer): TswsCustomConnection;
var
  i: integer;
begin
  Result := nil;
  fConnectionTermLock.Enter;
  for i := 0 to fConnections.Count - 1 do
  begin
    if (TswsCustomConnection(fConnections[i]).Index = index) then
    begin
      Result := TswsCustomConnection(fConnections[i]);
      break;
    end;
  end;
  fConnectionTermLock.Leave;
end;

function TswsCustomServer.CreateServerConnection(aSocket: TswsTCPCustomConnectionSocket): TswsCustomConnection;
begin
  Result := nil;
end;

function TswsCustomServer.AddConnection(var aSocket: TswsTCPCustomConnectionSocket): TswsCustomConnection;
begin
  if ((fMaxConnectionsCount = -1) or (fConnections.Count < fMaxConnectionsCount)) then
  begin
    Result := CreateServerConnection(aSocket);
    if (Result <> nil) then
    begin
      Result.fParent := self;
      fCurrentAddConnection := Result;
      fCanAddConnection := True;
      DoBeforeAddConnection;
      if (fCanAddConnection) then
      begin
        fConnections.add(Result);
        DoAfterAddConnection;
        Result.Resume;
      end
      else
      begin
        FreeAndNil(Result);
      end;
    end;

  end;
end;

procedure TswsCustomServer.Execute;
var
  c: TswsCustomConnection;
  s: TswsTCPCustomConnectionSocket;
  sock: TSocket;
  i: integer;
begin
  fCurrentSocket := TTCPBlockSocket.Create;
  with fCurrentSocket do
  begin
    CreateSocket;
    if lastError <> 0 then
      DoSocketError;
    SetLinger(True, 10000);
    if lastError <> 0 then
      DoSocketError;
    bind(fBind, fPort);
    if lastError <> 0 then
      DoSocketError;
    listen;
    if lastError <> 0 then
      DoSocketError;
    repeat
      if terminated then
        break;
      if canread(1000) then
      begin
        if LastError = 0 then
        begin
          sock := Accept;
          if lastError = 0 then
          begin
            s := TswsTCPCustomConnectionSocket.Create;
            s.Socket := sock;

            if (fSSL) then
            begin
              s.SSL.CertificateFile := fSSLCertificateFile;
              s.SSL.PrivateKeyFile := fSSLPrivateKeyFile;

              if (SSLKeyPassword <> '') then
                s.SSL.KeyPassword := fSSLKeyPassword;
              s.SSLAcceptConnection;
              i := s.SSL.LastError;
              if (i <> 0) then
              begin
                FreeAndNil(s);
              end;
            end;
            if (s <> nil) then
            begin
              s.GetSins;
              c := AddConnection(s);
              if (c = nil) and (s <> nil) then
                s.Free;
            end;
          end
          else
          begin
            DoSocketError;
          end;
        end
        else
        begin
          if lastError <> WSAETIMEDOUT then
            DoSocketError;
        end;
      end;
    until False;
  end;
  fOnAfterAddConnection := nil;
  fOnBeforeAddConnection := nil;
  fOnAfterRemoveConnection := nil;
  fOnBeforeRemoveConnection := nil;
  fOnSocketErrot := nil;

  for i := fConnections.Count - 1 downto 0 do
  begin
    c := TswsCustomConnection(fConnections[i]);
    try
      OnConnectionTerminate(c);
      c.TerminateThread;

    {$IFDEF WINDOWS}
      WaitForSingleObject(c.Handle, 100);
    {$ELSE}
      sleep(100);
    {$ENDIF}

    finally
    end;
  end;

  FreeAndNil(fCurrentSocket);
end;

procedure TswsCustomServer.LockTermination;
begin
  fConnectionTermLock.Enter;
end;

procedure TswsCustomServer.Start;
begin
  Resume;
end;

procedure TswsCustomServer.Stop;
begin
  Suspend;
end;

procedure TswsCustomServer.UnLockTermination;
begin
  fConnectionTermLock.Leave;
end;


//===================== TswsTCPCustomConnectionSocket =====================================

constructor TswsTCPCustomConnectionSocket.Create;
begin
  inherited Create;
  fConnection := nil;
  OnStatus := @DoOnStatus;
end;

destructor TswsTCPCustomConnectionSocket.Destroy;
begin
  OnStatus := nil;
  OnSyncStatus := nil;
  inherited;
end;

procedure TswsTCPCustomConnectionSocket.DoOnStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
begin
  if (fConnection <> nil) and (not fConnection.terminated) and (assigned(fOnSyncStatus)) then
  begin
    fCurrentStatusReason := Reason;
    fCurrentStatusValue := Value;
    fConnection.Synchronize(@SyncOnStatus);
  end;
end;

procedure TswsTCPCustomConnectionSocket.SyncOnStatus;
begin
  if (assigned(fOnSyncStatus)) then
    fOnSyncStatus(self, fCurrentStatusReason, fCurrentStatusValue);
end;


//================= TswsCustomConnection =========================================

constructor TswsCustomConnection.Create(aSocket: TswsTCPCustomConnectionSocket);
begin
  fSocket := aSocket;
  fSocket.fConnection := self;
  FreeOnTerminate := True;
  fIndex := getConnectionIndex;
  inherited Create(True);
end;

destructor TswsCustomConnection.Destroy;
begin
  if (fSocket <> nil) then
  begin
    fSocket.OnSyncStatus := nil;
    fSocket.OnStatus := nil;
    fSocket.Free;
  end;

  inherited Destroy;
end;

procedure TswsCustomConnection.Execute;
begin
  if (BeforeExecuteConnection) then
  begin
    ExecuteConnection;
    AfterConnectionExecute;
  end;
  if (fParent <> nil) then
    if (not fParent.Terminated) then
      fParent.OnConnectionTerminate(self);
end;


procedure TswsCustomConnection.Start;
begin
  Resume;
end;

procedure TswsCustomConnection.Stop;
begin
  Suspend;
end;

procedure TswsCustomConnection.TerminateThread;
begin
  if (terminated) then
    exit;

  Socket.OnSyncStatus := nil;
  Socket.OnStatus := nil;
  Terminate;
end;

function TswsCustomConnection.GetIsTerminated: boolean;
begin
  Result := terminated or (fSocket = nil);// or (fSocket.Socket = INVALID_SOCKET);
end;

procedure TswsCustomConnection.AfterConnectionExecute;
begin

end;

function TswsCustomConnection.BeforeExecuteConnection: boolean;
begin
  Result := True;
end;

procedure TswsCustomConnection.ExecuteConnection;
begin

end;


//===================== TswsWebSocketServer ======================================

procedure TswsWebSocketServer.BroadcastBinary(aData: TStream; aFinal: boolean = True; aRes1: boolean = False;
  aRes2: boolean = False; aRes3: boolean = False);
var
  i: integer;
begin
  LockTermination;
  for i := 0 to fConnections.Count - 1 do
  begin
    if (not TswsWebSocketServerConnection(fConnections[i]).IsTerminated) then
      TswsWebSocketServerConnection(fConnections[i]).SendBinary(aData, aFinal, aRes1, aRes2, aRes3);
  end;
  UnLockTermination;
end;

procedure TswsWebSocketServer.BroadcastText(aData: string; aFinal: boolean = True; aRes1: boolean = False;
  aRes2: boolean = False; aRes3: boolean = False);
var
  i: integer;
begin
  LockTermination;
  for i := 0 to fConnections.Count - 1 do
  begin
    if (not TswsWebSocketServerConnection(fConnections[i]).IsTerminated) then
      TswsWebSocketServerConnection(fConnections[i]).SendText(aData, aFinal, aRes1, aRes2, aRes3);
  end;
  UnLockTermination;
end;

procedure TswsWebSocketServer.CloseAllConnections(aCloseCode: integer; aReason: string);
var
  i: integer;
begin
  LockTermination;
  for i := fConnections.Count - 1 downto 0 do
  begin
    if (not TswsWebSocketServerConnection(fConnections[i]).IsTerminated) then
      TswsWebSocketServerConnection(fConnections[i]).Close(aCloseCode, aReason);
  end;
  UnLockTermination;

end;

function TswsWebSocketServer.CreateServerConnection(aSocket: TswsTCPCustomConnectionSocket): TswsCustomConnection;
var
  headers, hrs: TStringList;
  get: string;
  s, key, version: string;
  iversion, vv: integer;
  res: boolean;
  r: TswsWebSocketServerConnections;
begin
  fncSocket := aSocket;
  Result := inherited CreateServerConnection(aSocket);
  headers := TStringList.Create;
  try
    res := ReadHttpHeaders(aSocket, get, headers);
    if (res) then
    begin
      res := False;
      try
        //CHECK HTTP GET
        if ((Pos('GET ', Uppercase(get)) <> 0) and (Pos(' HTTP/1.1', Uppercase(get)) <> 0)) then
        begin
          fncResourceName := SeparateRight(get, ' ');
          fncResourceName := SeparateLeft(fncResourceName, ' ');
        end
        else
          exit;
        fncResourceName := trim(fncResourceName);

        //CHECK HOST AND PORT
        s := headers.Values['host'];
        if (s <> '') then
        begin
          fncHost := trim(s);
          fncPort := SeparateRight(fncHost, ':');
          fncHost := SeparateLeft(fncHost, ':');
        end;
        fncHost := trim(fncHost);
        fncPort := trim(fncPort);

        if (fncHost = '') then exit;

        //WEBSOCKET KEY
        s := headers.Values['sec-websocket-key'];
        if (s <> '') then
        begin
          if (Length(DecodeBase64(s)) = 16) then
          begin
            key := s;
          end;

        end;
        if (key = '') then
          exit;
        key := trim(key);

        //WEBSOCKET VERSION
        s := headers.Values['sec-websocket-version'];
        if (s <> '') then
        begin
          vv := StrToIntDef(s, -1);

          if ((vv >= 7) and (vv <= 13)) then
          begin
            version := s;
          end;
        end;
        if (version = '') then
          exit;
        version := trim(version);
        iversion := StrToIntDef(version, 13);

        if (LowerCase(headers.Values['upgrade']) <> LowerCase('websocket')) or (pos('upgrade', LowerCase(headers.Values['connection'])) = 0) then
          exit;

        //COOKIES


        fncProtocol := '-';
        fncExtensions := '-';
        fncCookie := '-';
        fncOrigin := '-';

        if (iversion < 13) then
        begin
          if (headers.IndexOfName('sec-websocket-origin') > -1) then
            fncOrigin := trim(headers.Values['sec-websocket-origin']);
        end
        else
        begin
          if (headers.IndexOfName('origin') > -1) then
            fncOrigin := trim(headers.Values['origin']);
        end;

        if (headers.IndexOfName('sec-websocket-protocol') > -1) then
          fncProtocol := trim(headers.Values['sec-websocket-protocol']);
        if (headers.IndexOfName('sec-websocket-extensions') > -1) then
          fncExtensions := trim(headers.Values['sec-websocket-extensions']);
        if (headers.IndexOfName('cookie') > -1) then
          fncCookie := trim(headers.Values['cookie']);

        fncHeaders := trim(headers.Text);

        res := True;
      finally
        if (res) then
        begin
          fncResultHttp := 101;
          hrs := TStringList.Create;
          hrs.Assign(headers);
          r := GetWebSocketConnectionClass(fncSocket, hrs, fncResourceName, fncHost, fncPort, fncOrigin,
            fncCookie, fncResultHttp, fncProtocol, fncExtensions);
          if (assigned(r)) then
          begin
            DoSyncReceiveConnection;
            if (fncResultHttp <> 101) then //HTTP ERROR FALLBACK
            begin
              aSocket.SendString(Format('HTTP/1.1 %d %s' + #13#10, [fncResultHttp, httpCode(fncResultHttp)]));
              aSocket.SendString(Format('%d %s' + #13#10#13#10, [fncResultHttp, httpCode(fncResultHttp)]));
            end
            else
            begin

              key := EncodeBase64(SHA1(key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));

              s := 'HTTP/1.1 101 Switching Protocols' + #13#10;
              s := s + 'Upgrade: websocket' + #13#10;
              s := s + 'Connection: Upgrade' + #13#10;
              s := s + 'Sec-WebSocket-Accept: ' + key + #13#10;
              if (fncProtocol <> '-') then
              begin
                s := s + 'Sec-WebSocket-Protocol: ' + fncProtocol + #13#10;
              end;
              if (fncExtensions <> '-') then
              begin
                s := s + 'Sec-WebSocket-Extensions: ' + fncExtensions + #13#10;
              end;
              s := s + #13#10;

              aSocket.SendString(s);
              if (aSocket.LastError = 0) then
              begin
                Result := r.Create(aSocket);
                TswsWebSockeCustomConnection(Result).fCookie := fncCookie;
                TswsWebSockeCustomConnection(Result).fVersion := StrToInt(version);
                TswsWebSockeCustomConnection(Result).fProtocol := fncProtocol;
                TswsWebSockeCustomConnection(Result).fResourceName := fncResourceName;
                TswsWebSockeCustomConnection(Result).fOrigin := fncOrigin;
                TswsWebSockeCustomConnection(Result).fExtension := fncExtensions;
                TswsWebSockeCustomConnection(Result).fPort := fncPort;
                TswsWebSockeCustomConnection(Result).fHost := fncHost;
                TswsWebSockeCustomConnection(Result).fHeaders.Assign(headers);
                TswsWebSockeCustomConnection(Result).fHandshake := True;
              end;
            end;
          end;
          hrs.Free;
        end;
      end;
    end;
  finally
    headers.Free;
  end;
end;

procedure TswsWebSocketServer.DoSyncReceiveConnection;
begin
  if (assigned(fOnReceiveConnection)) then
    Synchronize(@SyncReceiveConnection);
end;

function TswsWebSocketServer.GetWebSocketConnectionClass(Socket: TswsTCPCustomConnectionSocket; Header: TStringList;
  aResourceName, aHost, aPort, aOrigin, aCookie: string; out HttpResult: integer; var Protocol, Extensions: string): TswsWebSocketServerConnections;
begin
  Result := TswsWebSocketServerConnection;
end;

procedure TswsWebSocketServer.SyncReceiveConnection;
var
  h: TStringList;
begin
  if (assigned(fOnReceiveConnection)) then
  begin
    h := TStringList.Create;
    h.Text := fncHeaders;
    fOnReceiveConnection(
      self, fncSocket,
      h,
      fncResourceName, fncHost, fncPort, fncOrigin, fncCookie,
      fncResultHttp, fncProtocol, fncExtensions
      );
    h.Free;
  end;
end;

procedure TswsWebSocketServer.TerminateThread;
begin
  if (terminated) then
    exit;
  fOnReceiveConnection := nil;
  inherited;
end;

//=================== TswsWebSockeCustomConnection =======================================

function TswsWebSockeCustomConnection.CanReceiveOrSend: boolean;
begin
  Result := ValidConnection and not (fClosedByMe or fClosedByPeer) and fHandshake;
end;


constructor TswsWebSockeCustomConnection.Create(aSocket: TswsTCPCustomConnectionSocket);
begin
  fHeaders := TStringList.Create;
  fCookie := '';
  fVersion := 0;
  fProtocol := '-';
  fResourceName := '';
  fOrigin := '';
  fExtension := '-';
  fPort := '';
  fHost := '';
  fClosedByMe := False;
  fClosedByPeer := False;
  fMasking := False;
  fClosingByPeer := False;
  fRequireMasking := False;


  fReadFinal := False;
  fReadRes1 := False;
  fReadRes2 := False;
  fReadRes3 := False;
  fReadCode := 0;
  fReadStream := TMemoryStream.Create;

  fWriteFinal := False;
  fWriteRes1 := False;
  fWriteRes2 := False;
  fWriteRes3 := False;
  fWriteCode := 0;
  fWriteStream := TMemoryStream.Create;

  fFullDataProcess := False;
  fFullDataStream := TMemoryStream.Create;

  fSendCriticalSection := SyncObjs.TCriticalSection.Create; //--- ct9999 ----
  fHandshake := False;

  inherited;

end;

destructor TswsWebSockeCustomConnection.Destroy;
begin
  fSendCriticalSection.Free;
  fFullDataStream.Free;
  fWriteStream.Free;
  fReadStream.Free;
  fHeaders.Free;
  inherited;
end;

procedure TswsWebSockeCustomConnection.DoSyncClose;
begin
  if (assigned(fOnClose)) then
    Synchronize(@SyncClose);

end;

procedure TswsWebSockeCustomConnection.DoSyncOpen;
begin
  if (assigned(fOnOpen)) then
    Synchronize(@SyncOpen);
end;

procedure TswsWebSockeCustomConnection.DoSyncRead;
begin
  fReadStream.Position := 0;
  if (assigned(fOnRead)) then
    Synchronize(@SyncRead);

end;

procedure TswsWebSockeCustomConnection.DoSyncReadFull;
begin
  fFullDataStream.Position := 0;
  if (assigned(fOnReadFull)) then
    Synchronize(@SyncReadFull);
end;

procedure TswsWebSockeCustomConnection.DoSyncWrite;
begin
  if (assigned(fOnWrite)) then
    Synchronize(@SyncWrite);
end;

procedure TswsWebSockeCustomConnection.ExecuteConnection;
var
  Result: integer;
  closeCode: integer;
  closeResult: string;
  s: string;
  lastDataCode, lastDataCode2: integer;
begin
  DoSyncOpen;
  try
    lastDataCode := -1;
    lastDataCode2 := -1;
    while CanReceiveOrSend do
    begin
      Result := ReadData(fReadFinal, fReadRes1, fReadRes2, fReadRes3, fReadCode, fReadStream);
      if (CanReceiveOrSend) then
      begin
        if (Result = 0) then // no socket error occured
        begin
          fReadStream.Position := 0;
          ProcessData(fReadFinal, fReadRes1, fReadRes2, fReadRes3, fReadCode, fReadStream);
          fReadStream.Position := 0;

          if (fReadCode in [wsCodeText, wsCodeBinary]) and fFullDataProcess then
          begin
            fFullDataStream.Size := 0;
            fFullDataStream.Position := 0;
          end;
          if (fReadCode in [wsCodeContinuation, wsCodeText, wsCodeBinary]) and fFullDataProcess then
          begin
            fReadStream.Position := 0;
            fFullDataStream.CopyFrom(fReadStream, fReadStream.Size);
            fReadStream.Position := 0;
          end;
          begin
            case fReadCode of
              wsCodeContinuation:
              begin
                if (lastDataCode = wsCodeText) then
                begin
                  s := ReadStrFromStream(fReadStream, fReadStream.size);
                  ProcessTextContinuation(fReadFinal, fReadRes1, fReadRes2, fReadRes3, s);
                  DoSyncRead;
                end
                else if (lastDataCode = wsCodeBinary) then
                begin
                  ProcessStreamContinuation(fReadFinal, fReadRes1, fReadRes2, fReadRes3, fReadStream);
                  DoSyncRead;
                end
                else
                  Close(wsCloseErrorProtocol, 'Unknown continuaton');
                if (fReadFinal) then
                  lastDataCode := -1;
              end;
              wsCodeText:
              begin // text, binary frame
                s := ReadStrFromStream(fReadStream, fReadStream.size);
                ProcessText(fReadFinal, fReadRes1, fReadRes2, fReadRes3, s);
                DoSyncRead;
                if (not fReadFinal) then
                  lastDataCode := wsCodeText
                else
                  lastDataCode := -1;
                lastDataCode2 := wsCodeText;
              end;
              wsCodeBinary:
              begin // text, binary frame
                ProcessStream(fReadFinal, fReadRes1, fReadRes2, fReadRes3, fReadStream);
                DoSyncRead;
                if (not fReadFinal) then
                  lastDataCode := wsCodeBinary
                else
                  lastDataCode := -1;
                lastDataCode2 := wsCodeBinary;
              end;
              wsCodeClose:
              begin //connection close
                closeCode := wsCloseNoStatus;
                closeResult := ReadStrFromStream(fReadStream, fReadStream.size);
                if (length(closeResult) > 1) then
                begin
                  closeCode := Ord(closeResult[1]) * 256 + Ord(closeResult[2]);
                  Delete(closeResult, 1, 2);
                end;
                fClosedByPeer := True;
                ProcessClose(closeCode, closeResult, True);
                TerminateThread;
                fSendCriticalSection.Enter;
              end;
              wsCodePing:
              begin // ping
                ProcessPing(ReadStrFromStream(fReadStream, fReadStream.size));
                DoSyncRead;
              end;
              wsCodePong:
              begin // pong
                ProcessPong(ReadStrFromStream(fReadStream, fReadStream.size));
                DoSyncRead;
              end
              else
              begin //ERROR
                Close(wsCloseErrorData, Format('Unknown data type: %d', [fReadCode]));
              end;

            end;
          end;

          if (fReadCode in [wsCodeContinuation, wsCodeText, wsCodeBinary]) and fFullDataProcess and fReadFinal then
          begin
            fFullDataStream.Position := 0;
            if (lastDataCode2 = wsCodeText) then
            begin
              s := ReadStrFromStream(fFullDataStream, fFullDataStream.size);
              ProcessTextFull(s);
            end
            else if (lastDataCode2 = wsCodeBinary) then
              ProcessStreamFull(fFullDataStream);
            SyncReadFull;
          end;
        end
        else
          TerminateThread;
      end;
    end;
  finally
    {$IFDEF UNIX}
    sleep(2000);
{$ENDIF UNIX}
  end;
  while not terminated do
    sleep(500);

  fSendCriticalSection.Enter;
end;

function TswsWebSockeCustomConnection.GetClosed: boolean;
begin
  Result := not CanReceiveOrSend;
end;

function TswsWebSockeCustomConnection.GetClosing: boolean;
begin
  Result := (fClosedByMe or fClosedByPeer);
end;

procedure TswsWebSockeCustomConnection.Ping(aData: string);
begin
  if (CanReceiveOrSend) then
  begin
    SendData(True, False, False, False, wsCodePing, aData);
  end;
end;

procedure TswsWebSockeCustomConnection.Pong(aData: string);
begin
  if (CanReceiveOrSend) then
  begin
    SendData(True, False, False, False, wsCodePong, aData);
  end;
end;

procedure TswsWebSockeCustomConnection.ProcessClose(aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
begin
  fCloseCode := aCloseCode;
  fCloseReason := aCloseReason;
  fClosingByPeer := aClosedByPeer;
  DoSyncClose;
end;

procedure TswsWebSockeCustomConnection.ProcessData(var aFinal, aRes1, aRes2, aRes3: boolean; var aCode: integer; aData: TMemoryStream);
begin

end;

procedure TswsWebSockeCustomConnection.ProcessPing(aData: string);
begin
  Pong(aData);
end;

procedure TswsWebSockeCustomConnection.ProcessPong(aData: string);
begin

end;

procedure TswsWebSockeCustomConnection.ProcessStream(aFinal, aRes1, aRes2, aRes3: boolean; aData: TMemoryStream);
begin

end;

procedure TswsWebSockeCustomConnection.ProcessStreamContinuation(aFinal, aRes1, aRes2, aRes3: boolean; aData: TMemoryStream);
begin

end;

procedure TswsWebSockeCustomConnection.ProcessStreamFull(aData: TMemoryStream);
begin

end;

procedure TswsWebSockeCustomConnection.ProcessText(aFinal, aRes1, aRes2, aRes3: boolean; aData: string);
begin

end;

procedure TswsWebSockeCustomConnection.ProcessTextContinuation(aFinal, aRes1, aRes2, aRes3: boolean; aData: string);
begin

end;

procedure TswsWebSockeCustomConnection.ProcessTextFull(aData: string);
begin

end;

function TswsWebSockeCustomConnection.ReadData(var aFinal, aRes1, aRes2, aRes3: boolean; var aCode: integer; aData: TMemoryStream): integer;
var
  timeout: integer;
  b: byte;
  mask: boolean;
  len, i: int64;
  mBytes: array[0..3] of byte;
  ms: TMemoryStream;
begin
  Result := 0;
  len := 0;

  repeat
    timeout := 10 * 1000;
    if CanReceiveOrSend then
    begin

      if (fSocket.CanReadEx(1000)) then
      begin
        if CanReceiveOrSend then
        begin
          b := fSocket.RecvByte(1000);
          if (fSocket.LastError = 0) then
          begin
            try
              try
                // BASIC INFORMATIONS
                aFinal := (b and $80) = $80;
                aRes1 := (b and $40) = $40;
                aRes2 := (b and $20) = $20;
                aRes3 := (b and $10) = $10;
                aCode := b and $F;


                // MASK AND LENGTH
                mask := False;
                Result := GetByte(fSocket, b, timeout);
                if (Result = 0) then
                begin
                  mask := (b and $80) = $80;
                  len := (b and $7F);
                  if (len = 126) then
                  begin
                    Result := GetByte(fSocket, b, timeout);
                    if (Result = 0) then
                    begin
                      len := b * $100; // 00 00
                      Result := GetByte(fSocket, b, timeout);
                      if (Result = 0) then
                      begin
                        len := len + b;
                      end;
                    end;
                  end
                  else if (len = 127) then    //00 00 00 00 00 00 00 00
                  begin

                    //TODO nesting og get byte should be different
                    Result := GetByte(fSocket, b, timeout);
                    if (Result = 0) then
                    begin
                      len := b * $100000000000000;
                      if (Result = 0) then
                      begin
                        Result := GetByte(fSocket, b, timeout);
                        len := len + b * $1000000000000;
                      end;
                      if (Result = 0) then
                      begin
                        Result := GetByte(fSocket, b, timeout);
                        len := len + b * $10000000000;
                      end;
                      if (Result = 0) then
                      begin
                        Result := GetByte(fSocket, b, timeout);
                        len := len + b * $100000000;
                      end;
                      if (Result = 0) then
                      begin
                        Result := GetByte(fSocket, b, timeout);
                        len := len + b * $1000000;
                      end;
                      if (Result = 0) then
                      begin
                        Result := GetByte(fSocket, b, timeout);
                        len := len + b * $10000;
                      end;
                      if (Result = 0) then
                      begin
                        Result := GetByte(fSocket, b, timeout);
                        len := len + b * $100;
                      end;
                      if (Result = 0) then
                      begin
                        Result := GetByte(fSocket, b, timeout);
                        len := len + b;
                      end;
                    end;
                  end;
                end;

                if (Result = 0) and (fRequireMasking) and (not mask) then
                begin
                  raise Exception.Create('mask');
                end;

                // MASKING KEY
                if (mask) and (Result = 0) then
                begin
                  Result := GetByte(fSocket, mBytes[0], timeout);
                  if (Result = 0) then
                    Result := GetByte(fSocket, mBytes[1], timeout);
                  if (Result = 0) then
                    Result := GetByte(fSocket, mBytes[2], timeout);
                  if (Result = 0) then
                    Result := GetByte(fSocket, mBytes[3], timeout);
                end;
                // READ DATA
                if (Result = 0) then
                begin
                  aData.Clear;
                  ms := TMemoryStream.Create;
                  try
                    timeout := 1000 * 60 * 60 * 2; //(len div (1024 * 1024)) * 1000 * 60;
                    if (mask) then
                      fSocket.RecvStreamSize(ms, timeout, len)
                    else
                      fSocket.RecvStreamSize(aData, timeout, len);

                    ms.Position := 0;
                    aData.Position := 0;
                    Result := fSocket.LastError;
                    if (Result = 0) then
                    begin
                      if (mask) then
                      begin
                        i := 0;
                        while i < len do
                        begin
                          ms.ReadBuffer(b, sizeOf(b));
                          b := b xor mBytes[i mod 4];
                          aData.WriteBuffer(b, SizeOf(b));
                          Inc(i);
                        end;
                      end;
                    end;
                  finally
                    ms.Free;
                  end;
                  aData.Position := 0;
                  break;
                end;
              except
                Result := -1;
              end;
            finally
            end;
          end
          else
          begin
            Result := -1;
          end;
        end
        else
        begin
          Result := -1;
        end;
      end
      else
      begin

        if (fSocket.LastError <> WSAETIMEDOUT) and (fSocket.LastError <> 0) then
        begin
          Result := -1;
        end;
      end;
    end
    else
    begin
      Result := -1;
    end;
    if (Result <> 0) then
    begin
      if (not Terminated) then
      begin
        if (fSocket.LastError = WSAECONNRESET) then
        begin
          Result := 0;
          aCode := wsCodeClose;
          aFinal := True;
          aRes1 := False;
          aRes2 := False;
          aRes3 := False;
          aData.Size := 0;
          WriteStrToStream(aData, ansichar(wsCloseErrorClose div 256) + ansichar(wsCloseErrorClose mod 256));
          aData.Position := 0;
        end
        else
        begin
          if (not fClosedByMe) then
          begin
            Close(wsCloseErrorProtocol, '');
            TerminateThread;
          end;
        end;
      end;
      break;
    end
  until False;
end;


function TswsWebSockeCustomConnection.SendData(aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TStream): integer;
var
  b: byte;
  s: ansistring;
  mBytes: array[0..3] of byte;
  ms: TMemoryStream;
  i, len: int64;
begin
  Result := 0;
  if (CanReceiveOrSend) or ((aCode = wsCodeClose) and (not fClosedByPeer)) then
  begin
    fSendCriticalSection.Enter;
    try

      s := '';

      // BASIC INFORMATIONS
      b := IfThen(aFinal, 1, 0) * $80;
      b := b + IfThen(aRes1, 1, 0) * $40;
      b := b + IfThen(aRes2, 1, 0) * $20;
      b := b + IfThen(aRes3, 1, 0) * $10;
      b := b + aCode;
      s := s + ansichar(b);

      // MASK AND LENGTH
      b := IfThen(fMasking, 1, 0) * $80;
      if (aData.Size < 126) then
        b := b + aData.Size
      else if (aData.Size < 65536) then
        b := b + 126
      else
        b := b + 127;
      s := s + ansichar(b);
      if (aData.Size >= 126) then
      begin
        if (aData.Size < 65536) then
        begin
          s := s + hexToStr(aData.Size, 4);
        end
        else
        begin
          s := s + hexToStr(aData.Size, 16);
        end;
      end;

      // MASKING KEY
      if (fMasking) then
      begin
        mBytes[0] := Random(256);
        mBytes[1] := Random(256);
        mBytes[2] := Random(256);
        mBytes[3] := Random(256);


        s := s + ansichar(mBytes[0]);
        s := s + ansichar(mBytes[1]);
        s := s + ansichar(mBytes[2]);
        s := s + ansichar(mBytes[3]);
      end;

      fSocket.SendString(s);
      Result := fSocket.LastError;
      if (Result = 0) then
      begin
        aData.Position := 0;
        ms := TMemoryStream.Create;
        try
          if (not fMasking) then
          begin
            fSocket.SendStreamRaw(aData);
          end
          else
          begin
            i := 0;
            len := aData.Size;
            while i < len do
            begin
              aData.ReadBuffer(b, sizeOf(b));
              b := b xor mBytes[i mod 4];
              ms.WriteBuffer(b, SizeOf(b));
              Inc(i);
            end;
            ms.Position := 0;
            fSocket.SendStreamRaw(ms);
          end;

          Result := fSocket.LastError;
          if (Result = 0) then
          begin
            fWriteFinal := aFinal;
            fWriteRes1 := aRes1;
            fWriteRes2 := aRes2;
            fWriteRes3 := aRes3;
            fWriteCode := aCode;
            aData.Position := 0;
            fWriteStream.Clear;
            fWriteStream.LoadFromStream(aData);
            DoSyncWrite;
          end;

        finally
          ms.Free;
        end;
      end;
    finally
      if (aCode <> wsCodeClose) then
        while not fSocket.CanWrite(10) do
          sleep(10);
      fSendCriticalSection.Leave;
    end;
  end;
end;

function TswsWebSockeCustomConnection.SendData(aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: string): integer;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    WriteStrToStream(ms, aData);
    Result := SendData(aFinal, aRes1, aRes2, aRes3, aCode, ms);
  finally
    ms.Free;
  end;
end;

procedure TswsWebSockeCustomConnection.SendBinary(aData: TStream; aFinal: boolean = True; aRes1: boolean = False;
  aRes2: boolean = False; aRes3: boolean = False);
begin
  SendData(aFinal, aRes1, aRes2, aRes3, wsCodeBinary, aData);
end;

procedure TswsWebSockeCustomConnection.SendBinaryContinuation(aData: TStream; aFinal, aRes1, aRes2, aRes3: boolean);
begin
  SendData(aFinal, aRes1, aRes2, aRes3, wsCodeContinuation, aData);
end;

procedure TswsWebSockeCustomConnection.SendText(aData: string; aFinal: boolean = True; aRes1: boolean = False;
  aRes2: boolean = False; aRes3: boolean = False);
begin
  SendData(aFinal, aRes1, aRes2, aRes3, wsCodeText, aData);
end;

procedure TswsWebSockeCustomConnection.SendTextContinuation(aData: string; aFinal, aRes1, aRes2, aRes3: boolean);
begin
  SendData(aFinal, aRes1, aRes2, aRes3, wsCodeContinuation, aData);
end;

procedure TswsWebSockeCustomConnection.SyncClose;
begin
  if (assigned(fOnClose)) then
    fOnClose(self, fCloseCode, fCloseReason, fClosingByPeer);
end;

procedure TswsWebSockeCustomConnection.SyncOpen;
begin
  if (assigned(fOnOpen)) then
    fOnOpen(self);
end;

procedure TswsWebSockeCustomConnection.SyncRead;
begin
  fReadStream.Position := 0;
  if (assigned(fOnRead)) then
    fOnRead(self, fReadFinal, fReadRes1, fReadRes2, fReadRes3, fReadCode, fReadStream);
end;

procedure TswsWebSockeCustomConnection.SyncReadFull;
begin
  fFullDataStream.Position := 0;
  if (assigned(fOnReadFull)) then
    fOnReadFull(self, fReadCode, fFullDataStream);
end;

procedure TswsWebSockeCustomConnection.SyncWrite;
begin
  fWriteStream.Position := 0;
  if (assigned(fOnWrite)) then
    fOnWrite(self, fWriteFinal, fWriteRes1, fWriteRes2, fWriteRes3, fWriteCode, fWriteStream);
end;

procedure TswsWebSockeCustomConnection.TerminateThread;
begin
  if (Terminated) then
    exit;

  if (not Closed) then
    DoSyncClose;
  Socket.OnSyncStatus := nil;
  Socket.OnStatus := nil;
  fOnRead := nil;
  fOnReadFull := nil;
  fOnWrite := nil;
  fOnClose := nil;
  fOnOpen := nil;
  inherited;
end;

function TswsWebSockeCustomConnection.ValidConnection: boolean;
begin
  Result := (not IsTerminated) and (Socket.Socket <> INVALID_SOCKET);
end;


//======================= TswsWebSocketServerConnection =========================================

procedure TswsWebSocketServerConnection.Close(aCode: integer; aCloseReason: string);
begin
  if (Socket.Socket <> INVALID_SOCKET) and (not fClosedByMe) then
  begin
    fClosedByMe := True;
    if (not fClosedByPeer) then
    begin
      SendData(True, False, False, False, wsCodeClose, hexToStr(aCode, 4) + copy(aCloseReason, 1, 123));
      ProcessClose(aCode, aCloseReason, False);
    end;

    TerminateThread;
  end;
end;

constructor TswsWebSocketServerConnection.Create(aSocket: TswsTCPCustomConnectionSocket);
begin
  inherited;
  fRequireMasking := True;
end;

procedure TswsWebSocketServerConnection.TerminateThread;
begin
  if (Terminated) then exit;

  fOnClose := nil;
  inherited;

end;

//============================ TswsWebSocketClientConnection =======================================

function TswsWebSocketClientConnection.BeforeExecuteConnection: boolean;
var
  key, s, get: string;
  i: integer;
  headers: TStringList;
begin
  Result := not IsTerminated;
  if (Result) then
  begin
    s := Format('GET %s HTTP/1.1' + #13#10, [fResourceName]);
    s := s + Format('Upgrade: websocket' + #13#10, []);
    s := s + Format('Connection: Upgrade' + #13#10, []);
    s := s + Format('Host: %s:%s' + #13#10, [fHost, fPort]);

    for I := 1 to 16 do
      key := key + ansichar(Random(85) + 32);
    key := EncodeBase64(key);
    s := s + Format('Sec-WebSocket-Key: %s' + #13#10, [(key)]);
    s := s + Format('Sec-WebSocket-Version: %d' + #13#10, [fVersion]);

    if (fProtocol <> '-') then
      s := s + Format('Sec-WebSocket-Protocol: %s' + #13#10, [fProtocol]);
    if (fOrigin <> '-') then
    begin
      if (fVersion < 13) then
        s := s + Format('Sec-WebSocket-Origin: %s' + #13#10, [fOrigin])
      else
        s := s + Format('Origin: %s' + #13#10, [fOrigin]);
    end;
    if (fCookie <> '-') then
      s := s + Format('Cookie: %s' + #13#10, [(fCookie)]);
    if (fExtension <> '-') then
      s := s + Format('Sec-WebSocket-Extensions: %s' + #13#10, [fExtension]);
    s := s + #13#10;
    fSocket.SendString(s);
    Result := (not IsTerminated) and (fSocket.LastError = 0);
    if (Result) then
    begin
      headers := TStringList.Create;
      try
        Result := ReadHttpHeaders(fSocket, get, headers);
        if (Result) then
          Result := pos(LowerCase('HTTP/1.1 101'), LowerCase(get)) = 1;
        if (Result) then
          Result := (LowerCase(headers.Values['upgrade']) = LowerCase('websocket')) and (LowerCase(headers.Values['connection']) = 'upgrade');
        fProtocol := '-';
        fExtension := '-';
        if (headers.IndexOfName('sec-websocket-protocol') > -1) then
          fProtocol := trim(headers.Values['sec-websocket-protocol']);
        if (headers.IndexOfName('sec-websocket-extensions') > -1) then
          fExtension := trim(headers.Values['sec-websocket-extensions']);
        if (Result) then
          Result := (headers.Values['sec-websocket-accept'] = EncodeBase64(SHA1(key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11')));

      finally
        headers.Free;
      end;
    end;

  end;
  if (Result) then
    fHandshake := True;

end;

procedure TswsWebSocketClientConnection.Close(aCode: integer; aCloseReason: string);
begin
  if ValidConnection and (not fClosedByMe) then
  begin
    fClosedByMe := True;
    if (not fClosedByPeer) then
    begin
      SendData(True, False, False, False, wsCodeClose, hexToStr(aCode, 4) + copy(aCloseReason, 1, 123));

      ProcessClose(aCode, aCloseReason, False);
    end;

    TerminateThread;
  end;
end;

constructor TswsWebSocketClientConnection.Create(aHost, aPort, aResourceName, aOrigin, aProtocol: string;
  aExtension: string; aCookie: string; aVersion: integer);
begin
  fSocket := TswsTCPCustomConnectionSocket.Create;
  inherited Create(fSocket);
  fOrigin := aOrigin;
  fHost := aHost;
  fPort := aPort;
  fResourceName := aResourceName;
  fProtocol := aProtocol;
  fVersion := aVersion;
  fMasking := True;
  fCookie := aCookie;
  fExtension := aExtension;
end;

procedure TswsWebSocketClientConnection.Execute;
begin
  if (not IsTerminated) and (fVersion >= 8) then
  begin
    fSocket.Connect(fHost, fPort);
    if (SSL) then
      fSocket.SSLDoConnect;
    if (fSocket.LastError = 0) then
    begin
      inherited Execute;
    end
    else
      TerminateThread;
  end;
end;

//===============================================

initialization
  Randomize;

end.
