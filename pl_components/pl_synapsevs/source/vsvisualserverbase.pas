
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsVisualServerBase;

interface

uses {$IFDEF UNIX}Types, {$ELSE}Windows, {$ENDIF}Classes, SysUtils, syncobjs,
  blcksock, synautil,
  vsTypeDef, vsFileLogger, vsAuthentication,
  IniFiles, typinfo;

var
  ErrorCodes: array[0..3] of string = ('', 'No listen port specified', 'Listen failed', 'Port in use');

type

  TSettings = record //shared data between component, listen thread and handler
    FBaseDir: string;
    FListenPort: string;
    FListenIP: string;
    FServerName: string;
    FThreadSafe: boolean;
    FClients: TThreadList;
    Owner: TComponent;
    FHasCustomVars: boolean; //indicate to synchronize to copy component-specific variables
    FTimeOut: integer;
    FLastError: string;
    FLastErrorCode: integer;
    FLogger: TLogger;
    FErrorLogger: TLogger;
    FDoSSL: boolean;
    FAutoTLS: boolean;
    FSSLCertCAFile: string;
    FSSLPrivateKeyFile: string;
    FSSLCertificateFile: string;
    FAuthentication: TvsAuthentication;
  end;

  TServerHandler = class;
  TVisualListen = class;

  THandlerClass = class of TServerHandler;
  TThreadClass = class of TVisualListen;

  TRequest = class(TObject) //Universal object containing the commands
    RawRequest: string;
    Command: string;
    Parameter: string; //mostly one, else in customized format
    CommandLine: string; //The full command line
    Meta: string; //Meta data
    MimeType: string; //mime and encoding, in 'text/html ; charset=iso8990' format
    Header: TStrings;
    RawHeader: TStrings;
    Data: string;
    Domain: string;
    FileName: string;
    ProtoVersion: string;
    Args: TStrings;
    constructor Create;
    procedure Clear;
    destructor Destroy; override;
  end;

  TResponse = class(TObject)
    ResponseCode: integer;
    ResponseText: string;
    Data: string;
    DataStream: TStream;
    MimeType: string;
    Header: TStrings;
    RawHeader: TStrings;
    procedure Clear; virtual;
    procedure FixHeader; virtual;
    constructor Create;
    destructor Destroy; override;
  end;


  (*
  //Request examples:
  //HTTP
  TRequest = record //Universal record containing the commands
               RawRequest: String; // GET /index.php HTTP/1.0
               Command:String; // GET
               Parameter: String; // /index.php
               Meta: String; // www.somehost.com
               ParamMeta: String // GET params (after ?), in form name=some%20data
               Header: String; //Accept-Encoding=gzip CRLF Host=www.somehost.com CRLF User-Agent=Mozilla 5.0
               Data: String; // POST or PUT data
             end;
  //SMTP
  TRequest = record //Universal record containing the commands
               RawRequest: String;
               Command:String;
               Parameter: String; //mostly one, else in customized format
               Meta: String; //Meta data
               Header: String; //In TStrings.Text format
               Data: String;
             end;
  *)

  TOnConnect = procedure(Sender: TObject; IPInfo: TIPInfo; var DoContinue: boolean) of object;
  TOnDisConnect = procedure(Sender: TObject; IPInfo: TIPInfo) of object;

  //  TOnAuthenticate = procedure (Sender: TObject; User, Pass: String; IPInfo:TIPInfo) of Object;
  TOnMustAuthenticate = procedure(Sender: TObject; Request: TRequest; var MustAuthenticate: boolean; IPInfo: TIPInfo) of object;
  TOnRequest = procedure(Sender: TObject; Request: TRequest; var Response: TResponse; IPInfo: TIPInfo; var Handled: boolean) of object;
  TOnPut = procedure(Sender: TObject; Request: TRequest; var Accepted: boolean; IPInfo: TIPInfo) of object;
  TOnListen = procedure(Sender: TObject; IPInfo: TIPInfo) of object;
  TOnError = procedure(Sender: TObject; Error: integer; ErrorMsg: string; IPInfo: TIPInfo) of object;

  TCallbacks = record
    FOnConnect: TOnConnect;
    FOnDisconnect: TOnDisconnect;
    FOnAuthenticate: TOnAuthenticate;
    FOnMustAuthenticate: TOnMustAuthenticate;
    FOnRequest: TOnRequest;
    FOnPut: TOnPut;
    FOnError: TOnError;
  end;

  TVSThread = class(TThread)
  public
    FSettings: TSettings;
    FIPInfo: TIPInfo;
    procedure Error(ErrorCode: integer);
    procedure Log(Line: string);
    procedure LogError(Line: string);
    procedure SyncError;
  end;


  TVisualListen = class(TVSThread)
  public
    //    FSettings:TSettings;
    FCallBacks: TCallBacks;
    FSock: TTCPBlockSocket;
    FHandler: THandlerClass; //TClass; for hot creating specific class
    FNewHandler: TServerHandler;
    FInitialized: boolean;
    FListening: boolean;
    procedure Execute; override;
  end;

  TServerHandler = class(TVSThread)
  protected
    FProcToCall: TOnRequest;
    procedure CallBack(proc: TThreadMethod);
    procedure CallBackRequest(Proc: TOnRequest);
    procedure SyncCallBack;
  public
    //    FSettings: TSettings;
    FCallBacks: TCallBacks;
    FSock: TTCPBlockSocket;

    //vars to communicate with callback functions:
    FDoContinue: boolean;
    FRequest: TRequest;
    FResponse: TResponse;

    //Indicates if callback event has properly handled the request
    RequestHandled: boolean;

    //Authentication vars:
    FMustAuthenticate: boolean;
    FAccepted: boolean;
    FUser: string;
    FPass: string;

    //Send queue outside events:
    FSendQueue: TStrings;
    FSendCS: TCriticalSection;

    //common procedures
    procedure Init; virtual; //register thread, connect callback
    procedure Final; virtual; //unregister thread
    procedure CallBackThreadMethod(proc: TThreadMethod);
    procedure CheckToSend;

    //override this in implementations to copy component variables to the
    //client thread
    procedure CopyCustomVars; virtual;

    //callback procedures
    procedure OnConnect;
    procedure OnDisconnect;
    procedure OnAuthenticate;
    procedure OnMustAuthenticate;
    procedure OnRequest;
    procedure OnPut;

    //override Handler (or Execute if you like but make sure to call Init and Final)
    //in your protocol handler
    procedure Handler; virtual;
    procedure Execute; override;
  end;

  TVisualServer = class(TComponent)
  private
    function GetLogFile: string;
    procedure SetLogFile(const Value: string);
    function GetSSL: boolean;
    procedure SetSSL(const Value: boolean);
    procedure SetErrorLog(const Value: string);
    function GetErrorLog: string;
  protected
    FActive: boolean;
    FMakeActive: boolean;
    FSettings: TSettings;
    FCallBacks: TCallBacks;
    FListenThread: TVisualListen;
    FServerType: TThreadClass;
    FClientType: THandlerClass;
    FIni: TMemIniFile;
    FIniSettings: TStrings;
    FAuthentication: TvsAuthentication;
    procedure Loaded; override;
    function InitIniWrite(FileName: TFileName): boolean;
    function InitIniRead(FileName: TFileName): boolean;
    procedure FinishIni;
    procedure WriteSectionValues(section: string; namevalues: TStrings);
  public
    procedure SetActive(Value: boolean);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropClient(ConnectionHandle: integer);
    procedure SendData(ConnectionHandle: integer; Data: string);
    procedure SendStream(ConnectionHandle: integer; Data: TStream);
    function SaveSettings(FileName: TFileName): boolean; virtual;
    function LoadSettings(FileName: TFileName): boolean; virtual;
    property IniSettings: TStrings read FIniSettings;
  published
    //properties
    property Active: boolean read FActive write SetActive;
    property BaseDir: string read FSettings.FBaseDir write FSettings.FBaseDir;
    property ListenIP: string read FSettings.FListenIP write FSettings.FListenIP;
    property ListenPort: string read FSettings.FListenPort write FSettings.FListenPort;
    property ServerName: string read FSettings.FServerName write FSettings.FServerName;
    property ThreadSafe: boolean read FSettings.FThreadSafe write FSettings.FThreadSafe;
    property LogFile: string read GetLogFile write SetLogFile;
    property ErrorLog: string read GetErrorLog write SetErrorLog;
    property SSL: boolean read GetSSL write SetSSL;
    property AutoTLS: boolean read FSettings.FAutoTLS write FSettings.FAutoTLS;
    property SSLCertCAFile: string read FSettings.FSSLCertCAFile write FSettings.FSSLCertCAFile;
    property SSLPrivateKeyFile: string read FSettings.FSSLPrivateKeyFile write FSettings.FSSLPrivateKeyFile;
    property SSLCertificateFile: string read FSettings.FSSLCertificateFile write FSettings.FSSLCertificateFile;
    property vsAuthentication: TvsAuthentication read FSettings.FAuthentication write FSettings.FAuthentication;

    //events
    property OnConnect: TOnConnect read FCallBacks.FOnConnect write FCallBacks.FOnConnect;
    property OnDisConnect: TOnDisConnect read FCallBacks.FOnDisConnect write FCallBacks.FOnDisConnect;
    property OnAuthenticate: TOnAuthenticate read FCallBacks.FOnAuthenticate write FCallBacks.FOnAuthenticate;
    property OnMustAuthenticate: TOnMustAuthenticate read FCallBacks.FOnMustAuthenticate write FCallBacks.FOnMustAuthenticate;
    property OnRequest: TOnRequest read FCallBacks.FOnRequest write FCallBacks.FOnRequest;
    property OnPut: TOnPut read FCallBacks.FOnPut write FCallBacks.FOnPut;
    property OnError: TOnError read FCallBacks.FOnError write FCallBacks.FOnError;
  end;

//Global procedure that creates unique auto-increment integers:
function GetConnectionHandle: integer;
function IsInNetmask(IP1, IP2, NetMask: string): boolean;

implementation

var
  ConnectionIndex: integer = 0;
  ConnectionCS: TCriticalSection;

function GetConnectionHandle: integer;
begin
  ConnectionCS.Enter;
  Inc(ConnectionIndex);
  Result := ConnectionIndex;
  ConnectionCS.Leave;
end;

function IsInNetmask(IP1, IP2, NetMask: string): boolean;

  function IPToDWord(IP: string): DWord;
  var
    i, l: integer;
    n: DWord;
    v: string;
  begin
    Result := 0;
    IP := IP + '.';
    for i := 1 to 4 do
    begin
      l := pos('.', IP);
      v := copy(IP, 1, l - 1);
      IP := copy(IP, l + 1, maxint);
      n := StrToIntDef(v, 0);
      Result := (Result shl 8) or n;
    end;
  end;

var
  n1, n2, m: DWord;

begin
  if not (IsIP(IP1) and IsIP(IP2) and IsIP(Netmask)) then
  begin
    Result := False;
    exit;
  end;
  //transform ip's and mask to 4-byte integer (ip4 only here)
  //match them
  n1 := IPToDWord(IP1);
  n2 := IPToDWord(IP2);
  m := IPToDWord(NetMask);
  Result := (n1 and m) = (n2 and m);
end;



procedure TVisualListen.Execute;
var
  Launch: boolean;
begin
  //some vars
  with FSettings do
  begin
    if FListenIP = '' then
      FListenIP := '0.0.0.0';
    if FListenPort = '' then
    begin
      FListenPort := '0';
      FLastErrorCode := -1;
      FLastError := 'No listen port specified';
      LogError('No listen port specified');
      synchronize(@syncError);
      //break here?
      //sync onerror ('no listen port')
      FInitialized := True;
      FListening := False;
      exit;
    end;
    if FServerName = '' then
      FServerName := 'localhost';
  end;


  Log(Self.ClassName + ' Server startup');
  FSock := TTCPBlockSocket.Create;
  FSock.Bind(FSettings.FListenIP, FSettings.FListenPort);
  if FSock.LastError = 0 then
    FSock.Listen
  else
  begin
    LogError('Failed to bind on ' + FSettings.FListenIP + ':' + FSettings.FListenPort);
    Log('Service could not start');
  end;
  if FSock.LastError = 0 then
  begin
    FInitialized := True;
    FListening := True;

    while not Terminated do
    begin
      if FSock.CanRead(2000) then
      begin //spawn new thread
        if Assigned(FHandler) {and (FClient = TServerHandler)} then
        begin
          FSock.GetSinRemote;
          //writeln (FSock.GetRemoteSinIP);
          FNewHandler := FHandler.Create(True);
          FNewHandler.FSock := TTCPBlockSocket.Create;
          FNewHandler.FSock.Socket := FSock.Accept;
          begin
            FNewHandler.FSettings := FSettings;
            FNewHandler.FCallBacks := FCallBacks;
            FNewHandler.FreeOnTerminate := True;
            if FSettings.FHasCustomVars then
              //                  synchronize (FNewHandler.CopyCustomVars);
              //yes, we have some threading issue here
              //if component settings gets changed while server is running.
              //fix.
              FNewHandler.CopyCustomVars;
            FNewHandler.Resume;
          end;
        end;
      end
      else
        sleep(20);
    end;
  end
  else
  begin
    FInitialized := True;
    FListening := False;
    LogError('Failed to listen on ' + FSettings.FListenIP + ':' + FSettings.FListenPort);
    Log('Service could not start');
  end;

  Log(Self.ClassName + ' Server shutdown');

  FSock.Free;
  //Atomic:
  //TVisualServer (FSettings.Owner).FActive := False;
end;


// TVisualServer  component

constructor TVisualServer.Create(AOwner: TComponent);
begin
  inherited;
  ThreadSafe := True;
  FSettings.FClients := TThreadList.Create;
  FSettings.Owner := Self;
  FSettings.FTimeOut := 30000;
  FSettings.FLogger := TLogger.Create(Self);
  FSettings.FLogger.FileName := '';
  FSettings.FErrorLogger := TLogger.Create(Self);
  FSettings.FErrorLogger.FileName := '';
  FAuthentication := TvsAuthentication.Create(Self);
  FSettings.FAuthentication := FAuthentication;
  FSettings.FAuthentication.Method := amSystem;
  FIniSettings := TStringList.Create;
end;

destructor TVisualServer.Destroy;
var
  i: integer;
  timeout: integer;
  allfinished: boolean;
begin
  Active := False;
  //signal all clients to terminate
  with FSettings.FClients.LockList do
  begin
    for i := 0 to Count - 1 do
      TServerHandler(Items[i]).Terminate;
  end;
  FSettings.FClients.UnlockList;
  //now we must wait until clients are ready
  timeout := 0;
  allfinished := False;
  //note on this locklist.
  //the problem is that we may create a deadlock if we lock the list
  //and use waitfor.
  while (not allfinished) and (timeout < 150) do //about 15 seconds
  begin
    sleep(100);
    with FSettings.FClients.LockList do
      allfinished := Count = 0;
    FSettings.FClients.UnlockList;
    Inc(timeout);
  end;

  if not allfinished then
    //start killing threads.
  begin
    with FSettings.FClients.LockList do
    begin
          {$IFNDEF UNIX}
      for i := 0 to Count - 1 do
        try
          TerminateThread(TServerHandler(Items[i]).Handle, 0);
        except
        end;
          {$ENDIF}
    end;
    FSettings.FClients.UnlockList;
  end;
  FreeAndNil(FSettings.FClients);
  FSettings.FLogger.Free;
  FSettings.FErrorLogger.Free;
  FAuthentication.Free;
  FIniSettings.Free;
end;

procedure TVisualServer.DropClient(ConnectionHandle: integer);
var
  i: integer;
begin
  with FSettings.FClients.LockList do
    try
      for i := 0 to Count - 1 do
        if TServerHandler(Items[i]).FIPInfo.ConnectionHandle = ConnectionHandle then
        begin
          TServerHandler(Items[i]).Terminate;
          break;
        end;
    finally
      FSettings.FClients.UnlockList;
    end;
end;

procedure TVisualServer.Loaded;
begin
  inherited;
  if FMakeActive then
  begin
    //beware of Loaded called multiple times
    FMakeActive := False;
    SetActive(True);
  end;
end;

procedure TVisualServer.SendData(ConnectionHandle: integer; Data: string);
//This can send data to any specific client.
var
  i: integer;
begin
  //Add data to queue
  with FSettings.FClients.LockList do
    try
      for i := 0 to Count - 1 do
        if TServerHandler(Items[i]).FIPInfo.ConnectionHandle = ConnectionHandle then
        begin
          with TServerHandler(Items[i]) do
            try
              FSendCS.Enter;
              FSendQueue.Add(Data);
            finally
              FSendCS.Leave;
            end;
          break;
        end;
    finally
      FSettings.FClients.UnlockList;
    end;
end;

procedure TVisualServer.SendStream(ConnectionHandle: integer; Data: TStream);
var
  i: integer;
begin
  //Find proper connection
  with FSettings.FClients.LockList do
    try
      for i := 0 to Count - 1 do
        if TServerHandler(Items[i]).FIPInfo.ConnectionHandle = ConnectionHandle then
        begin
          with TServerHandler(Items[i]) do
            try
              FSendCS.Enter;
              FSendQueue.AddObject('', Data);
            finally
              FSendCS.Leave;
            end;
          break;
        end;
    finally
      FSettings.FClients.UnlockList;
    end;
end;

procedure TVisualServer.SetActive(Value: boolean);
var
  i: integer;
begin
  if Value = FActive then
    //do nothing
    exit;
  if csLoading in ComponentState then
    FMakeActive := Value
  else
  begin
    if csDesigning in ComponentState then
      FActive := Value
    else
    begin

      if not Value and Assigned(FListenThread) then
        //inactivate
      begin
        FListenThread.Terminate;
        //signal client threads to terminate:
        with FSettings.FClients.LockList do
        begin
          for i := 0 to Count - 1 do
            try
              TThread(Items[i]).Terminate;
            except
            end;
          Clear; //remove all items
        end;
        FSettings.FClients.UnlockList;
        FListenThread.WaitFor;

        FreeAndNil(FListenThread);

        FActive := False;
      end;

      if Value then
        //activate
      begin
        if not Assigned(FServerType) then
          //Set handler type to default
          //TVisualListen is not polymorphic but sufficient for most tcp/ip servers
          //Override if you like in the create handler of inherited component.
          FServerType := TVisualListen;
        //create listener thread:
        FListenThread := FServerType.Create(True);
        //copy common settings:
        FListenThread.FSettings := FSettings;
        //FClientType should point to correct handler type
        FListenThread.FHandler := FClientType;
        //launch thread
        FListenThread.Resume;
        i := 0;
        while (not FListenThread.FInitialized) and (i < 50) do
        begin
          sleep(200);
          Inc(i);
        end;
        FActive := FListenThread.FListening;
        if not FActive then
        begin
          FListenThread.Terminate;
          FListenThread.WaitFor;
          FreeAndNil(FListenThread);
        end;
      end;
    end;
  end;
end;


{ TServerHandler }
procedure TServerHandler.Init;
begin
  with FSettings.FClients.LockList do
    Add(Self);
  FSettings.FClients.UnlockList;
  FIPInfo.RemoteIP := FSock.GetRemoteSinIP;
  FIPInfo.RemotePort := IntToStr(FSock.GetRemoteSinPort);
  FIPInfo.ConnectionHandle := GetConnectionHandle;
  FSendCS := TCriticalSection.Create;
  FSendQueue := TStringList.Create;

  //Allow inheritents to create customized request/response instantces:
  if FRequest = nil then
    FRequest := TRequest.Create;
  if FResponse = nil then
    FResponse := TResponse.Create;

  //Check if SSL is needed
  if FSettings.FDoSSL then
    //use only for connections in full secure mode.
    //rfc 2246
  begin
    //this is supposed to be correct for various protocols
    //supported:
    // https (RFC 2818)
    // FTP over TLS
    FSock.SSL.CertCAFile := FSettings.FSSLCertCAFile;
    FSock.SSL.PrivateKeyFile := FSettings.FSSLPrivateKeyFile;
    FSock.SSL.CertificateFile := FSettings.FSSLCertificateFile;
    try
      //        FSock.SSLEnabled := True;
      if not FSock.SSLAcceptConnection then
      begin
        LogError(IntToStr(FSock.SSL.LastError));
        LogError(FSock.SSL.LastErrorDesc);
        Terminate; //signal handler to close...
      end;
    except
      on E: Exception do
      begin
        LogError(E.Message);
        Terminate;
      end;
    end;
  end;

  CallBackThreadMethod(@OnConnect);
end;

procedure TServerHandler.Final;
var
  i: integer;
begin
  //close socket

  FSock.CloseSocket; //will free optional SSL socket automatically
  CallBackThreadMethod(@OnDisconnect);

  //remove self from shared list:
  if Assigned(FSettings.FClients) then
  begin
    with FSettings.FClients.LockList do
    begin
      if IndexOf(Self) >= 0 then
        Delete(IndexOf(Self));
    end;
    FSettings.FClients.UnlockList;
  end;
  //empty send queue if any:
  FSendCS.Enter; //not really necessary here, we left the client list already.
  try
    for i := 0 to FSendQueue.Count - 1 do
      if Assigned(FSendQueue.Objects[i]) then
        try
          FSendQueue.Objects[i].Free;
        except
        end;
  except
  end;
  FSendCS.Leave;
  //Free variables
  FSendCS.Free;
  FSendQueue.Free;
  FSock.Free;
  FRequest.Free;
  FResponse.Free;
  //Free self:
  FreeOnTerminate := True;
end;

procedure TServerHandler.CallBack(proc: TThreadMethod);
begin
  if Assigned(Proc) then
    try
      if FSettings.FThreadSafe then
        Synchronize(proc)
      else
        proc;
    except
    end;
end;

procedure TServerHandler.OnAuthenticate;
var
  m: boolean;
begin
  m := False;
  if Assigned(FCallBacks.FOnAuthenticate) then
    FCallBacks.FOnAuthenticate(FSettings.Owner, FUser, FPass, FIPInfo, m);
end;

procedure TServerHandler.OnConnect;
begin
  FDoContinue := True;
  if Assigned(FCallBacks.FOnConnect) then
    try
      FCallBacks.FOnConnect(FSettings.Owner, FIPInfo, FDoContinue);
    except
      FDoContinue := False;
    end;
  if not FDoContinue then
    Terminate; //is this safe here?
end;

procedure TServerHandler.OnMustAuthenticate;
begin
  FMustAuthenticate := False;
  if Assigned(FCallBacks.FOnMustAuthenticate) then
    try
      FCallBacks.FOnMustAuthenticate(FSettings.Owner, FRequest, FMustAuthenticate, FIPInfo);
    except
    end;
end;

procedure TServerHandler.OnPut;
begin
  FAccepted := False;
  if Assigned(FCallBacks.FOnPut) then
    try
      FCallBacks.FOnPut(FSettings.Owner, FRequest, FAccepted, FIPInfo);
    except
    end;
end;

procedure TServerHandler.OnRequest;
begin
  RequestHandled := True;
  if Assigned(FCallBacks.FOnRequest) then
    try
      FCallBacks.FOnRequest(FSettings.Owner, FRequest, FResponse, FIPInfo, RequestHandled);
    except
    end;
end;

procedure TServerHandler.CheckToSend;
var
  D: string;
  S: TStream;
begin
  //See if there is data to send
  S := nil;
  D := '';
  FSendCS.Enter;
  try
    if FSendQueue.Count > 0 then //time to send
    begin
      S := TStream(FSendQueue.Objects[0]);
      D := FSendQueue[0];
      FSendQueue.Delete(0);
    end;
  except
  end;
  FSendCS.Leave;
  if D <> '' then
    FSock.SendString(D);
  if Assigned(S) then
    try
      FSock.SendStream(S);
      S.Free;
    except
    end;
end;

procedure TServerHandler.Execute;
begin
  Init; //calls OnConnect
  if not Terminated then
    try
      Handler;
    except
    end;
  Final;
end;

procedure TServerHandler.Handler;
begin
  //Virtual dummy proc, we're finished here.
end;


procedure TServerHandler.CopyCustomVars;
begin
  //called thread-safe by the listen thread

end;

procedure TServerHandler.CallBackThreadMethod(proc: TThreadMethod);
begin
  synchronize(proc);
end;

procedure TServerHandler.CallBackRequest(Proc: TOnRequest);
begin
  FProcToCall := Proc;
  RequestHandled := False;
  if Assigned(FProcToCall) then
  begin
    if FSettings.FThreadSafe then
      synchronize(@SyncCallBack)
    else
      SyncCallBack;
  end;
end;

procedure TServerHandler.SyncCallBack;
begin
  try
    RequestHandled := True;
    FProcToCall(FSettings.Owner, FRequest, FResponse, FIPInfo, RequestHandled);
  except
    //if logging then log..
    RequestHandled := False;
  end;
end;

procedure TServerHandler.OnDisconnect;
begin
  if Assigned(FCallBacks.FOnDisConnect) then
    try
      FCallBacks.FOnDisConnect(FSettings.Owner, FIPInfo);
    except
    end;
end;

{ TRequest }

procedure TRequest.Clear;
begin
  RawRequest := '';
  Command := '';
  Parameter := '';
  Meta := '';
  ProtoVersion := '';
  Header.Clear;
  RawHeader.Clear;
end;

constructor TRequest.Create;
begin
  inherited;
  Header := TStringList.Create;
  RawHeader := TStringList.Create;
  Args := TStringList.Create;
end;

destructor TRequest.Destroy;
begin
  Header.Free;
  RawHeader.Free;
  Args.Free;
  inherited;
end;

{ TResponse }

procedure TResponse.Clear;
begin
  ResponseCode := 0;
  ResponseText := '';
  Data := '';
  DataStream := nil;
  MimeType := '';
  Header.Clear;
  RawHeader.Clear;
end;

constructor TResponse.Create;
begin
  inherited;
  Header := TStringList.Create;
  RawHeader := TStringList.Create;
end;

destructor TResponse.Destroy;
begin
  Header.Free;
  RawHeader.Free;
  inherited;
end;

procedure TResponse.FixHeader;
begin
  //Nothing to do here..
end;

{ TVSThread }

procedure TVSThread.Error(ErrorCode: integer);
begin
  FSettings.FLastErrorCode := ErrorCode;
  if (ErrorCode >= 0) and (ErrorCode <= high(ErrorCodes)) then
    FSettings.FLastError := ErrorCodes[ErrorCode]
  else
    FSettings.FLastError := 'Unknown error';
  //FIPInfo is supposed to be filled with latest info, optionally zerod.
  if Assigned(FSettings.Owner) then
    synchronize(@SyncError);
  //else waiste of synchronize action.
end;

procedure TVSThread.LogError(Line: string);
begin
  FSettings.FErrorLogger.Log(FIPInfo.RemoteIP + ':' + FIPInfo.RemotePort + ' ' + Line);
end;

procedure TVSThread.Log(Line: string);
begin
  FSettings.FLogger.Log(FIPInfo.RemoteIP + ':' + FIPInfo.RemotePort + ' ' + Line);
end;

procedure TVSThread.SyncError;
begin
  if Assigned(FSettings.Owner) then
    try
      if Assigned(TVisualServer(FSettings.Owner).OnError) then
        TVisualServer(FSettings.Owner).OnError(
          FSettings.Owner, FSettings.FLastErrorCode, FSettings.FLastError, FIPInfo);
    except
    end;
end;

function TVisualServer.GetLogFile: string;
begin
  Result := FSettings.FLogger.FileName;
end;

procedure TVisualServer.SetLogFile(const Value: string);
begin
  FSettings.FLogger.FileName := Value;
end;


function TVisualServer.GetSSL: boolean;
begin
  Result := FSettings.FDoSSL;
end;

procedure TVisualServer.SetSSL(const Value: boolean);
begin
  FSettings.FDoSSL := Value;
end;

procedure TVisualServer.FinishIni;
begin
  FIni.UpdateFile;
  FIni.GetStrings(FIniSettings);
  if Assigned(FIni) then
    FreeAndNil(FIni);
end;

function TVisualServer.InitIniWrite(FileName: TFileName): boolean;
var
  i: integer;
begin
  Result := False;
  //create ini file
  //make backup of old
  //write some default server variables
  //we want fully qualified path:
  //  if ExtractFilePath (FileName)='' then
  //    FileName := ExpandFileName (FileName);

  //create backup of filename
  if (ExtractFilePath(FileName) <> '') and FileExists(FileName) then
  begin
    i := 1;
    while (fileexists(Filename + '.' + IntToStr(i))) and (i < 10000) do
      Inc(i);
    if not RenameFile(FileName, FileName + '.' + IntToStr(i)) then
    ; //exit;
  end;
  //Create ini file
  try
    FIni := TMemIniFile.Create(FileName);
    FileName := FIni.FileName;
    //Write settings:
    FIni.WriteString('global', 'port', ListenPort);
    FIni.WriteString('global', 'ip', ListenIP);
    FIni.WriteString('global', 'servername', servername);
    FIni.WriteString('global', 'logfile', logfile);
    FIni.WriteString('global', 'ErrorLog', ErrorLog);
    FIni.WriteString('authentication', 'AuthenticationMethod', copy(getEnumName(TypeInfo(TAuthMethod), integer(vsAuthentication.Method)),
      3, maxint));
    FIni.WriteString('authentication', 'PasswordFile', vsAuthentication.PasswordFile);
    FIni.WriteBool('startup', 'Active', Active);
    FIni.WriteBool('ssl', 'Enabled', SSL);
    FIni.WriteString('ssl', 'SSLCertCAFile', SSLCertCAFile);
    FIni.WriteString('ssl', 'SSLPrivateKeyFile', SSLPrivateKeyFile);
    FIni.WriteString('ssl', 'SSLCertificateFile', SSLCertificateFile);
    Result := True;
  except
    FreeAndNil(FIni);
    exit;
  end;
end;

procedure TVisualServer.WriteSectionValues(section: string; namevalues: TStrings);
var
  i: integer;
begin
  if not Assigned(FIni) then
    exit;
  FIni.DeleteKey(section, '##');
  for i := 0 to namevalues.Count - 1 do
    FIni.WriteString(section, namevalues.names[i], namevalues.values[namevalues.names[i]]);
  //add some white space
  FIni.WriteString(section, '##', '##');
end;

function TVisualServer.InitIniRead(FileName: TFileName): boolean;
var
  n: integer;
  v: string;
begin
  Result := False;
  //open ini file
  //write some default server variables
  //we want fully qualified path:
  if ExtractFilePath(FileName) = '' then
    FileName := ExpandFileName(FileName);

  //Create ini file
  try
    FIni := TMemIniFile.Create(FileName);
    //Read settings:
    //copy some settings from existing object if not overriden by config:
    ListenPort := FIni.ReadString('global', 'port', ListenPort);
    ListenIP := FIni.ReadString('global', 'ip', ListenIP);
    servername := FIni.ReadString('global', 'servername', ServerName);
    logfile := FIni.ReadString('global', 'logfile', '');
    Errorlog := FIni.ReadString('global', 'ErrorLog', '');
    v := FIni.ReadString('authentication', 'AuthenticationMethod', 'DenyAll');
    n := getEnumValue(TypeInfo(TAuthMethod), 'am' + v);
    if n <> -1 then
      vsAuthentication.Method := TAuthMethod(n)
    else
      vsAuthentication.Method := amDenyAll;
    vsAuthentication.PasswordFile := FIni.ReadString('authentication', 'PasswordFile', '');

    SSL := FIni.ReadBool('ssl', 'Enabled', False);
    SSLCertCAFile := FIni.ReadString('ssl', 'SSLCertCAFile', '');
    SSLPrivateKeyFile := FIni.ReadString('ssl', 'SSLPrivateKeyFile', '');
    SSLCertificateFile := FIni.ReadString('ssl', 'SSLCertificateFile', '');
    //better don't
    //Active := FIni.ReadBool ('startup', 'Active', False);
    Result := True;
  except
    FreeAndNil(FIni);
    exit;
  end;
end;


function TVisualServer.LoadSettings(FileName: TFileName): boolean;
begin
  Result := InitIniRead(FileName);
  if Result then
    FinishIni;
end;

function TVisualServer.SaveSettings(FileName: TFileName): boolean;
begin
  Result := InitIniWrite(FileName);
  if Result then
    FinishIni;
end;

procedure TVisualServer.SetErrorLog(const Value: string);
begin
  FSettings.FErrorLogger.FileName := Value;
end;

function TVisualServer.GetErrorLog: string;
begin
  Result := FSettings.FErrorLogger.FileName;
end;

initialization
  ConnectionCS := TCriticalSection.Create;

finalization
  ConnectionCS.Free;
end.
