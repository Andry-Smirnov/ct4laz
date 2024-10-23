
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsHttpServer;

interface


uses Classes, SysUtils, typinfo,
     {$IFNDEF FPC}{$IFNDEF LINUX}FileCtrl, {$ENDIF}{$ENDIF}
  blcksock, vsVisualServerBase, inifiles, vsTypeDef, vsExecCGI,
  synacode, synautil, mimemess, vsAuthentication;


const
  MAX_POST_DATA_SIZE = 8192;

{$IFDEF UNIX}
  PathDelim = '/';
{$ELSE}
  PathDelim = '\';
{$ENDIF}

type

  TEnumHTTPProtocols = (hpHEAD, hpGET, hpPUT, hpDELETE, hpPOST, hpTRACE, hpOPTIONS, hpCONNECT);

  THTTPProtocols = set of TEnumHTTPProtocols;

  //cmGET is default
  //cmClose forces connection close
  //cmDone marks all client IO as done, but keeps connection open if enabled
  //cmConnect marks proxy
  THttpConnectionMode = (cmWait, cmGET, cmPOST, cmPUT, cmCONNECT, cmReadCGI, cmCLOSE, cmDONE);


  TCGIPath = class
    CGIName: string;
    ExePath: TFileName;
  end;

  TPreParser = class
    ExePath: string;
    Params: string;
  end;

  TvsHTTPResponse = class(TResponse)
    procedure FixHeader; override;
  end;


  TvsVirtualDomain = class(TObject)
    FDefaultDocument: string;
    FDefaultDocuments: TStrings;
    FHostName: string;
    //    FRootPath:TFileName;
    FVirtualPath: TStrings;
    FCGI: TStrings; //Array of TCGIPath;
    FPreParser: TStrings; //Array TPreParser;
    FManualURL: TStrings;
    FMimeTypes: TStrings;
    FAuthNeeded: TStrings;
    constructor Create;
    destructor Destroy; override;
  end;

  THTTPVars = record
    FDoVirtualHosts: boolean;
    FDomain: TvsVirtualDomain;
    FVirtualDomainRoot: string;
    FPHPPath: TFileName;
    FCaseSensitive: boolean;
    FServerName: string;
    FVirtualDomains: TList;
    FSupported: THTTPProtocols;
    FAutomated: THTTPProtocols;
    FErrorDocs: string;
  end;

  THTTPDocument = record
    Headers: string;
    Data: string;
  end;

  THTTPDoc = record
    Command: TEnumHTTPProtocols;
    Request,
    Response: THTTPDocument;
  end;

  THostInfo = record
    Host,
    Port: string;
  end;

  //  THTTPCallBack = procedure (Sender: TObject; Request, Response: THTTPDoc; Info: THostInfo) of Object;

  TCallBack = record
    FOnHead,
    FOnGet,
    FOnPut,
    FOnPost,
    FOnTrace,
    FOnOptions: TOnRequest; //THTTPCallBack;
  end;


  //the component
  TvsHTTPServer = class(TVisualServer)
  protected
    FVirtualServers: TList;
    FHTTPVars: THTTPVars;

    FCallBack: TCallBack;

    //creates a virtual domain if not exists:
    function GetVirtualDomain(Domain: string): TvsVirtualDomain;

  public
    class function ParseDomain(Domain: string): string; //cleans up a domain name
    constructor Create(AOwner: TComponent); override;
    procedure AddVirtualDomain(Domain: TvsVirtualDomain);
    procedure RegisterDir(PhysicalDir: TFileName; VirtualDir: string; Domain: string = ''; Recursive: boolean = True);
    procedure RegisterPreParser(Binary: TFileName; Extension: string; Domain: string = ''; Params: string = '"%s"');
    procedure RegisterPHP(Binary: TFileName; Extension: string = ''; Domain: string = '');
    procedure RegisterDefaultDoc(FileName: string; Domain: string = '');
    procedure RegisterMimeType(Extension, MimeType: string; Domain: string = '');
    procedure RegisterCGI(PhysicalDir: TFileName; VirtualDir: string = '/cgi-bin'; Domain: string = '');
    procedure RegisterManualURL(URL: string; Domain: string = '');
    procedure RegisterAuthenticationDir(VirtualDir: string; Domain: string = ''; Recursive: boolean = True);
    procedure ClearSettings;
    //    procedure WriteSettings (Stream: TStream);
    //    procedure ReadSettings (Stream: TStream);
    function SaveSettings(FileName: TFileName): boolean; override;
    function LoadSettings(FileName: TFileName): boolean; override;
  published
    property OnHead: TOnRequest read FCallBack.FOnHead write FCallBack.FOnHead;
    property OnGet: TOnRequest read FCallBack.FOnGet write FCallBack.FOnGet;
    property OnPost: TOnRequest read FCallBack.FOnPost write FCallBack.FOnPost;
    property OnPut: TOnRequest read FCallBack.FOnPut write FCallBack.FOnPut;

    property DoVirtualHosts: boolean read FHTTPVars.FDoVirtualHosts write FHTTPVars.FDoVirtualHosts;

    //    property DefaultDocument:String read FHTTPVars.FDomain.FDefaultDocument write FHTTPVars.FDomain.FDefaultDocument;
    property PHPPath: TFileName read FHTTPVars.FPHPPath write FHTTPVars.FPHPPath;
    property CaseSensitive: boolean read FHTTPVars.FCaseSensitive write FHTTPVars.FCaseSensitive;
    property SupportedProtocols: THTTPProtocols read FHTTPVars.FSupported write FHTTPVars.FSupported;
    property AutomatedProtocols: THTTPProtocols read FHTTPVars.FAutomated write FHTTPVars.FAutomated;
    property ErrorDocs: string read FHTTPVars.FErrorDocs write FHTTPVars.FErrorDocs;
    property VirtualDomainRoot: string read FHTTPVars.FVirtualDomainRoot write FHTTPVars.FVirtualDomainRoot;
  end;

  //the protocol handler
  TvsHTTPHandler = class(TServerHandler)
  protected

    //Returns NIL on invalid domain:
    function GetVirtualDomain(Domain: string): TvsVirtualDomain;

    //support function
    function Compare(V1, V2: string): boolean; //compares string based on CaseSensitive property

    //procedures to find back vars:
    function GetCGIPath(URL: string; Domain: string = ''): string;
    function GetPreParser(URL: string; Domain: string = ''): TPreParser;
    function IsManualURL(URL: string; Domain: string = ''): boolean;
    function MapVirtualDir(URL: string; Domain: string = ''): TFileName;
    function GetDefaultDoc(Path: TFileName; Domain: string = ''): string;
    function IsAuthenticationNeeded(URL: string; Domain: string = ''): boolean;
    function IsAuthenticated: boolean;

    procedure CreateFileHeaders(FileName: TFileName);


    //function to see if there are any parsers (pre-parsers, cgi)
    //if so, call them
    //if not, return false and do rest
    function CheckGetHeadPostParser(Domain: string = ''): boolean;

    procedure Report404; //speaks for itself..

  public
    Buf: string;
    FMode: THttpConnectionMode;
    FPSock: TTCPBlockSocket;

    //vars shared with component:
    FHTTPVars: THTTPVars;
    FCallBack: TCallBack;

    //internal use:
    FCurrentGetFile: TFileName;
    FRangeStart: int64;
    FRangeEnd: int64;
    FPostData: string;
    FCGIInfo: TCGIResult;

    FKeepAlive: boolean;


    procedure Init; override;

    procedure CopyCustomVars; override;
    procedure Handler; override;
    procedure ProcessRequest(Header: string);

    function GetGetPath: TFileName; //returns path of file or cgi

    procedure ProcessGet;
    procedure ProcessHead;
    procedure ProcessPut;
    procedure ProcessPost;
    procedure ProcessConnect;
    procedure ProcessDelete;
    procedure ProcessTrace;
    procedure ProcessOptions;

    //Procedures to split url:
    function GetFile(URL: string): string;
    function GetFileNoPath(URL: string): string;
    function GetPath(URL: string): string;
    function GetParams(URL: string): string;
    procedure ReadPostData;
    procedure MakeResponseHeaders;
    procedure MakeErrorDoc;
    procedure MakeArgs;
    function MergeURL(Path, FileName: string): string;
    procedure ProcessGetHeadPost(Method: TEnumHTTPProtocols);
    procedure CheckRanges;

  end;

function HTTPCodeToMessage(Code: integer): string;


implementation


//support function:
function HTTPCodeToMessage(Code: integer): string;
  //Code is expected to be >= 100 (3 digits);
var
  M: string;
begin
  if (code < 100) or (code > 999) then
    code := 500; //internal server error
  case Code of
    100: M := 'Continue';
    101: M := 'Switching Protocols';
    200: M := 'Ok';
    201: M := 'Created';
    202: M := 'Accepted';
    203: M := 'Non-Authoritive Information';
    204: M := 'No Content';
    205: M := 'Reset Content';
    206: M := 'Partial Content';
    300: M := 'Multiple Choices';
    301: M := 'Moved Permanently';
    302: M := 'Found';
    303: M := 'See Other Method';
    304: M := 'Not Modified';
    305: M := 'Use Proxy';
    307: M := 'Temporary Redirect';
    400: M := 'Bad Request';
    401: M := 'Unauthorized';
    402: M := 'Payment required';
    403: M := 'Forbidden';
    404: M := 'Not Found';
    405: M := 'Method Not Allowed';
    406: M := 'Not Acceptable';
    407: M := 'Proxy Authentication Required';
    408: M := 'Request Timeout';
    409: M := 'Conflict';
    410: M := 'Gone';
    411: M := 'Length Required';
    412: M := 'Precondition Failed';
    413: M := 'Request Entity Too Large';
    414: M := 'Request-URI Too Long';
    415: M := 'Unsupported Media Type';
    416: M := 'Requested Range Not Satisfiable';
    417: M := 'Expectation Failed';
    500: M := 'Internal Error';
    501: M := 'Not Implemented';
    502: M := 'Bad Gateway';
    503: M := 'Service Unavailable';
    504: M := 'Gateway Timeout';
    505: M := 'HTTP Version Not Supported';
    else
      M := 'Unknown';
  end;
  M := IntToStr(Code) + ' ' + M;
  Result := M;
end;

function ListDirAsHTML(PhysicalDir, VirtualDir, Hostname: string): string;
  //list a directory as HTML
var
  v, w: string;
  sr: TSearchRec;
  f: integer;
begin
  v := 'Index of //' + HostName + VirtualDir;
  Result := '<html><head><title>' + v + '</title></head>' + '<body><h2><i>' + v + '</i></h2><br>'#13#10'<table>'#13#10;
  f := FindFirst(PhysicalDir + PathDelim + '*.*', faAnyFile - faHidden, sr);
  while f = 0 do
  begin
    w := sr.Name;
    if (sr.Attr and faDirectory) <> 0 then
      w := w + '/';
    Result := Result + Format('<tr><td><a href="%s">%s</a></td><td>%d</td><td>%s</td></tr>',
      ['//' + HostName + VirtualDir + w, VirtualDir + w,
      sr.Size,
      //                                    RFC822DateTime (FileDateToDateTime(sr.Time))
      DateTimeToStr(FileDateToDateTime(sr.Time))]);
    f := FindNext(sr);
  end;
  Result := Result + '</table>'#13#10'<hr>Visual Synapse HTTP Server</body>'#13#10'</html>';
  FindClose(sr);
end;


{ THTTPServer }

procedure TvsHTTPServer.AddVirtualDomain(Domain: TvsVirtualDomain);
begin
end;

procedure TvsHTTPServer.ClearSettings;
var
  VD: TvsVirtualDomain;
  i: integer;
begin
  //remove list of virtual domains:
  for i := 0 to FHTTPVars.FVirtualDomains.Count - 1 do
    TvsVirtualDomain(FHTTPVars.FVirtualDomains[i]).Free;
  FHTTPVars.FVirtualDomains.Clear;
  VD := TvsVirtualDomain.Create;
  VD.FHostName := '*';
  FHTTPVars.FVirtualDomains.Add(VD);
  FHTTPVars.FVirtualDomainRoot := '';
end;

constructor TvsHTTPServer.Create(AOwner: TComponent);
var
  VD: TvsVirtualDomain;
begin
  inherited;
  FClientType := TvsHTTPHandler;
  ListenPort := '80';
  //  DefaultDocument := 'index.*';
  FSettings.FHasCustomVars := True;
  FHTTPVars.FSupported := [hpHEAD, hpGET,{ hpPUT, hpDELETE,} hpPOST, hpTRACE, hpOPTIONS{, hpCONNECT}];
  FHTTPVars.FAutomated := FHTTPVars.FSupported + [hpCONNECT];
  FHTTPVars.FCaseSensitive := True;
  FHTTPVars.FVirtualDomains := TList.Create;
  ClearSettings;
end;

function TvsHTTPHandler.GetVirtualDomain(Domain: string): TvsVirtualDomain;
  //don't confuse with the THTTPServer.GetVirtualDomain method
  //which is slightly different.
var
  i: integer;
begin
  Domain := TvsHTTPServer.ParseDomain(Domain);
  Result := nil;
  for i := 0 to FHTTPVars.FVirtualDomains.Count - 1 do
    if (TvsVirtualDomain(FHTTPVars.FVirtualDomains[i]).FHostName = Domain) or
      (TvsVirtualDomain(FHTTPVars.FVirtualDomains[i]).FHostName = 'www.' + Domain) then
    begin
      Result := TvsVirtualDomain(FHTTPVars.FVirtualDomains[i]);
      break;
    end;
  if (Result = nil) and (FHTTPVars.FVirtualDomainRoot <> '') then
  begin
    //dynamic virtual domain mapping
    //parse virtual domain root
    //if directory exists, map
    if (pos('/', Domain) <= 0) and (pos('\', domain) <= 0) and (DirectoryExists(FHTTPVars.FVirtualDomainRoot + PathSep + Domain) or
      DirectoryExists(FHTTPVars.FVirtualDomainRoot + PathSep + 'www.' + Domain)) then
    begin
      //FHTTPVars.FVirtualDomains.Add ();
      //todo: thread safety.
      Result := TvsVirtualDomain.Create;
      Result.FVirtualPath.AddObject('+/', StrToObj(FHTTPVars.FVirtualDomainRoot + PathSep + Domain));
      Result.FHostName := Domain;
      //FHTTPVars.

      //          FHTTPVars.CS.Enter
      FHTTPVars.FVirtualDomains.Add(Result);
      //          FHTTPVars.CS.Leave;
    end;
  end;
  {
  if (not Assigned (Result)) and CreateIfNotExists then
    begin //add one
      Result := TVirtualDomain.Create;
      FHTTPVars.FVirtualDomains.Add (Result);
      Result.FHostName := Domain;
    end;
  }
end;

class function TvsHTTPServer.ParseDomain(Domain: string): string;
begin
  Result := lowercase(Domain);
  if Result = '' then
    Result := '*';
end;


procedure TvsHTTPServer.RegisterCGI(PhysicalDir: TFileName; VirtualDir, Domain: string);
var
  VD: TvsVirtualDomain;
begin
  PhysicalDir := ExpandUNCFileName(PhysicalDir);
  VD := GetVirtualDomain(Domain);
  VD.FCGI.AddObject(VirtualDir, StrToObj(PhysicalDir));
end;

procedure TvsHTTPServer.RegisterDefaultDoc(FileName, Domain: string);
var
  VD: TvsVirtualDomain;
begin
  VD := GetVirtualDomain(Domain);
  if VD.FDefaultDocuments.IndexOf(FileName) < 0 then
    VD.FDefaultDocuments.Add(FileName);
end;

procedure TvsHTTPServer.RegisterDir(PhysicalDir: TFileName; VirtualDir, Domain: string; Recursive: boolean);
var
  VD: TvsVirtualDomain;
  SR: TSearchRec;
  f: integer;
begin
  //todo?
  //check virtualdir for beginning slash
  PhysicalDir := ExpandUNCFileName(PhysicalDir);
  VD := GetVirtualDomain(Domain);
  if Recursive then
    VirtualDir := '+' + VirtualDir
  else
    VirtualDir := '-' + VirtualDir;

  if VD.FVirtualPath.IndexOf(VirtualDir) < 0 then
    VD.FVirtualPath.AddObject(VirtualDir, StrToObj(PhysicalDir));
{
  if Recursive then
    begin
      f := FindFirst (PhysicalDir+'\*.*', faDirectory, SR);
      while f = 0 do
        begin
          if ((SR.Attr and faDirectory)<>0) and
             (SR.Name<>'.') and (SR.Name<>'..') then //call self recursively
            RegisterDir (PhysicalDir+PathSep+SR.Name, VirtualDir+SR.Name+'/', Domain, True);
          f := FindNext (SR);
        end;
      FindClose (SR);
    end;
}
end;

procedure TvsHTTPServer.RegisterManualURL(URL, Domain: string);
var
  VD: TvsVirtualDomain;
begin
  VD := GetVirtualDomain(Domain);
  if VD.FManualURL.IndexOf(URL) < 0 then
    VD.FManualURL.Add(URL);
end;

procedure TvsHTTPServer.RegisterMimeType(Extension, MimeType: string; Domain: string = '');
var
  VD: TvsVirtualDomain;
  i: integer;
begin
  VD := GetVirtualDomain(Domain);
  i := VD.FMimeTypes.IndexOf(Extension);
  if i >= 0 then
    VD.FMimeTypes.Delete(i);
  VD.FMimeTypes.AddObject(Extension, StrToObj(MimeType));
end;

procedure TvsHTTPServer.RegisterPreParser(Binary: TFileName; Extension: string; Domain: string = ''; Params: string = '"%s"');
var
  VD: TvsVirtualDomain;
  i: integer;
  PreParser: TPreParser;
begin
  VD := GetVirtualDomain(Domain);
  i := VD.FPreParser.IndexOf(Extension);
  if i >= 0 then
  begin
    VD.FPreParser.Objects[i].Free;
    VD.FPreParser.Delete(i);
  end;
  PreParser := TPreParser.Create;
  PreParser.ExePath := Binary;
  PreParser.Params := Params;
  VD.FPreParser.AddObject(Extension, PreParser);
end;

function TvsHTTPServer.LoadSettings(FileName: TFileName): boolean;
var //FI: TIniFile;
  sec, nv: TStrings;
  i, j: integer;
  d, p, n, v, f: string;
  r: boolean;
  pe, pp: string;

begin

  Result := InitIniRead(FileName);

  if not Result then
    exit;
  FHTTPVars.FVirtualDomainRoot := FIni.ReadString('global', 'virtualdomainroot', '');
  //  if not Inherited LoadSettings (FileName) then
  //    exit;
  sec := TStringList.Create;
  nv := TStringList.Create;
  FIni.ReadSections(sec);
  for j := 0 to sec.Count - 1 do
    if pos(':', sec[j]) > 0 then
    begin
      d := copy(sec[j], 1, pos(':', sec[j]) - 1);
      p := lowercase(copy(sec[j], length(d) + 2, maxint));
      if (p <> '') then
      begin
        nv.Clear;
        FIni.ReadSectionValues(sec[j], nv);
        for i := 0 to nv.Count - 1 do
        begin
          n := nv.Names[i];
          v := nv.Values[nv.Names[i]];
          if (n <> '') and (n[1] <> '#') then //allow comments in ini file
          begin
            r := True;
            if n <> '' then
            begin
              if n[1] in ['-', '+'] then
              begin
                r := n[1] = '+';
                f := copy(n, 2, maxint);
              end
              else
                f := n;
            end
            else
              f := '';
            if pos('|', v) > 0 then
            begin
              pe := copy(v, 1, pos('|', v) - 1);
              pp := copy(v, length(pe) + 2, maxint);
            end
            else
            begin
              pe := v;
              pp := '';
            end;
            //d-domain
            //p-param/command
            //n-'name'/key
            //v-value
            //f-filename
            //r-recursive
            if p = 'pathmapping' then
              RegisterDir(v, f, d, r)
            else
            if p = 'cgi' then
              RegisterCGI(v, f, d)
            else
            if p = 'preparser' then
              RegisterPreParser(pe, n, d, pp)
            else
            if p = 'manualurl' then
              RegisterManualURL(n, d)
            else
            if p = 'authenticationneeded' then
              RegisterAuthenticationDir(f, d, r)
            else
            if p = 'defaultdocuments' then
              RegisterDefaultDoc(n, d)
            else
            if p = 'mimetypes' then
              RegisterMimeType(n, v, d);
          end;
        end;
      end;

    end;
  FinishIni;
end;

function TvsHTTPServer.SaveSettings(FileName: TFileName): boolean;
var
  i: integer;
  VD: TvsVirtualDomain;
  h, hc: string;
  sl: TStrings;

  procedure StrObjToNameValue(so, nv: TStrings; Reverse: boolean = False);
  var
    i, n: integer;
    v: string;
  begin
    nv.Clear;
    n := 0;
    for i := 0 to so.Count - 1 do
    begin
      v := '';
      if so.Objects[i] is TString then
        v := TString(so.Objects[i]).Value
      else
      if so.Objects[i] is TPreParser then
        v := TPreParser(so.Objects[i]).ExePath + '|' + TPreParser(so.Objects[i]).Params;
      if Reverse then
      begin
        if so[i] = '' then
          so[i] := '=';
        if v = '' then
        begin
          Inc(n);
          v := 'n' + IntToStr(n); //Add a default name to this value.
        end;
        v := v + '=' + so[i];
      end
      else
      if so[i] <> '' then
      begin
        if v = '' then
          v := '='; //add a default value '=' to name with no value
        v := so[i] + '=' + v;
      end;
      if v <> '' then
        nv.Add(v);
    end;
    if n <> 0 then
      nv.Add('N=' + IntToStr(n));
  end;

begin
  if not InitIniWrite(FileName) then
    exit;

  FIni.WriteString('global', 'virtualdomainroot', FHTTPVars.FVirtualDomainRoot);

  //  if not inherited SaveSettings (FileName) then
  //    exit;
  sl := TStringList.Create;
  //  FI.WriteString ('ssl',..);
  for i := 0 to FHTTPVars.FVirtualDomains.Count - 1 do
  begin
    //h contains section id
    vd := TvsVirtualDomain(FHTTPVars.FVirtualDomains[i]);
    h := vd.FHostName + ':';
    //Save help info
    if vd.FHostName = '*' then
    begin
      sl.Text := '#0= [+|-]/virtual_dir/=d:\physicaldir'#10 + '#1= The plus or minus determinate if the directory is'#10 +
        '#2= mapped recursively. + = recursive on, - = off';
      WriteSectionValues(h + 'PathMapping', sl);
      sl.Text := '#0= CGI directory, containing executable files.'#10 + '#1= those files will be executed'#10 +
        '#2= Directory listing of cgi directories is not allowed'#10 +
        '#3= Take extreme care with cgi directories (do not map against a ftp account)'#10 +
        '#4= Currently cgi are executed within the servers process space.';
      WriteSectionValues(h + 'CGI', sl);
      sl.Text := '#0= Preparsers, like php and perl are defined here'#10 +
        '#1= The executable is seperated from it''s parameters'#10 +
        '#2= by a pipe character | (vertical line). This character is mandatory'#10 +
        '#3= on the command line you can fill in %s to denote the path to the file'#10 +
        '#4= to the file to be preparsed (absolute physical path)';
      WriteSectionValues(h + 'PreParser', sl);
      sl.Text := '#0= Manual url''s are implemented by a custom-made webserver';
      WriteSectionValues(h + 'ManualURL', sl);
      sl.Text := '#0= A list of virtual directories for which authentication is needed.'#10 +
        '#1= Make sure to put some characters (or comment) after the equal sign (=)';
      WriteSectionValues(h + 'AuthenticationNeeded', sl);
      sl.Text := '#0= A list of default documents. If exist, client will get redirected'#10 +
        '#1= to this document. If default document is not found, the contents of'#10 +
        '#2= the directory are listed.'#10 +
        '#3= Make sure to put some characters (or comment) after the equal sign (=)';
      WriteSectionValues(h + 'DefaultDocuments', sl);
      sl.Text := '#0= Define addition mime types here, like: ext=mime/type';
      WriteSectionValues(h + 'MimeType', sl);
    end;

    //save settings...
    StrObjToNameValue(vd.FVirtualPath, sl);
    if sl.Count > 0 then
      WriteSectionValues(h + 'PathMapping', sl);
    StrObjToNameValue(vd.FCGI, sl);
    if sl.Count > 0 then
      WriteSectionValues(h + 'CGI', sl);
    StrObjToNameValue(vd.FPreParser, sl);
    if sl.Count > 0 then
      WriteSectionValues(h + 'PreParser', sl);
    StrObjToNameValue(vd.FManualURL, sl);
    if sl.Count > 0 then
      WriteSectionValues(h + 'ManualURL', sl);
    StrObjToNameValue(vd.FAuthNeeded, sl);
    if sl.Count > 0 then
      WriteSectionValues(h + 'AuthenticationNeeded', sl);
    StrObjToNameValue(vd.FDefaultDocuments, sl);
    if sl.Count > 0 then
      WriteSectionValues(h + 'DefaultDocuments', sl);
    StrObjToNameValue(vd.FMimeTypes, sl);
    if sl.Count > 0 then
      WriteSectionValues(h + 'MimeType', sl);
  end;
  sl.Free;
  FinishIni;
end;


procedure TvsHTTPServer.RegisterPHP(Binary: TFileName; Extension: string = ''; Domain: string = '');
begin
  Binary := ExpandUNCFileName(Binary);
  if Extension = '' then
    Extension := '.php';
  RegisterPreParser(Binary, Extension, Domain, '-f "%s"');
end;

function TvsHTTPHandler.GetFile(URL: string): string;
var
  i: integer;
begin
  i := pos('?', URL);
  if i > 0 then
    Result := Copy(URL, 1, i - 1)
  else
    Result := URL;
end;

function TvsHTTPHandler.GetParams(URL: string): string;
var
  i: integer;
begin
  i := pos('?', URL);
  if i > 0 then
    Result := Copy(URL, i + 1, maxint)
  else
    Result := '';
end;

function TvsHTTPHandler.GetPreParser(URL, Domain: string): TPreParser;
var
  Ext: string;
  i: integer;
  VD: TvsVirtualDomain;
begin
  Result := nil;
  Ext := ExtractFileExt(GetFile(URL));
  //  Compare
  VD := GetVirtualDomain(Domain);
  if Assigned(VD) then
    i := VD.FPreParser.IndexOf(Ext) //we ignore case.. fix.
  else
    i := -1; //domain does not exist.
  if i >= 0 then
    Result := TPreParser(VD.FPreParser.Objects[i])
  else //not found
  if Domain <> '' then //avoid endless lookups on empty (default) domain
    Result := GetPreParser(URL); //fetch parser of default (''no'') domain.
end;

function TvsHTTPHandler.GetCGIPath(URL, Domain: string): string;
var
  Path: string;
  VD: TvsVirtualDomain;
  i: integer;
begin
  Path := GetPath(URL); //S _should_ (?) be in form /path/ or /full/path/name/
  VD := GetVirtualDomain(Domain);
  if Assigned(VD) then
    i := VD.FCGI.IndexOf(Path) //we ignore case.. fix.
  else
    i := -1; //domain does not exist.
  if i >= 0 then
    Result := TString(VD.FCGI.Objects[i]).Value + PathSep + Copy(GetFile(URL), Length(Path) + 1, maxint)
  else
  if Domain <> '' then
    Result := GetCGIPath(URL, '');
end;

function TvsHTTPHandler.IsManualURL(URL, Domain: string): boolean;
var
  VD: TvsVirtualDomain;
  i: integer;
begin
  VD := GetVirtualDomain(Domain);
  if Assigned(VD) then
    i := VD.FManualURL.IndexOf(GetFile(URL)) //yeah.. case insensitive again.. fix.
  else
    i := -1;
  if (i < 0) and (Domain <> '') then
    Result := IsManualURL(URL)
  else
    Result := i >= 0;
end;

function TvsHTTPHandler.Compare(V1, V2: string): boolean;
begin
  if FHTTPVars.FCaseSensitive then
    Result := V1 = V2
  else
    Result := AnsiStrComp(PChar(V1), PChar(V2)) = 0;
end;

function TvsHTTPHandler.GetPath(URL: string): string;
var
  s: string;
  i: integer;
  p, q: integer;
begin
  s := GetFile(URL);
  i := length(s);
  while i > 1 do
    if s[i] = '/' then
      break
    else
      Dec(i);
  if i > 1 then //i holds position of last slash
    Result := copy(URL, 1, i)
  else
    Result := '/';
  //Now filter out dummy paths:

  //strip '/./' directories:
  Result := StringReplace(Result, '/./', '/', [rfReplaceAll]);

  //strip '/../' directories:
  while (pos('/../', Result)) > 0 do
  begin
    p := pos('/../', Result);
    q := p - 1;
    while (q > 1) and (Result[q] <> '/') do
      Dec(q);

    if (q >= 1) and (Result[q] = '/') then
    begin
      Delete(Result, q, p - q + 3);
    end
    else
    begin
      Result := ''; //invalid request
      exit;
    end;
  end;
end;

function TvsHTTPServer.GetVirtualDomain(Domain: string): TvsVirtualDomain;
var
  i: integer;
begin
  Domain := ParseDomain(Domain);
  Result := nil;
  for i := 0 to FHTTPVars.FVirtualDomains.Count - 1 do
    if TvsVirtualDomain(FHTTPVars.FVirtualDomains[i]).FHostName = Domain then
    begin
      Result := TvsVirtualDomain(FHTTPVars.FVirtualDomains[i]);
      break;
    end;
  //Add one if not yet exist:
  if (not Assigned(Result)) then
  begin //add one
    Result := TvsVirtualDomain.Create;
    FHTTPVars.FVirtualDomains.Add(Result);
    Result.FHostName := Domain;
  end;
end;

procedure TvsHTTPServer.RegisterAuthenticationDir(VirtualDir, Domain: string; Recursive: boolean);
var
  r: string;
  vd: TvsVirtualDomain;
begin
  if VirtualDir = '' then
    VirtualDir := '/';
  vd := GetVirtualDomain(Domain);
  with VD.FAuthNeeded do
  begin
    if Recursive then
      r := '+'
    else
      r := '-';
    r := r + VirtualDir;
    if IndexOf(r) < 0 then
      Add(r);
  end;
end;

{ THTTPHandler }

procedure TvsHTTPHandler.CopyCustomVars;
begin
  FHTTPVars := TvsHTTPServer(FSettings.Owner).FHTTPVars;
  FCallBack := TvsHTTPServer(FSettings.Owner).FCallBack;
end;

function TvsHTTPHandler.GetGetPath: TFileName;
begin
  //See if handler fits in some virtual domain

end;

procedure TvsHTTPHandler.Handler; //= Thread.Execute
//some local vars not shared with procedures:
var
  FS: TFileStream;
  i: integer;

begin
  FMode := cmWait;
  while not Terminated do
  begin
    //Read an HTTP header
    case FMode of
      cmWait: //this is not limited to GET
        //but it porcesses the header
        //if necessary, like PUT, POST or CONNECT it will change the mode
      begin
        Buf := FSock.RecvTerminated(FSettings.FTimeOut{30000}, CRLF + CRLF);

        //for the time being, don't support keep-alive (fix this)
        //fix: if connection keep-alive mode = get
        //Unless otherwise set, we close the connection after processing.
        FMode := cmClose;

        if Buf <> '' then
        begin
          FResponse.ResponseCode := 0;
          ProcessRequest(Buf);
          if FResponse.ResponseCode = 0 then
            FResponse.ResponseCode := 400; //Unknown request
          if (FResponse.ResponseCode >= 400) and (FResponse.Data = '') then //generate error doc
            MakeErrorDoc;


          FKeepAlive := (FMode = cmDone) and (FResponse.ResponseCode >= 200) and
            (FResponse.ResponseCode < 500) and
            //                              (FRequest.ProtoVersion = 'HTTP/1.1') and
            (lowercase(FRequest.Header.Values['Connection']) = 'keep-alive');

          FKeepAlive := False; //sorry, there are bugs currently.

          //keep alive conditionals:
          //the client must request it
          //it must not be a parsed PHP script
          //since there is no content-length available.

          if (FResponse.ResponseCode <> 0) then
          begin //We send data ourselves:
            Log(Format('%d %s %s %s', [FResponse.ResponseCode,
              FRequest.Command, FRequest.Parameter, FRequest.Domain,
              FRequest.FileName]));

            MakeResponseHeaders;
            FResponse.FixHeader;
            FSock.SendString('HTTP/1.1 ' + HTTPCodeToMessage(FResponse.ResponseCode) + CRLF +
              FResponse.RawHeader.Text + CRLF);
            if FResponse.Data <> '' then
              FSock.SendString(FResponse.Data);
            //                    if Assigned (FResponse.DataStream) then
            //                      FSock.SendStream (FResponse.DataStream);
          end
          else
          begin
            LogError('Internal error');
            FMode := cmClose;
          end;
          if FResponse.ResponseCode >= 500 then //disconnect
            FMode := cmClose;
        end
        else
          Terminate; //we're done here, 30s timeout.
      end;

      cmGet:
      begin
        //Send current file:
        FMode := cmClose;
        if (FCurrentGetFile <> '') then
          try
            FS := TFileStream.Create(FCurrentGetFile, fmOpenRead or fmShareDenyNone);
            FS.Seek(FRangeStart, soFromBeginning);
            while (FS.Position <= FRangeEnd{FS.Size}) and (FSock.LastError = 0) do
            begin
              SetLength(Buf, 8192);
              if (FRangeEnd - FS.Position) < 8192 then
                SetLength(Buf, FRangeEnd - FS.Position + 1);
              i := FS.Read(Buf[1], length(Buf));
              SetLength(Buf, i);
              if i > 0 then
                //todo: add bandwidth throttle
                FSock.SendString(Buf);
            end;
            FS.Free;
            //FMode := cmDone;
          except
          end;
      end;

      cmPost, cmPut:
      begin
        //read data into buffer
        //until max size.
      end;

      cmConnect:
      begin
        //Play proxy
        Log('PROXY_MODE');
        while Assigned(FSock) and Assigned(FPSock) and (FSock.LastError = 0) and (FPSock.LastError = 0) and
          not Terminated do
        begin
          //quick and dirty, there are nicer ways to play 2-way proxy but
          //at the moment this is sufficient because it is functional :)
          if (FSock.CanRead(50)) then
          begin
            if (FSock.WaitingData > 0) then
            begin
              Buf := FSock.RecvPacket(0);
              if Buf <> '' then
                FPSock.SendString(Buf);
            end
            else
              //Connection has terminated
              break;
          end;
          if (FPSock.CanRead(50)) then
          begin
            if (FPSock.WaitingData > 0) then
            begin
              Buf := FPSock.RecvPacket(0);
              if Buf <> '' then
                FSock.SendString(Buf);
            end
            else
              break;
          end;
        end;
        FPSock.Free;
        FMode := cmClose;
      end;

      cmReadCGI:
      begin
        CGISendResultsToSock(FCGIInfo, FSock);
        FMode := cmDone;
      end;

      cmDone:
      begin
        //clear all info
        //wait again for connection
        FCurrentGetFile := '';
        FPostData := '';
        FRequest.Clear;
        FResponse.Clear;

        if FKeepAlive then
          FMode := cmWait
        else
          FMode := cmClose;
      end;

      cmClose:
      begin
        //close connection
        //and end.
        //            FSock.SSLDoShutdown;
        FSock.CloseSocket;
        Terminate;
      end;

    end; //case
  end;
end;

procedure TvsHTTPHandler.ProcessRequest(Header: string);
var
  xBuf, s, proto: string;
  Meta: TStrings;
  i: integer;
  CMD: TEnumHTTPProtocols;
begin
  FRequest.Clear;
  FResponse.Clear;

  Meta := TStringList.Create;

  //May be valid HTTP header, process:
  xBuf := Header;
  FRequest.RawHeader.Text := xBuf;
  FRequest.RawRequest := xBuf; //as is


  //convert header to name-value pairs:
  for i := 1 to FRequest.RawHeader.Count - 1 do
    FRequest.Header.Add(StringReplace(FRequest.RawHeader[i], ': ', '=', []));
  if FRequest.RawHeader.Count > 0 then
  begin
    S := Trim(FRequest.RawHeader[0]);
    FRequest.RawRequest := S;

    FRequest.Command := UpperCase(copy(S, 1, pos(' ', s) - 1));

    s := trim(copy(s, pos(' ', s) + 1, maxint));
    proto := trim(copy(s, pos(' ', s) + 1, maxint)); //protocol

    Meta.Add('PROTOCOL_VERSION=' + proto);
    FRequest.ProtoVersion := proto;

    FRequest.Parameter := trim(copy(s, 1, pos(' ', s) - 1));

    s := FRequest.Command;

    //      FResponse.ResponseCode := 504; //if anything fails, send back not implemented

    FResponse.ResponseCode := 400; //Malformed request

    FRequest.Meta := Meta.Text;

    for CMD := low(TEnumHTTPProtocols) to high(TEnumHTTPProtocols) do
    begin
      if (S = UpperCase(copy(getEnumName(TypeInfo(TEnumHTTPProtocols), integer(CMD)), 3, 9))) and
        //      if ((S='GET') or (S='HEAD') or (S='POST') or (S='PUT') or (S='TRACE') or (S='OPTIONS') or (S='CONNECT')) and
        (copy(proto, 1, 5) = 'HTTP/') and (FRequest.Parameter <> '') then //looks like valid HTTP request
      begin
        //at least request is not malformed.
        FResponse.ResponseCode := 501; //not implemented

        //See if it is supported at all:
        if not (CMD in FHTTPVars.FSupported) then
        begin
          FResponse.ResponseCode := 405; //not allowed
          break;
        end;

        //              if (CMD in [hpPUT, hpPOST]) and
        //                 (FRequest.Header


        if (CMD in FHTTPVars.FAutomated) then
        begin
          case CMD of
            hpGET: ProcessGet;
            hpHEAD: ProcessHead;
            hpPOST: ProcessPost;
            //                    hpPUT: ProcessPut;
            hpDelete: ProcessDelete;
            hpConnect: ProcessConnect;
            hpOptions: ProcessOptions;
            else
              FResponse.ResponseCode := 501;
          end;
        end
        else
        begin
          CallBack(@OnRequest);
        end;
        break; //found one
      end;
    end;

  end;
  Meta.Free;

end;

procedure TvsHTTPHandler.ProcessTrace;
begin
  FResponse.ResponseCode := 200; //Ok, we'll trace
  FResponse.Data := FRequest.RawRequest;
  //that's all folks!
end;

procedure TvsHTTPHandler.ProcessConnect;
var
  v: string;
begin
  //kindly perform a connect
  if pos(':', FRequest.Parameter) > 0 then //sounds valid
  begin
    v := FRequest.Parameter;
    FPSock := TTCPBlockSocket.Create;
    FPSock.Connect(copy(v, 1, pos(':', v) - 1), copy(v, pos(':', v) + 1, Maxint));
    if FPSock.LastError = 0 then
    begin
      FResponse.ResponseCode := 200; //ok, connection accepted
      FMode := cmCONNECT;
    end
    else
    begin
      FPSock.Free;
      FMode := cmClose;
      FResponse.ResponseCode := 410; //not sure about this one. 4xx or 5xx seems ok.
    end;
  end
  else
    FResponse.ResponseCode := 400; //Bad request
end;

procedure TvsHTTPHandler.ProcessDelete;
begin
  //Not implemented yet
  //Forbidden
  FResponse.ResponseCode := 403;
end;

procedure TvsHTTPHandler.ProcessGet;
begin
  ProcessGetHeadPost(hpGET);
end;

procedure TvsHTTPHandler.ProcessHead;
begin
  ProcessGetHeadPost(hpHead);

end;

procedure TvsHTTPHandler.ProcessOptions;

  function ListOptions: string;
  var
    i: TEnumHTTPProtocols;
  begin
    for i := low(TEnumHTTPProtocols) to high(TEnumHTTPProtocols) do
      if i in FHTTPVars.FSupported then
        Result := Result + Copy(GetEnumName(TypeInfo(TEnumHTTPProtocols), integer(i)), 3, maxint) + ', '; //typinfo.name
    if Result <> '' then
    begin
      Result := 'Allow: ' + Result;
      Delete(Result, length(Result) - 1, 2);
    end;
  end;

begin
  if not (hpOptions in FHTTPVars.FSupported) then
  begin
    FResponse.ResponseCode := 405; //method not allowed
    exit;
  end;

  RequestHandled := False;

  if not (hpOptions in FHTTPVars.FAutomated) then
    CallBackRequest(FCallBack.FOnOptions)
  else
    //Server handled
  begin
    //Ignore the parameter. Any request will return the same info as
    //OPTIONS * HTTP/1.1
    //Also, ignore HTTP version.
    FResponse.Header.Add(ListOptions);
    RequestHandled := True;
    FResponse.MimeType := 'text/html';
    FResponse.Data := '<html><head></head><body>' + ListOptions + '</body></html>';
    //human readable form
    FResponse.ResponseCode := 200;
  end;
end;

procedure TvsHTTPHandler.ProcessPost;
begin
  ProcessGetHeadPost(hpPOST);
end;

procedure TvsHTTPHandler.ProcessPut;
begin
  //Sorry, not implemented (need authentication support first anyhow)
  FResponse.ResponseCode := 403;
end;


function TvsHTTPHandler.CheckGetHeadPostParser(Domain: string): boolean;
var
  Manual, CGI, FN: string;
  PreParser: TPreParser;
begin
  //test for cgi scripts or preparser or manual handled URL.
  //test manual URL first, prioritize above everything.

  //if found, call them.
  Result := False;

  if IsManualURL(FRequest.Parameter) then
  begin
    Result := True;
    //Call OnGet/OnPost/On
    if FRequest.Command = 'GET' then
      CallBackRequest(FCallBack.FOnGet)
    else
    if FRequest.Command = 'HEAD' then
      CallBackRequest(FCallBack.FOnHead)
    else
    if FRequest.Command = 'POST' then
      CallBackRequest(FCallBack.FOnPost)
    else
    begin
      FResponse.ResponseCode := 503; //Service unavailable
      exit;
    end;
  end
  else
  begin
    //Check CGI:
    PreParser := nil;
    CGI := GetCGIPath(FRequest.Parameter, Domain);
    if CGI = '' then
    begin
      FN := MapVirtualDir(FRequest.Parameter, Domain);
      if (FN <> '') and FileExists(FN) then
        PreParser := GetPreParser(FRequest.Parameter, Domain);
    end;
    if ((CGI <> '') or (Assigned(PreParser))) and (FRequest.Command = 'POST') then
    begin
      //Read in POST data

      //          ReadPostData; //this is done before...
    end;
    if CGI <> '' then
    begin
      //DoCGI
      Result := True; //prevent any further parsing
      if not FileExists(CGI) then  //todo: check case sensivity
        FResponse.ResponseCode := 403 //forbidden, also for browsing
      else
      begin
        //              FResponse.Data := ExecuteCGI (CGI, FRequest.Command, GetParams(FRequest.Parameter), FRequest.RawHeader.Text+#13#10+FPostData, FSettings, nil{FSock});
        FCGIInfo := ExecuteCGI(CGI, '', FN, FRequest.Command, GetParams(FRequest.Parameter), FRequest.RawHeader.Text,
          FPostData, FIPInfo, FRequest, FSettings, cmCGI);

        if FCGIInfo.HasResult then
        begin
          //trust CGI headers as sent (fix...)
          FResponse.RawHeader.Text := FCGIInfo.Header;
          FResponse.ResponseCode := 200;
          FMode := cmReadCGI;
        end
        else
        begin
          FResponse.ResponseCode := 404;
          FMode := cmDone;
        end;
      end;
    end;

    if Assigned(PreParser) then
    begin
      Result := True;
      FCGIInfo := ExecuteCGI(PreParser.ExePath, PreParser.Params, FN, FRequest.Command, GetParams(FRequest.Parameter),
        FRequest.RawHeader.Text, FPostData, FIPInfo, FRequest, FSettings, cmPP);
      if FCGIInfo.HasResult then
      begin
        //trust CGI headers as sent (fix...)
        FResponse.RawHeader.Text := FCGIInfo.Header;
        FResponse.ResponseCode := 200;
        FMode := cmReadCGI;
      end
      else
      begin
        FResponse.ResponseCode := 503; //Service unavailable
        FMode := cmDone;
      end;

    end;

  end;
end;

procedure TvsHTTPHandler.Report404;
begin
  //return error document.
  FResponse.ResponseCode := 404;
end;

procedure TvsHTTPHandler.Init;
begin
  FResponse := TvsHTTPResponse.Create;
  inherited;
end;

function TvsHTTPHandler.MapVirtualDir(URL, Domain: string): TFileName;
var
  Path: string;
  VD: TvsVirtualDomain;
  i: integer;
  vp, pp: string;
  recursive: boolean;
  pathplus: string;
  //if dirmapping starts with '+' recursive mapping is true
  //if starts with '-' or any other character, recursive mapping is false
begin
  Path := GetPath(URL);
  VD := GetVirtualDomain(Domain);
  if Assigned(VD) then
  begin
    for i := 0 to VD.FVirtualPath.Count - 1 do
    begin
      vp := VD.FVirtualPath[i];
      pp := TString(VD.FVirtualPath.Objects[i]).Value;
      if pp = '' then
        exit;
      recursive := False;
      if vp <> '' then
      begin
        recursive := (vp[1] = '+');
        if (vp[1] in ['+', '-']) then
          Delete(vp, 1, 1);
      end;
      if not recursive then //exact match needed:
      begin
        if Compare(Path, vp{VD.FVirtualPath[i]}) then //found
        begin
          Result := TString(VD.FVirtualPath.Objects[i]).Value + PathSep + Copy(GetFile(URL), Length(Path) + 1, maxint);
          break;
        end;
      end
      else
      begin
        if ((copy(path, 1, length(vp)) = vp) and //case sensitive
          ((length(path) = length(vp)) or //exact match
          (copy(path, length(vp), 1) = '/')  //virtual part is substring of path and full dirname matches
          )) then
        begin
          //strip the part that is not the virtual path:
          pathplus := copy(Path, length(vp) + 1, maxint);
          //convert to system slashes
          pathplus := StringReplace(PathPlus, '/', PathSep, [rfReplaceAll]);
          //filename:
          pathplus := pathplus + Copy(GetFile(URL), Length(Path) + 1, maxint);

          //some inconsistency exists
          if (PathPlus <> '') and (PathPlus[1] = PathSep) then
            Delete(PathPlus, 1, 1);

          //result is physical path + pathplus:
          Result := TString(VD.FVirtualPath.Objects[i]).Value + PathSep + PathPlus;

          if FileExists(Result) or DirectoryExists(Result) then //okidoki:
            break
          else
            Result := ''; //continue searching virtual domains.
        end
        else
        begin
          if (url + '/' = vp) and //url is a directory that is virtual mapped and needs redirect
            DirectoryExists(pp) then //mapped virtual directory exists
          begin
            Result := TString(VD.FVirtualPath.Objects[i]).Value;
            break;
          end
          else
            Result := '';
        end;

      end;
    end;
  end;
  if (Result = '') and (Domain <> '') then //try default (empty) domain:
    Result := MapVirtualDir(URL);
end;

procedure TvsHTTPHandler.CreateFileHeaders(FileName: TFileName);
var
  sr: TSearchRec;
  f: integer;
  timestamp, mimetype: string;
begin
  //Create file headers based on FileName:
  //maybe see if we have access rights? (time consuming to test rights..)

  if FileName = '' then
  begin
    FResponse.ResponseCode := 500; //Internal error:
    exit;
  end;

  if not FileExists(FileName) then
  begin
    //Maybe this is a directory, if so, redirect:
    if DirectoryExists(FileName) then
    begin
      FResponse.ResponseCode := 307; //Redirect
      FResponse.Header.Values['Location'] := FRequest.Parameter + '/';
      exit;
    end
    else
    begin
      FResponse.ResponseCode := 404; //Not found
      exit;
    end;
  end;

  f := FindFirst(FileName, faAnyFile, sr);

  if f <> 0 then //oops, error:
  begin
    FResponse.ResponseCode := 503; //Service unavailable
    exit;
  end;

  {  //try: open file
     //if not openened, not enough rights or file locked
     //return forbidden
     // ???
  }

  timestamp := RFC822DateTime(FileDateToDateTime(sr.Time));

  with FResponse.Header do
  begin
    Values['Content-Length'] := IntToStr(sr.Size);
    Values['Last-Modified'] := timestamp;
    mimetype := MimeTypeFromExtension(ExtractFileExt(FileName));
    if mimetype = '' then
      mimetype := 'application/octet-stream';
    Values['Content-Type'] := mimetype;

    //range support:
    Values['Accept-Ranges'] := 'bytes';
    FRangeStart := 0;
    FRangeEnd := sr.Size - 1;
  end;

  findclose(sr);

  //see if there is a modified tag
  if FRequest.Header.Values['If-Modified-Since'] = TimeStamp then //exact match
    FResponse.ResponseCode := 304 //Not modified
  else
    FResponse.ResponseCode := 200;

end;

function TvsHTTPHandler.GetDefaultDoc(Path: TFileName; Domain: string): string;
var
  VD: TvsVirtualDomain;
  i, j: integer;
  v: string;
begin
  //loop default documents of domain
  //until found on disc
  VD := GetVirtualDomain(Domain);
  if Assigned(VD) then
  begin
    for i := 0 to VD.FDefaultDocuments.Count - 1 do
    begin //check manually mapped files:
      for j := 0 to VD.FManualURL.Count - 1 do
        if VD.FManualURL[j] = VD.FDefaultDocuments[i] then
        begin
          Result := VD.FDefaultDocuments[i];
          break;
        end;
      if Result = '' then //check physical file locations
      begin
        v := Path +{PathSep+}VD.FDefaultDocuments[i];
        if FileExists(v) then
        begin
          Result := VD.FDefaultDocuments[i];
          break;
        end;
      end;
    end;
  end;
  if (Result = '') and (Domain <> '') then
    Result := GetDefaultDoc(Path);
end;

procedure TvsHTTPHandler.ReadPostData;

var
  j, l: integer;
  b: string;
begin
  //Read in postdata:
  FPostData := '';
  l := StrToIntDef(FRequest.Header.Values['Content-Length'], 0);
  if (l <= 0) or (l > MAX_POST_DATA_SIZE) then
  begin
    FMode := cmClose;
    exit;
  end;
  while (length(FPostData) < l) and (FSock.LastError = 0) do
  begin
    j := l - length(FPostData);
    if j > 2048 then
      j := 2048;
    b := FSock.RecvBufferStr(j, 30000);
    if b = '' then //sorry, taken too long, aborting
    begin
      FPostData := '';
      break;
    end
    else
      FPostData := FPostData + b;
  end;
end;

procedure TvsHTTPHandler.MakeResponseHeaders;
var
  connection: string;
begin
  with FResponse.Header do
  begin
    Values['Date'] := RFC822DateTime(now);
    Values['Server'] := FHTTPVars.FServerName;

    if FKeepAlive then
      Connection := 'Keep-Alive'
    else
      Connection := 'Close';
    Values['Connection'] := Connection;

    //      if Values ['Content-Type']='' then //some error doc. file transfer should set mimetype
    //        Values ['Content-Type'] := 'text/html';
    //      Values ['Accept-Ranges'] := 'bytes';
  end;
end;

procedure TvsHTTPHandler.MakeErrorDoc;
var
  FN: string;
  S: TStrings;
  ErrMsg: string;
  ServerName: string;
begin
  FResponse.Data := '';
  if (FHTTPVars.FErrorDocs <> '') then
  begin
    FN := FHTTPVars.FErrorDocs + PathSep + IntToStr(FResponse.ResponseCode) + '.html';
    if FileExists(FN) then
    begin
      S := TStringList.Create;
      try
        S.LoadFromFile(FN);
        FResponse.Data := S.Text;
      except
      end;
      S.Free;
    end;
  end;

  if FResponse.Data = '' then //Create one:
  begin
    ErrMsg := HTTPCodeToMessage(FResponse.ResponseCode);
    if ErrMsg = '' then
      ErrMsg := 'Unknown error';
    if FHTTPVars.FServerName <> '' then
      ServerName := FHTTPVars.FServerName
    else
      ServerName := 'Visual Synapse HTTP Server';
    if FRequest.Header.Values['Host'] <> '' then
      ServerName := ServerName + ' at ' + FRequest.Header.Values['Host'] + ':' + FSettings.FListenPort;
    FResponse.Data := Format('<html><head><title>%d - %s</title></head>'#10 +
      '<body><h1>Error %d</h1>   <h2>%s</h2>'#10 +
      'There was an error while processing ' + FRequest.Parameter +
      '<hr><i>' + ServerName + '</i>'#10 + '<hr><font size=-1>Visual Synapse 2004</font>'#10 +
      '</body></html>', [FResponse.ResponseCode, ErrMsg,
      FResponse.ResponseCode, ErrMsg]);
  end;
  FResponse.MimeType := 'text/html';
end;

procedure TvsHTTPHandler.MakeArgs;
var
  s: string;
  i: integer;
begin
  //split up Request.Parameter
  //into Request.FileName and Request.Args
  FRequest.FileName := GetFile(FRequest.Parameter);
  s := copy(FRequest.Parameter, length(FRequest.FileName) + 2, maxint);
  FRequest.Args.Clear;
  if s <> '' then
  begin
    s := StringReplace(s, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
    FRequest.Args.Text := StringReplace(s, '&', #13#10, [rfReplaceAll]);
    for i := 0 to FRequest.Args.Count - 1 do
      FRequest.Args[i] := StringReplace(DecodeURL(FRequest.Args[i]), '+', ' ', [rfReplaceAll]);
  end;
  //now Args contains a nice name/value pair if all went ok :)
end;

function TvsHTTPHandler.MergeURL(Path, FileName: string): string;
begin
  if Path = '' then
    Path := '/';
  if Path[length(Path)] <> '/' then
    Path := Path + '/';
  if (FileName <> '') and (FileName[1] = '/') then
    Delete(FileName, 1, 1);
  Result := Path + FileName;
end;

procedure TvsHTTPHandler.ProcessGetHeadPost(Method: TEnumHTTPProtocols);
var
  GetFileName, DefDoc, Domain: string;
begin
  //if Autonome
  //Check for manual url
  //if not, check Preparser
  //and/or CGI

  //Is GET allowed in the first place?
  if not (Method in FHTTPVars.FSupported) then
  begin
    FResponse.ResponseCode := 405; //method not allowed
    exit;
  end;

  if (FRequest.Parameter = '') or (FRequest.Parameter[1] <> '/') then
  begin
    FResponse.ResponseCode := 400; //Bad request
    exit;
  end;

  //from here, a callback to client app is possible

  //let client post before authentication, we don't care..
  //note: client to post to 404 file at the moment.. (...)
  //it is part of valid http request.
  //if dos is consideration, there are worse methods.

  if Method = hpPOST then
    ReadPostData;

  //todo: make args on postdata
  MakeArgs; //makes args for getdata


  Domain := FRequest.Header.Values['Host'];


  if IsAuthenticationNeeded(FRequest.Parameter, Domain) and not IsAuthenticated (*no params, will know by itself*) then
  begin
    FResponse.ResponseCode := 401; //Authentication needed
    FResponse.Header.Add('WWW-Authenticate: Basic realm="' + Domain + '"');
    exit;
  end;


  //Is GET automated:
  if not (Method in FHTTPVars.FAutomated) then
  begin
    //Manual.. no server action at all:
    case Method of
      hpGet: CallBackRequest(FCallBack.FOnGet);
      hpHead: CallBackRequest(FCallBack.FOnHead);
      hpPost: CallBackRequest(FCallBack.FOnPost);
    end;
  end

  else

  begin //process action:

    //see if this is CGI or PreParser:

    if not CheckGetHeadPostParser(Domain) then
    begin //try to find the file:
      //Map virtual dir:

      GetFileName := MapVirtualDir(FRequest.Parameter, Domain);
      if GetFileName = '' then //sorry, not found
      begin
        FResponse.ResponseCode := 404;
        exit;
      end;

      //Check if this is a directory, if so, redirect

      if FRequest.Parameter[Length(FRequest.Parameter)] = '/' then
        //directory request:

        //default document needed
      begin
        DefDoc := GetDefaultDoc(GetFileName, Domain);
        if DefDoc <> '' then
        begin //generate redirect
          //                   GetFileName := DefDoc
          FResponse.ResponseCode := 307; //Redirect
          FResponse.Header.Values['Location'] := MergeURL(FRequest.Parameter, DefDoc);
          exit;
        end
        else
        begin
          //else directory listing
          if True {FHTTPVars.FDirListing} then
          begin
            FResponse.ResponseCode := 200;
            if Method in [hpGet, hpPost] then //don't delived listing on HEAD
              FResponse.Data := ListDirAsHTML(GetFileName, GetFile(FRequest.Parameter), Domain);
            FResponse.MimeType := 'text/html';
            exit;
          end
          else
          begin
            //else forbidden
            FResponse.ResponseCode := 403; //forbidden
            exit;
          end;
        end;
      end
      else
      begin

      end;


      CreateFileHeaders(GetFileName); //will generate 404 or appropiate header

      //if file exists (assume readable by server)
      if ((FResponse.ResponseCode = 200))// or
        //               (FResponse.ResponseCode = 206 {Partial content}))
        and (Method in [hpGet, hpPost]) then
      begin
        //settings for file stream
        FMode := cmGet;
        FCurrentGetFile := GetFileName;

        CheckRanges;

      end
      else
        FMode := cmDone;

    end;
  end;
end;

procedure TvsHTTPHandler.CheckRanges;
var
  Range, srstart, srend: string;
  RStart, REnd: int64;
begin
  //see if client request range:
  Range := FRequest.Header.Values['Range'];
  //Valid according to rfc 2616 is also:
  //Range: bytes=1-2,3-4,5-6
  //we don't support that currently...
  //Range: bytes=2-3 is supported.
  if Range <> '' then
  begin
    if (lowercase(copy(Range, 1, 6)) <> 'bytes=') or (pos(',', Range) > 0) then
    begin
      FResponse.ResponseCode := 501; //Not implemented
      exit; //sorry...
    end;
    Range := copy(range, 7, maxint);
    srStart := copy(Range, 1, pos('-', Range) - 1);
    srEnd := copy(Range, pos('-', Range) + 1, maxint);
    rStart := StrToIntDef(srStart, -1);
    rEnd := StrToIntDef(srEnd, -1);
    if ((srStart <> '') and (rStart = -1)) or ((srEnd <> '') and (rEnd = -1)) or ((srEnd <> '') and (rEnd < rStart)) then
    begin
      FResponse.ResponseCode := 400; //Malformed request
      exit;
    end;
    if rStart < FRangeStart then
      rStart := FRangeStart;
    if (rEnd = -1) or (rEnd > FRangeEnd) then
      rEnd := FRangeEnd;
    FRangeStart := rStart;
    FRangeEnd := rEnd;
    FResponse.ResponseCode := 206;
  end;

  if FResponse.ResponseCode = 206 then
  begin
    FResponse.Header.Values['Content-Range'] :=
      Format('%d-%d/%s', [FRangeStart, FRangeEnd, FResponse.Header.Values['Content-Length']]);
    FResponse.Header.Values['Content-Length'] :=
      IntToStr(1 + FRangeEnd - FRangeStart);
  end;
end;

function TvsHTTPHandler.IsAuthenticationNeeded(URL, Domain: string): boolean;
var
  vd: TvsVirtualDomain;
  i: integer;
  vp: string;
  recursive: boolean;
  path: string;
begin
  // loop ALL domains
  // only on positive (authentication needed) break
  // do not break on mapped directory that does not need authentication
  VD := GetVirtualDomain(Domain);
  //clean stuff up
  Path := GetPath(URL) + GetFileNoPath(URL);
  Result := False;
  if Assigned(VD) then
  begin
    for i := 0 to VD.FAuthNeeded.Count - 1 do
    begin
      vp := VD.FAuthNeeded[i];
      recursive := False;
      if vp <> '' then
      begin
        recursive := (vp[1] = '+');
        if (vp[1] in ['+', '-']) then
          Delete(vp, 1, 1);
      end;
      if not recursive then //exact match needed:
      begin
        if Compare(Path, vp) then //found
        begin
          Result := True;
          break;
        end;
      end
      else
      begin
        //in contrary to MapVirtualDir this check does not require existing path
        //you may well need to authenticate to get a 404 thereafter.
        //this both allows to authenticate just specific files in stead of directories
        //but also easifies using wildcards since substrings are allowed.
        //Note that this only accounts the virtual mappings!
        //if you map the same physical dir twice, you must set authentication twice.

        if (lowercase(copy(Path, 1, length(vp))) = lowercase(vp)) then //case insensitive
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
  if (not Result) and (Domain <> '') then
    Result := IsAuthenticationNeeded(URL);
end;

function TvsHTTPHandler.GetFileNoPath(URL: string): string;
var
  i: integer;
begin
  i := pos('?', URL);
  if i > 0 then
    URL := Copy(URL, 1, i - 1)
  else
    URL := URL;
  i := length(URL);
  while i >= 1 do
  begin
    if URL[i] in ['\', '/'] then
    begin
      URL := Copy(URL, i + 1, maxint);
      break;
    end
    else
      Dec(i);
  end;
  Result := URL;
end;

function TvsHTTPHandler.IsAuthenticated: boolean;
var
  v: string;
  up, u, p: string;
begin
  Result := False;
  v := trim(FRequest.Header.Values['Authorization']);
  if v = '' then
    exit;
  //we expect 'basic ' here
  if lowercase(copy(v, 1, 6)) = 'basic ' then
  begin
    up := DecodeBase64(trim(copy(v, 7, maxint)));
    u := copy(up, 1, pos(':', up) - 1);
    p := copy(up, pos(':', up) + 1, maxint);
    if (u <> '') then
      Result := FSettings.FAuthentication.Authenticate(u, p);
  end;
  //sorry, digest not yet supported.
  //this involves some interaction with the authentication module...
  //also, this is not yet considered threadsafe..

end;

{ TVirtualDomain }

constructor TvsVirtualDomain.Create;
begin
  FDefaultDocument := '';
  FDefaultDocuments := TStringList.Create;
  FHostName := '';
  FVirtualPath := TStringList.Create;
  FCGI := TStringList.Create;
  FPreParser := TStringList.Create;
  FManualURL := TStringList.Create;
  FMimeTypes := TStringList.Create;
  FAuthNeeded := TStringList.Create;
end;

destructor TvsVirtualDomain.Destroy;

begin
  FDefaultDocuments.Free;
  FAuthNeeded.Free;
  FreeWithObj(FVirtualPath);
  FreeWithObj(FCGI);
  FreeWithObj(FPreParser);
  FreeWithObj(FManualURL);
  FreeWithObj(FMimeTypes);
end;

{ THTTPResponse }

procedure TvsHTTPResponse.FixHeader;
var
  i: integer;
begin
  if MimeType <> '' then
    Header.Values['Content-Type'] := MimeType;
  if Data <> '' then
    Header.Values['Content-Length'] := IntToStr(Length(Data));
  for i := 0 to Header.Count - 1 do
    RawHeader.Add(StringReplace(Header[i], '=', ': ', []));
  for i := RawHeader.Count - 1 downto 0 do
    if RawHeader[i] = '' then
      RawHeader.Delete(i);



end;

end.
