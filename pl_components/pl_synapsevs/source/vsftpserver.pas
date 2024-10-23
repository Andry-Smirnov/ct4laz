
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsFtpServer;

interface

uses
   {$IFDEF WINDOWS}
    Windows,
   {$ENDIF}
    Classes, SysUtils, typinfo, blcksock, vsVisualServerBase, inifiles, vsTypeDef,
    synacode, synautil;

const PathSeparator = {$IFDEF UNIX} '/' {$ELSE} '\' {$ENDIF};

//Force english output on non-english systems:
var Months : array[1..12] of String = (
      'Jan',
      'Feb',
      'Mar',
      'Apr',
      'May',
      'Jun',
      'Jul',
      'Aug',
      'Sep',
      'Oct',
      'Nov',
      'Dec'
    );


type

  TFtpConnectionMode = (cmWait, cmLogOn, cmWaitUser, cmWaitPass, cmLoggedOn, cmTransferBusy, cmTimeOut, cmClose);

  TFtpSettings = record
    FExternalIP: String;
    FPasvPortMin: Integer;
    FPasvPortMax: Integer;
  end;

  TvsFTPServer = class (TVisualServer)
  private
  protected
    FDirectories: TStrings;
    FFtpSettings: TFtpSettings;
  public
    constructor Create (AOwner:TComponent); override;
    destructor Destroy; override;
    function AddHomeDir (Directory: String; User: String=''; ReadOnly: Boolean=True): Boolean;
    function SaveSettings (FileName: TFileName): Boolean; override;
    function LoadSettings (FileName: TFileName): Boolean; override;
  published
    property ExternalIP:String read FFtpSettings.FExternalIP write FFtpSettings.FExternalIP;
    property PasvPortMin:Integer read FFtpSettings.FPasvPortMin write FFtpSettings.FPasvPortMin;
    property PasvPortMax:Integer read FFtpSettings.FPasvPortMax write FFtpSettings.FPasvPortMax;
  end;

  TvsFTPData = class;

  TvsFTPHandler = class (TServerHandler)
  private
  protected
    FMode: TftpConnectionMode;
    FDirectories: TStrings;
    FHomeDir: String;
    FVirtualDir: String;
    FPhysicalDir: String;
    FReadOnly: Boolean;
    FfUser: String;
    FPasv: TTCPBlockSocket;
    FDataSock: TTCPBlockSocket;
    FData: TvsFTPData;
    FTransferType: Char;
    FRemoteDataIP: String;
    FRemoteDataPort: String;
    FRenameFrom: String;
    FModeTLS: Boolean;
    FFtpSettings: TFtpSettings;

    //support functions:
    function GetHomeDir (User: String): String;
    function GetFullPhysicalDir(Value: String; MustExist: Boolean=True): String;
    function ProcessCommand (cmd, param: String): Boolean;  //returns true if busy

    //FTP command interpretation functions:
    procedure DoPasv;
    procedure MkDir (value: String);
    procedure RmDir (value: String);
    procedure DelFile (value: String);
    procedure GetFileSize (value: String);
    procedure UpDir;
    procedure SetType (value: String);
    procedure SetStructure (value: String);
    procedure SetMode (value: String);
    procedure SetRemoteIPPort (value: String);
    procedure SetRemoteIPPortEx (value: String);
    procedure ReInitUser;
    procedure ChangeWorkDir (Value: String);
    procedure ListFiles (Value: String=''; UseControlConnection: Boolean=False);
    procedure NameListFiles (Value: String='');
    procedure SendFile (Value: String);
    procedure PutFile (Value: String; Append: Boolean=False);
    procedure AppendFile (Value: String);
    procedure AbortFile;
    procedure RenameFrom (Value: String);
    procedure RenameTo (Value: String);
    procedure SendHelp (Value: String);
    procedure SiteInfo;
    procedure SystInfo;
    procedure StatDataConnection;

    //data transport functions:
    function  OpenDataConnection: Boolean;
    procedure SendDataString (Value: String);
    procedure SendDataStream (Stream: TStream);
    procedure RecvDataStream (Stream: TStream);
    procedure DoAutoTLS (Value: String);
  public
    procedure CopyCustomVars; override;
    procedure Handler; override;
    procedure Send (Code: Integer; Msg: String='');
    procedure SendMultiLine (Code: Integer; Lines: TStrings);
    function CodeToString (Code: Integer; Msg: String=''): String;
  end;


  TDataMethod = (dmBufferSend, dmStreamSend, dmAsciiStreamSend, dmStreamRecv, dmAsciiStreamRecv);

  TvsFTPData = class (TVSThread)
    FDataBuffer: String;
    FDataStream: TStream;
    FDataFile: TextFile;
    FDataMethod: TDataMethod;
    FCloseStream: Boolean;
    FDataSock: TTCPBlockSocket;
    procedure Execute; override;
  end;


implementation

{ TvsFTPServer }

constructor TvsFTPServer.Create(AOwner: TComponent);
begin
  inherited;
  FClientType := TvsFTPHandler;
  ListenPort := '21';
  FSettings.FHasCustomVars := True;
  FDirectories := TStringList.Create;
end;


destructor TvsFTPServer.Destroy;
begin
  FreeWithObj (FDirectories);
  inherited;
end;

function TvsFTPServer.AddHomeDir(Directory, User: String;
  ReadOnly: Boolean): Boolean;
begin
  Result := False;
  if Directory = '' then
    exit;
  if Directory[length(Directory)]<>PathSeparator then
    Directory := Directory + PathSeparator;
  if not (DirectoryExists (Directory)) then
    exit;
  if ReadOnly then
    Directory := '-' + Directory
  else
    Directory := '+' + Directory;
  //Add this directory
  FDirectories.AddObject (User, StrToObj(Directory));
  Result := True;
end;

function TvsFTPServer.LoadSettings(FileName: TFileName): Boolean;
var sl: TStrings;
    i: Integer;
    n,v: String;
    r: Boolean;
begin
//  ClearSettings;
  Result := False;
  if not InitIniRead(FileName) then
    exit;
  ExternalIP := FIni.ReadString ('ftp', 'ExternalIP', '');
  PasvPortMin := FIni.ReadInteger ('ftp', 'PasvPortMin', 0);
  PasvPortMax := FIni.ReadInteger ('ftp', 'PasvPortMax', 0);
  sl := TStringList.Create;
  FIni.ReadSectionValues ('homedir', sl);
  for i:=0 to sl.Count - 1 do
    begin
      n := sl.Names[i];
      v := sl.Values[n];
      if (n<>'') and (v<>'') and (v[1] in ['-', '+']) then
        begin
          r := v[1] = '-';
          Delete (v,1,1);
          AddHomeDir (v,n,r);
        end;
    end;

  Result := True;
end;

function TvsFTPServer.SaveSettings(FileName: TFileName): Boolean;
var i: Integer;
    u,d: String;
begin
  Result := False;
  if not InitIniWrite(FileName) then
    exit;
  FIni.WriteString ('ftp', 'ExternalIP', ExternalIP);
  FIni.WriteInteger ('ftp', 'PasvPortMin', PasvPortMin);
  FIni.WriteInteger ('ftp', 'PasvPortMax', PasvPortMax);
  for i:=0 to FDirectories.Count - 1 do
    FIni.WriteString ('HomeDir', FDirectories[i], TString(FDirectories.Objects[i]).Value);
  FinishIni;
  Result := True;
end;

{ TvsFTPHandler }



function TvsFTPHandler.CodeToString(Code: Integer; Msg: String=''): String;
begin
  Result := '';
  case Code of
    //as taken from RFC 959

    110: Result := ' Restart marker reply.';
   {
       In this case, the text is exact and not left to the
       particular implementation; it must read:
            MARK yyyy = mmmm
       Where yyyy is User-process data stream marker, and mmmm
       server's equivalent marker (note the spaces between markers
       and "=").}
    120: Result := ' Service ready in %s minutes.';
    125: Result := ' Data connection already open; transfer starting.';
    150: Result := ' File status okay; about to open data connection.';

    200: Result := ' Command okay.';
   1200: Result := ' NOOP okay.';
   2200: Result := ' TYPE set to %s';
   3200: Result := ' %s';
    202: Result := ' Command not implemented, superfluous at this site.';
    211: Result := ' System status, or system help reply.';
    212: Result := ' Directory status.';
    213: Result := ' File status.';
    214: Result := ' Help message.';
    {    On how to use the server or the meaning of a particular
       non-standard command.  This reply is useful only to the
       human user.}
    215: Result := ' %s system type.';
    {   Where NAME is an official system name from the list in the
       Assigned Numbers document.}
    220: Result := ' Service ready for new user.';
    221: Result := ' Service closing control connection.';
   1221: Result := ' Bye bye.';
    //    Logged out if appropriate.
    225: Result := ' Data connection open; no transfer in progress.';
    226: Result := ' Closing data connection.';
    //   Requested file action successful (for example, file
    //   transfer or file abort).
    227: Result := ' Entering Passive Mode (%s)';
    230: Result := ' User logged in, proceed.';
    250: Result := ' Requested file action okay, completed.';
    257: Result := ' "%s" created.';
   1257: Result := ' "%s" is current directory';
    331: Result := ' User name okay, need password.';
    332: Result := ' Need account for login.';
    350: Result := ' Requested file action pending further information.';

    421: Result := ' Service not available, closing control connection.';
    //   This may be a reply to any command if the service knows it
    //   must shut down.
    425: Result := ' Can''t open data connection.';
    426: Result := ' Connection closed; transfer aborted.';
    450: Result := ' Requested file action not taken.';
    //   File unavailable (e.g., file busy).
    451: Result := ' Requested action aborted: local error in processing.';
    452: Result := ' Requested action not taken.';
    //   Insufficient storage space in system.

    500: Result := ' Syntax error, command unrecognized.';
   1500: Result := ' %s not understood';
    //   This may include errors such as command line too long.
    501: Result := ' Syntax error in parameters or arguments.';
    502: Result := ' Command not implemented.';
    503: Result := ' Bad sequence of commands.';
   1503: Result := ' %s';
    504: Result := ' Command not implemented for that parameter.';
    530: Result := ' Not logged in.';
    532: Result := ' Need account for storing files.';
    550: Result := ' Requested action not taken.';
    //    File unavailable (e.g., file not found, no access).
    551: Result := ' Requested action aborted: page type unknown.';
    552: Result := ' Requested file action aborted.';
    //    Exceeded storage allocation (for current directory or
    //    dataset).
    553: Result := ' Requested action not taken.';
    //   File name not allowed.
  end;
  if Result = '' then
    Result := CodeToString (502) //command not implemented? in fact: internal server error
  else
    begin
      if pos ('%s', Result)>0 then
        begin
          if Msg='' then
            Msg := '<Unknown>';
          Result := StringReplace (Result, '%s', Msg, [rfIgnoreCase]);
        end;
      if Code > 1000 then
        Code := Code mod 1000; //for additional (customized) message types
      Result := IntToStr (Code)+Result;
    end;
end;

procedure TvsFTPHandler.CopyCustomVars;
begin
  //Please note that this is not thread-safe (!)
  //Adding users on running ftp server may be risky.
  //You should shutdown ftp server before adding users.
  FDirectories := TvsFTPServer(FSettings.Owner).FDirectories;
  FFtpSettings := TvsFTPServer(FSettings.Owner).FFtpSettings;
end;

procedure TvsFTPHandler.DoPasv;
var ip, ipport: String;
    port: Integer;
    c,pp, pb, pmin, pmax: Integer;
    extip: String;

begin
  if Assigned (FPasv) then
    FreeAndNil (FPasv);
  FPasv := TTCPBlockSocket.Create;
  ip := FSock.GetLocalSinIP;
  //set external ip if appropiate

  with FFtpSettings do
    if (FExternalIP = '') or
       (FExternalIP = '0.0.0.0') or
       (not isIP (FExternalIP)) or
//       (FSock.GetRemoteSinIP='127.0.0.1') or
       IsInNetMask (FSock.GetLocalSinIP, FSock.GetRemoteSinIP, '255.255.255.0') //assume class C network
       //or external ip within netmask
      then
      extip := ip
    else
      extip := FExternalIP;
  with FFtpSettings do
    //bind to first port or port within range if desired:
    if (FPasvPortMin=0) and (FPasvPortMax=0) then
      FPasv.Bind (ip, '0')
    else
      begin
        c := 0;
        pp := FPasvPortMax - FPasvPortMin;
        if pp<0 then pp := 0;
        inc (pp);
        repeat
          pb := FPasvPortMin + Random (pp);
          FPasv.Bind (ip, IntToStr(pb));
          inc (c);
        until (FPasv.LastError = 0) or (c>200);
      end;
  if FPasv.LastError = 0 then
    FPasv.Listen;
  if FPasv.LastError <> 0 then
    begin
      FreeAndNil (FPasv);
      Send (425); //unable to open data connection
      exit;
    end;
  port := FPasv.GetLocalSinPort;
  ipport := StringReplace (extip, '.', ',', [rfReplaceAll]) +
            ',' +
            IntToStr (port div 256) +
            ',' +
            IntToStr (port mod 256);
  Send (227, ipport);
end;

function TvsFTPHandler.GetFullPhysicalDir(Value: String; MustExist: Boolean=True): String;
var s: String;
    p,q: Integer;
begin
  if (Value='') then
    s := FPhysicalDir
  else
    if Value[1]='/' then
      begin
        Delete (Value, 1, 1);
        s := FHomeDir + Value
      end
    else //relative from current dir.
      s := FPhysicalDir + PathSeparator + Value;
  //Now clean up:
  //strip '/./' directories:
  s := StringReplace (s, '/./', '/', [rfReplaceAll]);

  //strip '/../' directories:
  while (pos ('/../', s)) > 0 do
    begin
      p := pos ('/../', s);
      q := p - 1;
      while (q>1) and (s[q] <> '/') do
        dec (q);

      if (q>=1) and (s[q] = '/') then
        begin
          Delete (s, q, p - q +3);
        end
      else
        begin
          s := ''; //invalid request
        end;
    end;
  //now match against homedir (safety)
  //homedir should be substring of physical dir starting at 1
  if copy (s, 1, length (FHomeDir)) <>
     FHomeDir then
    begin
      LogError ('Error: physical dir outrooted homedir');
      LogError ('Physical dir searched by client: '+s);
      LogError ('Actual Homedir: '+FHomeDir);
      LogError ('Directory requested by client: '+Value);
      s := FHomeDir;
    end;

  s := ExpandFileName (s); //not necessary?
  Result := s;
end;

function TvsFTPHandler.GetHomeDir(User: String): String;
var i: Integer;
begin
  FReadOnly := True;
  i := FDirectories.IndexOf (User);
  if i>=0 then
    begin
      Result := TString(FDirectories.Objects[i]).Value;
      if Result<>'' then  //should be
        begin
          FReadOnly := Result[1]<>'+';
          Delete (Result, 1, 1);
        end;
    end
  else
    Result := '';
  if (Result = '') and (User<>'') then
    Result := GetHomeDir ('');
end;

procedure TvsFTPHandler.Handler;
var cmd, c1, c2, cl1: String;
    Idle: Integer;
    sp: Integer;
    User, Pass: String;
    BadLogon: Integer;
    i: Integer;
    CmdTimeOut: Integer;
begin
  FMode := cmWait;
  FSock.SendString ('220 Visual Synapse FTP Server 0.1 Ready'#13#10);
  FMode := cmLogOn;
  Idle := 0;
  BadLogon := 0;
  FTransferType := 'A'; //Ascii by default
  CmdTimeOut := 20000;

  while not Terminated do
    begin
      sleep (200);
      if FMode = cmTransferBusy then
        begin
          cmdTimeOut := 200; //increase response speed
          Idle := 0;
          if FData.Terminated then
            begin
              FData.WaitFor;
              FreeAndNil (FData);
              Send (226); //data connection closed
              FMode := cmLoggedOn;
              cmdTimeOut := 20000;
            end;
        end;
      cmd := FSock.RecvString (cmdTimeOut);
      c1 := '';
      c2 := '';
      cl1 := '';
      sp := 0;
      if cmd='' then
        inc (Idle)
      else
        begin
          Idle := 0;
          //check for invalid characters
          cmd := trim (cmd);
          for i := 1 to length (cmd) do
            if (cmd[i] in ['\', '!', {'"',} '$', '%', '&', '''', {'*',} '+', {'-',} ':', ';', '<', '>', '=', '?', '^', '`']) or
               (cmd[i] < ' ') or
               (cmd[i] > 'z') then
              begin
                //invalid characters
                send (500);
                cmd := '';
                break;
              end;
          sp := pos (' ', cmd);
          if sp>0 then
            begin
              c1 := copy (cmd, 1, sp-1);
              c2 := trim(copy (cmd, sp+1, maxint));
              if (c2<>'') and (c2[1]='"') and (c2[length(c2)]='"') then
                begin
                  Delete (c2,1,1);
                  Delete (c2, length (c2), 1); //delete is safe, also if c2 would only contain '"'
                end;
            end
          else
            begin
              c1 := cmd;
              c2 := '';
            end;
          cl1 := lowercase (c1);
        end;
      if Idle > 25 then  // 5 minutes
        FMode := cmTimeOut;
      if FMode = cmLogOn then
        begin
          User := '';
          Pass := '';
          FMode := cmWaitUser;
        end;

      if cmd<>'' then //only parse if there was some command
        begin
          //stateless commands:
          if cl1='quit' then
            begin
              Send (1221); //closing connection
              FMode := cmClose;
            end;
          if cl1='help' then
            begin
              Send (214); //send help message
              continue; //don't process further..
            end;
          case FMode of
            cmTimeOut:
              begin
                Send (421); //timeout
                FMode := cmClose;
                Terminate;
              end;
            cmClose:
              begin
                Log ('CLOSING');
                Terminate;
              end;
            cmWaitUser:
              begin
                if cl1='user' then
                  begin
                    User := trim(c2);
                    Send (331);
                    FMode := cmWaitPass;
                  end
                else
                  begin
                    //if cl1='help' then sendhelpmessage
                    Send (500); //command not understood.
                  end;
              end;
            cmWaitPass:
              begin
                if cl1='pass' then
                  Pass := trim(c2);
                //go authenticate
                if (lowercase(user)='anonymous') or
                   FSettings.FAuthentication.Authenticate (user, pass) then
                  begin
                    FHomeDir := GetHomeDir (User);
                    Log ('LOGIN "'+user+'" ['+FHomeDir+']');
                    if FHomeDir <> '' then
                      begin
                        Send (230); //okidoki, logged on.
                        FMode := cmLoggedOn;
                        FUser := User;
                        FVirtualDir := '/';
                        FPhysicalDir := FHomeDir;
                        if lowercase(user)='anonymous' then
                          Log (FSock.GetRemoteSinIP + ' Anonymous logon; pass='+pass)
                        else
                          Log (FSock.GetRemoteSinIP + ' User logon; user='+user);
                      end
                    else  //oops, not configured, terminate.
                      begin
                        Send (421); //service unavailable, closing connection
                        Log ('FAILED_LOGON "'+user+'"');
                        FMode := cmClose;
                        Terminate;
                      end;
                    if Pass <> '' then //remove password from possible memory dump:
                      FillChar (Pass[1], Length(Pass), ' ');
                  end
                else
                  begin
                    //sorry, no go
                    FMode := cmLogOn;
                    Send (530); //not logged on
                    inc (BadLogon);
                    if BadLogOn < 5 then
                      Send (220) //ready for new user
                    else
                      begin
                        //too many retries
                        Send (221); //closing connection
                        FMode := cmClose;
                        Terminate;
                      end;
                  end;
              end;
            cmLoggedOn:
              begin
                //Process command, whatever it is
                ProcessCommand (cl1, Trim(c2));
              end;
            cmTransferBusy:
              begin
                //there is only one command we accept right now
                if cl1='abor' then
                  begin
                    Log ('TRANSFER_ABORTED');
                    AbortFile;
                  end
                else
                  Send (503); //bad sequence of commands.
              end;
          end;  //case
        end;
      //logged out..
    end;
end;

function TvsFTPHandler.ProcessCommand(cmd, param: String): Boolean;
begin
  //special checks
  if (FRenameFrom<>'') and
     (cmd<>'rnto') then
    begin
      Send (1503, 'Expected RNTO'); //bad command sequence
      FRenameFrom := '';
      exit;
    end;
  //various possible commands
  if cmd='pasv' then
    DoPasv
  else
  if (cmd='pwd') or (cmd='xpwd') then
    Send (1257, FVirtualDir)
  else
  if cmd='noop' then
    Send (1200) //noop ok
  else
  if cmd='port' then
    SetRemoteIPPort (param)
  else
  if cmd='eprt' then
    SetRemoteIPPortEx (param)
  else
  if (cmd='rmd') or (cmd='xrmd') then
    RmDir (param)
  else
  if (cmd='mkd') or (cmd='xmkd') then
    MkDir (param)
  else
  if (cmd='cdup') or (cmd='xcup') then
    UpDir
  else
  if cmd='type' then
    SetType (param)
  else
  if cmd='stru' then
    SetStructure (param)
  else
  if cmd='rein' then
    ReInitUser
  else
  if cmd='list' then
    ListFiles (param)
  else
  if (cmd='nlst') and (lowercase(param)='-l') then
    //do a 'normal' list files after all
    ListFiles ('')
  else
  if cmd='nlst' then
    NameListFiles (param)
  else
  if cmd='dele' then
    DelFile (param)
  else
  if cmd='size' then
    GetFileSize (param)
  else
  if (cmd='cwd') or (cmd='xcwd') then
    ChangeWorkDir (param)
  else
  if cmd='retr' then
    SendFile (param)
  else
  if cmd='stor' then
    PutFile (param)
  else
  if cmd='appe' then
    AppendFile (param)
  else
  if cmd='allo' then
    Send (200) //just ignore..
  else
  if cmd='rnfr' then
    RenameFrom (param)
  else
  if cmd='rnto' then
    RenameTo (param)
  else
  if cmd='auth' then
    DoAutoTLS (param)
  else
  //commands invalid at this stage:
  if (cmd='user') or
     (cmd='pass') then
    Send (503) //Bad sequence of commands (??? -> not valid at this state)
  else
  //currently unsupported commands
  if (cmd='acct') or
     (cmd='smnt') or
     (cmd='rein') or
     (cmd='syst') or
     (cmd='site') or
     (cmd='stat') or
     (cmd='allo') then
    Send (502) //Command not implemented
  else
  //unknown command (user typo)
    Send (500); //Invalid command

(*  From the RFC
    Valid FTP commands:
            USER <SP> <username> <CRLF>
            PASS <SP> <password> <CRLF>
            ACCT <SP> <account-information> <CRLF>
            CWD  <SP> <pathname> <CRLF>
            CDUP <CRLF>
            SMNT <SP> <pathname> <CRLF>
            QUIT <CRLF>
            REIN <CRLF>
            PORT <SP> <host-port> <CRLF>
            PASV <CRLF>
            TYPE <SP> <type-code> <CRLF>
            STRU <SP> <structure-code> <CRLF>
            MODE <SP> <mode-code> <CRLF>
            RETR <SP> <pathname> <CRLF>
            STOR <SP> <pathname> <CRLF>
            STOU <CRLF>
            APPE <SP> <pathname> <CRLF>
            ALLO <SP> <decimal-integer>
                [<SP> R <SP> <decimal-integer>] <CRLF>
            REST <SP> <marker> <CRLF>
            RNFR <SP> <pathname> <CRLF>
            RNTO <SP> <pathname> <CRLF>
            ABOR <CRLF>
            DELE <SP> <pathname> <CRLF>
            RMD  <SP> <pathname> <CRLF>
            MKD  <SP> <pathname> <CRLF>
            PWD  <CRLF>
            LIST [<SP> <pathname>] <CRLF>
            NLST [<SP> <pathname>] <CRLF>
            SITE <SP> <string> <CRLF>
            SYST <CRLF>
            STAT [<SP> <pathname>] <CRLF>
            HELP [<SP> <string>] <CRLF>
            NOOP <CRLF>
*)
  //special checks
  if cmd<>'rnfr' then
    FRenameFrom := '';
end;

procedure TvsFTPHandler.MkDir(value: String);
var fd: String;
begin
  if FReadOnly then
    Send (550) //no access
  else
    begin
      fd := GetFullPhysicalDir(value);
      //try to create
      if CreateDir (fd) then
        Send (250) //requested file action taken
      else
        Send (450); //not empty or access denied
    end;
end;


procedure TvsFTPHandler.RmDir(value: String);
var fd: String;
begin
  if FReadOnly then
    Send (550) //no access
  else
    begin
      fd := GetFullPhysicalDir(value);
      if DirectoryExists (fd) then
        begin
          //try to remove
          if RemoveDir (fd) then
            Send (250) //requested file action taken
          else
            Send (450); //not empty or access denied
        end
      else
        //not found
        Send (550, 'does not exist');
    end;
end;

procedure TvsFTPHandler.Send(Code: Integer; Msg: String='');
var f: String;
begin
  f := CodeToString (Code, Msg);
  FSock.SendString (f + #13#10);
  if FSock.LastError <> 0 then
    Terminate;
end;

procedure TvsFTPHandler.SendMultiLine(Code: Integer; Lines: TStrings);
var f: String;
    i: Integer;
//untested proc, beware of bugs.
begin
  if Lines.Count = 0 then
    f := CodeToString(Code)+#13#10;
  if Lines.Count = 1 then
    f := IntToStr(Code) + ' ' + Lines[0] + #13#10;
  if Lines.Count > 1 then
    begin
      for i:=0 to Lines.Count - 2 do
        Lines[i] := IntToStr(Code)+'-'+Lines[i];
      i:=Lines.Count - 1;
      Lines[i] := IntToStr(Code)+' '+Lines[i];
      f := Lines.Text;
    end;
  FSock.SendString (f);
  if FSock.LastError <> 0 then
    Terminate;
end;

procedure TvsFTPHandler.UpDir;
var v: String;
    i: Integer;
    oldpath: String;
begin
  oldpath := FPhysicalDir;
  ChangeWorkDir ('..');
{  if FPhysicalDir = oldpath then
    Send (450)
  else
    Send (250);
}
(**
  v := FVirtualDir;
  i := length (v) - 1;
  while i>1 do
    if v[i]<>'/' then
      dec (i)
    else
      v := copy (v, 1, i);
  if v=FVirtualDir then //did not succeed
    begin
      Send (450)
    end
  else
    begin
      FPhysicalDir := copy (FPhysicalDir, 1, length (FPhysicalDir) + length (v) - length (FVirtualDir));
      FVirtualDir := v;
      Send (250);
    end;
**)
end;

procedure TvsFTPHandler.SetStructure(value: String);
begin
  if length (Value)<> 1 then
    Send (501)
  else
    begin
      if Value='F' then
        Send (200) //ok, structure set to File
      else
        //Send (1500, 'Structure '+Value); //structure not understood
        Send (504);
    end;

end;

procedure TvsFTPHandler.SetType(value: String);
var ch: Char;
begin
  if length (Value)<1 then
    Send (501)
  else
    begin
      ch := UpCase (Value[1]);
      if (ch) in ['A', 'E', 'I', 'L'] then
        begin //at least it is valid
          //note that we simply ignore additional params on A, E and L types.
          if (ch) in ['A', 'I'] then
            begin
              FTransferType := ch;
              Send (2200, FTransferType);
            end
          else
            //Send (1500, 'TYPE '+Ch);
            Send (504); //unsupported
        end
      else
        Send (501);
    end;
end;

procedure TvsFTPHandler.SetRemoteIPPort(value: String);
var sl: TStrings;
    ok: Boolean;
    i,j: Integer;
begin
  if Assigned (FPasv) then //passive or active, not both
    FreeAndNil (FPasv);
  sl := TStringList.Create;
  sl.text := StringReplace (value, ',', #13#10, [rfReplaceAll]);
  if sl.Count <> 6 then //malformed
    begin
      Send (501)
    end
  else
    begin
      ok := true;
      for i:=0 to sl.Count - 1 do
        begin
          j := StrToIntDef (sl[i], -1);
          if (j<0) or (j>255) then
            begin
              ok := false;
              break;
            end;
        end;
      if ok then
        begin
          FRemoteDataIP := sl[0]+'.'+sl[1]+'.'+sl[2]+'.'+sl[3];
          FRemoteDataPort := IntToStr (StrToIntDef (sl[4], 0)*256 + StrToIntDef (sl[5], 0));
          Send (200);
        end
      else
        Send (501);
    end;
  sl.Free;
end;

procedure TvsFTPHandler.SetRemoteIPPortEx(value: String);
var sl: TStrings;
    i: Integer;
begin
  // see RFC 2428
  if Assigned (FPasv) then //passive or active, not both
    FreeAndNil (FPasv);
  sl := TStringList.Create;
  sl.text := StringReplace (value, '|', #13#10, [rfReplaceAll]);
  for i:= sl.count - 1 downto 0 do
    if sl[i]='' then
      sl.Delete(i);
  if sl.Count <> 3 then
    Send (501) //improperly formatted
  else
    begin
      i := StrToIntDef (sl[0], -1);
      if (i in [1,2]) then //assume to be ok
        begin
          //sanity check
          if ( (i=1) and (IsIP (sl[1])) or //valid IP4?
               (i=2) and (IsIP6 (sl[1])) ) and //valid IP6?
             (StrToIntDef (sl[2], -1) > 0) and //valid port?
             (StrToIntDef (sl[2], -1) < $FFFF) then
            begin
              FRemoteDataIP := sl[1];
              FRemoteDataPort := sl[2];
              Send (200);
            end
          else
            Send (501); //bad bad client
        end
      else
        Send (502); //unsupported network protocol
    end;
  sl.Free;
end;

procedure TvsFTPHandler.SetMode(value: String);
begin
  //accept only stream mode.
  if length (Value)<>1 then
    Send (501)
  else
    begin
      if UpCase(Value[1]) in ['S', 'B', 'C'] then
        begin
          if UpCase (Value[1])='S' then
            Send (200)
          else
            Send (504);
        end
      else
        Send (501);
    end;
end;

procedure TvsFTPHandler.ReInitUser;
begin
  FUser := '';
  FMode := cmLogOn;
  Send (220); //ok, ready to log on
end;

procedure TvsFTPHandler.ChangeWorkDir(Value: String);
var v,w: String;
    p,q: Integer;
    outdirred: Boolean;
begin
  if Value='' then
    Send (501)
  else
    begin
      outdirred := false;
      if Value[length(Value)]<>'/' then
        Value := Value + '/';
      if Value[1]='/' then
        begin
          v := Value
        end
      else

        v := FVirtualDir + '/'+Value;
        while pos ('//', v)>0 do
          v := StringReplace (v, '//', '/', [rfReplaceAll]);
        //strip '/./' directories:
        v := StringReplace (v, '/./', '/', [rfReplaceAll]);

        //strip '/../' directories:
        while (pos ('/../', v)) > 0 do
          begin
            p := pos ('/../', v);
            q := p - 1;
            while (q>1) and (v[q] <> '/') do
              dec (q);

            if (q>=1) and (v[q] = '/') then
              begin
                Delete (v, q, p - q +3);
              end
            else
              begin
                v := '/'; //invalid request
                outdirred := true;
              end;
          end;
{
            if pos ('/../', v)>0 then
              begin //quick and dirty fix. todo later (nicely parse)
                Send (501);
                exit;
              end;
}
      w := StringReplace (v, '/', PathSeparator, [rfReplaceAll]);
      while (w<>'') and (w[1]=PathSeparator) do
        Delete (w,1,1);

      w := FHomeDir + w;  //homedir always includes (back)slash

      if DirectoryExists (w) then
        begin
          if (outdirred) then
            //don't confuse buggy clients by letting them think they updirred out of the root.
            Send (450)
          else
            //ok, file action taken
            Send (250);
          FVirtualDir := v;
          FPhysicalDir := w;
        end
      else
        Send (450); //sorry
    end;
end;

procedure TvsFTPHandler.ListFiles (Value: String=''; UseControlConnection: Boolean=False);
var sr: TSearchRec;
    r, n, u: String;
    sl: TStrings;
    f: Integer;
    s: String;
    dt: Double;
    yn : String;
    dir, fdir: String;
    y,m,d: Word;
    month: String;
begin
  if UseControlConnection and (Value = '') then
    begin
      Send (1501, 'STAT needs arguments in this stage');
      exit;
    end;
  r := '';
  yn := FormatDateTime ('yyyy', now);
  dir := GetFullPhysicalDir (Value);
  fdir := dir;
  u := copy (FUser, 1, 8); //truncate if necessary
  while length (u)<8 do
    u := u + ' ';
  if dir='' then
    begin  //should never happen
      Send(500); //some internal error
      exit;
    end
  else
    begin
      if (pos ('*', dir)=0) then //yes, wildcards allowed
        begin
          if dir[length(dir)]<>'/' then
            dir := dir + '/';
          dir := dir + {$IFDEF UNIX}'*'{$ELSE}'*.*'{$ENDIF};
        end;
    end;
  sl := TStringList.Create;

  f := FindFirst (dir, faAnyFile - faHidden - faSysFile - faVolumeID, sr);
//  f := FindFirst (dir, faAnyFile{faReadOnly or faDirectory}, sr);
  while f=0 do
    begin
      //don't list updir from homedir:
      if (fdir = FHomeDir) and (sr.Name='..') then
        begin
          f := FindNext (sr);
          continue;
        end;

      //unix-style listing (fake on windows)
      // drwxrwxrwx   1 JOHN     JOHNGROUP   1234 Jul 01 01:01 somefile.ext
      // -r--r--r--   1 JOHN     JOHNGROUP  12345 Jul 02 2002  someotherfile.txt
      n := '-r--r--r--';
      if ((sr.Attr and faDirectory)<>0) then
        begin
          n[1] := 'd';
          n[4] := 'x';
          n[7] := 'x';
          n[10] := 'x';
        end;
      if ((sr.Attr and faReadOnly)=0) then
        begin
          n[3] := 'w';
          n[6] := 'w';
        end;
      n := n + '   1 ' + u + ' ' + u + ' ';
      s := IntToStr (sr.Size);
      while length (s) < 8 do
        s:= ' '+s;
      n := n + s;
      dt := FileDateToDateTime (sr.Time);
      DecodeDate (dt, y,m,d);
      if (m<1) or (m>12) then
        month := 'Err'
      else
        month := Months[m];

      if FormatDateTime ('yyyy', dt) <> yn then
        s := Month + FormatDateTime (' dd yyyy ', dt)
      else
        s := Month + FormatDateTime (' dd hh:nn', dt);
      n := n + ' ' + s + ' ' + sr.Name;
      sl.Add (n);
      f := FindNext (sr);
    end;
  FindClose (sr);
  if UseControlConnection then
    begin
      SendMultiLine (211, sl);
    end
  else
    begin
      r := sl.Text;
      Send (150); //about to open data connection
      if OpenDataConnection then
        begin
          SendDataString (r);
        end
      else
        Send (425); //can't open data connection
    end;
  sl.Free;
end;

procedure TvsFTPHandler.NameListFiles(Value: String);
var sr: TSearchRec;
    r: String;
    sl: TStrings;
    f: Integer;
    dir: String;
begin
  dir := GetFullPhysicalDir (Value);
  if dir='' then
    begin
      Send(500); //some internal error
      exit;
    end
  else
    begin
      if (pos ('*', dir)=0) then //yes, wildcards allowed
        begin
          if dir[length(dir)]<>'/' then
            dir := dir + '/';
          dir := dir + {$IFDEF UNIX}'*'{$ELSE}'*.*'{$ENDIF};
        end;
    end;
  sl := TStringList.Create;
  f := FindFirst (dir, faAnyFile - faHidden - faSysFile - faVolumeID, sr);
//  f := FindFirst (dir, faAnyFile{faReadOnly or faDirectory}, sr);
  while f=0 do
    begin
      sl.Add (sr.Name);
      f := FindNext (sr);
    end;
  FindClose (sr);
  r := sl.Text;
  sl.Free;
  Send (150); //about to open data connection
  if OpenDataConnection then
    begin
      SendDataString (r);
    end
  else
    Send (425); //can't open data connection
end;

function TvsFTPHandler.OpenDataConnection: Boolean;
begin
  Result := False;
  FDataSock := nil;
  //see if we can enter pasv or active mode
  if Assigned (FPasv) then
    begin
      FPasv.Listen;
      if FPasv.CanRead (20000) then
        begin
//          FPasv.Socket := FPasv.Accept;
//          FDataSock := FPasv;
//          FPasv := nil;

          FDataSock := TTCPBlockSocket.Create;
          FDataSock.Socket := FPasv.Accept;
          FPasv.CloseSocket;
          FreeAndNil (FPasv);
        end
      else
        FreeAndNil (FPasv); //use it only once (!)
    end
  else
    begin
      FDataSock := TTCPBlockSocket.Create;
      FDataSock.Connect (FRemoteDataIP, FRemoteDataPort);
      if FDataSock.LastError <> 0 then
        FreeAndNil (FDataSock);
    end;
  if Assigned (FDataSock) and FSettings.FDoSSL then
    begin
      FDataSock.SSL.CertCAFile := FSettings.FSSLCertCAFile;
      FDataSock.SSL.PrivateKeyFile := FSettings.FSSLPrivateKeyFile;
      FDataSock.SSL.CertificateFile := FSettings.FSSLCertificateFile;
      try
        if not FDataSock.SSLAcceptConnection then
          FreeAndNil (FDataSock);
      except
        FreeAndNil (FDataSock);
      end;
    end;

  if Assigned (FDataSock) then
    begin
      //launch a data thread
      // (todo: hold an array of transfers (?)
      FData := TvsFTPData.Create (true);
      FData.FSettings := FSettings;
      FData.FDataSock := FDataSock;
      FMode := cmTransferBusy;
      FDataSock := nil;
      Result := True;
      //Assume proper further handling of this freshly created thread
    end;
end;

procedure TvsFTPHandler.SendDataString(Value: String);
begin
  FData.FDataBuffer := Value;
  FData.FDataMethod := dmBufferSend;
  FData.Resume;
end;

procedure TvsFTPHandler.SendDataStream(Stream: TStream);
begin
  if FTransferType = 'A' then
    FData.FDataMethod := dmAsciiStreamSend
  else
    FData.FDataMethod := dmStreamSend;
  FData.FDataStream := Stream;
  FData.FCloseStream := True;
  FData.Resume;
end;

procedure TvsFTPHandler.RecvDataStream(Stream: TStream);
begin
  if FTransferType = 'A' then
    FData.FDataMethod := dmAsciiStreamRecv
  else
    FData.FDataMethod := dmStreamRecv;
  FData.FDataStream := Stream;
  FData.FCloseStream := True;
  FData.Resume;
end;


procedure TvsFTPHandler.DelFile(value: String);
var fn: String;
begin
  fn := GetFullPhysicalDir (value);
  if FReadOnly then
    Send (550)
  else
    if FileExists(fn) then
      begin //delete file
        Log ('DELETEFILE "'+value+'" ['+fn+']');
        if DeleteFile (fn) then
          Send (250)
        else
          Send (450);
      end
    else if DirectoryExists (fn) then
      begin
        Log ('REMOVEDIR "'+value+'" ['+fn+']');
        if RemoveDir (fn) then
          Send (250)
        else
          Send (450);
      end
    else
      Send (450);
end;

procedure TvsFTPHandler.AppendFile(Value: String);
//append file to server
begin
  PutFile (Value, True);
end;

procedure TvsFTPHandler.PutFile(Value: String; Append: Boolean=False);
//store file on server
var fs: TFileStream;
    orig: String;
begin
  if FReadOnly then
    begin
      Send (550); //sorry
      exit;
    end;
  orig := Value;
  Value := GetFullPhysicalDir(Value);
  if (Value<>'') then
    begin
      if Append then
        Log ('APPE "'+Orig+'" ['+Value+']')
      else
        Log ('STOR "'+Orig+'" ['+Value+']');
      try
        if FileExists (Value) then //beware not to overwrite existing file
                                   //if transfer cannot be initiated.
          fs := TFileStream.Create (Value, fmOpenWrite)
        else
          fs := TFileStream.Create (Value, fmCreate); //create or overwrite
      except
        try
          //fs.Free;
        except end;
        fs := nil;
      end;
      if Assigned (fs) then
        begin
          if Append then
            fs.Position := fs.Size;
          Send (150);
          if OpenDataConnection then
            begin
              RecvDataStream (fs);
            end
          else
            Send (425); //no data connection

        end
      else
        Send (550); //access denied
    end;
end;

procedure TvsFTPHandler.SendFile(Value: String);
var fs: TFileStream;
    orig: String;
begin
  Orig := Value;
  Value := GetFullPhysicalDir (Value);
  if (Value<>'') and FileExists (Value) then
    begin
      Log ('RETR "'+Orig+'" ['+Value+']');
      try
        fs := TFileStream.Create (Value, fmOpenRead or fmShareDenyNone);
      except
        try
          fs.Free;
        except end;
        fs := nil;
      end;
      if Assigned (fs) then
        begin
          //go upload
          Send (150);
          if OpenDataConnection then
            begin
              SendDataStream (fs);
            end
          else
            begin
              Send (425); //unable to open data connection
              fs.Free;
            end;
        end
      else
        Send (450); //access denied opening filestream
    end
  else
    Send (550); //not found
end;

procedure TvsFTPHandler.AbortFile;
begin
  //if data connection open, signal to close
  //in any case, send back 226
  //(even if data connection never existed, just ignore that fact)
  if Assigned (FData) and (not FData.Terminated) then
    begin
      FData.Terminate;
      Send (446); //data connection not normally ended.
    end;
  Send (226); //ok, data connection closed
end;

procedure TvsFTPHandler.RenameFrom(Value: String);
var v: String;
begin
  if FRenameFrom <> '' then
    begin
      Send (503); //bad command sequence
    end
  else
    begin
      if FReadOnly then
        Send (550) //sorry, no access
      else
        begin
          v := GetFullPhysicalDir (Value);
          if FileExists (v) or DirectoryExists(v) then
            begin
              Send (350); //ok, ready for destination
              FRenameFrom := Value;
            end
          else
            Send (550);
        end;
    end;
end;

procedure TvsFTPHandler.RenameTo(Value: String);
var v,w: String;
begin
  if FRenameFrom = '' then
    Send (503)
  else
    begin
      if FReadOnly then
        Send (550) //this should never happen here!
      else
        begin
          v := GetFullPhysicalDir (FRenameFrom);
          w := GetFullPhysicalDir (Value, False);
          //file system may
          if FileExists (v) then
            begin
              if not FileExists (w) and
                 RenameFile (v, w) then
                begin
                  Log ('RENAMEFILE "'+v+'" to "'+w+'"');
                  Send (250);
                end
              else
                Send (450);
            end
          else if DirectoryExists (v) then
            begin
              if not DirectoryExists (w) and
                 RenameFile (v,w) then //some os may support to do a move.
                begin
                  Log ('RENAMEDIR "'+v+'" to "'+w+'"');
                  Send (250)
                end
              else
                Send (450);
            end
          else
            Send (550);
        end;
    end;
  FRenameFrom := '';
end;


procedure TvsFTPHandler.DoAutoTLS(Value: String);
//rfc 2228
// loads of things to do here, so not implemented yet.
begin
  Value := Trim(Lowercase(Value));
  if (Value='tls') or (Value='ssl') then
    begin
      //try start SSL connection
      FModeTLS := True; //data socks need to know this
      //defaults to PROT P (?)

    end
  else
    Send (504); //
end;

procedure TvsFTPHandler.SendHelp(Value: String);
begin

end;

procedure TvsFTPHandler.SiteInfo;
begin

end;

procedure TvsFTPHandler.StatDataConnection;
var v: String;
begin
try
  if Assigned (FData) then
    begin
      if Assigned (FData.FDataStream) then
        begin
 //         p := FData.FStream.Position;
//          s := FData.FStream.Size;
        end;
  //    if FData.FDataMethod in [dmSendStream, dmSendAsciiStream] then
        begin
        //  v :=

        end;
    end;
except
  Send (500);
end;
end;

procedure TvsFTPHandler.SystInfo;
begin
  //this implementation is not correct
  //however, rfc 1700 doesn't list windows.
  //somebody point me to a Assigned Numbers document
  //containing windows ?

  {$IFDEF WINDOWS}
  Send (215, 'Windows');
  {$ELSE}
  Send (215, 'UNIX');
  {$ENDIF}
end;

procedure TvsFTPHandler.GetFileSize(value: String);
var fn: String;
    sr: TSearchRec;
begin
  fn := GetFullPhysicalDir (value);
  if FileExists (fn) then
    begin
      FindFirst (fn, faAnyFile, sr);
      Send (3200, IntToStr(sr.Size));
      FindClose (sr);
    end
  else
    Send (450);
end;

{ TvsFTPData }

procedure TvsFTPData.Execute;
var Buf: String;
    l: Integer;
begin
  //Send data nicely to client:
  //todo: handle abort method
  case FDataMethod of
    dmBufferSend:
      begin
        //implementing abort not very meaningfull, but in fact todo..
        FDataSock.SendString (FDataBuffer);
      end;
    dmStreamSend, dmAsciiStreamSend: 
      begin
//        FDataSock.SendStreamRaw (FDataStream);
        //we send the stream ourselves
        //so that we are capable to respond to abort operations
        //FDataSock.MaxBandwidth := 25000; //25kb / client
        repeat
          SetLength (Buf, 1490);  //convenient ethernet packet size..
          l := FDataStream.Read (Buf[1], Length(Buf));
          SetLength (Buf, l);
          if FDataMethod = dmAsciiStreamSend then
            begin //no support for MAC (#13 only) files.
                  //what we do is:
                  //strip out all #13 (CR) tags
                  //StringReplace #10 by #13#10
                  //so that we have telnet (ftp) compliant text file.
              Buf := StringReplace (Buf, #13, '', [rfReplaceAll]);
              Buf := StringReplace (Buf, #10, #13#10, [rfReplaceAll]);
            end;
          FDataSock.SendString (Buf);
          if FDataSock.LastError<>0 then
            Terminate;
        until (Buf='') or Terminated;
      end;
    dmStreamRecv, dmAsciiStreamRecv:
      begin
        while (not Terminated) and FDataSock.CanRead(30000) do
          begin
            if FDataSock.WaitingData > 0 then
              begin
                SetLength (Buf, FDataSock.WaitingData);
                l := FDataSock.RecvBuffer (@Buf[1], length (Buf));
                SetLength (Buf, l);
                {$IF DEFINED(LINUX) or DEFINED(FREEBSD)}
                if FDataMethod = dmAsciiStreamRecv then
                  begin  //strip out all CR:
                    Buf := StringReplace (Buf, #13, '', [rfReplaceAll]);
                  end;
                {$ENDIF}
                if Buf<>'' then
                  FDataStream.Write (Buf[1], length(Buf));
              end
            else
              Terminate; //connection closed
            if FDataSock.LastError <> 0 then  //extra check (unnecessary?)
              Terminate;
            sleep (20);
          end;
        //truncate stream if overwritten:  
        FDataStream.Size := FDataStream.Position;
      end;
  end;
  if FCloseStream then
    try
      FDataStream.Free;
    except
      //oops
      LogError ('Failed to close filestream');
    end;
  FDataSock.Free;
  Terminate; //set terminated property so handler thread can verify
end;

end.
