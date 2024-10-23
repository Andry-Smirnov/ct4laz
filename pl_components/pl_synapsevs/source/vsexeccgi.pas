
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsExecCGI;

interface

uses
 {$IFDEF WINDOWS}
   Windows,
 {$ELSE}
   Types,
 {$ENDIF}
  Classes, SysUtils, BlckSock, vsTypeDef, vsVisualServerBase;

//Taken a good look at DosCommand unit by Maxime Collomb

type
  TCGIResult = record
    hStdOut: integer; //handle to console stream
    hstdoutw: integer;
    pid: integer;     //process ID to watch
    ResultCode: integer;
    Header: string;   //Headers as sent by CGI
    HasResult: boolean;
  end;

  TCGIMode = (cmCGI, cmPP);


function ExecuteCGI(CGI: TFileName; Params, FileName, Method, Query, Header, PostData: string; IPInfo: TIPInfo;
  Request: TRequest; Settings: TSettings; Mode: TCGIMode; TimeOut: integer = 30): TCGIResult;

procedure CGISendResultsToSock(CGIResult: TCGIResult; Sock: TTCPBlockSocket);

//function MakeEnvironment (Header, PostData: String; IPInfo: TIPInfo; Script: TFileName; Settings: TSettings): String;

implementation


function ExecuteCGI(CGI: TFileName; Params, FileName, Method, Query, Header, PostData: string; IPInfo: TIPInfo;
  Request: TRequest; Settings: TSettings; Mode: TCGIMode; TimeOut: integer = 30): TCGIResult;

  function GetEnv(Name: string): string;
  begin
   {$IF DEFINED(LINUX) or DEFINED(FREEBSD)}
    Result := GetEnvironmentVariable(Name);
    {$ELSE}
    {$IFDEF FPC}
    Result := GetEnvironmentVariable(Name);
    {$ELSE}
    SetLength(Result, 1024);
    SetLength(Result,
      GetEnvironmentVariable(PChar(Name), PChar(Result), length(Result))
      );
    {$ENDIF}
    {$ENDIF}
  end;

  function MakeEnvironment(Header, PostData: string; IPInfo: TIPInfo; Script: TFileName; Settings: TSettings): string;
  var
    Env: TStrings;
    i: integer;
  begin
    //todo: fix this for more server vars.
    Env := TStringList.Create;
    with Env do
    begin
      //this may be dangerous (just copying..):
      //also, i provides redundant information
      //but, there may be important proxy information or other headers.
      for i := 1 to Request.Header.Count - 1 do
        Add(UpperCase(Request.Header.Names[i]) + '=' + Request.Header.Values[Request.Header.Names[i]]);

      //LD_LIBRARY_PATH= should be right
      Add('SERVER_SOFTWARE=Visual Synapse HTTP');
      Add('SERVER_NAME=' + Settings.FServerName);
      Add('GATEWAY_INTERFACE=CGI/1.1');
      Add('SERVER_PROTOCOL=HTTP/1.1');
      Add('SERVER_PORT=' + Settings.FListenPort);
      //        Add ('SERVER_IP='+Settings.FListenIP);
      Add('REQUEST_METHOD=' + Method);
      Add('SCRIPT_NAME=' + FileName);
      Add('SCRIPT_FILENAME=' + FileName);
      Add('PATH_INFO=');
      Add('PATH_TRANSLATED=' + FileName);
      Add('QUERY_STRING=' + Query);
      Add('REMOTE_HOST=' + IPInfo.RemoteIP);
      Add('REMOTE_ADDR=' + IPInfo.RemoteIP);
      Add('HTTP_REFERER=' + Request.Header.Values['Referer']);
      Add('HTTP_USER_AGENT=' + Request.Header.Values['User-Agent']);
      Add('QUERY_STRING'); //specifies GET parameters
      Add('CONTENT_TYPE=' + Request.Header.Values['Content-Type']);
      Add('CONTENT_LENGTH=' + IntToStr(Length(PostData))); //specifies size of POST data
      Add('HTTP_ACCEPT=' + Request.Header.Values['Accept']);
      Add('HTTP_HOST=' + Request.Header.Values['Host']);
      //wonder how to parse multiple cookies
      Add('HTTP_COOKIE=' + Request.Header.Values['Cookie']);
      if Mode = cmPP then
        Add('PHP_SELF=' + Request.Parameter);  //shouldn't harm CGI...

      // ?? :
      Add('HTTP_ACCEPT_LANGUAGE=' + Request.Header.Values['Accept-Language']);
      Add('HTTP_ACCEPT_ENCODING=' + Request.Header.Values['Accept-Encoding']);

      //system path
      Add('PATH=' + GetEnv('PATH'));
      Add('TEMP=' + GetEnv('TEMP'));
      Add('TMP=' + GetEnv('TEMP'));
      Add('windir=' + GetEnv('windir'));

      //critical for winsock support (PHP etc):
      Add('SystemRoot=' + GetEnv('SystemRoot'));
    end;
    Result := StringReplace(Env.Text, #13#10, #0, [rfReplaceAll]) + #0;
    Env.Free;
  end;


  //warning: CGI not tested yet.
const
  MaxBufSize = 4096;

var
  Buf: string;

  {$IFDEF WINDOWS}
  si: STARTUPINFOA;
  sa: TSECURITYATTRIBUTES; //security information for pipes
  sd: TSECURITYDESCRIPTOR;
  pi: PROCESS_INFORMATION;
  {$ENDIF}

  newstdin, newstdout, read_stdout, write_stdin: THandle; //pipe handles
  Exit_Code: longword; //process exit code
  bytesread: longword; //bytes read
  avail: longword; //bytes available

  Env: string;
  P: string;
  iBufSize: cardinal;
  app_spawn: PChar;

  lpostdata: integer;
  lBuf: integer;
  headsep: integer;

label
  final;

begin
  Result.HasResult := False;

  if Method = '' then
    Method := 'GET';

  Env := MakeEnvironment(Header, PostData, IPInfo, FileName, Settings);

{$IFDEF WINDOWS}

{$IFDEF WINCE}
    if (WincePlatform = VER_PLATFORM_WIN32_NT) then
{$ELSE}
    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
{$ENDIF}
    begin //initialize security descriptor (Windows NT)
      InitializeSecurityDescriptor(@sd, SECURITY_DESCRIPTOR_REVISION);
      SetSecurityDescriptorDacl(@sd, True, nil, False);
      sa.lpSecurityDescriptor := @sd;
    end
    else
    begin
      sa.lpSecurityDescriptor := nil;
    end;

  sa.nLength := sizeof(SECURITY_ATTRIBUTES);
  sa.bInheritHandle := True; //allow inheritable handles



  if not (CreatePipe(newstdin, write_stdin, @sa, 0)) then //create stdin pipe
  begin
    exit; //no cleanup here
  end;

  if not (CreatePipe(read_stdout, newstdout, @sa, 0)) then //create stdout pipe
  begin
    CloseHandle(newstdin);
    CloseHandle(write_stdin);
    Exit;
  end;

  GetStartupInfo(
{$IFDEF FPC}
    @
{$ENDIF}
    si); //set startupinfo for the spawned process
 {The dwFlags member tells CreateProcess how to make the process.
 STARTF_USESTDHANDLES validates the hStd* members. STARTF_USESHOWWINDOW
 validates the wShowWindow member.}

  si.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_HIDE;
  si.hStdOutput := newstdout;
  si.hStdError := newstdout; //set the new handles for the child process
  si.hStdInput := newstdin;

  app_spawn := PChar(CGI);


  if Mode = cmCGI then
  begin
    if pos('=', Query) > 0 then
      P := ''
    else
      P := '"' + StringReplace(Query, ' ', '+', [rfReplaceAll]) + '"';
  end
  else //PreParser
  begin
    try
      //duh, bug in format if there is more than one parameter:
      //        P := Format (Params, [FileName]);
      P := StringReplace(Params, '%s', Filename, [rfIgnoreCase]);
      P := StringReplace(P, '%s', '', [rfReplaceAll]);
    except
      P := FileName;
    end;
  end;

  //spawn the child process
  if not (CreateProcess(app_spawn, PChar(app_spawn + ' ' + P), nil, nil, True, CREATE_NEW_CONSOLE {or NORMAL_PRIORITY_CLASS FPriority},
    PChar(Env), PChar(ExtractFilePath(app_spawn)), si, pi)) then
  begin

    //    FCreateProcessError := TCreateProcessError.Create(string(app_spawn)
    //      + ' doesn''t exist.');
    //    raise FCreateProcessError;
    Result.ResultCode := 404; //probably cgi app not found or not executable
    goto final;
{    CloseHandle(newstdin);
    CloseHandle(newstdout);
    CloseHandle(read_stdout);
    CloseHandle(write_stdin);}
  end;

  sleep(0);

  iBufSize := MaxBufSize;

  lpostdata := Length(PostData);

  if Mode = cmCGI then //submit headers to the CGI script (?)
  begin
    Buf := Request.RawHeader.Text + #13#10;
    lBuf := length(Buf);
    if lBuf > 0 then //should be
    begin
      WriteFile(write_stdin, Buf[1], lBuf, bytesread, nil);
      if lBuf <> bytesread then
      begin
        Result.ResultCode := 500; //CGI Script misbehaved
        goto final;
      end;
    end;
  end;

  if lpostdata > 0 then
  begin
    WriteFile(write_stdin, PostData[1], lpostdata, bytesread, nil); //send it to stdin

    if bytesread <> lpostdata then //error..
    begin
      Result.ResultCode := 500;  //internal error
      goto final;
    end;
  end;



  SetLength(Buf, iBufSize);
  try
    repeat //main program loop
      GetExitCodeProcess(pi.hProcess, Exit_Code); //while the process is running
      PeekNamedPipe(read_stdout, @Buf[1], iBufSize, @bytesread, @avail, nil);
      //check to see if there is any data to read from stdout
      if (bytesread <> 0) then
      begin

        SetLength(Buf, bytesread);

        headsep := pos(#13#10#13#10, Buf);

        if headsep > 0 then //header found
        begin
          //actual read of the buffer:
          ReadFile(read_stdout, Buf[1]{pBuf^}, headsep + 3, bytesread, nil); //read the stdout pipe
          SetLength(Buf, bytesread);
          if bytesread <> (headsep + 3) then //oops, something went wrong
          begin
            Result.ResultCode := 501; //internal server error
            exit;
          end;
          Result.Header := Buf;
          Result.hStdOut := read_stdout;
          Result.pid := pi.hProcess;
          Result.HasResult := True;
          break;
        end;
      end;

      Sleep(20); // Give other processes a chance

      //        TerminateProcess(pi.hProcess, 0);

    until (Exit_Code <> STILL_ACTIVE); //process terminated (normally)

    SetLength(Buf, iBufSize);
    //see if there is data left in the buffer:
    PeekNamedPipe(read_stdout, @Buf[1], iBufSize, @bytesread, @avail, nil);
    //check to see if there is any data to read from stdout
    if (bytesread <> 0) then
    begin
      // there is data. read and let it sent it back to the client
      Result.hStdOut := read_stdout;
      Result.pid := pi.hProcess;
      Result.HasResult := True;
      Result.ResultCode := 200;
    end;

  finally
  end;

  final:

    CloseHandle(newstdin); //clean stuff up
  CloseHandle(write_stdin);

  if not Result.HasResult then
  begin
    CloseHandle(read_stdout);
    CloseHandle(newstdout);
    //      CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
  end
  else
  begin
    Result.pid := pi.hProcess;
    Result.hStdOut := read_stdout;
    Result.hstdoutw := newstdout;
    Result.ResultCode := 200;
  end;

  {$ENDIF}

end;

procedure CGISendResultsToSock(CGIResult: TCGIResult; Sock: TTCPBlockSocket);
var
  Exit_Code: longword;
  Buf: string;
  bytesread: longword;
  avail: longword;
begin
  //Read from stdout
  //write to Sock
  //until app finished
  {$IFDEF WINDOWS}
  repeat
    sleep(20);
    GetExitCodeProcess(CGIResult.pid, Exit_Code); //while the process is running
    repeat

      SetLength(Buf, 8192);
      PeekNamedPipe(CGIResult.hStdOut, @Buf[1], length(Buf), @bytesread, @avail, nil);
      if bytesread > 0 then
      begin
        ReadFile(CGIResult.hStdOut, Buf[1]{pBuf^}, length(Buf), bytesread, nil); //read the stdout pipe
        SetLength(Buf, bytesread);

        if (Buf <> '') then
          Sock.SendString(Buf);
      end
      else
        break;

    until (Buf = '') or (Sock.LastError <> 0);


    if Sock.LastError <> 0 then
    begin
      break;
    end;

  until Exit_Code <> STILL_ACTIVE;

  TerminateProcess(CGIResult.pid, 0);

  //close file handles:
  CloseHandle(CGIResult.hstdout);
  CloseHandle(CGIResult.hstdoutw);
  {$ENDIF}
end;

end.
(*
  ISAPI notes

  unit: isapiapp

  HttpExtensionProc :

  DWORD WINAPI HttpExtensionProc(
  LPEXTENSION_CONTROL_BLOCK lpECB
);


function GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL; stdcall;
function HttpExtensionProc(var ECB: TEXTENSION_CONTROL_BLOCK): DWORD; stdcall;
function TerminateExtension(dwFlags: DWORD): BOOL; stdcall;


*)(* CGI ENVIRONMENT VARIABLES:
  Taken from http://hoohoo.ncsa.uiuc.edu/cgi/env.html

Specification

The following environment variables are not request-specific and are set for all requests:

    * SERVER_SOFTWARE

      The name and version of the information server software answering the request (and running the gateway). Format: name/version

    * SERVER_NAME

      The server's hostname, DNS alias, or IP address as it would appear in self-referencing URLs.

    * GATEWAY_INTERFACE

      The revision of the CGI specification to which this server complies. Format: CGI/revision

The following environment variables are specific to the request being fulfilled by the gateway program:

    * SERVER_PROTOCOL

      The name and revision of the information protcol this request came in with. Format: protocol/revision

    * SERVER_PORT

      The port number to which the request was sent.

    * REQUEST_METHOD

      The method with which the request was made. For HTTP, this is "GET", "HEAD", "POST", etc.

    * PATH_INFO

      The extra path information, as given by the client. In other words, scripts can be accessed by their virtual pathname, followed by extra information at the end of this path. The extra information is sent as PATH_INFO. This information should be decoded by the server if it comes from a URL before it is passed to the CGI script.

    * PATH_TRANSLATED

      The server provides a translated version of PATH_INFO, which takes the path and does any virtual-to-physical mapping to it.

    * SCRIPT_NAME

      A virtual path to the script being executed, used for self-referencing URLs.

    * QUERY_STRING

      The information which follows the ? in the URL which referenced this script. This is the query information. It should not be decoded in any fashion. This variable should always be set when there is query information, regardless of command line decoding.

    * REMOTE_HOST

      The hostname making the request. If the server does not have this information, it should set REMOTE_ADDR and leave this unset.

    * REMOTE_ADDR

      The IP address of the remote host making the request.

    * AUTH_TYPE

      If the server supports user authentication, and the script is protects, this is the protocol-specific authentication method used to validate the user.

    * REMOTE_USER

      If the server supports user authentication, and the script is protected, this is the username they have authenticated as.

    * REMOTE_IDENT

      If the HTTP server supports RFC 931 identification, then this variable will be set to the remote user name retrieved from the server. Usage of this variable should be limited to logging only.

    * CONTENT_TYPE

      For queries which have attached information, such as HTTP POST and PUT, this is the content type of the data.

    * CONTENT_LENGTH

      The length of the said content as given by the client.

In addition to these, the header lines received from the client, if any, are placed into the environment with the prefix HTTP_ followed by the header name. Any - characters in the header name are changed to _ characters. The server may exclude any headers which it has already processed, such as Authorization, Content-type, and Content-length. If necessary, the server may choose to exclude any or all of these headers if including them would exceed any system environment limits.

An example of this is the HTTP_ACCEPT variable which was defined in CGI/1.0. Another example is the header User-Agent.

    * HTTP_ACCEPT

      The MIME types which the client will accept, as given by HTTP headers. Other protocols may need to get this information from elsewhere. Each item in this list should be separated by commas as per the HTTP spec.

      Format: type/subtype, type/subtype

    * HTTP_USER_AGENT

      The browser the client is using to send the request. General format: software/version library/version.
      *)
