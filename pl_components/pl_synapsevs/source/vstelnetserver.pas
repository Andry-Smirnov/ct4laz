
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsTelnetServer;

interface

uses

  Windows,
  Classes, SysUtils, vsVisualServerBase;

const
  serverversion = 'Visual Telnet Server 0.1';

type
  TvsTelnetServer = class(TVisualServer) //basic TCP server
    constructor Create(AOwner: TComponent); override;
  end;

  TvsTelnetHandler = class(TServerHandler)
    procedure EchoLastError;
    procedure Handler; override;
  end;

implementation

constructor TvsTelnetServer.Create(AOwner: TComponent);
begin
  inherited;
  FClientType := TvsTelnetHandler;
  FSettings.FListenPort := '23';
end;

procedure TvsTelnetHandler.EchoLastError;
var
  Buf: string;
begin
  SetLength(Buf, 1024);
  SetLength(Buf,
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    GetLastError, 0, @Buf[1],
    length(Buf), nil)
    );

  FSock.SendString(Buf);
  FSock.SendString(#13#10);
end;

procedure TvsTelnetHandler.Handler;
var
  Buf: string;
  User, Pass: string;
  token: THandle;
  FLoggedOn: boolean;
  pid: longword;
  pidexc: DWord;
  c: string;
  si: STARTUPINFOA;
  sa: TSECURITYATTRIBUTES; //security information for pipes
  sd: TSECURITYDESCRIPTOR;
  pi: PROCESS_INFORMATION;
  newstdin, newstdout, read_stdout, write_stdin: THandle;
  iBufSize, bytesread, byteswritten, avail: cardinal;
  commandline: string;

begin
  //basic server example
  Log(FSock.GetRemoteSinIP + ':' + IntToStr(FSock.GetRemoteSinPort) + ' connected');
  if not Terminated and (FSock.LastError = 0) then
  begin
    //write some message:
    FSock.SendString(serverversion + #13#10);
    FSock.SendString('Welcome'#13#10);

    if True {FMustLogon} then
    begin
      FSock.SendString('Username: ');
      User := FSock.RecvString(30000);
      if FSock.LastError = 0 then
      begin
        FSock.SendString('Password: ');
        Pass := FSock.RecvString(30000);

        if FSock.LastError = 0 then
        begin
          //FLoggedOn := Authenticate (User, Pass);
          //                  FLoggedOn := True;
          FLoggedOn :=
            FSettings.FAuthentication.Authenticate(User, Pass);
{                    LogonUser ( PChar(User),
                                nil, //PChar('.'), //nil, //domain
                                PChar(Pass),
                                LOGON32_LOGON_INTERACTIVE,  //NETWORK, //BATCH, //
                                LOGON32_PROVIDER_DEFAULT,
                                Token
                              );
}
          if not FLoggedOn then
          begin
            FSock.SendString('Error: ' + IntToStr(GetLastError) + #13#10);

          end;
          if not FLoggedOn then
            FSock.SendString('Sorry, username doesn''t match password'#13#10);
        end;
      end;
    end
    else
      FLoggedOn := True;


    if FLoggedOn then
    begin
      if (Win32Platform = VER_PLATFORM_WIN32_NT) then
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
        //                break;
      end;

      if not (CreatePipe(read_stdout, newstdout, @sa, 0)) then //create stdout pipe
      begin
        CloseHandle(newstdin);
        CloseHandle(write_stdin);
        //                Exit;
      end;
      //              GetStartupInfo(si);
      GetStartupInfo(si); //set startupinfo for the spawned process
             {The dwFlags member tells CreateProcess how to make the process.
             STARTF_USESTDHANDLES validates the hStd* members. STARTF_USESHOWWINDOW
             validates the wShowWindow member.}

      si.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      si.wShowWindow := SW_HIDE; //SW_SHOWNORMAL; //
      si.hStdOutput := newstdout;
      si.hStdError := newstdout; //set the new handles for the child process
      si.hStdInput := newstdin;


      //          CommandLine := 'c:\winnt\system32\cmd.exe';
      CommandLine := 'c:\winnt\system32\cmd.exe';
      //Launch process as this user:
      FSock.SendString('Launching command line'#13#10);
      //sorry, can't get createprocessasuser to work correctly.
      //may be due to lack of environment?
      //also, authentication needs option to return token
      CreateProcess{AsUser}( //Token,
        PChar(CommandLine),
        nil, //PChar ('/c c:\cygwin\bin\bash.exe'),
        //                                nil, //PChar(CommandLine),
        @sa,
        nil,
        True,
        CREATE_NEW_CONSOLE,
        nil,
        nil, //PChar ('c:\'), //'home' directory
        si,
        pi
        );
      EchoLastError;
      //  FSock.SendString ('Error: '+IntToStr(GetLastError)+#13#10);


      while not Terminated do
      begin
        GetExitCodeProcess(pi.hProcess, pidexc);
        if pidexc <> STILL_ACTIVE then
          break;

        //see if there is input from exe:
        iBufSize := 1024;
        SetLength(Buf, iBufSize);
        PeekNamedPipe(read_stdout, @Buf[1], iBufSize, @bytesread, @avail, nil);
        SetLength(Buf, bytesread);
        if Buf <> '' then
        begin
          ReadFile(read_stdout, Buf[1], iBufSize, bytesread, nil); //read the stdout pipe
          SetLength(Buf, bytesread);
          FSock.SendString(Buf);
          if FSock.LastError <> 0 then
            break;
        end;

        //see if there is input from user:
        if FSock.CanRead(30) then
        begin
          if FSock.WaitingData >= 0 then
          begin
            c := FSock.RecvPacket(30);
            if c <> '' then
            begin
              if FSock.LastError <> 0 then
                break;
              WriteFile(write_stdin, c[1], length(c), byteswritten, nil);
            end;
          end
          else
            break;
        end;

        //finally, sleep some
        sleep(50);
      end;
      //time to terminate the process if still alive
      GetExitCodeProcess(pi.hProcess, pidexc);
      if pidexc = STILL_ACTIVE then
        TerminateProcess(pi.hProcess, 0);
      if FSock.LastError = 0 then
        FSock.SendString('Bye'#13#10);
      CloseHandle(read_stdout);
      CloseHandle(write_stdin);
    end;
  end;
end;



end.
