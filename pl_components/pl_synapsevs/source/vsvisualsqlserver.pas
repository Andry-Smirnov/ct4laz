
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsvisualsqlserver;

interface


uses Windows, Classes, SysUtils, vsvisualserverbase, passql, passqlite, pasodbc, pasjansql;

const vssqlversion = 'Visual SQL Server 0.1';

type
  TvsSQLServer = class (TVisualServer) //basic TCP server
    constructor Create (AOwner: TComponent); override;
  end;

  TvsSQLHandler = class (TServerHandler)
    FTimeOut: Integer;
    procedure Handler; override;
  end;

implementation

constructor TvsSQLServer.Create(AOwner: TComponent);
begin
  inherited;
  FClientType := TvsSQLHandler;
  FSettings.FListenPort := '3339';
end;

procedure TvsSQLHandler.Handler;
var Buf:String;
    User, Pass: String;
    FLoggedOn: Boolean;
    db: TSQLDB;
    dbtype: String;
    dbr: Boolean;
    database: String;
    input,s: String;
    sl: TStringList;
    i: Integer;
    sp1,sp2: String;
    issp: Boolean;

begin
  //basic server example
  FTimeOut := 5 * 60000;
  Log (FSock.GetRemoteSinIP+':'+IntToStr(FSock.GetRemoteSinPort)+' connected');
  if not Terminated and (FSock.LastError = 0) then
    begin
      //write some message:
      FSock.SendString (vssqlversion+#13#10);
      FSock.SendString ('Welcome'#13#10);

      if true {FMustLogon} then
        begin
          FSock.SendString ('Username: ');
          User := FSock.RecvString(30000);
          if FSock.LastError = 0 then
            begin
              FSock.SendString ('Password: ');
              Pass := FSock.RecvString (30000);

              if FSock.LastError = 0 then
                begin
                  //FLoggedOn := Authenticate (User, Pass);
//                  FLoggedOn := True;
                  FLoggedOn :=
                    FSettings.FAuthentication.Authenticate (User, Pass);

                  if not FLoggedOn then
                    begin
                      FSock.SendString ('Error: '+IntToStr(GetLastError)+#13#10);

                    end;
                  if not FLoggedOn then
                    FSock.SendString ('Sorry, username doesn''t match password'#13#10);
                end;
            end;
        end
      else
        FLoggedOn := True;

      database := '';
      dbtype := 'sqlite';

      sl := TStringList.Create;
      db := nil;

      if FLoggedOn then
        begin
          FSock.SendString(Format ('Inactivity time: %d seconds'#13#10,[FTimeOut div 1000]));
          FSock.SendString(Format ('Default database format "%s". Change with "type <dbtype>"'#13#10,[dbtype]));
          FSock.SendString(Format ('Enter "?" or "help" for available commands.'#13#10,[]));

          while not Terminated do
            begin
              FSock.SendString(Format(#13#10'%s@%s[%s] # ',[User, FSock.GetLocalSinIP, database]));
              input := '';
              s := FSock.RecvString(120000);
              while (s<>'') and (s[length(s)]='\') and (FSock.LastError=0) do
                begin
                  input := input + copy (s,1,length(s)-1);
                  s := FSock.RecvString(120000);
                end;
              input := trim(input + s);
              if (input = '') then
                input := '?'; //help

              if (FSock.LastError <> 0) then
                begin
                  FSock.SendString(Format(#13#10'Connection timed out after %d seconds. Bye.',[FTimeOut div 1000]));
                  Terminate;
                  break;
                end;

              //split input line, fetch commands
              sl.Text := lowercase(StringReplace (copy (input,1,1024), ' ', #13#10, [rfreplaceall]));
              sp1 := '';
              sp2 := '';
              if sl.Count > 0 then
                sp1 := sl[0];
              if sl.Count > 1 then
                sp2 := sl[1];
              issp := false;

              //look for special commands
              if (sp1='?') or (sp1='help') then
                begin
                  s := 'Commands: ? | help | use <database> | type <sqlite|odbc|jansql> | close | quit'#13#10;
                  FSock.SendString (s);
                  issp := true;
                end;
              if (sp1='quit') then
                begin
                  issp := true;
                  Terminate;
                  break;
                end;
              if (sp1='type') then
                begin
                  if (sp2='') then
                    FSock.SendString (Format('Current database type: %s', [dbtype]))
                  else
                    begin
                      if (sp2='sqlite') or (sp2='odbc') or (sp2='jansql') then
                        begin
                          dbtype := sp2;
                          FSock.SendString (Format('Current database type: %s', [dbtype]));
                        end
                      else
                        FSock.SendString (Format('%s is not a supported database type', [sp2]));
                    end;
                  issp := true;
                end;
              if (sp1='close') or ((sp1='use') and (sp2<>'')) then
                begin
                  if Assigned (db) then
                    begin
                      FSock.SendString (Format('Closing %s.%s', [db.classname, database]));
                      FreeAndNil (db);
                      database := '';
                    end
                  else
                    if (sp1='close') then
                      FSock.SendString (Format('No active database', []));
                  issp := true;
                end;
              if (sp1='use') then
                begin
                  issp := true;
                  if (sp2='') then
                    FSock.SendString (Format('No database selected', []))
                  else
                    begin
                      database := '';
                      for i := 1 to length(sp2) do
                        if not (sp2[i] in ['0'..'9', 'a'..'z', 'A'..'Z', '_', '-']) then
                          begin
                            FSock.SendString (Format('Illegal database name "%s". Invalid character "%s" at position %d', [sp2, sp2[i], i]));
                            sp2 := '';
                            break;
                          end;
                      if (sp2<>'') then
                        begin
                          if Assigned(db) then
                            FreeAndNil(db);
                          if dbtype='sqlite' then
                            db := TLiteDB.Create(nil);
                          if dbtype='odbc' then
                            db := TODBCDB.Create(nil);
                          if dbtype='jansql' then
                            db := TJanDB.Create(nil);

                          //try to use this database
                          if Assigned (db) then
                            begin
                              //db.Use (sp2);
                              dbr := db.Connect(sp2,'','');

                              if not (dbr) then
                                begin
                                  if not (db.DllLoaded) then
                                    begin
                                      FSock.SendString (Format('Sorry, this database type is not supported (library failed to load)'#13#10, []));
                                      FreeAndNil(db);
                                    end
                                  else
                                  //if (db.LastError<>0) then
                                    begin
                                      FSock.SendString (Format('Failed to open database %s:%s with error ''%s'''#13#10, [dbtype, sp2, db.ErrorMessage]));
                                    end;
                                  FreeAndNil(db);
                                end;

                            end;

                          if assigned(db) then
                            begin
                              FSock.SendString (Format('Connected to %s:%s@%s'#13#10, [dbtype, sp2, FSock.GetLocalSinIP]));
                              database := sp2;
                            end
                          else
                            FSock.SendString (Format('Failed to open database %s:%s'#13#10, [dbtype, sp2]));

                        end;
                    end;



                end;

              //if not special command, execute query as-is
              if not (issp) and not (assigned(db)) then
                FSock.SendString('No database selected'#13#10);
              if not issp and assigned (db) then
                begin
                  db.Query(input);
                  if db.LastError <> 0 then
                    begin
                      FSock.SendString(Format ('Error %d'#13#10, [db.LastError]));
                    end
                  else
                    begin
                      FSock.SendString(Format ('RowCount=%d'#9'RowsAffected=%d'#9'LastInsertId=%d'#13#10, [db.RowCount, db.RowsAffected, db.LastInsertID]));
                      FSock.SendString(db.Results[0].FieldsAsTabSep+#13#10);

                      for i := 0 to db.RowCount - 1 do
                        begin
                          FSock.SendString(db.Results[i].GetAsTabSep+#13#10);
                        end;
                    end;

                end;

            end;
        end;
      sl.Free;
    end;
end;



end.
