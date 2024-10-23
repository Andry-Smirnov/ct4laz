
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsFileLogger;

 {$MODE DELPHI}

interface

uses
 {$IFDEF WINDOWS}
   Windows,
 {$ELSE}
   Types,
 {$ENDIF}
  Classes, SysUtils, SyncObjs;

//Object that can be easily linked to server
//provides thread-safe method to write log lines
//holds 'lazy' threads for writing to file
//provides an 'OnEvent' log that gets triggered

type
  TOnLoggerLines = procedure (Sender: TObject; Lines: TStrings) of Object;

  TLoggerThread = class;
  TLogger = class (TComponent)
  private
    FOnLoggerLines: TOnLoggerLines;
    procedure SetFileName(const Value: String);
  public
    FFileName: String;
    CS: TCriticalSection;
    FLines: TStrings; //timestamp added by sender. be as dump as possible.
    FLoggerThread: TLoggerThread;
    procedure Log (Line: String);
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FileName: String read FFileName write SetFileName;
    property OnLoggerLines: TOnLoggerLines read FOnLoggerLines write FOnLoggerLines;
  end;

  TLoggerThread = class (TThread)
  public
    FLogger: TLogger;
    FLines: TStrings;
    CS: TCriticalSection;
    procedure SyncLines;
    procedure Execute; override;
  end;


implementation

{ TLogger }

constructor TLogger.Create(AOwner: TComponent);
begin
  CS := TCriticalSection.Create;
  FLines := TStringList.Create;
  inherited;
end;

destructor TLogger.Destroy;
begin
  if Assigned (FLoggerThread) then
    begin
      FLoggerThread.Terminate;
      FLoggerThread.WaitFor;
      FLoggerThread.Free;
    end;
  FLines.Free;
  CS.Free;
  inherited;
end;

procedure TLogger.Log(Line: String);
begin
  CS.Enter;
  FLines.Add (DateTimeToStr(now)+' '+Line);
  CS.Leave;
end;

procedure TLogger.SetFileName(const Value: String);
begin
  FFileName := Value;
  if Assigned (FLoggerThread) then
    begin
      FLoggerThread.Terminate;
      FLoggerThread.WaitFor;
      FLoggerThread.Free;
    end;
  FLoggerThread := nil;
  if Value <> '' then
    begin
      FLoggerThread := TLoggerThread.Create (True);
      FLoggerThread.CS := CS;
      FLoggerThread.FLines := FLines;
      FLoggerThread.FLogger := Self;
      FLoggerThread.Resume;
    end;
end;

{ TLoggerThread }

procedure TLoggerThread.Execute;
var F:TFileStream;
    Lines: TStrings;
    v: String;
begin
  try
    Lines := TStringList.Create;
    if not FileExists (FLogger.FFileName) then
      try
        F := TFileStream.Create (FLogger.FFileName, fmCreate or fmShareDenyNone);
        F.Free;
      except end;
    try
        F := TFileStream.Create (FLogger.FFileName, fmOpenWrite or fmShareDenyNone);
        F.Seek (0, soFromEnd);
        while not terminated do
          begin
            sleep (500);
            CS.Enter;
            if FLines.Count > 0 then
              begin
                Lines.Assign (FLines);
                FLines.Clear;
              end;
            CS.Leave;
            if Lines.Count > 0 then
              begin
                v := Lines.Text;
                if (v<>'') then
                  F.Write (v[1], length(v));
                Lines.Clear;
              end;
          end;
        F.Free;
    except end;
  except end;
end;

procedure TLoggerThread.SyncLines;
begin
  try
    if Assigned (FLogger.FOnLoggerLines) then
      FLogger.FOnLoggerLines (FLogger, FLines);
  except end;
end;

end.
