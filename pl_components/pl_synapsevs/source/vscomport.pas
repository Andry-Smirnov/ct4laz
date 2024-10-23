
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsComPort;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF UNIX}
  Classes,
{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}
{$ELSE}
  Windows, Classes,
{$ENDIF}
  SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  synaser, vsComPortbase, vsComPortsetup;


type
  TvsComPort = class;

  TComPortReadThread=class(TThread)
  public
    MustDie: boolean;
    Owner: TvsComPort;
  protected
    procedure CallEvent;
    procedure Execute; override;
  published
    property Terminated;
  end;

  TvsComPort = class(TComponent)
  private
    FActive: boolean;
    FSynSer: TBlockSerial;
    FDevice: string;

    FBaudRate: TBaudRate;
    FDataBits: TDataBits;
    FParity: TParity;
    FStopBits: TStopBits;
    
    FSoftflow, FHardflow: boolean;
    FFlowControl: TFlowControl;
    FRcvLineCRLF : Boolean;

    FOnRxData: TNotifyEvent;
    FOnStatus: TStatusEvent;
    ReadThread: TComPortReadThread;

    procedure DeviceOpen;
    procedure DeviceClose;

    procedure ComException(str: string);

  protected
    procedure SetActive(state: boolean);
    procedure SetBaudRate(br: TBaudRate);
    procedure SetDataBits(db: TDataBits);
    procedure SetParity(pr: TParity);
    procedure SetFlowControl(fc: TFlowControl);
    procedure SetStopBits(sb: TStopBits);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Open;
    procedure Close;
    // show a port settings dialog form
    procedure ShowSetupDialog;
    // read data from port
    function DataAvailable: boolean;
    function ReadData: string;
//    function ReadBuffer(var buf; size: integer): integer;

    // write data to port
    function WriteData(data: string): integer;
    function WriteBuffer(var buf; size: integer): integer;

    // read pin states
    function ModemSignals: TModemSignals;
    function GetDSR: boolean;
    function GetCTS: boolean;
    function GetRing: boolean;
    function GetCarrier: boolean;

    // set pin states
//    procedure SetRTSDTR(RtsState, DtrState: boolean);
    procedure SetDTR(OnOff: boolean);
    procedure SetRTS(OnOff: boolean);
//  procedure SetBreak(OnOff: boolean);

  published
    property Active: boolean read FActive write SetActive;

    property BaudRate: TBaudRate read FBaudRate write SetBaudRate; // default br115200;
    property DataBits: TDataBits read FDataBits write SetDataBits;
    property Parity: TParity read FParity write SetParity;
    property FlowControl: TFlowControl read FFlowControl write SetFlowControl;
    property StopBits: TStopBits read FStopBits write SetStopBits;
    
    property SynSer: TBlockSerial read FSynSer write FSynSer;
    property Device: string read FDevice write FDevice;
    property RcvLineCRLF: Boolean read FRcvLineCRLF write FRcvLineCRLF;

    property OnRxData: TNotifyEvent read FOnRxData write FOnRxData;
    property OnStatus: TStatusEvent read FOnStatus write FOnStatus;
  end;


procedure EditComPort(ComPort: TvsComPort);

implementation

procedure EditComPort(ComPort: TvsComPort);
begin
  with TComSetupFrm.Create(nil) do
  begin
    ComComboBox1.Text := ComPort.Device;
    ComComboBox2.Text :=  BaudRateToStr(ComPort.BaudRate);
    ComComboBox3.Text :=  DataBitsToStr(ComPort.DataBits);
    ComComBoBox4.Text :=  StopBitsToStr(ComPort.StopBits);
    ComComBoBox5.Text :=  ParityToStr(ComPort.Parity);
    ComComBoBox6.Text :=  FlowControlToStr(ComPort.FlowControl);

 if ShowModal = mrOK then
    begin
      ComPort.Close;
      ComPort.Device := ComComboBox1.Text;
      ComPort.BaudRate := StrToBaudRate(ComComboBox2.Text);
      ComPort.DataBits := StrToDataBits(ComComboBox3.Text);
      ComPort.StopBits := StrToStopBits(ComComboBox4.Text);
      ComPort.Parity := StrToParity(ComComboBox5.Text);
      ComPort.FlowCOntrol := StrToFlowControl(ComComboBox6.Text);
      // ComPort.Open;
    end;
    Free;
  end;
end;


// ===================== TvsComPort ========================================

procedure TvsComPort.Close;
begin
  Active:=false;
end;

procedure TvsComPort.DeviceClose;
begin
  // flush device
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Flush;
    FSynSer.Purge;
  end;
  
  // stop capture thread
  if ReadThread<>nil then begin
    ReadThread.FreeOnTerminate:=false;
    ReadThread.MustDie:= true;
    while not ReadThread.Terminated do begin
      Application.ProcessMessages;
    end;
    ReadThread.Free;
    ReadThread:=nil;
  end;

  // close device
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Flush;
    FSynSer.CloseSocket;
  end;
end;

constructor TvsComPort.Create(AOwner: TComponent);
begin
  inherited;
  //FHandle:=-1;
  ReadThread:=nil;
  FSynSer:=TBlockSerial.Create;
  FSynSer.LinuxLock:=false;
  FHardflow:=false;
  FSoftflow:=false;
  FFlowControl:=fcNone;
  {$IFDEF UNIX}
  FDevice:='/dev/ttyS0';
  {$ELSE}
  FDevice:='COM1';
  {$ENDIF}
  FRcvLineCRLF := False;;
//  FBaudRate:=br115200;
end;

function TvsComPort.DataAvailable: boolean;
begin
  if FSynSer.Handle=INVALID_HANDLE_VALUE then begin
    result:=false;
    exit;
  end;
  result:=FSynSer.CanReadEx(0);
end;

destructor TvsComPort.Destroy;
begin
  Close;
  FSynSer.Free;
  inherited;
end;

procedure TvsComPort.Open;
begin
    // Initialize OnStatus;
  if Assigned(OnStatus) then SynSer.OnStatus := OnStatus;
  Active:=true;
end;

procedure TvsComPort.ShowSetupDialog;
begin
  EditComPort(self);
end;

procedure TvsComPort.DeviceOpen;
begin
  FSynSer.Connect(FDevice);
  if FSynSer.Handle=INVALID_HANDLE_VALUE then
    raise Exception.Create('Could not open device '+ FSynSer.Device);

  FSynSer.Config(ConstsBaud[FBaudRate],
                 ConstsBits[FDataBits],
                 ConstsParity[FParity],
                 ConstsStopBits[FStopBits],
                 FSoftflow, FHardflow);

  // Launch Thread
  ReadThread := TComPortReadThread.Create(true);
  ReadThread.Owner := Self;
  ReadThread.MustDie := false;
//  ReadThread.Resume;   --> deprecated
  ReadThread.Start;
end;


function TvsComPort.ReadData: string;
begin
  result:='';
  if FSynSer.Handle=INVALID_HANDLE_VALUE then
    ComException('can not read from a closed port.');
  if FRcvLineCRLF then
  result:=FSynSer.RecvString(0)
  else
  result:=FSynSer.RecvPacket(0);
end;

procedure TvsComPort.SetActive(state: boolean);
begin
  if state=FActive then exit;

  if state then DeviceOpen
  else DeviceClose;

  FActive:=state;
end;

procedure TvsComPort.SetBaudRate(br: TBaudRate);
begin
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
  FBaudRate:=br;
end;

procedure TvsComPort.SetDataBits(db: TDataBits);
begin
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
  FDataBits:=db;
end;

procedure TvsComPort.SetFlowControl(fc: TFlowControl);
begin
  if fc=fcNone then begin
    FSoftflow:=false;
    FHardflow:=false;
  end else if fc=fcXonXoff then begin
    FSoftflow:=true;
    FHardflow:=false;
  end else if fc=fcHardware then begin
    FSoftflow:=false;
    FHardflow:=true;
  end;

  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
  FFlowControl:=fc;
end;

{
procedure TvsComPort.SetFlowControl(fc: TFlowControl);
begin
  if FHandle<>-1 then begin
    if fc=fcNone then CurTermIO.c_cflag:=CurTermIO.c_cflag and (not CRTSCTS)
    else CurTermIO.c_cflag:=CurTermIO.c_cflag or CRTSCTS;
    tcsetattr(FHandle,TCSADRAIN,CurTermIO);
  end;
  FFlowControl:=fc;
end;
}
procedure TvsComPort.SetParity(pr: TParity);
begin
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
  FParity:=pr;
end;

procedure TvsComPort.SetStopBits(sb: TStopBits);
begin
  if FSynSer.Handle<>INVALID_HANDLE_VALUE then begin
    FSynSer.Config(ConstsBaud[FBaudRate], ConstsBits[FDataBits], ConstsParity[FParity],
                   ConstsStopBits[FStopBits], FSoftflow, FHardflow);
  end;
  FStopBits:=sb;
end;


function TvsComPort.WriteBuffer(var buf; size: integer): integer;
begin
//  if FSynSer.Handle=INVALID_HANDLE_VALUE then
 //   ComException('can not write to a closed port.');
  result:= FSynSer.SendBuffer(Pointer(@buf), size);
end;

function TvsComPort.WriteData(data: string): integer;
begin
  result:=length(data);
  FSynSer.SendString(data);
end;


function TvsComPort.ModemSignals: TModemSignals;
begin
  result:=[];
  if FSynSer.CTS then result := result + [ msCTS ];
  if FSynSer.carrier then result := result + [ msCD ];
  if FSynSer.ring then result := result + [ msRI ];
  if FSynSer.DSR then result := result + [ msDSR ];
end;

function TvsComPort.GetDSR: boolean;
begin
  result := FSynSer.DSR;
end;

function TvsComPort.GetCTS: boolean;
begin
  result := FSynSer.CTS;
end;

function TvsComPort.GetRing: boolean;
begin
  result := FSynSer.ring;
end;

function TvsComPort.GetCarrier: boolean;
begin
  result := FSynSer.carrier;
end;

{procedure TvsComPort.SetBreak(OnOff: boolean);
begin
//  if FHandle=-1 then
//    ComException('can not set break state on a closed port.');
//  if OnOff=false then ioctl(FHandle,TIOCCBRK,1)
//  else ioctl(FHandle,TIOCSBRK,0);
end;  }


procedure TvsComPort.SetDTR(OnOff: boolean);
begin
  FSynSer.DTR := OnOff;
end;


procedure TvsComPort.SetRTS(OnOff: boolean);
begin
  FSynSer.RTS := OnOff;
end;


procedure TvsComPort.ComException(str: string);
begin
  raise Exception.Create('ComPort error: '+str);
end;

//==================== TComPortReadThread ==========================================

procedure TComPortReadThread.CallEvent;
begin
  if Assigned(Owner.FOnRxData) then begin
    Owner.FOnRxData(Owner);
  end;
end;

procedure TComPortReadThread.Execute;
begin
  try
    while not MustDie do begin
      if Owner.FSynSer.CanReadEx(100) then  Synchronize(@CallEvent);
    end;
  finally
    Terminate;
  end;

end;

end.
