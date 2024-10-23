
{**********************************************************************
 Package pl_SynapseVS
 From PilotLogic Software House(https://www.pilotlogic.com/)
 This file is part of CodeTyphon Studio
***********************************************************************}

unit vsComPortbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synaser;


type
  TBaudRate=(br___110,br___300, br___600, br__1200, br__2400, br__4800,
             br__9600,br_14400, br_19200, br_38400,br_56000, br_57600,
             br115200,br128000, br230400,br256000, br460800, br921600);

  TDataBits=(db8bits,db7bits,db6bits,db5bits);
  TParity=(pNone,pOdd,pEven,pMark,pSpace);
  TFlowControl=(fcNone,fcXonXoff,fcHardware);
  TStopBits=(sbOne,sbOneAndHalf,sbTwo);

  TModemSignal = (msRI,msCD,msCTS,msDSR);
  TModemSignals = Set of TModemSignal;
  TStatusEvent = procedure(Sender: TObject; Reason: THookSerialReason; const Value: string) of object;

const
  ConstsBaud: array[TBaudRate] of integer=(110,
    300, 600, 1200, 2400, 4800, 9600,14400, 19200, 38400,56000, 57600,
    115200,128000,230400,256000, 460800, 921600);

  ConstsBits: array[TDataBits] of integer=(8, 7 , 6, 5);
  ConstsParity: array[TParity] of char=('N', 'O', 'E', 'M', 'S');
  ConstsStopBits: array[TStopBits] of integer=(SB1,SB1AndHalf,SB2);



  const
    BaudRateStrings: array[TBaudRate] of string = ('110', '300', '600',
      '1200', '2400', '4800', '9600', '14400', '19200', '38400', '56000', '57600',
      '115200', '128000', '230400', '256000','460800', '921600');
    StopBitsStrings: array[TStopBits] of string = ('1', '1.5', '2');
    DataBitsStrings: array[TDataBits] of string = ('8', '7', '6', '5');
    ParityBitsStrings: array[TParity] of string = ('None', 'Odd', 'Even', 'Mark', 'Space');
    FlowControlStrings: array[TFlowControl] of string = ('None', 'Software', 'HardWare');


// conversion functions
function StrToBaudRate(Str: string): TBaudRate;
function StrToStopBits(Str: string): TStopBits;
function StrToDataBits(Str: string): TDataBits;
function StrToParity(Str: string): TParity;
function StrToFlowControl(Str: string): TFlowControl;
function BaudRateToStr(BaudRate: TBaudRate): string;
function StopBitsToStr(StopBits: TStopBits): string;
function DataBitsToStr(DataBits: TDataBits): string;
function ParityToStr(Parity: TParity): string;
function FlowControlToStr(FlowControl: TFlowControl): string;

procedure StringArrayToList(AList: TStrings; const AStrings: array of string);

implementation

procedure StringArrayToList(AList: TStrings; const AStrings: array of string);
var
 Cpt: Integer;
begin
  for Cpt := Low(AStrings) to High(AStrings) do
   AList.Add(AStrings[Cpt]);
end;

// string to baud rate
function StrToBaudRate(Str: string): TBaudRate;
var
  I: TBaudRate;
begin
  I := Low(TBaudRate);
  while (I <= High(TBaudRate)) do
  begin
    if UpperCase(Str) = UpperCase(BaudRateToStr(TBaudRate(I))) then
      Break;
    I := Succ(I);
  end;
  if I > High(TBaudRate) then
    Result := br__9600
  else
    Result := I;
end;
// string to stop bits
function StrToStopBits(Str: string): TStopBits;
var
  I: TStopBits;
begin
  I := Low(TStopBits);
  while (I <= High(TStopBits)) do
  begin
    if UpperCase(Str) = UpperCase(StopBitsToStr(TStopBits(I))) then
      Break;
    I := Succ(I);
  end;
  if I > High(TStopBits) then
    Result := sbOne
  else
    Result := I;
end;

// string to data bits
function StrToDataBits(Str: string): TDataBits;
var
  I: TDataBits;
begin
  I := Low(TDataBits);
  while (I <= High(TDataBits)) do
  begin
    if UpperCase(Str) = UpperCase(DataBitsToStr(I)) then
      Break;
    I := Succ(I);
  end;
  if I > High(TDataBits) then
    Result := db8bits
  else
    Result := I;
end;

// string to parity
function StrToParity(Str: string): TParity;
var
  I: TParity;
begin
  I := Low(TParity);
  while (I <= High(TParity)) do
  begin
    if UpperCase(Str) = UpperCase(ParityToStr(I)) then
      Break;
    I := Succ(I);
  end;
  if I > High(TParity) then
    Result := pNone
  else
    Result := I;
end;

// string to flow control
function StrToFlowControl(Str: string): TFlowControl;
var
  I: TFlowControl;
begin
  I := Low(TFlowControl);
  while (I <= High(TFlowControl)) do
  begin
    if UpperCase(Str) = UpperCase(FlowControlToStr(I)) then
      Break;
    I := Succ(I);
  end;
  if I > High(TFlowControl) then
    Result := fcNone
  else
    Result := I;
end;

// baud rate to string
function BaudRateToStr(BaudRate: TBaudRate): string;
begin
  Result := BaudRateStrings[BaudRate];
end;

// stop bits to string
function StopBitsToStr(StopBits: TStopBits): string;
begin
  Result := StopBitsStrings[StopBits];
end;

// data bits to string
function DataBitsToStr(DataBits: TDataBits): string;
begin
  Result := DataBitsStrings[DataBits];
end;

// parity to string
function ParityToStr(Parity: TParity): string;
begin
  Result := ParityBitsStrings[Parity];
end;

// flow control to string
function FlowControlToStr(FlowControl: TFlowControl): string;
begin
  Result := FlowControlStrings[FlowControl];
end;


end.

