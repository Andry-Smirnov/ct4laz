{*************************************************************************
                PilotLogic Software House

  Package pl_Graphics32
  This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)

 ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * https://www.mozilla.org/en-US/MPL/1.1/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All rights reserved
 *
 * ***** END LICENSE BLOCK *****
 ************************************************************************}

unit GR32_System;

interface

{$I GR32.inc}

uses

  LCLIntf, LCLType,
  {$IFDEF Windows}
    Windows,
  {$ENDIF}
  {$IFDEF UNIX}
    Unix, BaseUnix,
  {$ENDIF}
  SysUtils;

type
  TPerfTimer = class
  private
{$IFDEF UNIX}
  {$IFDEF FPC}
    FStart: Int64;
  {$ENDIF}
{$ENDIF}
{$IFDEF Windows}
    FFrequency, FPerformanceCountStart, FPerformanceCountStop: Int64;
{$ENDIF}
  public
    procedure Start;
    function ReadNanoseconds: string;
    function ReadMilliseconds: string;
    function ReadSeconds: string;

    function ReadValue: Int64;
  end;

{ Pseudo GetTickCount implementation for Linux - for compatibility
  This works for basic time testing, however, it doesnt work like its
  Windows counterpart, ie. it doesnt return the number of milliseconds since
  system boot. Will definitely overflow. }
function GetTickCount: Cardinal;

{ Returns the number of processors configured by the operating system. }
function GetProcessorCount: Cardinal;

type
  {$IFNDEF PUREPASCAL}
  { TCPUInstructionSet, defines specific CPU technologies }
  TCPUInstructionSet = (ciMMX, ciEMMX, ciSSE, ciSSE2, ci3DNow, ci3DNowExt);
  {$ELSE}
  TCPUInstructionSet = (ciDummy);
  {$DEFINE NO_REQUIREMENTS}
  {$ENDIF}

  PCPUFeatures = ^TCPUFeatures;
  TCPUFeatures = set of TCPUInstructionSet;

{ General function that returns whether a particular instruction set is
  supported for the current CPU or not }
function HasInstructionSet(const InstructionSet: TCPUInstructionSet): Boolean;
function CPUFeatures: TCPUFeatures;

var
  GlobalPerfTimer: TPerfTimer;

implementation

uses
  Forms, Classes, TypInfo;

var
  CPUFeaturesInitialized : Boolean = False;
  CPUFeaturesData: TCPUFeatures;

{$IFDEF UNIX}
{$IFDEF FPC}
function GetTickCount: Cardinal;
var
  t : timeval;
begin
  fpgettimeofday(@t,nil);
   // Build a 64 bit microsecond tick from the seconds and microsecond longints
  Result := (Int64(t.tv_sec) * 1000000) + t.tv_usec;
end;


{ TPerfTimer }

function TPerfTimer.ReadNanoseconds: string;
begin
  Result := IntToStr(ReadValue);
end;

function TPerfTimer.ReadMilliseconds: string;
begin
  Result := IntToStr(ReadValue div 1000);
end;

function TPerfTimer.ReadSeconds: string;
begin
  Result := IntToStr(ReadValue div 1000000);
end;

function TPerfTimer.ReadValue: Int64;
begin
  Result := GetTickCount - FStart;
end;

procedure TPerfTimer.Start;
begin
  FStart := GetTickCount;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF Windows}
function GetTickCount: Cardinal;
begin
  Result := Windows.GetTickCount;
end;


{ TPerfTimer }

function TPerfTimer.ReadNanoseconds: string;
begin
  QueryPerformanceCounter(FPerformanceCountStop);
  QueryPerformanceFrequency(FFrequency);
  Assert(FFrequency > 0);

  Result := IntToStr(Round(1000000 * (FPerformanceCountStop - FPerformanceCountStart) / FFrequency));
end;

function TPerfTimer.ReadMilliseconds: string;
begin
  QueryPerformanceCounter(FPerformanceCountStop);
  QueryPerformanceFrequency(FFrequency);
  Assert(FFrequency > 0);

  Result := FloatToStrF(1000 * (FPerformanceCountStop - FPerformanceCountStart) / FFrequency, ffFixed, 15, 3);
end;

function TPerfTimer.ReadSeconds: String;
begin
  QueryPerformanceCounter(FPerformanceCountStop);
  QueryPerformanceFrequency(FFrequency);
  Result := FloatToStrF((FPerformanceCountStop - FPerformanceCountStart) / FFrequency, ffFixed, 15, 3);
end;

function TPerfTimer.ReadValue: Int64;
begin
  QueryPerformanceCounter(FPerformanceCountStop);
  QueryPerformanceFrequency(FFrequency);
  Assert(FFrequency > 0);

  Result := Round(1000000 * (FPerformanceCountStop - FPerformanceCountStart) / FFrequency);
end;

procedure TPerfTimer.Start;
begin
  QueryPerformanceCounter(FPerformanceCountStart);
end;
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF FPC}
function GetProcessorCount: Cardinal;
begin
  Result := 1;
end;
{$ENDIF}
{$ENDIF}
{$IFDEF Windows}
function GetProcessorCount: Cardinal;
var
  lpSysInfo: TSystemInfo;
begin
  GetSystemInfo(lpSysInfo);
  Result := lpSysInfo.dwNumberOfProcessors;
end;
{$ENDIF}

{$IFNDEF PUREPASCAL}
const
  CPUISChecks: array [TCPUInstructionSet] of Cardinal =
    ($800000,  $400000, $2000000, $4000000, $80000000, $40000000);
    {ciMMX  ,  ciEMMX,  ciSSE   , ciSSE2  , ci3DNow ,  ci3DNowExt}

function CPUID_Available: Boolean;
asm
{$IFDEF TARGET_x86}
        MOV       EDX,False
        PUSHFD
        POP       EAX
        MOV       ECX,EAX
        XOR       EAX,$00200000
        PUSH      EAX
        POPFD
        PUSHFD
        POP       EAX
        XOR       ECX,EAX
        JZ        @1
        MOV       EDX,True
@1:     PUSH      EAX
        POPFD
        MOV       EAX,EDX
{$ENDIF}
{$IFDEF TARGET_x64}
        MOV       RDX,False
        PUSHFQ
        POP       RAX
        MOV       ECX,EAX
        XOR       EAX,$00200000
        PUSH      RAX
        POPFQ
        PUSHFQ
        POP       RAX
        XOR       ECX,EAX
        JZ        @1
        MOV       EDX,True
@1:     PUSH      RAX
        POPFQ
        MOV       EAX,EDX
{$ENDIF}
end;

function CPU_Signature: Integer;
asm
{$IFDEF TARGET_x86}
        PUSH      EBX
        MOV       EAX,1
        {$IFDEF FPC}
        CPUID
        {$ELSE}
        DW        $A20F   // CPUID
        {$ENDIF}
        POP       EBX
{$ENDIF}
{$IFDEF TARGET_x64}
        PUSH      RBX
        MOV       EAX,1
        CPUID
        POP       RBX
{$ENDIF}
end;

function CPU_Features: Integer;
asm
{$IFDEF TARGET_x86}
        PUSH      EBX
        MOV       EAX,1
        {$IFDEF FPC}
        CPUID
        {$ELSE}
        DW        $A20F   // CPUID
        {$ENDIF}
        POP       EBX
        MOV       EAX,EDX
{$ENDIF}
{$IFDEF TARGET_x64}
        PUSH      RBX
        MOV       EAX,1
        CPUID
        POP       RBX
        MOV       EAX,EDX
{$ENDIF}
end;

function CPU_ExtensionsAvailable: Boolean;
asm
{$IFDEF TARGET_x86}
        PUSH      EBX
        MOV       @Result, True
        MOV       EAX, $80000000
        {$IFDEF FPC}
        CPUID
        {$ELSE}
        DW        $A20F   // CPUID
        {$ENDIF}
        CMP       EAX, $80000000
        JBE       @NOEXTENSION
        JMP       @EXIT
      @NOEXTENSION:
        MOV       @Result, False
      @EXIT:
        POP       EBX
{$ENDIF}
{$IFDEF TARGET_x64}
        PUSH      RBX
        MOV       @Result, True
        MOV       EAX, $80000000
        CPUID
        CMP       EAX, $80000000
        JBE       @NOEXTENSION
        JMP       @EXIT
        @NOEXTENSION:
        MOV       @Result, False
        @EXIT:
        POP       RBX
{$ENDIF}
end;

function CPU_ExtFeatures: Integer;
asm
{$IFDEF TARGET_x86}
        PUSH      EBX
        MOV       EAX, $80000001
        {$IFDEF FPC}
        CPUID
        {$ELSE}
        DW        $A20F   // CPUID
        {$ENDIF}
        POP       EBX
        MOV       EAX,EDX
{$ENDIF}
{$IFDEF TARGET_x64}
        PUSH      RBX
        MOV       EAX, $80000001
        CPUID
        POP       RBX
        MOV       EAX,EDX
{$ENDIF}
end;

function HasInstructionSet(const InstructionSet: TCPUInstructionSet): Boolean;
// Must be implemented for each target CPU on which specific functions rely
begin
  Result := False;
  if not CPUID_Available then Exit;                   // no CPUID available
  if CPU_Signature shr 8 and $0F < 5 then Exit;       // not a Pentium class

  case InstructionSet of
    ci3DNow, ci3DNowExt:
      {$IFNDEF FPC}
      if not CPU_ExtensionsAvailable or (CPU_ExtFeatures and CPUISChecks[InstructionSet] = 0) then
      {$ENDIF}
        Exit;
    ciEMMX:
      begin
        // check for SSE, necessary for Intel CPUs because they don't implement the
        // extended info
        if (CPU_Features and CPUISChecks[ciSSE] = 0) and
          (not CPU_ExtensionsAvailable or (CPU_ExtFeatures and CPUISChecks[ciEMMX] = 0)) then
          Exit;
      end;
  else
    if CPU_Features and CPUISChecks[InstructionSet] = 0 then
      Exit; // return -> instruction set not supported
    end;

  Result := True;
end;

{$ELSE}

function HasInstructionSet(const InstructionSet: TCPUInstructionSet): Boolean;
begin
  Result := False;
end;
{$ENDIF}

procedure InitCPUFeaturesData;
var
  I: TCPUInstructionSet;
begin
  if CPUFeaturesInitialized then Exit;

  CPUFeaturesData := [];
  for I := Low(TCPUInstructionSet) to High(TCPUInstructionSet) do
    if HasInstructionSet(I) then CPUFeaturesData := CPUFeaturesData + [I];

  CPUFeaturesInitialized := True;
end;

function CPUFeatures: TCPUFeatures;
begin
  if not CPUFeaturesInitialized then
    InitCPUFeaturesData;
  Result := CPUFeaturesData;
end;

initialization
  InitCPUFeaturesData;
  GlobalPerfTimer := TPerfTimer.Create;

finalization
  GlobalPerfTimer.Free;

end.
