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
 * ***** END LICENSE BLOCK *****
 ************************************************************************}


{$DEFINE PLATFORM_INDEPENDENT}
{$MODE Delphi}

  // Set up internal CPU target directives according to FPC directives

{$IFDEF CPUI386}
      // target is an Intel 80386 or later.
  {$DEFINE TARGET_x86}
{$ENDIF}

{$IFDEF CPUPOWERPC}
  {$DEFINE TARGET_POWERPC}
{$ENDIF}

{$IFDEF CPU64}
    // target is a 64-bit processor (AMD or INTEL).
    {$DEFINE TARGET_x64}
    {$ASMMODE INTEL}
{$ENDIF}

  // Widgetsets other then Windows will want to implement BITS_GETTER
{$IFNDEF MSWINDOWS}
   {$DEFINE BITS_GETTER}
{$ENDIF}


(* Symbol PUREPASCAL :
    Forces GR32 into pure pascal mode. *)
// FPC generates very efficient code, there is no need for assembler,
// which just causes trouble
{$IFDEF FPC}
  {$DEFINE PUREPASCAL}
  {-$DEFINE USEMOVE}
{$ENDIF}

{$IFDEF TARGET_x64}
  {$DEFINE PUREPASCAL}
  {$DEFINE USEMOVE}
  {.$EXCESSPRECISION OFF}
{$ENDIF}

// The DARWIN target of Free Pascal generates PIC code by default
// Which isn't compatible with the current Assembler
// So force PUREPASCAL
{$IFDEF DARWIN}
  {$DEFINE PUREPASCAL}
{$ENDIF}

(* Symbol PUREPASCAL :
    Forces GR32 into pure pascal mode. Should be used only for testing and
    debugging purposes. *)
    {-$DEFINE PUREPASCAL}


(* Symbol DEPRECATEDMODE :
    Defines if GR32 should behave like version 1.7.x and down.
    NOTE:
    Support for this will be dropped at some time and is only provided for
    transition *)

  {-$DEFINE DEPRECATEDMODE}


(* Symbol CHANGED_IN_PIXELS :
    Defines if the OnChange / OnAreaChange event should be fired with
    safe pixel setters (PixelS, PixelTS, PixelFS etc.)
    NOTE:
    While enabling this feature is generally better for the repaint and layer
    optimization, in "lazy code" it is also much slower. *)

  {-$DEFINE CHANGED_IN_PIXELS}


{$IFDEF INLININGSUPPORTED}
(* Symbol USEINLINING :
    Defines whether to use function inlining.
    NOTE:
    Enabling this symbol increases executable size but will result in better
    performance in most cases. *)

  {$DEFINE USEINLINING}
{$ENDIF}


(* Symbol USE_GUIDS_IN_MMF :
    Defines whether to use GUIDs for temporary MMFs filename instead of
    using the GetTempFilename in WinAPI. *)

  {-$DEFINE USE_GUIDS_IN_MMF}


(* Symbol USEMOVE :
    Defines whether to use Move instead of MoveLongword. *)

  {-$DEFINE USEMOVE}


(* Symbol USEMULTITHREADING :
    Use multithreading by default if possible. *)

  {-$DEFINE USEMULTITHREADING}

(* FastCode specific symbols:
    Adding these symbols to your project's define list will force use of the
    particular routine over the standard GR32 routine.

   GR32_FASTCODEMOVE     - Uses the patched Move routine rather than MoveLongword.

   For more information on the FastCode project take a look at this URL:
     http://fastcode.sourceforge.net/

   For FastMove make sure to download the CPU id based function, ie. runtime
   RTL patching.
*)

{$IFDEF GR32_FASTCODEMOVE}
  {$DEFINE USEMOVE}
{$ENDIF}

{$IFDEF COMPILER6}
  {$DEFINE EXT_PROP_EDIT}
{$ENDIF}

{$IFNDEF FPC}
  {$DEFINE XPTHEMES} // enable support for windows xp themes
{$ENDIF}



(*
  Symbol OMIT_MMX:
  ----------------

     If defined MMX optimizations are not used (omitted)

   For faster pixel/color processing, MMX can be used which results in a huge
   performance boost over PUREPASCAL code or native assembler code.
   However, there's a penalty (a call to EMMS) when switching between FPU and
   MMX registers. This call is not necessary when SSE2 is available. Though,
   for backward compatibility it is necessary to call EMMS even if SSE2 is used.

   NOTE: On every x64 system SSE2 is available and thus MMX support is not
   necessary.
*)

  {-$DEFINE OMIT_MMX}
  {$IFDEF TARGET_x64}
    {$DEFINE OMIT_MMX}
  {$ENDIF}


(*
  Symbol OMIT_SSE2:
  ----------------

    If defined SSE2 optimizations are not used (omitted)

  For faster pixel/color processing, SSE2 can be used which results in a huge
  performance boost over PUREPASCAL code or native assembler code.

*)

  {-$DEFINE OMIT_SSE2}



(*
  Symbol: USEGR32GAMMA
  --------------------

    If defined the polygon rasterizer will use the GR32 gamma correction LUT.

  Disable for a slight performance increase.
*)

{$DEFINE USEGR32GAMMA}



(*
  Symbol: CHANGENOTIFICATIONS
  ---------------------------

    If defined the polygon rasterizer will trigger change notifications.

  Undefining this will avoid bounding box computations, which may
  improve performance slightly.
*)

{-$DEFINE CHANGENOTIFICATIONS}



(*
  Symbol: USESTACKALLOC
  ---------------------

    If defined stack allocation routines will be used in some functions.

  Allocating memory on the stack is usually more efficient than using the
  memory manager. If a routine uses StackAllock/StackFree then it should
  always be wrapped inside a {$W+}...{$W-} block in order to generate
  a stack frame.

  NOTE: Undefine this symbol if you get stack overflow errors.
*)

{-$DEFINE USESTACKALLOC}
(*
  Symbol: RGBA_FORMAT
  -------------------

     Assume RGBA pixel format instead of BGRA (used by e.g. OpenGL.)
*)

{-$DEFINE RGBA_FORMAT}



(*
  Symbol: NOHINTING
  -----------------

    Disables font hinting by default when using TextToPath() method.

  It is usually preferrable to disable hinting when using a high quality
  polygon renderer like VPR. However, hinting can sometimes improve
  visual quality when rendering small text (text is adjusted to pixel
  boundaries which makes it more crisp.)
*)

{$DEFINE NOHINTING}


(*
  Symbol: NOHORIZONTALHINTING
  ---------------------------

    Disables horizontal font hinting when using TextToPath() method.

  The following should not be used in conjunction with NOHINTING.
  It will attempt to address the problem of extreme font hinting in the
  GDI by disabling horizontal, but keeping vertical hinting.
*)

{-$DEFINE NOHORIZONTALHINTING}


(*
  Symbol: USEKERNING
  -----------------

    Enables font kerning when using TextToPath() method.

  Kerning is the process of adjusting the spacing between characters in a
  proportional font, usually to achieve a visually pleasing result. However,
  parsing for kerning pairs is quite expensive in terms of CPU usage while the
  effect is often very little. Thus kerning is not enabled by default.
*)

{-$DEFINE USEKERNING}


{-$DEFINE TEST_BLENDMEMRGB128SSE4}



//=============== ct9999 for CodeTyphon ============================

{$DEFINE FPC_HAS_CONSTREF}
{$DEFINE HAS_NATIVEINT}
{$DEFINE NATIVE_SINCOS}
{$DEFINE USESTACKALLOC}
{$DEFINE RGBA_FORMAT}

{$IFDEF WINCE}
  {$DEFINE WIN_COLOR_FIX}
  {$DEFINE PUREPASCAL}     // test on 19-6-2011
  {$DEFINE USENATIVECODE}  // test on 19-6-2011
  {$DEFINE USE_GUIDS_IN_MMF}
{$ENDIF}

{$IF DEFINED(CPUx86_64)}
   {$DEFINE PUREPASCAL}
   {.$UNDEF PUREPASCAL}
{$elseif defined(CPUi386)}
   {$DEFINE PUREPASCAL}
   {.$UNDEF PUREPASCAL}
{$ELSE}
  {$DEFINE PUREPASCAL}
{$ENDIF}

