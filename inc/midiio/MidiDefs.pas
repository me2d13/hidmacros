{ $Header: /MidiComp/MIDIDEFS.PAS 2     10/06/97 7:33 Davec $ }

{ Written by David Churcher <dchurcher@cix.compulink.co.uk>,
  released to the public domain. }

(**
 * MidiDefs.pas v2010-05r1
 **)
 
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 3.0/LGPL 3.0
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is MIDI type definitons.
 *
 * The Initial Developer of the Original Code is
 * David Churcher <dchurcher@cix.compulink.co.uk>.
 * Portions created by the Initial Developer are Copyright (C) 1997
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Manuel Kroeber <manuel.kroeber@googlemail.com>
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 3 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 3 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** *)

{ Common definitions used by DELPHMID.DPR and the MIDI components.
  This must be a separate unit to prevent large chunks of the VCL being
  linked into the DLL. }
unit MidiDefs;

interface

uses  Windows, MMsystem, CircBuf;

type
	{-------------------------------------------------------------------}
	{ This is the information about the control that must be accessed by
	  the MIDI input callback function in the DLL at interrupt time }
	PMidiCtlInfo = ^TMidiCtlInfo;
	TMidiCtlInfo = record
		hMem: THandle; 				    { Memory handle for this record }
		PBuffer: PCircularBuffer;	{ Pointer to the MIDI input data buffer }
		hWindow: HWnd;		   			{ Control's window handle }
		SysexOnly: Boolean;	   		{ Only process System Exclusive input }
    FilterMTC: Boolean;       { Filter Midi Time Code messages out }
    FilterAS: Boolean;        { Filter Active Sensing messages out }
	end;

	{ Information for the output timer callback function, also required at
	  interrupt time. }
	PMidiOutTimerInfo = ^TMidiOutTimerInfo;
	TMidiOutTimerInfo = record
		hMem: THandle;				{ Memory handle for this record }
		PBuffer: PCircularBuffer;	{ Pointer to MIDI output data buffer }
		hWindow: HWnd;				{ Control's window handle }
		TimeToNextEvent: DWORD;	{ Delay to next event after timer set }
		MIDIHandle: HMidiOut;		{ MIDI handle to send output to
									(copy of component's FMidiHandle property) }
		PeriodMin: Word;			{ Multimedia timer minimum period supported }
		PeriodMax: Word;			{ Multimedia timer maximum period supported }
		TimerId: Word;				{ Multimedia timer ID of current event }
	end;

implementation

end.
