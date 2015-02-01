{ $Header: /MidiComp/CIRCBUF.PAS 2     10/06/97 7:33 Davec $ }

{ Written by David Churcher <dchurcher@cix.compulink.co.uk>,
  released to the public domain. }

(**
 * CircBuf.pas v2010-05r1
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
 * The Original Code is FIFO circular buffer.
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
 

{ A First-In First-Out circular buffer.
  Port of circbuf.c from Microsoft's Windows MIDI monitor example.
  I did do a version of this as an object (see Rev 1.1) but it was getting too 
  complicated and I couldn't see any real benefits to it so I dumped it 
  for an ordinary memory buffer with pointers. 

  This unit is a bit C-like, everything is done with pointers and extensive
  use is made of the undocumented feature of the Inc() function that 
  increments pointers by the size of the object pointed to.
  All of this could probably be done using Pascal array notation with
  range-checking turned off, but I'm not sure it's worth it.
}

Unit CircBuf;

interface

Uses Windows, MMSystem;

type
    {$IFNDEF WIN32}
    { API types not defined in Delphi 1 }
    DWORD = Longint;
    HGLOBAL = THandle;
    UINT = Word;
    TFNTimeCallBack = procedure(uTimerID, uMessage: UINT; dwUser, dw1, dw2: DWORD);
    {$ENDIF}

	{ MIDI input event }
	TMidiBufferItem = record
	  	timestamp: DWORD;	{ Timestamp in milliseconds after midiInStart }
		data: DWORD;		{ MIDI message received }
		sysex: PMidiHdr;	{ Pointer to sysex MIDIHDR, nil if not sysex }
	end;
	PMidiBufferItem = ^TMidiBufferItem;

	{ MIDI input buffer }
	TCircularBuffer = record
		RecordHandle: HGLOBAL;		{ Windows memory handle for this record }
		BufferHandle: HGLOBAL;		{ Windows memory handle for the buffer }
		pStart: PMidiBufferItem;		{ ptr to start of buffer }
		pEnd: PMidiBufferItem;			{ ptr to end of buffer }
		pNextPut: PMidiBufferItem;		{ next location to fill }
		pNextGet: PMidiBufferItem;		{ next location to empty }
		Error: Word;		 		{ error code from MMSYSTEM functions }
		Capacity: Word;				{ buffer size (in TMidiBufferItems) }
		EventCount: Word;			{ Number of events in buffer }
	end;

   PCircularBuffer = ^TCircularBuffer;

function GlobalSharedLockedAlloc( Capacity: Word; var hMem: HGLOBAL ): Pointer;
procedure GlobalSharedLockedFree( hMem: HGLOBAL; ptr: Pointer );

function CircbufAlloc( Capacity: Word ): PCircularBuffer;
procedure CircbufFree( PBuffer: PCircularBuffer );
function CircbufRemoveEvent( PBuffer: PCircularBuffer ): Boolean;
function CircbufReadEvent( PBuffer: PCircularBuffer; PEvent: PMidiBufferItem ): Boolean;
{ Note: The PutEvent function is in the DLL }

implementation

{ Allocates in global shared memory, returns pointer and handle }
function GlobalSharedLockedAlloc( Capacity: Word; var hMem: HGLOBAL ): Pointer;
var
	ptr: Pointer;
begin
	{ Allocate the buffer memory }
	hMem := GlobalAlloc(GMEM_SHARE Or GMEM_MOVEABLE Or GMEM_ZEROINIT, Capacity );

	if (hMem = 0) then
		ptr := Nil
	else
		begin
		ptr := GlobalLock(hMem);
		if (ptr = Nil) then
			GlobalFree(hMem);
		end;

	Result := Ptr;
end;

procedure GlobalSharedLockedFree( hMem: HGLOBAL; ptr: Pointer );
begin
	if (hMem <> 0) then
	begin
    GlobalUnlock(hMem);
		GlobalFree(hMem);
  end;
end;

function CircbufAlloc( Capacity: Word ): PCircularBuffer;
var
	NewCircularBuffer: PCircularBuffer;
	NewMIDIBuffer: PMidiBufferItem;
	hMem: HGLOBAL;
begin
	{ TODO: Validate circbuf size, <64K }
	NewCircularBuffer :=
		GlobalSharedLockedAlloc( Sizeof(TCircularBuffer), hMem );
	if (NewCircularBuffer <> Nil) then
		begin
		NewCircularBuffer^.RecordHandle := hMem;
		NewMIDIBuffer :=
			GlobalSharedLockedAlloc( Capacity * Sizeof(TMidiBufferItem), hMem );
		if (NewMIDIBuffer = Nil) then
			begin
			{ TODO: Exception here? }
			GlobalSharedLockedFree( NewCircularBuffer^.RecordHandle,
											NewCircularBuffer );
			NewCircularBuffer := Nil;
			end
		else
			begin
                	NewCircularBuffer^.pStart := NewMidiBuffer;
			{ Point to item at end of buffer }
			NewCircularBuffer^.pEnd := NewMidiBuffer;
			Inc(NewCircularBuffer^.pEnd, Capacity);
			{ Start off the get and put pointers in the same position. These
			  will get out of sync as the interrupts start rolling in }
			NewCircularBuffer^.pNextPut := NewMidiBuffer;
			NewCircularBuffer^.pNextGet := NewMidiBuffer;
			NewCircularBuffer^.Error := 0;
			NewCircularBuffer^.Capacity := Capacity;
			NewCircularBuffer^.EventCount := 0;
			end;
		end;
	CircbufAlloc := NewCircularBuffer;
end;

procedure CircbufFree( pBuffer: PCircularBuffer );
begin
	if (pBuffer <> Nil) then
		begin
		GlobalSharedLockedFree(pBuffer^.BufferHandle, pBuffer^.pStart);
		GlobalSharedLockedFree(pBuffer^.RecordHandle, pBuffer);
		end;
end;

{ Reads first event in queue without removing it.
  Returns true if successful, False if no events in queue }
function CircbufReadEvent( PBuffer: PCircularBuffer; PEvent: PMidiBufferItem ): Boolean;
var
	PCurrentEvent: PMidiBufferItem;
begin
	if (PBuffer^.EventCount <= 0) then
			CircbufReadEvent := False
	else
		begin
		PCurrentEvent := PBuffer^.PNextget;

		{ Copy the object from the "tail" of the buffer to the caller's object }
		PEvent^.Timestamp := PCurrentEvent^.Timestamp;
		PEvent^.Data := PCurrentEvent^.Data;
        PEvent^.Sysex := PCurrentEvent^.Sysex;
		CircbufReadEvent := True;
		end;
end;

{ Remove current event from the queue }
function CircbufRemoveEvent(PBuffer: PCircularBuffer): Boolean;
begin
	if (PBuffer^.EventCount > 0) then
		begin
		Dec( Pbuffer^.EventCount);

		{ Advance the buffer pointer, with wrap }
		Inc( Pbuffer^.PNextGet );
		If (PBuffer^.PNextGet = PBuffer^.PEnd) then
			PBuffer^.PNextGet := PBuffer^.PStart;

		CircbufRemoveEvent := True;
		end
	else
		CircbufRemoveEvent := False;
end;

end.
