{ $Header: /MidiComp/DELPHMCB.PAS 2     10/06/97 7:33 Davec $ }

(**
 * DelphiMidiCallback.pas v2010-05r1
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
 * The Original Code is MIDI type definitions.
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

{MIDI callback for Delphi, was DLL for Delphi 1}

unit MidiCallback;

{ These segment options required for the MIDI callback functions }
{$C PRELOAD FIXED PERMANENT}

interface

uses Windows, MMsystem, Circbuf, MidiDefs, MidiCons;

{$IFDEF WIN32}
procedure midiHandler(
  hMidiIn: HMidiIn;
  wMsg: UINT;
  dwInstance: DWORD;
  dwParam1: DWORD;
  dwParam2: DWORD); stdcall export;
function CircbufPutEvent(PBuffer: PCircularBuffer; PTheEvent: PMidiBufferItem): Boolean; stdcall; export;
{$ELSE}
procedure midiHandler(
  hMidiIn: HMidiIn;
  wMsg: Word;
  dwInstance: DWORD;
  dwParam1: DWORD;
  dwParam2: DWORD); export;
function CircbufPutEvent(PBuffer: PCircularBuffer; PTheEvent: PMidiBufferItem): Boolean; export;
{$ENDIF}

implementation

{ Add an event to the circular input buffer. }

function CircbufPutEvent(PBuffer: PCircularBuffer; PTheEvent: PMidiBufferItem): Boolean;
begin
  if (PBuffer^.EventCount < PBuffer^.Capacity) then
  begin
    Inc(Pbuffer^.EventCount);

  { Todo: better way of copying this record }
    with PBuffer^.PNextput^ do
    begin
      Timestamp := PTheEvent^.Timestamp;
      Data := PTheEvent^.Data;
      Sysex := PTheEvent^.Sysex;
    end;

  { Move to next put location, with wrap }
    Inc(Pbuffer^.PNextPut);
    if (PBuffer^.PNextPut = PBuffer^.PEnd) then
      PBuffer^.PNextPut := PBuffer^.PStart;

    CircbufPutEvent := True;
  end
  else
    CircbufPutEvent := False;
end;

{ This is the callback function specified when the MIDI device was opened
  by midiInOpen. It's called at interrupt time when MIDI input is seen
  by the MIDI device driver(s). See the docs for midiInOpen for restrictions
  on the Windows functions that can be called in this interrupt. }

procedure midiHandler(
  hMidiIn: HMidiIn;
  wMsg: UINT;
  dwInstance: DWORD;
  dwParam1: DWORD;
  dwParam2: DWORD);

var
  thisEvent: TMidiBufferItem;
  thisCtlInfo: PMidiCtlInfo;
  thisBuffer: PCircularBuffer;
  ProcessThisMessage: Boolean;

begin
  case wMsg of

    mim_Open: {nothing};

    mim_Error: {TODO: handle (message to trigger exception?) };

    mim_Data, mim_Longdata, mim_Longerror:
      { Note: mim_Longerror included because there's a bug in the Maui
      input driver that sends MIM_LONGERROR for subsequent buffers when
      the input buffer is smaller than the sysex block being received }
      begin
        thisCtlInfo := PMidiCtlInfo(dwInstance);

        // Filter messages if enabled (is there a more efficient way?
        case dwParam1 of
          MIDI_ACTIVESENSING: ProcessThisMessage := not thisCtlInfo^.FilterAS;
          MIDI_TIMINGCLOCK: ProcessThisMessage := not thisCtlInfo^.FilterMTC;
        else
          ProcessThisMessage := True;
        end;

        if ProcessThisMessage then
        begin
          { The device driver passes us the instance data pointer we
          specified for midiInOpen. Use this to get the buffer address
          and window handle for the MIDI control }

          thisBuffer := thisCtlInfo^.PBuffer;

          { Screen out short messages if we've been asked to }
          if ((wMsg <> mim_Data) or (thisCtlInfo^.SysexOnly = False))
            and (thisCtlInfo <> nil) and (thisBuffer <> nil) then
          begin
            with thisEvent do
            begin
              timestamp := dwParam2;
              if (wMsg = mim_Longdata) or
                (wMsg = mim_Longerror) then
              begin
                data := 0;
                sysex := PMidiHdr(dwParam1);
              end
              else
              begin
                data := dwParam1;
                sysex := nil;
              end;
            end;
            if CircbufPutEvent(thisBuffer, @thisEvent) then
      { Send a message to the control to say input's arrived }
              PostMessage(thisCtlInfo^.hWindow, mim_Data, 0, 0)
            else
      { Buffer overflow }
              PostMessage(thisCtlInfo^.hWindow, mim_Overflow, 0, 0);
          end;
        end;
      end;

    mom_Done: { Sysex output complete, dwParam1 is pointer to MIDIHDR }
      begin
   { Notify the control that its sysex output is finished.
     The control should call midiOutUnprepareHeader before freeing the buffer }
        PostMessage(PMidiCtlInfo(dwInstance)^.hWindow, mom_Done, 0, dwParam1);
      end;

  end; { Case }
end;

end.

