{ $Header: /MidiComp/MidiOut.pas 3     28/02/01 11:24 Davec $ }

{ Written by David Churcher <dchurcher@cix.compulink.co.uk>,
  released to the public domain. }

{ Thanks very much to Fred Kohler for the Technology code. }

(**
 * MidiOut.pas v2010-05r1
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
 * The Original Code is TMidiOutput class.
 *
 * The Initial Developer of the Original Code is
 * David Churcher <dchurcher@cix.compulink.co.uk>.
 * Portions created by the Initial Developer are Copyright (C) 1997
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   turboPASCAL < http://www.delphipraxis.net/user13047.html >
 *   FAlter < http://www.delphipraxis.net/user7745.html >
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
 
unit MidiOut;

{
  MIDI Output component.

  Properties:
   DeviceID: 	Windows numeric device ID for the MIDI output device.
 Between 0 and (midioutGetNumDevs-1), or MIDI_MAPPER (-1).
    Special value MIDI_MAPPER specifies output to the Windows MIDI mapper
 Read-only while device is open, exception if changed while open

 MIDIHandle:	The output handle to the MIDI device.
 0 when device is not open
 Read-only, runtime-only

 ProductName: Name of the output device product that corresponds to the
 DeviceID property (e.g. 'MPU 401 out').
 You can write to this while the device is closed to select a particular
 output device by name (the DeviceID property will change to match).
 Exception if this property is changed while the device is open.

 NumDevs: Number of MIDI output devices installed on the system. This
 is the value returned by midiOutGetNumDevs. It's included for
 completeness.

    Technology: Type of technology used by the MIDI device. You can set this
    property to one of the values listed for OutportTech (below) and the component
    will find an appropriate MIDI device. For example:
     MidiOutput.Technology := opt_FMSynth;
    will set MidiInput.DeviceID to the MIDI device ID of the FM synth, if one
    is installed. If no such device is available an exception is raised,
    see MidiOutput.SetTechnology.

 See the MIDIOUTCAPS entry in MMSYSTEM.HLP for descriptions of the
 following properties:
  DriverVersion
  Voices
  Notes
  ChannelMask
  Support

 Error: The error code for the last MMSYSTEM error. See the MMSYSERR_
 entries in MMSYSTEM.INT for possible values.

  Methods:
 Open: Open MIDI device specified by DeviceID property for output

 Close: Close device

 PutMidiEvent(Event:TMyMidiEvent): Output a note or sysex message to the
 device. This method takes a TMyMidiEvent object and transmits it.
 Notes:
   1. If the object contains a sysex event the OnMidiOutput event will
    be triggered when the sysex transmission is complete.
   2. You can queue up multiple blocks of system exclusive data for
    transmission by chucking them at this method; they will be
   transmitted as quickly as the device can manage.
   3. This method will not free the TMyMidiEvent object, the caller
    must do that. Any sysex data in the TMyMidiEvent is copied before
   transmission so you can free the TMyMidiEvent immediately after
   calling PutMidiEvent, even if output has not yet finished.

 PutShort(MidiMessage: Byte; Data1: Byte; Data2: Byte): Output a short
 MIDI message. Handy when you can't be bothered to build a TMyMidiEvent.
 If the message you're sending doesn't use Data1 or Data2, set them to 0.

 PutLong(TheSysex: Pointer; msgLength: Word): Output sysex data.
  SysexPointer: Pointer to sysex data to send
  msgLength: Length of sysex data.
 This is handy when you don't have a TMyMidiEvent.

 SetVolume(Left: Word, Right: Word): Set the volume of the
 left and right channels on the output device (only on internal devices?).
 0xFFFF is maximum volume. If the device doesn't support separate
 left/right volume control, the value of the Left parameter will be used.
 Check the Support property to see whether the device supports volume
 control. See also other notes on volume control under midiOutSetVolume()
 in MMSYSTEM.HLP.

  Events:
 OnMidiOutput: Procedure called when output of a system exclusive block
 is completed.

  Notes:
   I haven't implemented any methods for midiOutCachePatches and
  midiOutCacheDrumpatches, mainly 'cause I don't have any way of testing
  them. Does anyone really use these?

  -- Manuel Kroeber/2010
  Added methods for midiOutCache*Patches. Untested. Hinted as experimental.
}

interface

uses
  SysUtils, Classes, Messages, Windows,

  MMSystem,
  CircBuf, MidiType, MidiDefs, MidiCons, MidiCallback, MidiKeyPatchArray;

type
  MidiOutputState = (mosOpen, mosClosed);
  EMidiOutputError = class(Exception);

 { These are the equivalent of constants prefixed with mod_
   as defined in MMSystem. See SetTechnology }
  OutPortTech = (
    opt_None,     { none }
    opt_MidiPort, { output port }
    opt_Synth,    { generic internal synth }
    opt_SQSynth,  { square wave internal synth }
    opt_FMSynth,  { FM internal synth }
    opt_Mapper);  { MIDI mapper }
  TechNameMap = array[OutPortTech] of string;

const
  TechName: TechNameMap = (
    'None', 'MIDI Port', 'Generic Synth', 'Square Wave Synth',
    'FM Synth', 'MIDI Mapper');

{-------------------------------------------------------------------}
type
  TMidiOutput = class(TMidiIO)
  private
    function GetSupportsCaching: Boolean;
    procedure MidiOutput(var Message: TMessage);
    procedure SetDeviceID(DeviceID: Cardinal);
    procedure SetProductName(NewProductName: string);
    procedure SetTechnology(NewTechnology: OutPortTech);
    function MidiOutErrorString(const WError: Cardinal;
      const ErrorContext: TMIDIErrorContext = ecGeneric): string;
    function GetSupportsStreaming: Boolean;
    function GetFeaturesAsSet: TFeatureSet;
    function GetSupportsLRVolCtrl: Boolean;
    function GetSupportsVolControl: Boolean;
  protected
    Handle: THandle;          { Window handle used for callback notification }
    FDeviceID: Cardinal;      { MIDI device ID }
    FMIDIHandle: Hmidiout;    { Handle to output device }
    FState: MidiOutputState;  { Current device state }
    PCtlInfo: PMidiCtlInfo;   { Pointer to control info for DLL }

    PBuffer: PCircularBuffer; { Output queue for PutTimedEvent, set by Open }

    FError: DWord; { Last MMSYSTEM error } //FAlter: DWord statt Word
    FUseFullReset: Boolean;

  { Stuff from midioutCAPS }
    FMID: Word; { Manufacturer ID }
    FPID: Word; { Product ID }
    FDriverVersion: Version;  { Driver version from midioutGetDevCaps }
    FProductName: string;     { product name }
    FTechnology: OutPortTech; { Type of MIDI output device }
    FVoices: Word;            { Number of voices (internal synth) }
    FNotes: Word;             { Number of notes (internal synth) }
    FChannelMask: Word;       { Bit set for each MIDI channels that the
                                device responds to (internal synth) }
    FSupport: DWORD;          { Technology supported (volume control,
                                patch caching etc. }
    FNumDevs: Word;           { Number of MIDI output devices on system }

  { Events }
    FOnMidiOutput: TNotifyEvent; { Sysex output finished }
    FOnDeviceChanged: TNotifyEvent; // after successfully changing the DeviceID

  public
  { Properties }
    property MIDIHandle: Hmidiout read FMIDIHandle;

    property MID: Word read FMID; { Manufacturer ID }
    property PID: Word read FPID; { Product ID }
    property DriverVersion: Version  { Driver version from midioutGetDevCaps }
      read FDriverVersion;
    property Technology: OutPortTech { Type of MIDI output device }
      read FTechnology
      write SetTechnology
      default opt_Synth;
    property Voices: Word  { Number of voices (internal synth) }
      read FVoices;
    property Notes: Word { Number of notes (internal synth) }
      read FNotes;
    property ChannelMask: Word { Bit set for each MIDI channels that the }
      read FChannelMask; { device responds to (internal synth) }
    property Support: DWORD { Technology supported (volume control, }
      read FSupport; { patch caching etc. }

    property Error: DWord read FError; //FAlter DWord statt Word

    property NumDevs: Word read FNumDevs; // Buffered output

    property SupportedFeatures: TFeatureSet read GetFeaturesAsSet;
    // if ftStereoVolume is supported, ftVolume is allways supported, too.
    property SupportsCaching: Boolean read GetSupportsCaching;
    property SupportsStreaming: Boolean read GetSupportsStreaming;
    property SupportsVolumeControl: Boolean read GetSupportsVolControl;
    property SupportsStereoVolumeControl: Boolean read GetSupportsLRVolCtrl;

    property FullResetOnClose: Boolean read FUseFullReset write FUseFullReset;
    property State: MidiOutputState read FState;
    
  { Methods }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Open: Boolean; virtual;
    function Close: Boolean; virtual;
    function ChangeDevice(const NewDeviceID: Cardinal;
      const OpenAfterChange: Boolean = True): Boolean; virtual;

    function DeviceCount: Cardinal; override;

    procedure PutMidiEvent(theEvent: TMyMidiEvent); virtual;
    procedure PutShort(MidiMessage: Byte; Data1: Byte; Data2: Byte); virtual;
    procedure PutLong(const TheSysex: Pointer; const msgLength: Word); virtual;
    function DriverMidiMessage(const Msg: Cardinal; const dw1, dw2: DWORD): DWORD; {$IFDEF VER170}experimental;{$ENDIF}

    procedure SetVolume(Left, Right: Word); overload;
    // right volume is ignored if stereo volume is not supported
		procedure SetVolume(const MonoVolume: Word); overload;
		// use this is you don't care about stereo volume
    procedure GetVolume(var Left, Right: Word); overload;
    // right = left if stereo volume is not supported
    function GetVolume: Word; overload;
    procedure GetVolume(var MonoVolume: Word); overload;
    // use these if you don't care about stereo volume


  { Methods encapsulating PutShort provided for your convenience }
    procedure NoteOn(const Channel, Note: Byte; const Dynamics: Byte = 127);
    procedure NoteOff(const Channel, Note: Byte; const Dynamics: Byte = 127);

    procedure NoteAftertouch(const Channel, Note: Byte;
      const NewDynamics: Byte = 127);
    procedure ChannelAftertouch(const Channel: Byte;
      const NewDynamics: Byte = 127);

    procedure ControllerChange(const Channel, NewController, Value: Byte);
    procedure ProgramChange(const Channel, NewProgram: Byte);
    procedure ChangeInstrument(const Channel: Byte;
      const NewInstrument: TGMInstrumentPatch); // for your convenience


  { Experimental and/or untested stuff }
    // Caching. Returns true if successful. Check LastError on False.
    // Use property "SupportsCaching" for a pre-check.
    function CachePatches(const Bank: Cardinal; var PatchArray: TKeyPatchArray;
      const OperationFlag: Byte): Boolean; {$IFDEF VER170}experimental;{$ENDIF}
    function CacheDrumPatches(const Patch: Cardinal; var KeyArray: TKeyPatchArray;
      const OperationFlag: Byte): Boolean; {$IFDEF VER170}experimental;{$ENDIF}


  { Some functions to decode and classify incoming messages would be nice }

  published
  { TODO: Property editor with dropdown list of product names }
    property ProductName: string read FProductName write SetProductName;

    property DeviceID: Cardinal read FDeviceID write SetDeviceID default 0;

 { Events }
    property OnMidiOutput: TNotifyEvent
      read FOnMidiOutput write FOnMidiOutput;
    property OnDeviceChanged: TNotifyEvent
      read FOnDeviceChanged write FOnDeviceChanged;
  end;

procedure Register;

{-------------------------------------------------------------------}
implementation

constructor TMidiOutput.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FState := mosClosed;
  FNumDevs := midiOutGetNumDevs;
  FUseFullReset := False;

 { Create the window for callback notification }
  if not (csDesigning in ComponentState) then
  begin
    Handle := Classes.AllocateHWnd(MidiOutput);
  end;
end;

{-------------------------------------------------------------------}

destructor TMidiOutput.Destroy;
begin
  if FState = mosOpen then
    Close;
  if (PCtlInfo <> nil) then
    GlobalSharedLockedFree(PCtlinfo^.hMem, PCtlInfo);
  Classes.DeallocateHWnd(Handle);
  inherited Destroy;
end;

function TMidiOutput.DeviceCount: Cardinal;
begin
  FNumDevs := midiOutGetNumDevs;
  Result := FNumDevs;
end;

function TMidiOutput.DriverMidiMessage(const Msg: Cardinal; const dw1,
  dw2: DWORD): DWORD;
begin
  Result := midiOutMessage(HMIDIOUT(FDeviceID), msg, dw1, dw2);
end;

function TMidiOutput.GetFeaturesAsSet: TFeatureSet;
begin
  Result := [];
  // Check for "Device supports caching" flag
  if (FSupport and MIDICAPS_CACHE) = MIDICAPS_CACHE then
    Result := Result + [ftCaching];
  // Check for "Device supports MIDI streaming" flag
  if (FSupport and MIDICAPS_STREAM) = MIDICAPS_STREAM then
    Result := Result + [ftStreaming];
  // Check for "Device supports volume control (Mono)" flag
  if (FSupport and MIDICAPS_VOLUME) = MIDICAPS_VOLUME then
    Result := Result + [ftVolume];
  // Check for "Device supports stereo volume control for left and right" flag
  if (FSupport and MIDICAPS_LRVOLUME) = MIDICAPS_LRVOLUME then
    Result := Result + [ftStereoVolume];
end;

function TMidiOutput.GetSupportsCaching: Boolean;
begin
  Result := (ftCaching in SupportedFeatures);
end;

function TMidiOutput.GetSupportsLRVolCtrl: Boolean;
begin
  Result := (ftStereoVolume in SupportedFeatures);
end;

function TMidiOutput.GetSupportsStreaming: Boolean;
begin
  Result := (ftStreaming in SupportedFeatures);
end;

function TMidiOutput.GetSupportsVolControl: Boolean;
begin
  Result := (ftVolume in SupportedFeatures);
end;

function TMidiOutput.GetVolume: Word;
var
  LVol, RVol: Word;
begin
  GetVolume(LVol, RVol);
  Result := LVol;
end;

procedure TMidiOutput.GetVolume(var MonoVolume: Word);
begin
  MonoVolume:= GetVolume;
end;

procedure TMidiOutput.GetVolume(var Left, Right: Word);
var
  dwVolume: DWORD;
begin
  FError := midiOutGetVolume(DeviceID, @dwVolume);
  if FError <> MMSYSERR_NOERROR then
    raise EMidiOutputError.Create(MidiOutErrorString(FError));

  // Volume is stored as stereo value in the higher and lower WORD of
  // an unsigned DWORD (Cardinal). MSB = left, LSB = right channel.

  // move high WORD to the right & blank high WORD area to get high WORD
  Left := WORD((dwVolume shr 16) and $0000FFFF);
  if SupportsStereoVolumeControl then
    Right := WORD(dwVolume and $0000FFFF) // blank high WORD to get low WORD
  else
    Right := Left;
end;

{-------------------------------------------------------------------}
{ Convert the numeric return code from an MMSYSTEM function to a string
  using midioutGetErrorText. TODO: These errors aren't very helpful
  (e.g. "an invalid parameter was passed to a system function") so
  some proper error strings would be nice.

  MKr: Problem with an enhancement: Many functions share the same error codes
  but give them different meanings. This function needs to know the context
  to output a suitable error message.
  }


function TMidiOutput.MidiOutErrorString(const WError: Cardinal;
  const ErrorContext: TMIDIErrorContext): string;

  function GetGenericErrorMessage(ErrorCode: Cardinal): string;
  var
    errorDesc: PChar;
  begin
    errorDesc := nil;
    try
      errorDesc := StrAlloc(MAXERRORLENGTH);
      if midioutGetErrorText(ErrorCode, errorDesc, MAXERRORLENGTH) = 0 then
        Result := StrPas(errorDesc)
      else
        Result := 'Specified error number is out of range';
    finally
      if errorDesc <> nil then
        StrDispose(errorDesc);
    end;
  end;

var
  SpecificError: string;
begin
  case ErrorContext of
    ecGeneric: Result := GetGenericErrorMessage(WError);
    ecCaching: begin
      case WError of
        MMSYSERR_INVALFLAG: SpecificError := '(Caching) The flag specified by wFlags is invalid.';
        MMSYSERR_INVALHANDLE: SpecificError := '(Caching) The specified device handle is invalid.';
        MMSYSERR_INVALPARAM: SpecificError := '(Caching) The array pointed to by lpPatchArray is invalid.';
        MMSYSERR_NOMEM: SpecificError := '(Caching) The device does not have enough memory to cache all of the requested patches.';
        MMSYSERR_NOTSUPPORTED: SpecificError := '(Caching) The specified device does not support patch caching.';
      end;
    end;
    ecPutShort: begin
      case WError of
        MIDIERR_BADOPENMODE: SpecificError := '(Short Msg) The application sent a message without a status byte to a stream handle.';
        MIDIERR_NOTREADY: SpecificError := '(Short Msg) The hardware is busy with other data.';
        MMSYSERR_INVALHANDLE: SpecificError := '(Short Msg) The specified device handle is invalid.';
      end;
    end;
    ecPutLong: begin
      case WError of
        MIDIERR_NOTREADY: SpecificError := '(Long Msg) The hardware is busy with other data.';
        MIDIERR_UNPREPARED: SpecificError := '(Long Msg) The buffer pointed to by lpMidiOutHdr has not been prepared.';
        MMSYSERR_INVALHANDLE: SpecificError := '(Long Msg) The specified device handle is invalid.';
        MMSYSERR_INVALPARAM: SpecificError := '(Long Msg) The specified pointer or structure is invalid.';
      end;
    end;
    ecOutPrepareHeader: begin
      case WError of
        MMSYSERR_INVALHANDLE: SpecificError := '(Prep Header) The specified device handle is invalid.';
        MMSYSERR_INVALPARAM: SpecificError := '(Prep Header) The specified address is invalid or the given stream buffer is greater than 64K.';
        MMSYSERR_NOMEM: SpecificError := '(Prep Header) The system is unable to allocate or lock memory.';
      end;
    end;
  end;

  // Fallback
  if SpecificError = '' then
    Result := GetGenericErrorMessage(WError)
  else
    Result := SpecificError;
end;

{-------------------------------------------------------------------}
{ Set the output device ID and change the other properties to match }

procedure TMidiOutput.SetDeviceID(DeviceID: Cardinal);
var
  midioutCaps: TmidioutCaps;
begin
  if FState = mosOpen then
    raise EMidiOutputError.Create('Change to DeviceID while device was open')
  else
    if (DeviceID >= midioutGetNumDevs) and
      (DeviceID <> MIDI_MAPPER) then
      raise EMidiOutputError.Create('Invalid device ID')
    else
    begin
      FDeviceID := DeviceID;

   { Set the name and other midioutCAPS properties to match the ID }
      FError := midioutGetDevCaps(
        DeviceID, //FAlter wohl in der MMSystem falscher Typ? -1=Default
        // MKr: Reverted to Cardinal type.
        // DeviceID is declared as Unsigned Integer in MSDN and MIDI MAPPER
        // default device as UINT(-1). Whatever this is producing, it works.
        // Use const MIDI_MAPPER or MIDIMAPPER found in MidiCons or MMSystem.
        @midioutCaps,
        sizeof(TmidioutCaps)
      );
      if Ferror > 0 then
        raise EMidiOutputError.Create(MidiOutErrorString(FError));

      with midiOutCaps do
      begin
        FMID := wMid;
        FPID := wPid;
        FProductName := StrPas(szPname);
        FDriverVersion := vDriverVersion;
        FTechnology := OutPortTech(wTechnology);
        FVoices := wVoices;
        FNotes := wNotes;
        FChannelMask := wChannelMask;
        FSupport := dwSupport;
      end;

      if Assigned(FOnDeviceChanged) then
        FOnDeviceChanged(Self);
    end;
end;

{-------------------------------------------------------------------}
{ Set the product name property and put the matching output device number
  in FDeviceID.
  This is handy if you want to save a configured output/output device
  by device name instead of device number, because device numbers may
  change if users install or remove MIDI devices.
  Exception if output device with matching name not found,
  or if output device is open }

procedure TMidiOutput.SetProductName(NewProductName: string);
var
  midioutCaps: TmidioutCaps;
  testDeviceID: Integer;
  testProductName: string;
begin
  if FState = mosOpen then
    raise EMidiOutputError.Create('Change to ProductName while device was open')
  else
  { Don't set the name if the component is reading properties because
  the saved Productname will be from the machine the application was compiled
  on, which may not be the same for the corresponding DeviceID on the user's
  machine. The FProductname property will still be set by SetDeviceID }
    if not (csLoading in ComponentState) then
    begin
    { Loop uses -1 to test for MIDI_MAPPER as well }
      for testDeviceID := -1 to (midioutGetNumDevs - 1) do
      begin
        FError :=
          midioutGetDevCaps(testDeviceID, @midioutCaps, sizeof(TmidioutCaps));
        if Ferror > 0 then
          raise EMidiOutputError.Create(MidiOutErrorString(FError));
        testProductName := StrPas(midioutCaps.szPname);
        if testProductName = NewProductName then
        begin
          FProductName := NewProductName;
          Break;
        end;
      end;
      if FProductName <> NewProductName then
        raise EMidiOutputError.Create('MIDI output Device ' +
          NewProductName + ' not installed')
      else
        SetDeviceID(testDeviceID);
    end;
end;

{-------------------------------------------------------------------}
{ Set the output technology property and put the matching output device
 number in FDeviceID.
  This is handy, for example, if you want to be able to switch between a
  sound card and a MIDI port }

procedure TMidiOutput.SetTechnology(NewTechnology: OutPortTech);
var
  midiOutCaps: TMidiOutCaps;
  testDeviceID: Integer;
  testTechnology: OutPortTech;
begin
  if FState = mosOpen then
    raise EMidiOutputError.Create(
      'Change to Product Technology while device was open')
  else
  begin
    { Loop uses -1 to test for MIDI_MAPPER as well }
    for testDeviceID := -1 to (midiOutGetNumDevs - 1) do
    begin
      // get device info
      FError := midioutGetDevCaps(testDeviceID, @midioutCaps,
        sizeof(TmidioutCaps));

      if FError > 0 then
        raise EMidiOutputError.Create(MidiOutErrorString(FError));

      // translate to OutPortTech
      testTechnology := OutPortTech(midioutCaps.wTechnology);
      // and test for support
      if testTechnology = NewTechnology then
      begin
        FTechnology := NewTechnology;
        Break;
      end;
    end;
    if FTechnology <> NewTechnology then
      raise EMidiOutputError.Create
        ('MIDI output technology ' + TechName[NewTechnology]
          + ' not installed')
    else
      // switch to new device if tech was found
      SetDeviceID(testDeviceID);
  end;
end;

procedure TMidiOutput.SetVolume(const MonoVolume: Word);
begin
  SetVolume(MonoVolume, MonoVolume);
end;

{-------------------------------------------------------------------}

function TMidiOutput.Open: Boolean;
var
  hMem: THandle;
begin
  Result := False;
  try
  { Create the control info for the DLL }
    if (PCtlInfo = nil) then
    begin
      PCtlInfo := GlobalSharedLockedAlloc(Sizeof(TMidiCtlInfo), hMem);
      PctlInfo^.hMem := hMem;
    end;

    Pctlinfo^.hWindow := Handle; { Control's window handle }

    FError := midioutOpen(@FMidiHandle,
      Cardinal(FDeviceId),
      DWORD(@midiHandler),
      DWORD(PCtlInfo),
      CALLBACK_FUNCTION);

    if (FError <> 0) then
   { TODO: use CreateFmtHelp to add MIDI device name/ID to message }
      raise EMidiOutputError.Create(MidiOutErrorString(FError))
    else
    begin
      Result := True;
      FState := mosOpen;
    end;

  except
    if PCtlInfo <> nil then
    begin
      GlobalSharedLockedFree(PCtlInfo^.hMem, PCtlInfo);
      PCtlInfo := nil;
    end;
  end;

end;

{-------------------------------------------------------------------}

procedure TMidiOutput.PutShort(MidiMessage: Byte; Data1: Byte; Data2: Byte);
var
  thisMsg: DWORD;
begin
  if FState = mosOpen then
  begin
    thisMsg := DWORD(MidiMessage) or
      (DWORD(Data1) shl 8) or
      (DWORD(Data2) shl 16);

    FError := midiOutShortMsg(FMidiHandle, thisMsg);
    if Ferror > 0 then
      raise EMidiOutputError.Create(MidiOutErrorString(FError, ecPutShort));
  end
  else
    raise EMidiOutputError.Create('(Short Msg) Device not opened.');
end;

{-------------------------------------------------------------------}

procedure TMidiOutput.PutLong(const TheSysex: Pointer; const msgLength: Word);
{ Notes: This works asynchronously; you send your sysex output by
calling this function, which returns immediately. When the MIDI device
driver has finished sending the data the MidiOutPut function in this
component is called, which will in turn call the OnMidiOutput method
if the component user has defined one. }
{ TODO: Combine common functions with PutTimedLong into subroutine }

// MKr: Does anyone know what's meant with PutTimedLong?
// Can't find any timed WinAPI MIDI stuff on the internet...

var
  MyMidiHdr: TMyMidiHdr;
begin
  if FState = mosOpen then
  begin
   { Initialize the header and allocate buffer memory }
    MyMidiHdr := TMyMidiHdr.Create(msgLength);

   { Copy the data over to the MidiHdr buffer
     We can't just use the caller's PChar because the buffer memory
     has to be global, shareable, and locked. }
    CopyMemory(MyMidiHdr.SysexPointer, TheSysex, msgLength);

   { Store the MyMidiHdr address in the header so we can find it again quickly
        (see the MidiOutput proc) }
    MyMidiHdr.hdrPointer^.dwUser := DWORD(MyMidiHdr);

   { Get MMSYSTEM's blessing for this header }
    FError := midiOutPrepareHeader(FMidiHandle, MyMidiHdr.hdrPointer,
      sizeof(TMIDIHDR));
    if Ferror > 0 then
      raise EMidiOutputError.Create(MidiOutErrorString(FError, ecOutPrepareHeader));

   { Send it }
    FError := midiOutLongMsg(FMidiHandle, MyMidiHdr.hdrPointer,
      sizeof(TMIDIHDR));
    if Ferror > 0 then
      raise EMidiOutputError.Create(MidiOutErrorString(FError, ecPutLong));
  end
  else
    raise EMidiOutputError.Create('(Long Msg) Device not opened.');
end;

{-------------------------------------------------------------------}

procedure TMidiOutput.PutMidiEvent(theEvent: TMyMidiEvent);
begin
  if FState <> mosOpen then
    raise EMidiOutputError.Create('MIDI Output device not open');

  if theEvent.Sysex = nil then
  begin
    PutShort(theEvent.MidiMessage, theEvent.Data1, theEvent.Data2)
  end
  else
    PutLong(theEvent.Sysex, theEvent.SysexLength);
end;

{-------------------------------------------------------------------}

procedure TMidiOutput.NoteAftertouch(const Channel, Note, NewDynamics: Byte);
begin
  // See NoteOff for details
  PutShort(MIDI_KEYAFTERTOUCH or ($0F and Channel), Note, NewDynamics);
end;

function TMidiOutput.CacheDrumPatches(const Patch: Cardinal;
  var KeyArray: TKeyPatchArray; const OperationFlag: Byte): Boolean;
begin
  FError := midiOutCacheDrumPatches(FMIDIHandle, Patch, @KeyArray,
    OperationFlag);

  if FError <> MMSYSERR_NOERROR then
  begin
    Result := False;
    //raise EMidiOutputError.Create(MidiOutErrorString(FError));
  end
  else
    Result := True;
end;

function TMidiOutput.CachePatches(const Bank: Cardinal;
  var PatchArray: TKeyPatchArray; const OperationFlag: Byte): Boolean;
begin
  FError := midiOutCachePatches(FMIDIHandle, Bank, @PatchArray, OperationFlag);

  if FError <> MMSYSERR_NOERROR then
  begin
    Result := False;
    //raise EMidiOutputError.Create(MidiOutErrorString(FError));
  end
  else
    Result := True;
end;

function TMidiOutput.ChangeDevice(const NewDeviceID: Cardinal;
  const OpenAfterChange: Boolean): Boolean;
begin
  Result := False;
  
  if FState <> mosClosed then
    Close;

  if FState = mosClosed then
  begin
    DeviceID := NewDeviceID;
    if OpenAfterChange then
      Result := Open;
  end;
end;

procedure TMidiOutput.ChangeInstrument(const Channel: Byte;
  const NewInstrument: TGMInstrumentPatch);
begin
  ProgramChange(Channel, Byte(NewInstrument));
end;

procedure TMidiOutput.ChannelAftertouch(const Channel, NewDynamics: Byte);
begin
  // See NoteOff for details
  PutShort(MIDI_CHANAFTERTOUCH or ($0F and Channel), NewDynamics, $00);
end;

procedure TMidiOutput.ProgramChange(const Channel, NewProgram: Byte);
begin
  // See NoteOff for details
  PutShort(MIDI_PROGRAMCHANGE or ($0F and Channel), NewProgram, $00);
end;

function TMidiOutput.Close: Boolean;
begin
  Result := False;
  if FState = mosOpen then
  begin
    if FUseFullReset then
    begin
      // Note: this sends a lot of fast control change messages which some\
      // synths can't handle thus it's off by default.
      FError := midioutReset(FMidiHandle);

      if FError <> 0 then
        raise EMidiOutputError.Create(MidiOutErrorString(FError));
    end;

    FError := midioutClose(FMidiHandle);
    if Ferror <> 0 then
      raise EMidiOutputError.Create(MidiOutErrorString(FError))
    else
      Result := True;
  end;

  FMidiHandle := 0;
  FState := mosClosed;
end;

procedure TMidiOutput.ControllerChange(const Channel, NewController,
  Value: Byte);
begin
  // See NoteOff for details
  PutShort(MIDI_CONTROLCHANGE or ($0F and Channel), NewController, Value);
end;

{-------------------------------------------------------------------}

procedure TMidiOutput.SetVolume(Left, Right: Word);
var
  dwVolume: DWORD;
begin
  if not SupportsStereoVolumeControl then
    Right := Left;

  dwVolume := (DWORD(Left) shl 16) or Right;
  FError := midiOutSetVolume(DeviceID, dwVolume);
  if FError <> 0 then
    raise EMidiOutputError.Create(MidiOutErrorString(FError));
end;

{-------------------------------------------------------------------}

procedure TMidiOutput.MidiOutput(var Message: TMessage);
{ Triggered when sysex output from PutLong is complete }
var
  MyMidiHdr: TMyMidiHdr;
  thisHdr: PMidiHdr;
begin
  if Message.Msg = MOM_DONE then
  begin
  { Find the MIDIHDR we used for the output. Message.lParam is its address }
    thisHdr := PMidiHdr(Message.lParam);

  { Remove it from the output device }
    midiOutUnprepareHeader(FMidiHandle, thisHdr, sizeof(TMIDIHDR));

  { Get the address of the MyMidiHdr object containing this MIDIHDR structure.
   We stored this address in the PutLong procedure }
    MyMidiHdr := TMyMidiHdr(thisHdr^.dwUser);

  { Header and copy of sysex data no longer required since output is complete }
    MyMidiHdr.Free;

  { Call the user's event handler if any }
    if Assigned(FOnMidiOutput) then
      FOnMidiOutput(Self);
  end;
 { TODO: Case for MOM_PLAYBACK_DONE }
end;

procedure TMidiOutput.NoteOff(const Channel, Note: Byte;
  const Dynamics: Byte);
begin
  // ($0F and Channel) kills the upper 4 bits of the byte to 0000 to ensure the
  // OR-operation works fine to "build" the Note command. The note event just
  // recognizes 16 channels ($00 to $0F). This way you can input any Byte
  // value. Note that using values > 15 results in an overflow behavior
  // (h10/d16 is channel 0, h11/d17 is channel 1, h12/d18 channel 2, a.s.o)
  PutShort(MIDI_NOTEOFF or ($0F and Channel), Note, Dynamics);
end;

procedure TMidiOutput.NoteOn(const Channel, Note: Byte;
  const Dynamics: Byte);
begin
  // See NoteOff for details
  PutShort(MIDI_NOTEON or ($0F and Channel), Note, Dynamics);
end;

{-------------------------------------------------------------------}

procedure Register;
begin
  RegisterComponents('MIDI I/O', [TMidiOutput]);
end;

end.
