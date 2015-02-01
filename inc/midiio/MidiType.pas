{ $Header: /MidiComp/MIDITYPE.PAS 2     10/06/97 7:33 Davec $ }

{ Written by David Churcher <dchurcher@cix.compulink.co.uk>,
  released to the public domain. }

(**
 * MidiType.pas v2010-05r1
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

unit MidiType;

interface

uses Classes, Windows, MMSystem, MidiDefs, Circbuf;

type

  TMidiIO = class(TComponent)
  public
    function DeviceCount: Cardinal; virtual; abstract;
  end;

	{-------------------------------------------------------------------}
	{ A MIDI input/output event }
	TMyMidiEvent = class(TPersistent)
	public
		MidiMessage: Byte;          { MIDI message status byte }
		Data1: Byte;            { MIDI message data 1 byte }
		Data2: Byte;            { MIDI message data 2 byte }
		Time: DWORD;          { Time in ms since midiInOpen }
		SysexLength: Word;  { Length of sysex data (0 if none) }
		Sysex: PAnsiChar;           { Pointer to sysex data buffer }
		destructor Destroy; override;   { Frees sysex data buffer if nec. }
	end;
	PMyMidiEvent = ^TMyMidiEvent;

	{-------------------------------------------------------------------}
	{ Encapsulates the MIDIHDR with its memory handle and sysex buffer }
	PMyMidiHdr = ^TMyMidiHdr;
	TMyMidiHdr = class(TObject)
	public
		hdrHandle: THandle;
		hdrPointer: PMIDIHDR;
		sysexHandle: THandle;
		sysexPointer: Pointer;
		constructor Create(const BufferSize: Word);
		destructor Destroy; override;
	end;

  TMidiChannel = 0..15;

  TMidiChannelNamed = (
    ch00, ch01, ch02, ch03,
    ch04, ch05, ch06, ch07,
    ch08, ch09, ch10, ch11,
    ch12, ch13, ch14, ch15
  );

  TMidiChannels = set of TMidiChannel;

  TFeature = (
    ftCaching, ftStreaming, ftVolume, ftStereoVolume
  );

  TFeatureSet = set of TFeature;

  TMIDIErrorContext = (
    ecGeneric,
    ecCaching,
    ecPutShort,
    ecPutLong,
    ecOutPrepareHeader
  );

  TMidiMsg = (
    msgActiveSensing,
    msgMidiTimeCode
  );

  TMidiMsgFilter = set of TMidiMsg;

  TGMInstrumentPatch = (
    // Piano
    gmiAcousticGrandPiano, gmiBrightAcousticPiano,
    gmiElectricGrandPiano, gmiHonkyTonkPiano,
    gmiElectricPiano1, gmiElectricPiano2,
    gmiHarpsichord, gmiClavi,
    // Chromatic Percussion
    gmiCelesta, gmiGlockenspiel,
    gmiMusicBox, gmiVibraphone,
    gmiMarimba, gmiXylophone,
    gmiTubularBells, gmiDulcimer,
    // Organ
    gmiDrawbarOrgan, gmiPercussiveOrgan,
    gmiRockOrgan, gmiChurchOrgan,
    gmiReedOrgan, gmiAccordion,
    gmiHarmonica, gmiTangoAccordion,
    // Guitar
    gmiAcousticGuitarNylon, gmiAcousticGuitarSteel,
    gmiElectricGuitarJazz, gmiElectricGuitarClean,
    gmiElectricGuitarMuted, gmiOverdrivenGuitar,
    gmiDistortionGuitar, gmiGuitarHarmonics,
    // Bass
    gmiAcousticBass, gmiElectricBassFinger,
    gmiElectricBassPick, gmiFretlessBass,
    gmiSlapBass1, gmiSlapBass2,
    gmiSynthBass1, gmiSynthBass2,
    // Strings
    gmiViolin, gmiViola,
    gmiCello, gmiContrabass,
    gmiTremoloStrings, gmiPizzicatoStrings,
    gmiOrchestralHarp, gmiTimpani,
    // Ensemble
    gmiStringEnsemble1, gmiStringEnsemble2,
    gmiSynthStrings1, gmiSynthString2,
    gmiChoirAahs, gmiVoiceOohs,
    gmiSynthVoice, gmiOrchestraHit,
    // Brass
    gmiTrumpet, gmiTrobone,
    gmiTuba, gmiMutedTrumpet,
    gmiFrenchHorn, gmiBrassSection,
    gmiSynthBrass1, gmiSynthBrass2,
    // Reed
    gmiSopranoSax, gmiAltoSax,
    gmiTenorSax, gmiBaritoneSax,
    gmiOboe, gmiEnglishHorn,
    gmiBassoon, gmiClarinet,
    // Pipe
    gmiPiccolo, gmiFlute,
    gmiRecorder, gmiPanFlute,
    gmiBlownBottle, gmiShakuhachi,
    gmiWhistle, gmiOcarina,
    // Synth Lead
    gmiLead1Square, gmiLead2Sawtooth,
    gmiLead3Calliope, gmiLead4Chiff,
    gmiLead5Charang, gmiLead6Voice,
    gmiLead7Fifths, gmiLead8BassAndLead,
    // Synth Pad
    gmiPad1NewAge, gmiPad2Warm,
    gmiPad3Polysynth, gmiPad4Choir,
    gmiPad5Bowed, gmiPad6Metallic,
    gmiPad7Halo, gmiPad8Sweep,
    // Synth Effects
    gmiFX1Rain, gmiFX2Soundtrack,
    gmiFX3Crystal, gmiFX4Atmosphere,
    gmiFX5Brightness, gmiFX6Goblins,
    gmiFX7Echoes, gmiFX8SciFi,
    // Ethnic
    gmiSitar, gmiBanjo,
    gmiShamisen, gmiKoto,
    gmiKalimba, gmiBagPipe,
    gmiFiddle, gmiShania,
    // Percussive
    gmiTinkleBell, gmiAgogo,
    gmiSteelDrums, gmiWoodblock,
    gmiTaikoDrum, gmiMelodicTom,
    gmiSynthDrum, gmiReverseCymbal,
    // Sound Effects
    gmiGuitarFretNoise, gmiBreathNoise,
    gmiSeashore, gmiBirdTweet,
    gmiTelephoneRing, gmiHelicopter,
    gmiApplause, gmiGunshot
  );

  TGMInstrumentStringMap = array[TGMInstrumentPatch] of string;

  function MIDIInstrumentToStr(const Instrument: TGMInstrumentPatch): string;

implementation

const
  GeneralMidiIstrumentStrings: TGMInstrumentStringMap = (
    // Piano
    'Acoustic Grand Piano', 'Bright Acoustic Piano',
    'Electric Grand Piano', 'Honky-Tonk Piano',
    'Electric Piano 1', 'Electric Piano 2',
    'Harpsichord', 'Clavi',
    // Chromatic Percussion
    'Celesta', 'Glockenspiel',
    'Music Box', 'Vibraphone',
    'Marimba', 'Xylophone',
    'TubularBells', 'Dulcimer',
    // Organ
    'Drawbar Organ', 'Percussive Organ',
    'Rock Organ', 'Church Organ',
    'Reed Organ', 'Accordion',
    'Harmonica', 'Tango Accordion',
    // Guitar
    'Acoustic Guitar (Nylon)', 'Acoustic Guitar (Steel)',
    'Electric Guitar (Jazz)', 'Electric Guitar (Clean)',
    'Electric Guitar (Muted)', 'Overdriven Guitar',
    'Distortion Guitar', 'Guitar Harmonics',
    // Bass
    'Acoustic Bass', 'Electric Bass (Finger)',
    'Electric Bass (Pick)', 'Fretless Bass',
    'Slap Bass 1', 'Slap Bass 2',
    'Synth Bass 1', 'Synth Bass 2',
    // Strings
    'Violin', 'Viola',
    'Cello', 'Contrabass',
    'Tremolo Strings', 'Pizzicato Strings',
    'Orchestral Harp', 'Timpani',
    // Ensemble
    'String Ensemble 1', 'String Ensemble 2',
    'Synth Strings 1', 'Synth String 2',
    'Choir Aahs', 'Voice Oohs',
    'Synth Voice', 'Orchestra Hit',
    // Brass
    'Trumpet', 'Trobone',
    'Tuba', 'Muted Trumpet',
    'French Horn', 'Brass Section',
    'Synth Brass 1', 'Synth Brass2',
    // Reed
    'Soprano Sax', 'Alto Sax',
    'Tenor Sax', 'Baritone Sax',
    'Oboe', 'English Horn',
    'Bassoon', 'Clarinet',
    // Pipe
    'Piccolo', 'Flute',
    'Recorder', 'Pan Flute',
    'Blown Bottle', 'Shakuhachi',
    'Whistle', 'Ocarina',
    // Synth Lead
    'Lead 1 (Square)', 'Lead 2 (Sawtooth)',
    'Lead 3 (Calliope)', 'Lead 4 (Chiff)',
    'Lead 5 (Charang)', 'Lead 6 (Voice)',
    'Lead 7 (Fifths)', 'Lead 8 (Bass + Lead)',
    // Synth Pad
    'Pad 1 (NewAge)', 'Pad 2 (Warm)',
    'Pad 3 (Polysynth)', 'Pad 4 (Choir)',
    'Pad 5 (Bowed)', 'Pad 6 (Metallic)',
    'Pad 7 (Halo)', 'Pad 8 (Sweep)',
    // Synth Effects
    'FX 1 (Rain)', 'FX 2 (Soundtrack)',
    'FX 3 (Crystal)', 'FX 4 (Atmosphere)',
    'FX 5 (Brightness)', 'FX 6 (Goblins)',
    'FX 7 (Echoes)', 'FX 8 (SciFi)',
    // Ethnic
    'Sitar', 'Banjo',
    'Shamisen', 'Koto',
    'Kalimba', 'Bag Pipe',
    'Fiddle', 'Shania',
    // Percussive
    'Tinkle Bell', 'Agogo',
    'Steel Drums', 'Woodblock',
    'Taiko Drum', 'Melodic Tom',
    'Synth Drum', 'Reverse Cymbal',
    // Sound Effects
    'Guitar Fret Noise', 'Breath Noise',
    'Seashore', 'Bird Tweet',
    'Telephone Ring', 'Helicopter',
    'Applause', 'Gunshot'
  );


function MIDIInstrumentToStr(const Instrument: TGMInstrumentPatch): string;
begin
  Result := GeneralMidiIstrumentStrings[Instrument];
end;


{-------------------------------------------------------------------}
{ Free any sysex buffer associated with the event }
destructor TMyMidiEvent.Destroy;
begin
	if (Sysex <> Nil) then
		Freemem(Sysex, SysexLength);

	inherited Destroy;
end;

{-------------------------------------------------------------------}
{ Allocate memory for the sysex header and buffer }
constructor TMyMidiHdr.Create(const BufferSize:Word);
begin
	inherited Create;

	if BufferSize > 0 then
		begin
		hdrPointer := GlobalSharedLockedAlloc(sizeof(TMIDIHDR), hdrHandle);
		sysexPointer := GlobalSharedLockedAlloc(BufferSize, sysexHandle);

		hdrPointer^.lpData := sysexPointer;
		hdrPointer^.dwBufferLength := BufferSize;
		end;
end;

{-------------------------------------------------------------------}
destructor TMyMidiHdr.Destroy;
begin
	GlobalSharedLockedFree( hdrHandle, hdrPointer );
	GlobalSharedLockedFree( sysexHandle, sysexPointer );
	inherited Destroy;
end;



end.
