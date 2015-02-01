(**
 * MIDI Event Streamer
 *
 *   Author: Manuel Kröber <manuel.kroeber@googlmail.com>
 *      Web: http://bitbucket.org/h4ndy/midiio-dev
 *
 * This class can be used to save a MIDI communication into a single file.
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
 * The Original Code is a MIDI Event Streamer class.
 *
 * The Initial Developer of the Original Code is
 * Manuel Kröber <manuel.kroeber@googlmail.com>.
 * Portions created by the Initial Developer are Copyright (C) 2010
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   None so far...
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

unit MidiEventStreamer;

interface

uses
  Classes, SysUtils,

  MidiType;

type
  TStreamMode = (
    smFile, smMemory
  );

  TMidiEventStreamerError = class(Exception);

  TMidiEventStreamer = class(TObject)
  private
    FStream: TStream;
    FStreamMode: TStreamMode;

    FEventCount: Cardinal;
  protected
    function MidiEventToStr(const MidiEvent: TMyMidiEvent): string;
  public
    constructor Create(const Mode: TStreamMode = smMemory; const FileName: string = '');
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(const Stream: TStream);

    procedure AddEvent(const MidiEvent: TMyMidiEvent);

    property StreamMode: TStreamMode read FStreamMode;
  end;

implementation

{ TMidiEventStreamer }

procedure TMidiEventStreamer.AddEvent(const MidiEvent: TMyMidiEvent);
begin

end;

constructor TMidiEventStreamer.Create(const Mode: TStreamMode;
  const FileName: string);
begin
  inherited Create;
  FStreamMode := Mode;

  if Mode = smFile then
  begin
    // Stream to file
    if trim(FileName) = ''  then
      raise TMidiEventStreamerError.Create('No filename for mode smFile given.')
    else
    begin
      FStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    end;
  end
  else
  begin
    // Stream to memory
    FStream := TMemoryStream.Create;
  end;
end;

destructor TMidiEventStreamer.Destroy;
begin
  FStream.Free;

  inherited;
end;

procedure TMidiEventStreamer.LoadFromFile(const FileName: string);
var
  tmpFileStream: TFileStream;
begin
  tmpFileStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(tmpFileStream);
  finally
    tmpFileStream.Free;
  end;
end;

procedure TMidiEventStreamer.LoadFromStream(const Stream: TStream);
begin
  Stream.Position := 0;
  FStream.CopyFrom(Stream, Stream.Size);
end;

function TMidiEventStreamer.MidiEventToStr(
  const MidiEvent: TMyMidiEvent): string;
begin

end;

end.
