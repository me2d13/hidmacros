(**
 * MidiKeyPatchArray.pas v2010-05r1
 *
 *    Author: Manuel Kröber
 *      Mail: manuel.kroeber@googlemail.com
 *       Web: http://saso-labs.com/midi/
 *
 * Description:
 * Provides TKeyPatchArray type and helpful functions for it.
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
 * The Original Code is TKeyPatchArray type code.
 *
 * The Initial Developer of the Original Code is
 * Manuel Kroeber <manuel.kroeber@googlemail.com>.
 * Portions created by the Initial Developer are Copyright (C) 2010
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
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

unit MidiKeyPatchArray;

interface

uses MidiType, MMsystem;

type
  // For patch caching.
  // http://msdn.microsoft.com/en-us/library/dd757122(VS.85).aspx
  // Each array element represents a key with 16 midi channels (word = 16 bit).
  // The bit represents on or off of the channel for this key for called
  // command, e.g. drum patch caching. Defined KEYARRAY and PATCHARRAY are equal
  TKeyPatchArray = array[0..(MIDIPATCHSIZE-1)] of Word;

  // Used to easy enable channels in given patch number in a PatchArray.
  // Use ChannelsToEnable = [] or omit to disable all channels from given patch.
  function SetPatchChannels(var PatchArray: TKeyPatchArray; const PatchNumber: Byte;
    const ChannelsToEnable: TMidiChannels = []): Boolean;

		
implementation


function SetPatchChannels(var PatchArray: TKeyPatchArray; const PatchNumber: Byte;
  const ChannelsToEnable: TMidiChannels): Boolean;
var
  CurChannel: TMidiChannel;
begin
  if high(PatchArray) < PatchNumber then
  begin
    Result := True;
    PatchArray[PatchNumber] := 0; // Reset

    for CurChannel := low(TMidiChannel) to high(TMidiChannel) do
    begin
      if (CurChannel in ChannelsToEnable) then
        PatchArray[PatchNumber] := (
          PatchArray[PatchNumber] or (1 shl CurChannel)
        );
    end;
  end
  else
  begin
    Result := False;
  end;
end;

end.
