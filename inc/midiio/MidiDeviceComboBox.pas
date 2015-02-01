(**
 * TMidiDeviceCombobox
 *
 *    Author: Manuel Kröber
 *      Mail: manuel.kroeber@googlemail.com
 *       Web: http://bitbucket.org/h4ndy/midiio-dev
 *
 * Description:
 * Provides a Combobox for listing MIDI In or Out devices.
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
 * The Original Code is TMidiDeviceCombobox component.
 *
 * The Initial Developer of the Original Code is
 * Manuel Kroeber <manuel.kroeber@googlemail.com>.
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

unit MidiDeviceComboBox;

interface

uses
  SysUtils, Classes, Controls, StdCtrls,
  MidiIn, MidiOut;

type
  TMidiDeviceType = (
    mdtInput, mdtOutput
  );

  TMidiDeviceComboBox = class(TComboBox)
  private
    FDeviceType: TMidiDeviceType;
    procedure SetDeviceType(const Value: TMidiDeviceType);
    function GetHasDevices: Boolean;
    { Private declarations }
  protected
    { Protected declarations }
    procedure SetParent(AParent: TWinControl); override;
    procedure CreateWnd(); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure RefreshDeviceList;
    function SelectedDeviceID: Cardinal;
  published
    { Published declarations }
    property DeviceType: TMidiDeviceType read FDeviceType write SetDeviceType default mdtInput;
    property HasDevices: Boolean read GetHasDevices;
    property Style default csDropDownList;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MIDI I/O', [TMidiDeviceComboBox]);
end;

{ TMidiDeviceComboBox }

constructor TMidiDeviceComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDeviceType := mdtInput;
end;

function TMidiDeviceComboBox.SelectedDeviceID: Cardinal;
begin
  if ItemIndex <> -1 then
    Result := ItemIndex
  else
    Result := 0;
end;

procedure TMidiDeviceComboBox.CreateWnd;
begin
  inherited;
  // workaround for "Control '' has no parent window"
  RefreshDeviceList;
end;

function TMidiDeviceComboBox.GetHasDevices: Boolean;
begin
  Result := Items.Count > 0;
end;

procedure TMidiDeviceComboBox.RefreshDeviceList;
var
  tmpMidiIn: TMidiInput;
  tmpMidiOut: TMidiOutput;
  i: Cardinal;
begin
  Items.Clear;
  Text := '';

  if FDeviceType = mdtInput then
  begin
    tmpMidiIn := TMidiInput.Create(nil);
    try
      if tmpMidiIn.DeviceCount > 0 then
      begin
        for i := 0 to tmpMidiIn.NumDevs - 1 do
        begin
          tmpMidiIn.ChangeDevice(i, False);
          Items.Append(tmpMidiIn.ProductName);
        end;
        ItemIndex := 0;
      end
      else
        Text := 'No MIDI Input Devices found.';
    finally
      tmpMidiIn.Free;
    end;
  end
  else
  begin
    tmpMidiOut := TMidiOutput.Create(nil);
    try
      if tmpMidiOut.DeviceCount > 0 then
      begin
        for i := 0 to tmpMidiOut.NumDevs - 1 do
        begin
          tmpMidiOut.ChangeDevice(i, False);
          Items.Append(tmpMidiOut.ProductName);
        end;
        ItemIndex := 0;
      end
      else
        Text := 'No MIDI Output Devices found.';
    finally
      tmpMidiOut.Free;
    end;
  end;
end;

procedure TMidiDeviceComboBox.SetDeviceType(const Value: TMidiDeviceType);
begin
  if Value <> FDeviceType then
  begin
    FDeviceType := Value;
    RefreshDeviceList;
  end;
end;

procedure TMidiDeviceComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  // workaround for "Control '' has no parent window"
//  RefreshDeviceList;
end;

end.
