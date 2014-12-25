unit uMidiDevices;

interface

uses KbdMacro,
  Windows, Messages, Classes, MidiIn, MidiType, MidiCons;

const

  WM_MIDI_KEY = WM_USER + 304;


type

  TMIDIDevice = class (THIDKeyboard)
  private
    fMidiIn: TMidiInput;
    procedure ReceivedMidiInput(Sender: TObject);
  public
    constructor Create(pSystemID: string; pHandle: HWND);
    destructor Destroy; Override;
    procedure Open;
  end;

  TNewMidiDeviceCallback = procedure(pDev: TMIDIDevice) of object;

  TMidiControl = class (TObject)
  private
    fDevices: TList;
    fOnNewDevice: TNewMidiDeviceCallback;
    function GetAvailable: Boolean;
  public
    constructor Create;
    procedure Init;
    procedure DebugLog(Value: String);
    function GetDevice(pName: String): TMIDIDevice;
    destructor Destroy; Override;
    property OnNewDevice: TNewMidiDeviceCallback read fOnNewDevice write fOnNewDevice;
    property Available: Boolean read GetAvailable;
  end;



implementation

uses uGlobals, Forms, SysUtils;

{ TMidiControl }

function TMidiControl.GetAvailable: Boolean;
begin
  Result := True;
end;

function TMidiControl.GetDevice(pName: String): TMIDIDevice;
var I: Integer;
begin
  Result := nil;
  for I := 0 to fDevices.Count - 1 do
    if UpperCase(TMIDIDevice(fDevices[I]).Name) = UpperCase(pName) then
    begin
      Result := TMIDIDevice(fDevices[I]);
      break;
    end;
end;

procedure TMidiControl.Init;
var
  I : Integer;
  lMidiDevice: TMIDIDevice;
  lMidiIn: TMidiInput;
begin
  // get devices
  lMidiIn := TMidiInput.Create(nil);
  try
    if lMidiIn.DeviceCount > 0 then
    begin
      for I := 0 to lMidiIn.DeviceCount - 1 do
      begin
        lMidiIn.DeviceID := I;
        lMidiDevice := TMIDIDevice.Create(lMidiIn.ProductName+' (ID '+IntToStr(i)+')', i+1);
        fDevices.Add(lMidiDevice);
        if Assigned(fOnNewDevice) then
          fOnNewDevice(lMidiDevice);
      end;
    end;
    lMidiIn.StopAndClose;
  finally
    lMidiIn.Free;
  end;
  for I := 0 to fDevices.Count - 1 do
    TMIDIDevice(fDevices[I]).Open;
end;

constructor TMidiControl.Create;
begin
  fDevices := TList.Create;
end;

procedure TMidiControl.DebugLog(Value: String);
begin
  Glb.DebugLog(Value, 'MIDI');
end;

destructor TMidiControl.Destroy;
var
  I : Integer;
begin
  //for I := 0 to fDevices.Count - 1 do
  //  TMIDIDevice(fDevices[I]).Free;
  fDevices.Free; // each devices are destroyed from form
  inherited;
end;

{ TMIDIDevice }

constructor TMIDIDevice.Create(pSystemID: string; pHandle: HWND);
begin
  inherited;
  if IsAlive then // which means handle > 0
  begin
    fMidiIn := TMidiInput.Create(nil);
    fMidiIn.DeviceID := pHandle - 1;
    fMidiIn.OnMidiInput := ReceivedMidiInput; 
 end
  else
    fMidiIn := nil;
end;

destructor TMIDIDevice.Destroy;
begin
  if fMidiIn <> nil then
  begin
    fMidiIn.StopAndClose;
    fMidiIn.Free;
  end;
  inherited;
end;

procedure TMIDIDevice.Open;
begin
  if fMidiIn <> nil then
    fMidiIn.OpenAndStart;
end;

procedure TMIDIDevice.ReceivedMidiInput(Sender: TObject);
var
  lMidiEvent: TMyMidiEvent;
begin
  while fMidiIn.MessageCount > 0 do
  begin
    lMidiEvent := fMidiIn.GetMidiEvent;
    PostMessage(Glb.MainForm.Handle, WM_MIDI_KEY,
        Integer(self), Integer(lMidiEvent));
  end;
end;

end.
