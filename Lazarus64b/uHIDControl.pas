unit uHIDControl;

interface
uses Classes, KbdMacro, XMLIntf, uGameDevices, uMidiDevices, Windows;


type
  THIDControl = class (TObject)
  private
    fDevices: TList;
    fGameControl: TGameControl;
    fMidiControl: TMidiControl;
    fKeyboardsCount: Integer;
    fMousesCount: Integer;
    fGameCount: Integer;
    fMidiCount: Integer;
    fWindowHandle: HWND; // handle of CfgWindow - where to sent messages
    function GetItems(Index: Integer): THIDKeyboard;
    procedure DebugLog(pMessage: String);
    function GetGameAvailable: Boolean;
    function GetCount: Integer;
    procedure SetWindowHandle(const Value: HWND);
  public
    constructor Create;
    procedure Init;
    function GetDevice(pName: String): THIDKeyboard;
    function GetDeviceBySysId(pName: String): THIDKeyboard;
    destructor Destroy; Override;
    function DetectDevices: Integer;
    procedure AssignDefaultNames;
    procedure AddGameDevice(pDevice: THIDJoystick);
    procedure AddMidiDevice(pDevice: TMIDIDevice);
    function LoadFromXml(parent : IXMLNode): Boolean;
    procedure SaveToXML(parent : IXMLNode);
    property GameAvailable: Boolean read GetGameAvailable;
    property Items[Index: Integer]: THIDKeyboard read GetItems;
    property Count: Integer read GetCount;
    property KeyboardsCount: Integer read fKeyboardsCount;
    property MousesCount: Integer read fMousesCount;
    property GameCount: Integer read fGameCount;
    property MidiCount: Integer read fMidiCount;
    property WindowHandle: HWND read fWindowHandle write SetWindowHandle;
  end;

implementation

uses SysUtils,
     uRawInput,
     uGlobals;

procedure THIDControl.AddGameDevice(pDevice: THIDJoystick);
begin
  fDevices.Add(pDevice);
  Inc(fGameCount);
end;

procedure THIDControl.AddMidiDevice(pDevice: TMIDIDevice);
begin
  fDevices.Add(pDevice);
  Inc(fMidiCount);
end;

procedure THIDControl.AssignDefaultNames;
var
  I: Integer;
  lPrefix: String;
  lCnt: Integer;
  lDev: THIDKeyboard;
begin
  for I := 0 to fDevices.Count - 1 do
  begin
    lDev := THIDKeyboard(fDevices[I]);
    if lDev.Name = '' then
    begin
      if lDev is THIDMouse then
        lPrefix := 'Mouse'
      else if lDev is TMIDIDevice then
        lPrefix := 'Midi'
      else if lDev is THIDKeyboard then
        lPrefix := 'Keyb'
      else if lDev is THIDJoystick then
        lPrefix := 'Game'
      else
        lPrefix := 'Dev';
      lCnt := 1;
      while (GetDevice(lPrefix + IntToStr(lCnt)) <> nil) do
        Inc(lCnt);
      lDev.Name := lPrefix + IntToStr(lCnt);
    end;
  end;
end;

constructor THIDControl.Create;
begin
  fDevices := TList.Create;
  fGameControl := TGameControl.Create;
  fGameControl.OnNewDevice := AddGameDevice;
  fMidiControl := TMidiControl.Create;
  fMidiControl.OnNewDevice := AddMidiDevice;
  fKeyboardsCount := 0;
  fMousesCount := 0;
  fGameCount := 0;
  fMidiCount := 0;
end;

procedure THIDControl.DebugLog(pMessage: String);
begin
  Glb.DebugLog(pMessage, 'HID');
end;

destructor THIDControl.Destroy;
begin
  fGameControl.Free;
  fMidiControl.Free;
  fDevices.Free;
  inherited;
end;

function THIDControl.DetectDevices: Integer;
var
  deviceCount, StrLen, TmpSize: UINT;
  //devicesHID: array of RAWINPUTDEVICELIST;
  pDevicesHID: PRAWINPUTDEVICELIST;
  pDevice: PRAWINPUTDEVICELIST;
  pDeviceName: PChar;
  I: Integer;
  pDeviceInfo: PRID_DEVICE_INFO;
  newKbd: THIDKeyboard;
  newMou: THIDMouse;
begin
  pDeviceInfo := nil;
  pDevicesHID := nil;
  deviceCount := 0;
  if (GetRawInputDeviceList(nil, deviceCount, sizeof(RAWINPUTDEVICELIST)) = 0) then
  begin
    try
      GetMem(pDevicesHID, deviceCount * sizeOf(RAWINPUTDEVICELIST));
      GetMem(pDeviceInfo, sizeOf(RID_DEVICE_INFO));
      pDevice := pDevicesHID;
      GetRawInputDeviceList(pDevicesHID, deviceCount, sizeof(RAWINPUTDEVICELIST));
      begin
        // process the list
        strLen := 0;
        for I := 0 to deviceCount - 1 do
        begin
          if (GetRawInputDeviceInfo(pDevice^.hDevice, RIDI_DEVICENAME,
              nil, StrLen) = 0) then
          begin
            GetMem(pDeviceName, StrLen + 1);
            try
              GetRawInputDeviceInfo(pDevice^.hDevice, RIDI_DEVICENAME,
                  pDeviceName, StrLen);
              TmpSize := sizeof(RID_DEVICE_INFO);
              pDeviceInfo^.cbSize := TmpSize;
              GetRawInputDeviceInfo(pDevice^.hDevice, RIDI_DEVICEINFO,
                  pDeviceInfo, TmpSize);
              if (pDeviceInfo^.dwType = RIM_TYPEKEYBOARD) and (strpos(strUpper(pDeviceName), 'ROOT') = nil) then
              begin
                // add string to log
                DebugLog('Found keyboard: ' + IntToStr(pDevice^.hDevice) + ': ' + pDeviceName);
                // create kbd object
                newKbd := THIDKeyboard.Create(pDeviceName, pDevice^.hDevice);
                newKbd.Name := '';
                fDevices.Add(newKbd);
                Inc(fKeyboardsCount);
              end else if (pDeviceInfo^.dwType = RIM_TYPEMOUSE) and (strpos(strUpper(pDeviceName), 'ROOT') = nil) then
              begin
                // add string to log
                DebugLog('Found mouse: ' + IntToStr(pDevice^.hDevice) + ': ' + pDeviceName);
                // create kbd object
                newMou := THIDMouse.Create(pDeviceName, pDevice^.hDevice);
                newMou.Name := '';
                fDevices.Add(newMou);
                Inc(fMousesCount);
              end
            finally
              FreeMem(pDeviceName);
            end;
          end;
          Inc(pDevice);
        end;
      end;
    finally
      //devicesHID := nil;
      FreeMem(pDevicesHID);
      FreeMem(pDeviceInfo);
    end;
  end;
  Result := deviceCount;
end;

function THIDControl.GetCount: Integer;
begin
  Result := fDevices.Count;
end;

function THIDControl.GetDevice(pName: String): THIDKeyboard;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to fDevices.Count - 1 do
    if (THIDKeyboard(fDevices[I]).Name = pName) then
    begin
      Result := THIDKeyboard(fDevices[I]);
      break;
    end;
end;

function THIDControl.GetDeviceBySysId(pName: String): THIDKeyboard;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to fDevices.Count - 1 do
    if (THIDKeyboard(fDevices[I]).SystemID = pName) then
    begin
      Result := THIDKeyboard(fDevices[I]);
      break;
    end;
end;

function THIDControl.GetGameAvailable: Boolean;
begin
  Result := fGameControl.Available;
end;

function THIDControl.GetItems(Index: Integer): THIDKeyboard;
begin
  if (Index < 0) or (Index > fDevices.Count - 1) then
    Result := nil
  else
    Result := THIDKeyboard(fDevices[Index]);
end;

procedure THIDControl.Init;
begin
  fKeyboardsCount := 0;
  fMousesCount := 0;
  fGameCount := 0;
  fMidiCount := 0;
  fGameControl.InitDirectX;
  fMidiControl.Init;
end;

function THIDControl.LoadFromXml(parent: IXMLNode): Boolean;
var
  parentNode, aNode: IXMLNode;
  J :Integer;
  tmpSysID, tmpName, lDevType: String;
  lDev: THIDKeyboard;
begin
  parentNode := parent.ChildNodes.First;
  while parentNode <> nil do
  begin
    lDevType := UpperCase(parentNode.NodeName);
    if (lDevType = 'KEYBOARD') or
       (lDevType = 'MOUSE') or
       (lDevType = 'GAME') then
    begin
      tmpSysID := '';
      tmpName := '';
      aNode := parentNode.ChildNodes.First;
      while aNode <> nil do
      begin
        if UpperCase(aNode.NodeName) = 'SYSTEMID' then
          tmpSysID := aNode.Text
        else if UpperCase(aNode.NodeName) = 'NAME' then
          tmpName := aNode.Text;
        aNode := aNode.NextSibling;
      end;
      lDev := GetDeviceBySysId(tmpSysID);
      if (lDev <> nil) then
        lDev.Name := tmpName
      else
      begin
        // create "dead" keyboard - just loaded, but without real systemId (handle = 0)
        if (lDevType = 'KEYBOARD') then
        begin
          lDev := THIDKeyboard.Create(tmpSysID, 0);
          Inc(fKeyboardsCount);
        end;
        if (lDevType = 'MOUSE') then
        begin
          lDev := THIDMouse.Create(tmpSysID, 0);
          Inc(fMousesCount);
        end;
        if (lDevType = 'GAME') then
        begin
          lDev := THIDJoystick.Create(tmpSysID, 0);
          Inc(fGameCount);
        end;
        if (lDev <> nil) then // should be always
        begin
          lDev.Name := tmpName;
          fDevices.Add(lDev);
        end;
      end;
    end;
    parentNode := parentNode.NextSibling;
  end;
end;

procedure THIDControl.SaveToXML(parent: IXMLNode);
var
  kb : THIDKeyboard;
  I: Integer;
begin
  // save keyboards
  for I := 0 to fDevices.Count - 1 do
  begin
    kb := THIDKeyboard(fDevices[I]);
    if (kb is THIDMouse) then
      kb.SaveToXml(parent, 'Mouse')
    else if (kb is THIDJoystick) then
      kb.SaveToXml(parent, 'Game')
    else if (kb is TMIDIDevice) then
      kb.SaveToXml(parent, 'Midi')
    else
      kb.SaveToXml(parent, 'Keyboard');
  end;
end;

procedure THIDControl.SetWindowHandle(const Value: HWND);
begin
  fWindowHandle := Value;
  //fMidiControl.WindowHandle := Value;
end;

end.
