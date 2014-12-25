unit uGameDevices;

interface

uses KbdMacro,
  Windows, DirectInput, Messages, Classes;

const

  WM_JOY_BUTTON_DOWN = WM_USER + 302;
  WM_JOY_BUTTON_UP   = WM_USER + 303;


type

  THIDJoystick = class (THIDKeyboard)
  private
    fButtonsCount: Integer;
    fRegisteredProcs: TStrings;
    fDIDevice: IDIRECTINPUTDEVICE8;
    fJoyState: DIJOYSTATE2;
    fAxisTrigger: DIJOYSTATE2;
    fLastAxis: DIJOYSTATE2;
    fAxisTickSkip: Integer;
    fCurrentAxisTick: Integer;
    procedure DoAxisCall(pProcName, pAxisName: String; pValue: LongInt);
    function GetAxisPointer(pIn: PDIJoyState2; pAxisName: String): PLongInt;
  public
    constructor Create(pSystemID: string; pHandle: HWND);
    destructor Destroy; Override;
    procedure GenerateEvents(Handle: HWND);
    procedure GetButtons;
    function GetButton(pNumber: Integer): Shortint;
    function GetAxis(pName: String): LongInt;
    procedure RegisterAxisEvent(pAxis, pProcName: String; pDelta: Integer);
    procedure UnRegisterAxisEvent(pAxis: String);
    property ButtonsCount : Integer read fButtonsCount write fButtonsCount;
    property DIDevice : IDIRECTINPUTDEVICE8 read fDIDevice write fDIDevice;
    property JoyState : DIJOYSTATE2 read fJoyState write fJoyState;
    property AxisTickSkip: Integer read fAxisTickSkip write fAxisTickSkip;
  end;

  TNewGameDeviceCallback = procedure(pDev: THIDJoystick) of object;

  TGameControl = class (TObject)
  private
    fDevices: TList;
    fOnNewDevice: TNewGameDeviceCallback;
    DInput : IDIRECTINPUT8; //DirectInput
    procedure AddGameDevice(pDeviceName: String; Data: IDIRECTINPUTDEVICE8; pGUID: TGUID);
    function GUID2Str(pGUID: TGUID): String;
    function GetAvailable: Boolean;
  public
    constructor Create;
    procedure InitDirectX;
    procedure DebugLog(Value: String);
    function GetDevice(pName: String): THIDJoystick;
    destructor Destroy; Override;
    property OnNewDevice: TNewGameDeviceCallback read fOnNewDevice write fOnNewDevice;
    property Available: Boolean read GetAvailable;
  end;



implementation

uses uGlobals, Forms, SysUtils, ActiveX, Variants;

function EnumJoysticksCallback(const lpddi: TDIDeviceInstanceA;
  pvRef: Pointer): HRESULT; stdcall;
var logStr: String;
    DI_JoyDevice : IDIRECTINPUTDEVICE8;
    hr: HRESULT;
begin
  Result := Integer(DIENUM_CONTINUE);
  with TGameControl(pvRef) do
  begin
    hr := DInput.CreateDevice(lpddi.guidInstance, DI_JoyDevice, nil);
    if (not FAILED(hr)) then
    begin
      hr := DI_JoyDevice.SetDataFormat(c_dfDIJoystick2);
      if (not FAILED(hr)) then
      begin
        hr := DI_JoyDevice.SetCooperativeLevel((Glb.Owner as TForm).Handle, DISCL_NONEXCLUSIVE or DISCL_BACKGROUND);
        if (not FAILED(hr)) then
        begin
          logStr := lpddi.tszInstanceName;
          //logStr := lpddi.tszProductName;
          AddGameDevice(logStr, DI_JoyDevice, lpddi.guidInstance);
        end;
      end;
    end;
    //FDeviceGUID := lpddi.guidInstance;
    //Result := Integer(DIENUM_STOP);
  end;
end;

{ THIDJoystick }

constructor THIDJoystick.Create(pSystemID: string; pHandle: HWND);
begin
  inherited;
  fRegisteredProcs := TStringList.Create;
  fAxisTickSkip := 1;
  fCurrentAxisTick := fAxisTickSkip;
  ZeroMemory(@fAxisTrigger, SizeOf(fAxisTrigger));
  fDIDevice := nil; // for dead devices (just loaded)
end;

destructor THIDJoystick.Destroy;
begin
  fRegisteredProcs.Free;
  inherited;
end;

procedure THIDJoystick.DoAxisCall(pProcName, pAxisName: String;
  pValue: Integer);
var
  Params : PSafeArray;
  v : Variant;
begin
  Glb.DebugLog(Format('Dev %s axis %s change to %d.',
    [Name, pAxisName, pValue]), 'GAME');
  // prepare params
  v := VarArrayCreate([0, 2], varVariant);
  v[0] := Name;
  v[1] := pAxisName;
  v[2] := pValue;

  Params := PSafeArray(TVarData(v).VArray);
  try
    Glb.ScriptEngine.ExecutionSC.Run(pProcName, Params);
  except
    On E: Exception do
    begin
      Glb.LogError(E.Message);
    end;
  end;
end;

procedure THIDJoystick.GenerateEvents(Handle: HWND);
var
  I: Integer;
  lOldState: DIJoyState2;
begin
  if DIDevice = nil then
    exit;
  
  lOldState := fJoyState;
  GetButtons;
  for I := 0 to fButtonsCount - 1 do
    if fJoyState.rgbButtons[I] <> lOldState.rgbButtons[I] then
    begin
      if lOldState.rgbButtons[I] = 0 then
        PostMessage(Handle, WM_JOY_BUTTON_DOWN, Integer(self), I)
      else
        PostMessage(Handle, WM_JOY_BUTTON_UP, Integer(self), I);
    end;

  if (fRegisteredProcs.Count > 0) then
  begin
    Dec(fCurrentAxisTick);
    if fCurrentAxisTick = 0 then
    begin
      fCurrentAxisTick := fAxisTickSkip;
      // codefor each axis copied for performance
      // lX
      if (fAxisTrigger.lX > 0) and (Abs(fLastAxis.lX - fJoyState.lX) > fAxisTrigger.lX) then
      begin
        DoAxisCall(fRegisteredProcs.Values['X'], 'X', fJoyState.lX);
        fLastAxis.lX := fJoyState.lX;
      end;
      //lY Y
      if (fAxisTrigger.lY > 0) and (Abs(fLastAxis.lY - fJoyState.lY) > fAxisTrigger.lY) then
      begin
        DoAxisCall(fRegisteredProcs.Values['Y'], 'Y', fJoyState.lY);
        fLastAxis.lY := fJoyState.lY;
      end;
      //lZ Z
      if (fAxisTrigger.lZ > 0) and (Abs(fLastAxis.lZ - fJoyState.lZ) > fAxisTrigger.lZ) then
      begin
        DoAxisCall(fRegisteredProcs.Values['Z'], 'Z', fJoyState.lZ);
        fLastAxis.lZ := fJoyState.lZ;
      end;
      //lRx RX
      if (fAxisTrigger.lRx > 0) and (Abs(fLastAxis.lRx - fJoyState.lRx) > fAxisTrigger.lRx) then
      begin
        DoAxisCall(fRegisteredProcs.Values['RX'], 'Rx', fJoyState.lRx);
        fLastAxis.lRx := fJoyState.lRx;
      end;
      //lRy RY
      if (fAxisTrigger.lRy > 0) and (Abs(fLastAxis.lRy - fJoyState.lRy) > fAxisTrigger.lRy) then
      begin
        DoAxisCall(fRegisteredProcs.Values['RY'], 'Ry', fJoyState.lRy);
        fLastAxis.lRy := fJoyState.lRy;
      end;
      //lRz RZ
      if (fAxisTrigger.lRz > 0) and (Abs(fLastAxis.lRz - fJoyState.lRz) > fAxisTrigger.lRz) then
      begin
        DoAxisCall(fRegisteredProcs.Values['RZ'], 'Rz', fJoyState.lRz);
        fLastAxis.lRz := fJoyState.lRz;
      end;
      //rglSlider[0] SLIDER1
      if (fAxisTrigger.rglSlider[0] > 0) and (Abs(fLastAxis.rglSlider[0] - fJoyState.rglSlider[0]) > fAxisTrigger.rglSlider[0]) then
      begin
        DoAxisCall(fRegisteredProcs.Values['SLIDER1'], 'Slider1', fJoyState.rglSlider[0]);
        fLastAxis.rglSlider[0] := fJoyState.rglSlider[0];
      end;
      //rglSlider[1] SLIDER2
      if (fAxisTrigger.rglSlider[1] > 0) and (Abs(fLastAxis.rglSlider[1] - fJoyState.rglSlider[1]) > fAxisTrigger.rglSlider[1]) then
      begin
        DoAxisCall(fRegisteredProcs.Values['SLIDER2'], 'Slider2', fJoyState.rglSlider[1]);
        fLastAxis.rglSlider[1] := fJoyState.rglSlider[1];
      end;
      //rgdwPOV[0] POV1
      if (fAxisTrigger.rgdwPOV[0] > 0) and (Abs(fLastAxis.rgdwPOV[0] - fJoyState.rgdwPOV[0]) > fAxisTrigger.rgdwPOV[0]) then
      begin
        DoAxisCall(fRegisteredProcs.Values['POV1'], 'POV1', fJoyState.rgdwPOV[0]);
        fLastAxis.rgdwPOV[0] := fJoyState.rgdwPOV[0];
      end;
      //rgdwPOV[1] POV2
      if (fAxisTrigger.rgdwPOV[1] > 0) and (Abs(fLastAxis.rgdwPOV[1] - fJoyState.rgdwPOV[1]) > fAxisTrigger.rgdwPOV[1]) then
      begin
        DoAxisCall(fRegisteredProcs.Values['POV2'], 'POV2', fJoyState.rgdwPOV[1]);
        fLastAxis.rgdwPOV[1] := fJoyState.rgdwPOV[1];
      end;
      //rgdwPOV[2] POV3
      if (fAxisTrigger.rgdwPOV[2] > 0) and (Abs(fLastAxis.rgdwPOV[2] - fJoyState.rgdwPOV[2]) > fAxisTrigger.rgdwPOV[2]) then
      begin
        DoAxisCall(fRegisteredProcs.Values['POV3'], 'POV3', fJoyState.rgdwPOV[2]);
        fLastAxis.rgdwPOV[2] := fJoyState.rgdwPOV[2];
      end;
      //rgdwPOV[3] POV4
      if (fAxisTrigger.rgdwPOV[3] > 0) and (Abs(fLastAxis.rgdwPOV[3] - fJoyState.rgdwPOV[3]) > fAxisTrigger.rgdwPOV[3]) then
      begin
        DoAxisCall(fRegisteredProcs.Values['POV4'], 'POV4', fJoyState.rgdwPOV[3]);
        fLastAxis.rgdwPOV[3] := fJoyState.rgdwPOV[3];
      end;
      //lVX VX
      if (fAxisTrigger.lVX > 0) and (Abs(fLastAxis.lVX - fJoyState.lVX) > fAxisTrigger.lVX) then
      begin
        DoAxisCall(fRegisteredProcs.Values['VX'], 'VX', fJoyState.lVX);
        fLastAxis.lVX := fJoyState.lVX;
      end;
      //lVY VY
      if (fAxisTrigger.lVY > 0) and (Abs(fLastAxis.lVY - fJoyState.lVY) > fAxisTrigger.lVY) then
      begin
        DoAxisCall(fRegisteredProcs.Values['VY'], 'VY', fJoyState.lVY);
        fLastAxis.lVY := fJoyState.lVY;
      end;
      //lVZ VZ
      if (fAxisTrigger.lVZ > 0) and (Abs(fLastAxis.lVZ - fJoyState.lVZ) > fAxisTrigger.lVZ) then
      begin
        DoAxisCall(fRegisteredProcs.Values['VZ'], 'VZ', fJoyState.lVZ);
        fLastAxis.lVZ := fJoyState.lVZ;
      end;
      //lVRx VRX
      if (fAxisTrigger.lVRx > 0) and (Abs(fLastAxis.lVRx - fJoyState.lVRx) > fAxisTrigger.lVRx) then
      begin
        DoAxisCall(fRegisteredProcs.Values['VRX'], 'VRx', fJoyState.lVRx);
        fLastAxis.lVRx := fJoyState.lVRx;
      end;
      //lVRy VRY
      if (fAxisTrigger.lVRy > 0) and (Abs(fLastAxis.lVRy - fJoyState.lVRy) > fAxisTrigger.lVRy) then
      begin
        DoAxisCall(fRegisteredProcs.Values['VRY'], 'VRy', fJoyState.lVRy);
        fLastAxis.lVRy := fJoyState.lVRy;
      end;
      //lVRZ VRZ
      if (fAxisTrigger.lVRZ > 0) and (Abs(fLastAxis.lVRZ - fJoyState.lVRZ) > fAxisTrigger.lVRZ) then
      begin
        DoAxisCall(fRegisteredProcs.Values['VRZ'], 'VRz', fJoyState.lVRZ);
        fLastAxis.lVRZ := fJoyState.lVRZ;
      end;
      //rglVSlider[0] VSLIDER1
      if (fAxisTrigger.rglVSlider[0] > 0) and (Abs(fLastAxis.rglVSlider[0] - fJoyState.rglVSlider[0]) > fAxisTrigger.rglVSlider[0]) then
      begin
        DoAxisCall(fRegisteredProcs.Values['VSLIDER1'], 'VSlider1', fJoyState.rglVSlider[0]);
        fLastAxis.rglVSlider[0] := fJoyState.rglVSlider[0];
      end;
      //rglVSlider[1] VSLIDER2
      if (fAxisTrigger.rglVSlider[1] > 0) and (Abs(fLastAxis.rglVSlider[1] - fJoyState.rglVSlider[1]) > fAxisTrigger.rglVSlider[1]) then
      begin
        DoAxisCall(fRegisteredProcs.Values['VSLIDER2'], 'VSlider2', fJoyState.rglVSlider[1]);
        fLastAxis.rglVSlider[1] := fJoyState.rglVSlider[1];
      end;
      //lAX AX
      if (fAxisTrigger.lAX > 0) and (Abs(fLastAxis.lAX - fJoyState.lAX) > fAxisTrigger.lAX) then
      begin
        DoAxisCall(fRegisteredProcs.Values['AX'], 'AX', fJoyState.lAX);
        fLastAxis.lAX := fJoyState.lAX;
      end;
      //lAY AY
      if (fAxisTrigger.lAY > 0) and (Abs(fLastAxis.lAY - fJoyState.lAY) > fAxisTrigger.lAY) then
      begin
        DoAxisCall(fRegisteredProcs.Values['AY'], 'AY', fJoyState.lAY);
        fLastAxis.lAY := fJoyState.lAY;
      end;
      //lAZ AZ
      if (fAxisTrigger.lAZ > 0) and (Abs(fLastAxis.lAZ - fJoyState.lAZ) > fAxisTrigger.lAZ) then
      begin
        DoAxisCall(fRegisteredProcs.Values['AZ'], 'AZ', fJoyState.lAZ);
        fLastAxis.lAZ := fJoyState.lAZ;
      end;
      //lARx ARX
      if (fAxisTrigger.lARx > 0) and (Abs(fLastAxis.lARx - fJoyState.lARx) > fAxisTrigger.lARx) then
      begin
        DoAxisCall(fRegisteredProcs.Values['ARX'], 'ARx', fJoyState.lARx);
        fLastAxis.lARx := fJoyState.lARx;
      end;
      //lARy ARY
      if (fAxisTrigger.lARy > 0) and (Abs(fLastAxis.lARy - fJoyState.lARy) > fAxisTrigger.lARy) then
      begin
        DoAxisCall(fRegisteredProcs.Values['ARY'], 'ARy', fJoyState.lARy);
        fLastAxis.lARy := fJoyState.lARy;
      end;
      //lARz ARZ
      if (fAxisTrigger.lARz > 0) and (Abs(fLastAxis.lARz - fJoyState.lARz) > fAxisTrigger.lARz) then
      begin
        DoAxisCall(fRegisteredProcs.Values['ARZ'], 'ARz', fJoyState.lARz);
        fLastAxis.lARz := fJoyState.lARz;
      end;
      //rglASlider[0] ASLIDER1
      if (fAxisTrigger.rglASlider[0] > 0) and (Abs(fLastAxis.rglASlider[0] - fJoyState.rglASlider[0]) > fAxisTrigger.rglASlider[0]) then
      begin
        DoAxisCall(fRegisteredProcs.Values['ASLIDER1'], 'ASlider1', fJoyState.rglASlider[0]);
        fLastAxis.rglASlider[0] := fJoyState.rglASlider[0];
      end;
      //rglASlider[1] ASLIDER2
      if (fAxisTrigger.rglASlider[1] > 0) and (Abs(fLastAxis.rglASlider[1] - fJoyState.rglASlider[1]) > fAxisTrigger.rglASlider[1]) then
      begin
        DoAxisCall(fRegisteredProcs.Values['ASLIDER2'], 'ASlider2', fJoyState.rglASlider[1]);
        fLastAxis.rglASlider[1] := fJoyState.rglASlider[1];
      end;
      //lFRx FRX
      if (fAxisTrigger.lFRx > 0) and (Abs(fLastAxis.lFRx - fJoyState.lFRx) > fAxisTrigger.lFRx) then
      begin
        DoAxisCall(fRegisteredProcs.Values['FRX'], 'FRx', fJoyState.lFRx);
        fLastAxis.lFRx := fJoyState.lFRx;
      end;
      //lFRy FRY
      if (fAxisTrigger.lFRy > 0) and (Abs(fLastAxis.lFRy - fJoyState.lFRy) > fAxisTrigger.lFRy) then
      begin
        DoAxisCall(fRegisteredProcs.Values['FRY'], 'FRy', fJoyState.lFRy);
        fLastAxis.lFRy := fJoyState.lFRy;
      end;
      //lFRz FRZ
      if (fAxisTrigger.lFRz > 0) and (Abs(fLastAxis.lFRz - fJoyState.lFRz) > fAxisTrigger.lFRz) then
      begin
        DoAxisCall(fRegisteredProcs.Values['FRZ'], 'FRz', fJoyState.lFRz);
        fLastAxis.lFRz := fJoyState.lFRz;
      end;
      // rglFSlider[0] FSLIDER1
      if (fAxisTrigger.rglFSlider[0] > 0) and (Abs(fLastAxis.rglFSlider[0] - fJoyState.rglFSlider[0]) > fAxisTrigger.rglFSlider[0]) then
      begin
        DoAxisCall(fRegisteredProcs.Values['FSLIDER1'], 'FSlider1', fJoyState.rglFSlider[0]);
        fLastAxis.rglFSlider[0] := fJoyState.rglFSlider[0];
      end;
      // rglFSlider[1] FSLIDER2
      if (fAxisTrigger.rglFSlider[1] > 0) and (Abs(fLastAxis.rglFSlider[1] - fJoyState.rglFSlider[1]) > fAxisTrigger.rglFSlider[1]) then
      begin
        DoAxisCall(fRegisteredProcs.Values['FSLIDER2'], 'FSlider2', fJoyState.rglFSlider[1]);
        fLastAxis.rglFSlider[1] := fJoyState.rglFSlider[1];
      end;
    end;
  end;
end;



function THIDJoystick.GetAxis(pName: String): LongInt;
var
  lTrPtr: PLongInt;
begin
  lTrPtr := GetAxisPointer(@fJoyState, pName);
  if lTrPtr = nil then
    Result := -99
  else
    Result := lTrPtr^;
end;

function THIDJoystick.GetAxisPointer(pIn: PDIJoyState2;
  pAxisName: String): PLongInt;
var
  lName : String;
begin
  lName := UpperCase(pAxisName);
  if lName = 'X' then Result := @(pIn^.lX) else
  if lName = 'Y' then Result := @(pIn^.lY) else
  if lName = 'Z' then Result := @(pIn^.lZ) else
  if lName = 'RX' then Result := @(pIn^.lRx) else
  if lName = 'RY' then Result := @(pIn^.lRy) else
  if lName = 'RZ' then Result := @(pIn^.lRz) else
  if lName = 'SLIDER1' then Result := @(pIn^.rglSlider[0]) else
  if lName = 'SLIDER2' then Result := @(pIn^.rglSlider[1]) else
  if lName = 'POV1' then Result := @(pIn^.rgdwPOV[0]) else
  if lName = 'POV2' then Result := @(pIn^.rgdwPOV[1]) else
  if lName = 'POV3' then Result := @(pIn^.rgdwPOV[2]) else
  if lName = 'POV4' then Result := @(pIn^.rgdwPOV[3]) else
  if lName = 'VX' then Result := @(pIn^.lVX) else
  if lName = 'VY' then Result := @(pIn^.lVY) else
  if lName = 'VZ' then Result := @(pIn^.lVZ) else
  if lName = 'VRX' then Result := @(pIn^.lVRx) else
  if lName = 'VRY' then Result := @(pIn^.lVRy) else
  if lName = 'VRZ' then Result := @(pIn^.lVRZ) else
  if lName = 'VSLIDER1' then Result := @(pIn^.rglVSlider[0]) else
  if lName = 'VSLIDER2' then Result := @(pIn^.rglVSlider[1]) else
  if lName = 'AX' then Result := @(pIn^.lAX) else
  if lName = 'AY' then Result := @(pIn^.lAY) else
  if lName = 'AZ' then Result := @(pIn^.lAZ) else
  if lName = 'ARX' then Result := @(pIn^.lARx) else
  if lName = 'ARY' then Result := @(pIn^.lARy) else
  if lName = 'ARZ' then Result := @(pIn^.lARz) else
  if lName = 'ASLIDER1' then Result := @(pIn^.rglASlider[0]) else
  if lName = 'ASLIDER2' then Result := @(pIn^.rglASlider[1]) else
  if lName = 'FRX' then Result := @(pIn^.lFRx) else
  if lName = 'FRY' then Result := @(pIn^.lFRy) else
  if lName = 'FRZ' then Result := @(pIn^.lFRz) else
  if lName = 'FSLIDER1' then Result := @(pIn^.rglFSlider[0]) else
  if lName = 'FSLIDER2' then Result := @(pIn^.rglFSlider[1]) else
  Result := nil;
end;

function THIDJoystick.GetButton(pNumber: Integer): Shortint;
begin
  Result := -1;
  if (pNumber > 0) and (pNumber <= fButtonsCount) then
    Result := fJoyState.rgbButtons[pNumber-1];
end;

procedure THIDJoystick.GetButtons;
var
  I: Integer;
  hr: HRESULT;
begin
  if (Glb = nil) or (not Glb.HIDControl.GameAvailable) then
    exit;
  hr := fDIDevice.Poll;
  if (FAILED(hr)) then
  begin
    hr := fDIDevice.Acquire;
    if FAILED(hr) then
    begin
      //DIERR_INPUTLOST
      //DebugLog('Warning: Can''t acquire joy ' + joy.Name);
      //continue;
      exit;
    end;
  end;
  hr := fDIDevice.GetDeviceState(SizeOf(DIJOYSTATE2), @fJoyState);
  if FAILED(hr) then
    exit;
end;

procedure THIDJoystick.RegisterAxisEvent(pAxis, pProcName: String; pDelta: Integer);
var
  lName: String;
  lTrPtr: PLongInt;
  lIndex : Integer;
begin
  lName := UpperCase(pAxis);
  lTrPtr := GetAxisPointer(@fAxisTrigger, lName);
  if (lTrPtr <> nil) and (pDelta > 0) then
  begin
    lTrPtr^ := pDelta;
    lIndex := fRegisteredProcs.IndexOfName(lName);
    if lIndex >= 0 then
      fRegisteredProcs.Delete(lIndex);
    fRegisteredProcs.Values[lName] := pProcName;
    Glb.DebugLog(Format('Registered handler for dev %s axis %s proc %s.',
    [Name, lName, pProcName]), 'GAME');
    //Glb.GameControl.DebugLog('Trigger is ' + IntToStr(
  end;
end;

procedure THIDJoystick.UnRegisterAxisEvent(pAxis: String);
var
  lName: String;
  lIndex : Integer;
begin
  lName := UpperCase(pAxis);
  lIndex := fRegisteredProcs.IndexOfName(lName);
  if lIndex >= 0 then
  begin
    fRegisteredProcs.Delete(lIndex);
    Glb.DebugLog(Format('Handler for dev %s axis %s unregistered.',
    [Name, lName]), 'GAME');
  end;
end;

{ TGameControl }

procedure TGameControl.AddGameDevice(pDeviceName: String;
  Data: IDIRECTINPUTDEVICE8; pGUID: TGUID);
var
  caps: DIDEVCAPS;
  newJoy: THIDJoystick;
  hr: HRESULT;
  I, newIndex : Integer;
  NameOK: Boolean;
begin
  caps.dwSize := SizeOf(DIDEVCAPS);
  hr := Data.GetCapabilities(caps);
  if FAILED(hr) then
    exit;
  // add string to log
  DebugLog('Found game device: ' + pDeviceName + ', no of buttons: ' + IntToStr(caps.dwButtons));
  // create kbd object
  newJoy := THIDJoystick.Create(GUID2Str(pGUID), 1); // faked handle, but show this is real device
  //newJoy.Name := 'Game'+IntToStr(GameDevCounter+1);
  newJoy.Name := pDeviceName;
  newJoy.ButtonsCount := caps.dwButtons;
  newJoy.DIDevice := Data;
  fDevices.Add(newJoy);
  if Assigned(fOnNewDevice) then
    fOnNewDevice(newJoy);
end;

function TGameControl.GetAvailable: Boolean;
begin
  Result := DInput <> nil;
end;

function TGameControl.GetDevice(pName: String): THIDJoystick;
var I: Integer;
begin
  Result := nil;
  for I := 0 to fDevices.Count - 1 do
    if UpperCase(THIDJoystick(fDevices[I]).Name) = UpperCase(pName) then
    begin
      Result := THIDJoystick(fDevices[I]);
      break;
    end;
end;

function TGameControl.GUID2Str(pGUID: TGUID): String;
var I: Integer;
begin
  Result := IntToHex(pGUID.D1, 8);
  Result := Result + ':' + IntToHex(pGUID.D2, 4);
  Result := Result + ':' + IntToHex(pGUID.D3, 4) + ':';
  for I := Low(pGUID.D4) to High(pGUID.D4) do
    Result := Result + IntToHex(pGUID.D4[I], 2);
end;


constructor TGameControl.Create;
begin
  fDevices := TList.Create;
end;

procedure TGameControl.DebugLog(Value: String);
begin
  Glb.DebugLog(Value, 'GAME');
end;

destructor TGameControl.Destroy;
begin
  fDevices.Free; // are destroyed from form
  if DInput <> nil then
    DInput._Release;
  inherited;
end;

procedure TGameControl.InitDirectX;
var
  hr: HRESULT;
begin
  fDevices.Clear;
  DInput := nil;
  hr := DirectInput8Create(GetModuleHandle(nil),DIRECTINPUT_VERSION,IID_IDirectInput8,DInput,nil);
  if (FAILED(hr)) then
  begin
    DebugLog('Can not init Direct input. Error ' + IntToHex(hr, 8));
    exit;
  end;
  if (DInput = nil) then
  begin
    DebugLog('Direct input initialization error');
    exit;
  end;
  DInput.EnumDevices(DI8DEVCLASS_GAMECTRL, @EnumJoysticksCallback, self, DIEDFL_ATTACHEDONLY);
end;

end.
