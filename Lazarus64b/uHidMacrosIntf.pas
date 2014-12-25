unit uHidMacrosIntf;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, HIDMacros_TLB, StdVcl, Forms;

type
  THIDMacrosIntf = class(TAutoObject, IHIDMacrosIntf)
  protected
    procedure Sleep(pMsec: SYSINT); safecall;
    procedure FSXtext(const lValue: WideString); safecall;
    procedure FSXEvent(const pName: WideString; pParam: Double); safecall;
    procedure FSXEventByTitle(const pTitle: WideString; pParam: Double); safecall;
    procedure MouseClick(pLeft, pTop: SYSINT; pAction: Smallint); safecall;
    procedure SendKeys(const pSeqence: WideString); safecall;
    function GetFSXVariable(const pName: WideString): OleVariant; safecall;
    procedure RegisterFSXVariable(const pName, pUnits: WideString); safecall;
    procedure SetFSXVariable(const pName, pUnits: WideString; pValue: Double);
      safecall;
    function GetFSUIPCFloat(pOffset: SYSINT; pSize: Smallint): OleVariant; safecall;
    function GetFSUIPCInt(pOffset: SYSINT; pSize: Smallint): OleVariant; safecall;
    procedure SetFSUIPCFloat(pOffset: SYSINT; pSize: Smallint; pValue: Double);
      safecall;
    procedure SetFSUIPCInt(pOffset: SYSINT; pSize: Smallint; pValue: SYSINT);
      safecall;
    function GetFSUIPCRaw(pOffset: SYSINT; pSize: Smallint): OleVariant; safecall;
    procedure PlaySound(const aFileName: WideString); safecall;
    procedure Debug(const pMessage: WideString); safecall;
    function GetFSUIPCString(pOffset: SYSINT; pSize: Smallint): OleVariant;
      safecall;
    function GetBuffer: OleVariant; safecall;
    procedure AddToBuffer(const pValue: WideString); safecall;
    procedure ClearBuffer; safecall;
    procedure SetBuffer(const pValue: WideString); safecall;
    function IsButtonPressed(const pDevice: WideString;
      pNumber: Smallint): OleVariant; safecall;
    function StrRPad(const pString, pChar: WideString;
      pLength: Smallint): OleVariant; safecall;
    function GetXplVariable(const aName: WideString): OleVariant; safecall;
    function GetXplArrayItem(const pName: WideString; pIndex: SYSINT): OleVariant;
      safecall;
    procedure SetXplArrayItem(const pName: WideString; pIndex: SYSINT;
      pValue: OleVariant); safecall;
    procedure SetXplVariable(const pName: WideString; pValue: OleVariant); safecall;
    function GetAxis(const pDevice, pAxis: WideString): OleVariant; safecall;
    procedure RegisterAxisEvent(const pDevice, pAxis, pProcName: WideString;
      pDelta: SYSINT); safecall;
    function Axis2Float(pValue: SYSINT; pLowValue, pHighValue: Double): OleVariant;
      safecall;
    function Axis2Int(pValue, pLowValue, pHighValue: SYSINT): OleVariant; safecall;
    procedure UnRegisterAxisEvent(const pDevice, pAxis: WideString); safecall;
    function AxisRemap(pValue: SYSINT; pMin, pMax, pLowValue, pHighValue,
      pDefault: Double): OleVariant; safecall;
    function GetKeyState(pKey: SYSINT): OleVariant; safecall;
    procedure SendKeysSlow(const pSequence: WideString; pDealy: SYSINT); safecall;
    procedure XPLCommand(const pCmdName: WideString); safecall;
    procedure SetMovingMapVisible(pVisible: SYSINT); safecall;
    procedure ToggleMovingMap; safecall;
    procedure SetFSUIPCString(pOffset: SYSINT; pSize: Smallint;
      const pValue: WideString); safecall;
    procedure XPLCommandBegin(const pName: WideString); safecall;
    procedure XPLCommandEnd(const pName: WideString); safecall;
  public
    destructor Destroy; Override;
  end;

implementation

uses ComServ, SysUtils, Windows, FSXControl, FSUIPCcontrol,
     uAutoPilot, uGlobals, uGameDevices, uXplControl, uSendKeys, uXplCommon;
  //, MPlayer;

destructor THIDMacrosIntf.Destroy;
begin
  //OutputDebugString('[HDM]Intf shutdown');
  inherited;
end;

procedure THIDMacrosIntf.Sleep(pMsec: SYSINT);
//var I:Integer;
begin
//  for I := 0 to 10000 do
//    Application.ProcessMessages;
  SysUtils.Sleep(pMsec);
end;

procedure THIDMacrosIntf.FSXtext(const lValue: WideString);
begin
  Glb.FSX.SendText(lValue);
end;

procedure THIDMacrosIntf.FSXEvent(const pName: WideString; pParam: Double);
begin
  Glb.FSX.SendEvent(pName, Trunc(pParam));
end;

procedure THIDMacrosIntf.FSXEventByTitle(const pTitle: WideString;
  pParam: Double);
begin
  Glb.FSX.SendEventByTitle(pTitle, Trunc(pParam));
end;

procedure THIDMacrosIntf.MouseClick(pLeft, pTop: SYSINT; pAction: Smallint);
var
    Pt : TPoint;
    MausPos: TPoint;
    NumerOfClicks, I: Integer;
 begin
    Application.ProcessMessages;
    // Save emouse pos
    GetCursorPos(MausPos);

   {Convert Pt to screen coordinates and Mickeys}
    Pt.x := Round(pLeft * (65535 / Screen.Width)) ;
    Pt.y := Round(pTop * (65535 / Screen.Height)) ;
   {Simulate the mouse move}
    Mouse_Event(MOUSEEVENTF_ABSOLUTE or
                MOUSEEVENTF_MOVE,
                Pt.x, Pt.y, 0, 0) ;
    if (pAction > 3) and (pAction < 7) then
    begin
      NumerOfClicks := 2;
      pAction := pAction - 3;
    end
    else
      NumerOfClicks := 1;
    case pAction of
      0: ; // do nothing, just move
      1:
      begin
        for I := 1 to NumerOfClicks do
        begin
         {Simulate the left mouse button down}
         Mouse_Event(MOUSEEVENTF_ABSOLUTE or
                     MOUSEEVENTF_LEFTDOWN,
                     Pt.x, Pt.y, 0, 0) ;;
        {Simulate the left mouse button up}
         Mouse_Event(MOUSEEVENTF_ABSOLUTE or
                     MOUSEEVENTF_LEFTUP,
                     Pt.x, Pt.y, 0, 0) ;;
        end;
      end;
      2:
      begin
        for I := 1 to NumerOfClicks do
        begin
         {Simulate the middle mouse button down}
         Mouse_Event(MOUSEEVENTF_ABSOLUTE or
                     MOUSEEVENTF_MIDDLEDOWN,
                     Pt.x, Pt.y, 0, 0) ;;
        {Simulate the middle mouse button up}
         Mouse_Event(MOUSEEVENTF_ABSOLUTE or
                     MOUSEEVENTF_MIDDLEUP,
                     Pt.x, Pt.y, 0, 0) ;;
        end;
      end;
      3:
      begin
        for I := 1 to NumerOfClicks do
        begin
         {Simulate the right mouse button down}
         Mouse_Event(MOUSEEVENTF_ABSOLUTE or
                     MOUSEEVENTF_RIGHTDOWN,
                     Pt.x, Pt.y, 0, 0) ;;
        {Simulate the right mouse button up}
         Mouse_Event(MOUSEEVENTF_ABSOLUTE or
                     MOUSEEVENTF_RIGHTUP,
                     Pt.x, Pt.y, 0, 0) ;;
        end;
      end;
    end;
    if (pAction > 0) and (pAction < 4) then
    begin
      Application.ProcessMessages;
      // return mouse pointer
      Pt.x := Round(MausPos.X * (65535 / Screen.Width)) ;
      Pt.y := Round(MausPos.Y * (65535 / Screen.Height)) ;
      Mouse_Event(MOUSEEVENTF_ABSOLUTE or
                MOUSEEVENTF_MOVE,
                 Pt.x, Pt.y, 0, 0) ;;
    end;
 end;

procedure THIDMacrosIntf.SendKeys(const pSeqence: WideString);
var
  lSndKey: TKeySequence;
begin
  lSndKey := TKeySequence.Create;
  lSndKey.Sequence := pSeqence;
  lSndKey.Resume;
end;

function THIDMacrosIntf.GetFSXVariable(const pName: WideString): OleVariant;
begin
  //Result := 'Ahoj';
  Result := Glb.FSX.GetFSXVariable(pName);
end;

procedure THIDMacrosIntf.RegisterFSXVariable(const pName, pUnits: WideString);
begin
  Glb.FSX.RegisterFSXVariable(pName, punits);
end;

procedure THIDMacrosIntf.SetFSXVariable(const pName, pUnits: WideString;
  pValue: Double);
begin
  Glb.FSX.SetFSXVariable(pName, pUnits, pValue);
end;

function THIDMacrosIntf.GetFSUIPCFloat(pOffset: SYSINT;
  pSize: Smallint): OleVariant;
begin
  Result := Glb.FSUIPC.GetFloat(pOffset, pSize);
end;

function THIDMacrosIntf.GetFSUIPCInt(pOffset: SYSINT;
  pSize: Smallint): OleVariant;
begin
  Result := Glb.FSUIPC.GetInt(pOffset, pSize);
end;

procedure THIDMacrosIntf.SetFSUIPCFloat(pOffset: SYSINT; pSize: Smallint;
  pValue: Double);
begin
  Glb.FSUIPC.SetFloat(pOffset, pSize, pValue);
end;

procedure THIDMacrosIntf.SetFSUIPCInt(pOffset: SYSINT; pSize: Smallint;
  pValue: SYSINT);
begin
  Glb.FSUIPC.SetInt(pOffset, pSize, pValue);
end;

function THIDMacrosIntf.GetFSUIPCRaw(pOffset: SYSINT;
  pSize: Smallint): OleVariant;
begin
  Result := Glb.FSUIPC.GetRaw(pOffset, pSize);
end;

procedure THIDMacrosIntf.PlaySound(const aFileName: WideString);
//var
//  player: TMediaPlayer;
begin
//  player := TMediaPlayer.Create(Application.MainForm);
//  try
//    player.Parent := Application.MainForm;
//    player.FileName := aFileName;
//    player.Open;
//    player.Play;
//  finally
//    player.Free;
//  end;
end;

procedure THIDMacrosIntf.Debug(const pMessage: WideString);
var arg: PChar;
begin
  arg := StrAlloc(Length(pMessage)+1);
  StrPCopy(arg, pMessage);
  try
    OutputDebugString(arg);
  finally
    StrDispose(arg);
  end;
  Glb.LogMessage(pMessage, 'DBG');
end;

function THIDMacrosIntf.GetFSUIPCString(pOffset: SYSINT;
  pSize: Smallint): OleVariant;
begin
  Result := Glb.FSUIPC.GetString(pOffset, pSize);
end;

function THIDMacrosIntf.GetBuffer: OleVariant;
begin
  Result := Glb.Buffer.Value;
end;

procedure THIDMacrosIntf.AddToBuffer(const pValue: WideString);
begin
  Glb.Buffer.Add(pValue);
end;

procedure THIDMacrosIntf.ClearBuffer;
begin
  Glb.Buffer.Clear;
end;

procedure THIDMacrosIntf.SetBuffer(const pValue: WideString);
begin
  Glb.Buffer.Value := pValue;
end;

function THIDMacrosIntf.IsButtonPressed(const pDevice: WideString;
  pNumber: Smallint): OleVariant;
var
  lJoy: THIDJoystick;
begin
  Result := 0;
  lJoy := THIDJoystick(Glb.HIDControl.GetDevice(pDevice));
  if lJoy <> nil then
    Result := lJoy.GetButton(pNumber);
end;

function THIDMacrosIntf.StrRPad(const pString, pChar: WideString;
  pLength: Smallint): OleVariant;
var I: Integer;
begin
  if Length(pString) > pLength then
    Result := Copy(pString, Length(pString) - pLength + 1, pLength)
  else
  begin
    Result := pString;
    for I := Length(pString) to pLength - 1 do
      Result := Result + pChar;
  end;
end;

// RegOverridePredefKey

function THIDMacrosIntf.GetXplVariable(const aName: WideString): OleVariant;
begin
  Result := Glb.Xpl.GetXplVariable(aName);
end;
 
function THIDMacrosIntf.GetXplArrayItem(const pName: WideString;
  pIndex: SYSINT): OleVariant;
begin
  Result := Glb.Xpl.GetXplArrayItem(pName, pIndex);
end;

procedure THIDMacrosIntf.SetXplArrayItem(const pName: WideString;
  pIndex: SYSINT; pValue: OleVariant);
begin
  Glb.Xpl.SetXplArrayItem(pName, pIndex, pValue);
end;

procedure THIDMacrosIntf.SetXplVariable(const pName: WideString;
  pValue: OleVariant);
begin
  Glb.Xpl.SetXplVariable(pName, pValue);
end;

function THIDMacrosIntf.GetAxis(const pDevice, pAxis: WideString): OleVariant;
var
  lJoy: THIDJoystick;
begin
  Result := 0;
  lJoy := THIDJoystick(Glb.HIDControl.GetDevice(pDevice));
  if lJoy <> nil then
    Result := lJoy.GetAxis(pAxis);
end;

procedure THIDMacrosIntf.RegisterAxisEvent(const pDevice, pAxis,
  pProcName: WideString; pDelta: SYSINT);
var
  lJoy: THIDJoystick;
begin
  lJoy := THIDJoystick(Glb.HIDControl.GetDevice(pDevice));
  if lJoy <> nil then
    lJoy.RegisterAxisEvent(pAxis, pProcName, pDelta)
  else
    Glb.LogError('Device ' + pDevice + ' not found.');
end;

function THIDMacrosIntf.Axis2Float(pValue: SYSINT; pLowValue,
  pHighValue: Double): OleVariant;
var
  lRange, lRes: Double;
begin
  lRange := pHighValue - pLowValue;
  lRes := pValue / 65535 * lRange;
  lRes := lRes + pLowValue;
  Result := lRes;
end;

function THIDMacrosIntf.Axis2Int(pValue, pLowValue,
  pHighValue: SYSINT): OleVariant;
var
  lRange, lRes: LongInt;
begin
  lRange := pHighValue - pLowValue;
  lRes := Round(pValue / 65535 * lRange);
  lRes := lRes + pLowValue;
  Result := lRes;
end;

procedure THIDMacrosIntf.UnRegisterAxisEvent(const pDevice, pAxis: WideString);
var
  lJoy: THIDJoystick;
begin
  lJoy := THIDJoystick(Glb.HIDControl.GetDevice(pDevice));
  if lJoy <> nil then
    lJoy.UnRegisterAxisEvent(pAxis)
  else
    Glb.LogError('Device ' + pDevice + ' not found.');
end;

function THIDMacrosIntf.AxisRemap(pValue: SYSINT; pMin, pMax, pLowValue,
  pHighValue, pDefault: Double): OleVariant;
var
  lRange, lRes, lValue: Double;
begin
  lValue := pValue / 65535; // input value in fraction
  Result := pDefault;
  if (lValue < pMin) or (lValue > pMax) then
    exit;
  // recalculate input value into Min/Max range
  lValue := (lValue - pMin) / (pMax - pMin);
  // now just deal with output recalculation by lValue
  lRange := pHighValue - pLowValue;
  lRes := lValue * lRange;
  lRes := lRes + pLowValue;
  Result := lRes;
end;

function THIDMacrosIntf.GetKeyState(pKey: SYSINT): OleVariant;
begin
  Result := Windows.GetKeyState(pKey);
end;

procedure THIDMacrosIntf.SendKeysSlow(const pSequence: WideString;
  pDealy: SYSINT);
var
  lSndKey: TKeySequence;
begin
  lSndKey := TKeySequence.Create;
  lSndKey.Sequence := pSequence;
  lSndKey.DelayModifiers := pDealy;
  lSndKey.DelayKeys := pDealy;
  lSndKey.Resume;
end;

procedure THIDMacrosIntf.XPLCommand(const pCmdName: WideString);
begin
  Glb.Xpl.ExecuteCommand(pCmdName);
end;

procedure THIDMacrosIntf.SetMovingMapVisible(pVisible: SYSINT);
begin
  Glb.MapForm.SetVisibility(pVisible > 0);
end;

procedure THIDMacrosIntf.ToggleMovingMap;
begin
  Glb.MapForm.Toggle;
end;

procedure THIDMacrosIntf.SetFSUIPCString(pOffset: SYSINT; pSize: Smallint;
  const pValue: WideString);
begin
  Glb.FSUIPC.SetString(pOffset, pSize, pValue);
end;

procedure THIDMacrosIntf.XPLCommandBegin(const pName: WideString);
begin
  Glb.Xpl.ExecuteCommand(pName, HDMC_COMMAND_BEGIN);
end;

procedure THIDMacrosIntf.XPLCommandEnd(const pName: WideString);
begin
  Glb.Xpl.ExecuteCommand(pName, HDMC_COMMAND_END);
end;

initialization
  TAutoObjectFactory.Create(ComServer, THIDMacrosIntf, Class_THIDMacrosIntf,
    ciMultiInstance, tmApartment);
end.
