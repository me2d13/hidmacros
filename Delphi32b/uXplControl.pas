unit uXplControl;

interface

uses MemMap, uXplCommon, Classes;

type

  TXPLcontrol = class
  private
    fMM: TMemMap;
    pBuffer: PXplComRecord;
    fVars: TStringList;
    fCommands: TStringList;
    fActiveDuringCompilation: Boolean;
    procedure DebugLog(Value: String);
    function GetCustomXplVariable(pName: String; pIndex: Integer; isArray: Boolean): Variant;
    procedure SetCustomXplVariable(pName: String; pIndex: Integer; pValue: Variant; isArray: Boolean; pToggleCommand: Integer);
    procedure WaitForXplane(mSec: Integer = 500);
    procedure WaitForXplaneSlot(pSlot: Integer; mSec: Integer = 500);
    function GetLatitude: double;
    function GetLongitude: double;
    function GetHeading: double;
    function GetElevation: double;
    function GetPosRefreshInterval: Integer;
    procedure SetPosRefreshInterval(const Value: Integer);
    function GetFreeComSlot: Integer;
  public
    { Public declarations }
    constructor Create(pOwner: TObject);
    destructor Destroy; Override;
    function IsXplaneReady: Boolean;
    function isXplaneConnected: Boolean;
    function GetXplVariable(pName: String): Variant;
    function GetXplArrayItem(pName: String; pIndex: Integer): Variant;
    procedure SetXplVariable(pName: String; pValue: Variant);
    procedure SetXplArrayItem(pName: String; pIndex: Integer; pValue: Variant);
    procedure ExecuteCommand(pCmdName: String; pMode: Byte = HDMC_EXEC_COMMAND);
    procedure AddCommands(l : TStrings);
    procedure ToggleDataRef(pName: String; pValues: String; pCommand: Integer);
    procedure DrawText(pText: String; pPos: Single = 0; pSec: Integer = 5);
    property ActiveDuringCompilation: Boolean read fActiveDuringCompilation write fActiveDuringCompilation;
    property Latitude: double read GetLatitude;
    property Longitude: double read GetLongitude;
    property Heading: double read GetHeading;
    property Elevation: double read GetElevation;
    property PosRefreshInterval: Integer read GetPosRefreshInterval write SetPosRefreshInterval;
  end;

  TXPLRefHolder = class
  private
    fData: Pointer8b;
  public
    property Data: Pointer8b read fData write fData;
  end;



implementation

uses SysUtils, uGlobals, Windows, Forms, Variants, XPLMDataAccess,
  XPLMUtilities;

{ TXPLcontrol }

constructor TXPLcontrol.Create(pOwner: TObject);
var
  lGlb: THDMGlobals;
begin
  lGlb := (pOwner as THDMGlobals);
  lGlb.DebugLog('Xplane control created.', 'XPL');
  fVars := TStringList.Create;
  fVars.CaseSensitive := False;
  fCommands := TStringList.Create;
  fCommands.CaseSensitive := False;
  fActiveDuringCompilation := False;
  fMM := TMemMap.Create(XPL_MEM_FILE, SizeOf(TXplComRecord));
  if fMM.Memory <> nil then
  begin
    pBuffer := fMM.Memory;
    //DebugLog(Format('pBuffer addr is %s.', [IntToStr(ULong(pBuffer))]));
    if pBuffer^.XplConnected > 0 then
      lGlb.DebugLog('Xplane already connected to shared memory.', 'XPL')
    else
      lGlb.DebugLog('Xplane not yet connected to shared memory.', 'XPL');
    pBuffer^.HdmConnected := 1;
    pBuffer^.ComSlots[0].HDMcommand := 0; // NOP, just ping
    pBuffer^.ComSlots[0].XplRequestFlag := 1;
    pBuffer^.XplRequestFlag := 1; // ask XPL for response
  end;
  lGlb.DebugLog(Format('Slot size: %d, mem size: %d', [SizeOf(TXplComSlot), SizeOf(TXplComRecord)]), 'XPL');
end;

procedure TXPLcontrol.DebugLog(Value: String);
begin
  if Glb <> nil then
    Glb.DebugLog(Value, 'XPL');
end;

destructor TXPLcontrol.Destroy;
var I: Integer;
begin
  for I := 0 to fVars.Count - 1 do
    fVars.Objects[I].Free;
  fVars.Free;
  for I := 0 to fCommands.Count - 1 do
    fCommands.Objects[I].Free;
  fCommands.Free;
  fMM.Free;
  inherited;
end;

procedure TXPLcontrol.ExecuteCommand(pCmdName: String; pMode: Byte = HDMC_EXEC_COMMAND);
var
  lCommandIndex: Integer;
  //lCommand: XPLMCommandRef;
  lCommand: Pointer8b;
  lStr: String;
  lUnregistered: Boolean;
  lSlot: Integer;
  lRef: TXPLRefHolder;
begin
  if (fMM.Memory = nil) or (not isXplaneConnected) then
    exit;
  if (not fActiveDuringCompilation) and (Glb.ScriptEngine.Compiling) then
    exit;
  lSlot := GetFreeComSlot;
  if lSlot < 0 then
  begin
    WaitForXplane;
    lSlot := GetFreeComSlot;
    if lSlot < 0 then
    begin
      DebugLog('Can''t execute command, Xplane not listening.');
      exit;
    end;
  end;
  if Glb.IsModuleLogged('XPL') then
    pBuffer^.Debug := True;
  lCommandIndex := fCommands.IndexOf(pCmdName);
  if lCommandIndex > -1 then
  begin
    lCommand := TXPLRefHolder(fCommands.Objects[lCommandIndex]).Data;
    pBuffer^.ComSlots[lSlot].CommandRef := lCommand;
    lUnregistered := False;
  end
  else
  begin
    // find out first
    StrPCopy(pBuffer^.ComSlots[lSlot].ValueName, pCmdName);
    pBuffer^.ComSlots[lSlot].CommandRef := 0;
    lUnregistered := True;
  end;
  pBuffer^.ComSlots[lSlot].HDMcommand := pMode;
  pBuffer^.ComSlots[lSlot].XplRequestFlag := 1; // trigger xpl
  pBuffer^.XplRequestFlag := 1;
  // if unregistered, wait for result and note down the address
  if lUnregistered then
  begin
    WaitForXplaneSlot(lSlot);
    if pBuffer^.ComSlots[lSlot].XplRequestFlag = 0 then
    begin
      lCommand := pBuffer^.ComSlots[lSlot].CommandRef;
      if lCommand = 0 then
        Glb.LogError(Format('Command %s not recognized by Xplane.', [pCmdName]))
      else
      begin
        lRef := TXPLRefHolder.Create;
        lRef.Data := lCommand;
        fCommands.AddObject(pCmdName, lRef);
        DebugLog(Format('Registered command %s as address %x.', [pCmdName, pBuffer^.ComSlots[lSlot].CommandRef]));
      end;
    end;
  end;
end;

procedure TXPLcontrol.DrawText(pText: String; pPos: Single = 0; pSec: Integer = 5);
var
  lSlot: Integer;
begin
  if (fMM.Memory = nil) or (not isXplaneConnected) then
    exit;
  if (not fActiveDuringCompilation) and (Glb.ScriptEngine.Compiling) then
    exit;
  lSlot := GetFreeComSlot;
  if lSlot < 0 then
  begin
    WaitForXplane;
    lSlot := GetFreeComSlot;
    if lSlot < 0 then
    begin
      DebugLog('Can''t draw text, Xplane not listening.');
      exit;
    end;
  end;
  if Glb.IsModuleLogged('XPL') then
    pBuffer^.Debug := True;

  // find out first
  StrPCopy(pBuffer^.ComSlots[lSlot].StringBuffer, pText);
  pBuffer^.ComSlots[lSlot].HDMcommand := HDMC_SHOW_TEXT;
  pBuffer^.ComSlots[lSlot].Value.floatData := pPos;
  pBuffer^.ComSlots[lSlot].Length := pSec;
  DebugLog(Format('Sending DrawText command for text %s at pos %f.', [pText, pPos]));
  pBuffer^.ComSlots[lSlot].XplRequestFlag := 1; // trigger xpl
  pBuffer^.XplRequestFlag := 1;
end;

function TXPLcontrol.GetCustomXplVariable(pName: String; pIndex: Integer;
  isArray: Boolean): Variant;
var
  lVarIndex: Integer;
  lVar: TXplVariable;
  lSecCounter: Integer;
  lRes: String;
  lSlot: Integer;
  lVariant: Variant;
begin
  Result := 0;
  if (fMM.Memory = nil) or (not isXplaneConnected) then
    exit;
  if (not fActiveDuringCompilation) and (Glb.ScriptEngine.Compiling) then
    exit;
  lSlot := GetFreeComSlot;
  if lSlot < 0 then
  begin
    WaitForXplane;
    lSlot := GetFreeComSlot;
    if lSlot < 0 then
    begin
      DebugLog('Can''t get variable, Xplane not listening.');
      exit;
    end;
  end;
  if Glb.IsModuleLogged('XPL') then
    pBuffer^.Debug := True;
  lVarIndex := fVars.IndexOf(pName);
  if lVarIndex > -1 then
  begin
    lVar := fVars.Objects[lVarIndex] as TXplVariable;
    pBuffer^.ComSlots[lSlot].DataRef := lVar.DataRef;
    pBuffer^.ComSlots[lSlot].DataType := lVar.DataType;
  end
  else
  begin
    // find out first
    StrPCopy(pBuffer^.ComSlots[lSlot].ValueName, pName);
    pBuffer^.ComSlots[lSlot].DataRef := 0;
  end;
  if isArray then
    pBuffer^.ComSlots[lSlot].Index := pIndex;
  pBuffer^.ComSlots[lSlot].HDMcommand := HDMC_GET_VAR;
  pBuffer^.ComSlots[lSlot].XplRequestFlag := 1; // trigger xpl
  pBuffer^.XplRequestFlag := 1;
  // wait for response
  WaitForXplaneSlot(lSlot);
  if pBuffer^.ComSlots[lSlot].XplRequestFlag = 0 then
  begin
    if pBuffer^.ComSlots[lSlot].DataType <> xplmType_Data then
    begin
      case pBuffer^.ComSlots[lSlot].DataType of
        xplmType_Float:  Result := pBuffer^.ComSlots[lSlot].Value.floatData;
        xplmType_Double: Result := pBuffer^.ComSlots[lSlot].Value.doubleData;
        xplmType_Int:    Result := pBuffer^.ComSlots[lSlot].Value.intData;
        xplmType_IntArray:   Result := pBuffer^.ComSlots[lSlot].Value.intData;
        xplmType_FloatArray: Result := pBuffer^.ComSlots[lSlot].Value.floatData;
      end;
    end
    else
    begin
      // string
      pBuffer^.ComSlots[lSlot].StringBuffer[XPL_MAX_STRING_SIZE -1] := #0; // for sure
      lRes := pBuffer^.ComSlots[lSlot].StringBuffer;
      DebugLog('Received string result ' + lRes);
      Result := lRes;
    end;
    if lVarIndex < 0 then
    begin
      // register returned variable ref
      if (pBuffer^.ComSlots[lSlot].DataRef = 0) then
        DebugLog('WARNING: no dataref received from Xplane for variable ' + pName + '.')
      else
      begin
        lVar := TXplVariable.Create;
        lVar.Name := pName;
        lVar.DataRef := pBuffer^.ComSlots[lSlot].DataRef;
        lVar.DataType := pBuffer^.ComSlots[lSlot].DataType;
        lVar.Writable := pBuffer^.ComSlots[lSlot].Writable;
        if lVar.IsArray then
          lVar.Length := pBuffer^.ComSlots[lSlot].Length
        else
          lVar.Length := 0;
        fVars.AddObject(pName, lVar);
        DebugLog(Format('Registered var %s at address %x.', [pName, pBuffer^.ComSlots[lSlot].DataRef]))
      end;
    end;
  end
  else
  begin
    DebugLog('Variable ' + pName + ' timed out - no response from Xplane.');
  end;
end;

function TXPLcontrol.GetElevation: double;
begin
  Result := pBuffer^.Height;
end;

function TXPLcontrol.GetFreeComSlot: Integer;
var I: Integer;
begin
  for I := 0 to COM_SLOTS_COUNT - 1 do
    if pBuffer^.ComSlots[I].XplRequestFlag = 0 then
    begin
      Result := I;
      exit;
    end;
  Result := -1;
end;

function TXPLcontrol.GetHeading: double;
begin
  Result := pBuffer^.Heading;
end;

function TXPLcontrol.GetLatitude: double;
begin
  Result := pBuffer^.Latitude;
end;

function TXPLcontrol.GetLongitude: double;
begin
  Result := pBuffer^.Longitude;
end;

function TXPLcontrol.GetPosRefreshInterval: Integer;
begin
  Result := pBuffer^.PosInterval;
end;

function TXPLcontrol.GetXplArrayItem(pName: String; pIndex: Integer): Variant;
begin
  Result := GetCustomXplVariable(pName, pIndex, True);
end;

function TXPLcontrol.GetXplVariable(pName: String): Variant;
begin
  Result := GetCustomXplVariable(pName, 0, False);
end;

function TXPLcontrol.isXplaneConnected: Boolean;
begin
  Result := (pBuffer <> nil) and (pBuffer^.XplConnected = 1);
end;

function TXPLcontrol.IsXplaneReady: Boolean;
begin
  Result := (pBuffer <> nil) and (pBuffer^.XplRequestFlag = 0);
end;

procedure TXPLcontrol.SetCustomXplVariable(pName: String; pIndex: Integer;
  pValue: Variant; isArray: Boolean; pToggleCommand: Integer);
var
  lVarIndex: Integer;
  lVar: TXplVariable;
  lSecCounter: Integer;
  lEndMsec: Cardinal;
  lIsString: Boolean;
  lStr: String;
  lUnregistered: Boolean;
  lSlot: Integer;
begin
  if (fMM.Memory = nil) or (not isXplaneConnected) then
    exit;
  if (not fActiveDuringCompilation) and (Glb.ScriptEngine.Compiling) then
    exit;
  lSlot := GetFreeComSlot;
  if lSlot < 0 then
  begin
    WaitForXplane;
    lSlot := GetFreeComSlot;
    if lSlot < 0 then
    begin
      DebugLog('Can''t set variable '+pName+', Xplane not listening.');
      exit;
    end;
  end;
  if Glb.IsModuleLogged('XPL') then
    pBuffer^.Debug := True;
  lIsString := VarType(pValue) = varOleStr;
  lVarIndex := fVars.IndexOf(pName);
  if lVarIndex > -1 then
  begin
    lVar := fVars.Objects[lVarIndex] as TXplVariable;
    pBuffer^.ComSlots[lSlot].DataRef := lVar.DataRef;
    pBuffer^.ComSlots[lSlot].DataType := lVar.DataType;
    lIsString := pBuffer^.ComSlots[lSlot].DataType = xplmType_Data;
    lUnregistered := False;
  end
  else
  begin
    // find out first
    StrPCopy(pBuffer^.ComSlots[lSlot].ValueName, pName);
    pBuffer^.ComSlots[lSlot].DataRef := 0;
    lUnregistered := True;
  end;
  if isArray then
    pBuffer^.ComSlots[lSlot].Index := pIndex;
  lStr := pValue;
  if lIsString then
  begin
    StrLCopy(pBuffer^.ComSlots[lSlot].StringBuffer, PChar(lStr), XPL_MAX_STRING_SIZE);
    //pBuffer^.Length := Length(lStr); // keep always value from Xplane
    DebugLog('Setting string variable to ' + lStr);
  end
  else
    //pBuffer^.ComSlots[lSlot].Value := Variant2VariantBuffer(pValue);
    StrLCopy(pBuffer^.ComSlots[lSlot].ValueUntyped, PChar(lStr), 255);
  if pToggleCommand > 0 then
    pBuffer^.ComSlots[lSlot].HDMcommand := pToggleCommand
  else
    pBuffer^.ComSlots[lSlot].HDMcommand := HDMC_SET_VAR;
  pBuffer^.ComSlots[lSlot].XplRequestFlag := 1;
  pBuffer^.XplRequestFlag := 1; // trigger xpl
  // if unregistered, wait for result and note down the address
  if lUnregistered then
  begin
    WaitForXplaneSlot(lSlot);
    if pBuffer^.ComSlots[lSlot].XplRequestFlag = 0 then
    begin
      lVar := TXplVariable.Create;
      lVar.Name := pName;
      lVar.DataRef := pBuffer^.ComSlots[lSlot].DataRef;
      lVar.DataType := pBuffer^.ComSlots[lSlot].DataType;
      lVar.Writable := pBuffer^.ComSlots[lSlot].Writable;
      if lVar.IsArray or lVar.IsString then
        lVar.Length := pBuffer^.ComSlots[lSlot].Length
      else
        lVar.Length := 0;
      fVars.AddObject(pName, lVar);
      DebugLog(Format('Registered var %s at address %x.', [pName, pBuffer^.ComSlots[lSlot].DataRef]))
    end;
  end;
end;

procedure TXPLcontrol.SetPosRefreshInterval(const Value: Integer);
var
  lSlot: Integer;
begin
  if (fMM.Memory = nil) or (not isXplaneConnected) then
    exit;
  if (not fActiveDuringCompilation) and (Glb.ScriptEngine.Compiling) then
    exit;
  lSlot := GetFreeComSlot;
  if lSlot < 0 then
  begin
    WaitForXplane;
    lSlot := GetFreeComSlot;
    if lSlot < 0 then
    begin
      DebugLog('Can''t set moving map refresh interval, Xplane not listening.');
      exit;
    end;
  end;
  if Glb.IsModuleLogged('XPL') then
    pBuffer^.Debug := True;
  pBuffer^.ComSlots[lSlot].HDMcommand := HDMC_SET_POSINTERVAL;
  pBuffer^.PosInterval := Value;
  pBuffer^.ComSlots[lSlot].XplRequestFlag := 1; // trigger xpl
  pBuffer^.XplRequestFlag := 1; // trigger xpl
  // if unregistered, wait for result and note down the address
  //WaitForXplaneSlot(lSlot);
end;

procedure TXPLcontrol.SetXplArrayItem(pName: String; pIndex: Integer;
  pValue: Variant);
begin
  SetCustomXplVariable(pName, pIndex, pValue, True, 0);
end;

procedure TXPLcontrol.SetXplVariable(pName: String; pValue: Variant);
begin
  SetCustomXplVariable(pName, 0, pValue, False, 0);
end;

procedure TXPLcontrol.ToggleDataRef(pName, pValues: String; pCommand: Integer);
begin
  SetCustomXplVariable(pName, 0, pValues, False, pCommand);
end;

procedure TXPLcontrol.WaitForXplane(mSec: Integer = 500);
var
  lEndMsec: Cardinal;
begin
  if fMM.Memory = nil then
    exit;
  lEndMsec := GetTickCount + mSec;
  while (GetTickCount < lEndMsec) and (pBuffer^.XplRequestFlag = 1) do
    Application.ProcessMessages;
    //Sleep(10);
end;

procedure TXPLcontrol.WaitForXplaneSlot(pSlot, mSec: Integer);
var
  lEndMsec: Cardinal;
begin
  if fMM.Memory = nil then
    exit;
  lEndMsec := GetTickCount + mSec;
  while (GetTickCount < lEndMsec) and (pBuffer^.ComSlots[pSlot].XplRequestFlag = 1) do
    Application.ProcessMessages;
    //Sleep(10);
end;

procedure TXPLcontrol.AddCommands(l: TStrings);
begin
//  l.Add('');
  l.Add('sim/none/none');
  l.Add('sim/operation/quit');
  l.Add('sim/fadec/fadec_toggle');
  l.Add('sim/engines/governor_toggle');
  l.Add('sim/engines/clutch_toggle');
  l.Add('sim/engines/engage_starters');
  l.Add('sim/engines/throttle_down');
  l.Add('sim/engines/throttle_up');
  l.Add('sim/engines/prop_down');
  l.Add('sim/engines/prop_up');
  l.Add('sim/engines/mixture_down');
  l.Add('sim/engines/mixture_up');
  l.Add('sim/engines/mixture_min');
  l.Add('sim/engines/mixture_max');
  l.Add('sim/engines/carb_heat_on');
  l.Add('sim/engines/carb_heat_off');
  l.Add('sim/engines/carb_heat_toggle');
  l.Add('sim/engines/idle_hi_lo_toggle');
  l.Add('sim/engines/TOGA_power');
  l.Add('sim/engines/thrust_reverse_toggle');
  l.Add('sim/engines/thrust_reverse_hold');
  l.Add('sim/magnetos/magnetos_off');
  l.Add('sim/magnetos/magnetos_both');
  l.Add('sim/starters/engage_start_run');
  l.Add('sim/starters/shut_down');
  l.Add('sim/magnetos/magnetos_off_1');
  l.Add('sim/magnetos/magnetos_off_2');
  l.Add('sim/magnetos/magnetos_off_3');
  l.Add('sim/magnetos/magnetos_off_4');
  l.Add('sim/magnetos/magnetos_off_5');
  l.Add('sim/magnetos/magnetos_off_6');
  l.Add('sim/magnetos/magnetos_off_7');
  l.Add('sim/magnetos/magnetos_off_8');
  l.Add('sim/magnetos/magnetos_down_1');
  l.Add('sim/magnetos/magnetos_down_2');
  l.Add('sim/magnetos/magnetos_down_3');
  l.Add('sim/magnetos/magnetos_down_4');
  l.Add('sim/magnetos/magnetos_down_5');
  l.Add('sim/magnetos/magnetos_down_6');
  l.Add('sim/magnetos/magnetos_down_7');
  l.Add('sim/magnetos/magnetos_down_8');
  l.Add('sim/magnetos/magnetos_up_1');
  l.Add('sim/magnetos/magnetos_up_2');
  l.Add('sim/magnetos/magnetos_up_3');
  l.Add('sim/magnetos/magnetos_up_4');
  l.Add('sim/magnetos/magnetos_up_5');
  l.Add('sim/magnetos/magnetos_up_6');
  l.Add('sim/magnetos/magnetos_up_7');
  l.Add('sim/magnetos/magnetos_up_8');
  l.Add('sim/magnetos/magnetos_left_1');
  l.Add('sim/magnetos/magnetos_left_2');
  l.Add('sim/magnetos/magnetos_left_3');
  l.Add('sim/magnetos/magnetos_left_4');
  l.Add('sim/magnetos/magnetos_left_5');
  l.Add('sim/magnetos/magnetos_left_6');
  l.Add('sim/magnetos/magnetos_left_7');
  l.Add('sim/magnetos/magnetos_left_8');
  l.Add('sim/magnetos/magnetos_right_1');
  l.Add('sim/magnetos/magnetos_right_2');
  l.Add('sim/magnetos/magnetos_right_3');
  l.Add('sim/magnetos/magnetos_right_4');
  l.Add('sim/magnetos/magnetos_right_5');
  l.Add('sim/magnetos/magnetos_right_6');
  l.Add('sim/magnetos/magnetos_right_7');
  l.Add('sim/magnetos/magnetos_right_8');
  l.Add('sim/magnetos/magnetos_both_1');
  l.Add('sim/magnetos/magnetos_both_2');
  l.Add('sim/magnetos/magnetos_both_3');
  l.Add('sim/magnetos/magnetos_both_4');
  l.Add('sim/magnetos/magnetos_both_5');
  l.Add('sim/magnetos/magnetos_both_6');
  l.Add('sim/magnetos/magnetos_both_7');
  l.Add('sim/magnetos/magnetos_both_8');
  l.Add('sim/starters/engage_starter_1');
  l.Add('sim/starters/engage_starter_2');
  l.Add('sim/starters/engage_starter_3');
  l.Add('sim/starters/engage_starter_4');
  l.Add('sim/starters/engage_starter_5');
  l.Add('sim/starters/engage_starter_6');
  l.Add('sim/starters/engage_starter_7');
  l.Add('sim/starters/engage_starter_8');
  l.Add('sim/starters/engage_start_run_1');
  l.Add('sim/starters/engage_start_run_2');
  l.Add('sim/starters/engage_start_run_3');
  l.Add('sim/starters/engage_start_run_4');
  l.Add('sim/starters/engage_start_run_5');
  l.Add('sim/starters/engage_start_run_6');
  l.Add('sim/starters/engage_start_run_7');
  l.Add('sim/starters/engage_start_run_8');
  l.Add('sim/starters/shut_down_1');
  l.Add('sim/starters/shut_down_2');
  l.Add('sim/starters/shut_down_3');
  l.Add('sim/starters/shut_down_4');
  l.Add('sim/starters/shut_down_5');
  l.Add('sim/starters/shut_down_6');
  l.Add('sim/starters/shut_down_7');
  l.Add('sim/starters/shut_down_8');
  l.Add('sim/igniters/igniter_arm_on_1');
  l.Add('sim/igniters/igniter_arm_on_2');
  l.Add('sim/igniters/igniter_arm_on_3');
  l.Add('sim/igniters/igniter_arm_on_4');
  l.Add('sim/igniters/igniter_arm_on_5');
  l.Add('sim/igniters/igniter_arm_on_6');
  l.Add('sim/igniters/igniter_arm_on_7');
  l.Add('sim/igniters/igniter_arm_on_8');
  l.Add('sim/igniters/igniter_arm_off_1');
  l.Add('sim/igniters/igniter_arm_off_2');
  l.Add('sim/igniters/igniter_arm_off_3');
  l.Add('sim/igniters/igniter_arm_off_4');
  l.Add('sim/igniters/igniter_arm_off_5');
  l.Add('sim/igniters/igniter_arm_off_6');
  l.Add('sim/igniters/igniter_arm_off_7');
  l.Add('sim/igniters/igniter_arm_off_8');
  l.Add('sim/igniters/igniter_contin_on_1');
  l.Add('sim/igniters/igniter_contin_on_2');
  l.Add('sim/igniters/igniter_contin_on_3');
  l.Add('sim/igniters/igniter_contin_on_4');
  l.Add('sim/igniters/igniter_contin_on_5');
  l.Add('sim/igniters/igniter_contin_on_6');
  l.Add('sim/igniters/igniter_contin_on_7');
  l.Add('sim/igniters/igniter_contin_on_8');
  l.Add('sim/igniters/igniter_contin_off_1');
  l.Add('sim/igniters/igniter_contin_off_2');
  l.Add('sim/igniters/igniter_contin_off_3');
  l.Add('sim/igniters/igniter_contin_off_4');
  l.Add('sim/igniters/igniter_contin_off_5');
  l.Add('sim/igniters/igniter_contin_off_6');
  l.Add('sim/igniters/igniter_contin_off_7');
  l.Add('sim/igniters/igniter_contin_off_8');
  l.Add('sim/fadec/fadec_1_on');
  l.Add('sim/fadec/fadec_2_on');
  l.Add('sim/fadec/fadec_3_on');
  l.Add('sim/fadec/fadec_4_on');
  l.Add('sim/fadec/fadec_5_on');
  l.Add('sim/fadec/fadec_6_on');
  l.Add('sim/fadec/fadec_7_on');
  l.Add('sim/fadec/fadec_8_on');
  l.Add('sim/fadec/fadec_1_off');
  l.Add('sim/fadec/fadec_2_off');
  l.Add('sim/fadec/fadec_3_off');
  l.Add('sim/fadec/fadec_4_off');
  l.Add('sim/fadec/fadec_5_off');
  l.Add('sim/fadec/fadec_6_off');
  l.Add('sim/fadec/fadec_7_off');
  l.Add('sim/fadec/fadec_8_off');
  l.Add('sim/engines/fire_ext_1_on');
  l.Add('sim/engines/fire_ext_2_on');
  l.Add('sim/engines/fire_ext_3_on');
  l.Add('sim/engines/fire_ext_4_on');
  l.Add('sim/engines/fire_ext_5_on');
  l.Add('sim/engines/fire_ext_6_on');
  l.Add('sim/engines/fire_ext_7_on');
  l.Add('sim/engines/fire_ext_8_on');
  l.Add('sim/engines/fire_ext_1_off');
  l.Add('sim/engines/fire_ext_2_off');
  l.Add('sim/engines/fire_ext_3_off');
  l.Add('sim/engines/fire_ext_4_off');
  l.Add('sim/engines/fire_ext_5_off');
  l.Add('sim/engines/fire_ext_6_off');
  l.Add('sim/engines/fire_ext_7_off');
  l.Add('sim/engines/fire_ext_8_off');
  l.Add('sim/electrical/generator_1_on');
  l.Add('sim/electrical/generator_2_on');
  l.Add('sim/electrical/generator_3_on');
  l.Add('sim/electrical/generator_4_on');
  l.Add('sim/electrical/generator_5_on');
  l.Add('sim/electrical/generator_6_on');
  l.Add('sim/electrical/generator_7_on');
  l.Add('sim/electrical/generator_8_on');
  l.Add('sim/electrical/generator_1_off');
  l.Add('sim/electrical/generator_2_off');
  l.Add('sim/electrical/generator_3_off');
  l.Add('sim/electrical/generator_4_off');
  l.Add('sim/electrical/generator_5_off');
  l.Add('sim/electrical/generator_6_off');
  l.Add('sim/electrical/generator_7_off');
  l.Add('sim/electrical/generator_8_off');
  l.Add('sim/electrical/generator_1_reset');
  l.Add('sim/electrical/generator_2_reset');
  l.Add('sim/electrical/generator_3_reset');
  l.Add('sim/electrical/generator_4_reset');
  l.Add('sim/electrical/generator_5_reset');
  l.Add('sim/electrical/generator_6_reset');
  l.Add('sim/electrical/generator_7_reset');
  l.Add('sim/electrical/generator_8_reset');
  l.Add('sim/altair/alternate_air_on_1');
  l.Add('sim/altair/alternate_air_on_2');
  l.Add('sim/altair/alternate_air_on_3');
  l.Add('sim/altair/alternate_air_on_4');
  l.Add('sim/altair/alternate_air_on_5');
  l.Add('sim/altair/alternate_air_on_6');
  l.Add('sim/altair/alternate_air_on_7');
  l.Add('sim/altair/alternate_air_on_8');
  l.Add('sim/altair/alternate_air_off_1');
  l.Add('sim/altair/alternate_air_off_2');
  l.Add('sim/altair/alternate_air_off_3');
  l.Add('sim/altair/alternate_air_off_4');
  l.Add('sim/altair/alternate_air_off_5');
  l.Add('sim/altair/alternate_air_off_6');
  l.Add('sim/altair/alternate_air_off_7');
  l.Add('sim/altair/alternate_air_off_8');
  l.Add('sim/altair/alternate_air_backup_on_1');
  l.Add('sim/altair/alternate_air_backup_on_2');
  l.Add('sim/altair/alternate_air_backup_on_3');
  l.Add('sim/altair/alternate_air_backup_on_4');
  l.Add('sim/altair/alternate_air_backup_on_5');
  l.Add('sim/altair/alternate_air_backup_on_6');
  l.Add('sim/altair/alternate_air_backup_on_7');
  l.Add('sim/altair/alternate_air_backup_on_8');
  l.Add('sim/altair/alternate_air_backup_off_1');
  l.Add('sim/altair/alternate_air_backup_off_2');
  l.Add('sim/altair/alternate_air_backup_off_3');
  l.Add('sim/altair/alternate_air_backup_off_4');
  l.Add('sim/altair/alternate_air_backup_off_5');
  l.Add('sim/altair/alternate_air_backup_off_6');
  l.Add('sim/altair/alternate_air_backup_off_7');
  l.Add('sim/altair/alternate_air_backup_off_8');
  l.Add('sim/engines/throttle_down_1');
  l.Add('sim/engines/throttle_down_2');
  l.Add('sim/engines/throttle_down_3');
  l.Add('sim/engines/throttle_down_4');
  l.Add('sim/engines/throttle_down_5');
  l.Add('sim/engines/throttle_down_6');
  l.Add('sim/engines/throttle_down_7');
  l.Add('sim/engines/throttle_down_8');
  l.Add('sim/engines/throttle_up_1');
  l.Add('sim/engines/throttle_up_2');
  l.Add('sim/engines/throttle_up_3');
  l.Add('sim/engines/throttle_up_4');
  l.Add('sim/engines/throttle_up_5');
  l.Add('sim/engines/throttle_up_6');
  l.Add('sim/engines/throttle_up_7');
  l.Add('sim/engines/throttle_up_8');
  l.Add('sim/engines/prop_down_1');
  l.Add('sim/engines/prop_down_2');
  l.Add('sim/engines/prop_down_3');
  l.Add('sim/engines/prop_down_4');
  l.Add('sim/engines/prop_down_5');
  l.Add('sim/engines/prop_down_6');
  l.Add('sim/engines/prop_down_7');
  l.Add('sim/engines/prop_down_8');
  l.Add('sim/engines/prop_up_1');
  l.Add('sim/engines/prop_up_2');
  l.Add('sim/engines/prop_up_3');
  l.Add('sim/engines/prop_up_4');
  l.Add('sim/engines/prop_up_5');
  l.Add('sim/engines/prop_up_6');
  l.Add('sim/engines/prop_up_7');
  l.Add('sim/engines/prop_up_8');
  l.Add('sim/engines/mixture_down_1');
  l.Add('sim/engines/mixture_down_2');
  l.Add('sim/engines/mixture_down_3');
  l.Add('sim/engines/mixture_down_4');
  l.Add('sim/engines/mixture_down_5');
  l.Add('sim/engines/mixture_down_6');
  l.Add('sim/engines/mixture_down_7');
  l.Add('sim/engines/mixture_down_8');
  l.Add('sim/engines/mixture_up_1');
  l.Add('sim/engines/mixture_up_2');
  l.Add('sim/engines/mixture_up_3');
  l.Add('sim/engines/mixture_up_4');
  l.Add('sim/engines/mixture_up_5');
  l.Add('sim/engines/mixture_up_6');
  l.Add('sim/engines/mixture_up_7');
  l.Add('sim/engines/mixture_up_8');
  l.Add('sim/flight_controls/pitch_trim_up');
  l.Add('sim/flight_controls/pitch_trim_takeoff');
  l.Add('sim/flight_controls/pitch_trim_down');
  l.Add('sim/flight_controls/rudder_trim_left');
  l.Add('sim/flight_controls/rudder_trim_center');
  l.Add('sim/flight_controls/rudder_trim_right');
  l.Add('sim/flight_controls/aileron_trim_left');
  l.Add('sim/flight_controls/aileron_trim_center');
  l.Add('sim/flight_controls/aileron_trim_right');
  l.Add('sim/flight_controls/gyro_rotor_trim_up');
  l.Add('sim/flight_controls/gyro_rotor_trim_down');
  l.Add('sim/flight_controls/rudder_lft');
  l.Add('sim/flight_controls/rudder_ctr');
  l.Add('sim/flight_controls/rudder_rgt');
  l.Add('sim/view/forward');
  l.Add('sim/view/pan_up');
  l.Add('sim/view/pan_down');
  l.Add('sim/view/pan_up_fast');
  l.Add('sim/view/pan_down_fast');
  l.Add('sim/view/pan_left');
  l.Add('sim/view/pan_right');
  l.Add('sim/view/pan_left_fast');
  l.Add('sim/view/pan_right_fast');
  l.Add('sim/view/glance_left');
  l.Add('sim/view/glance_right');
  l.Add('sim/view/straight_up');
  l.Add('sim/view/straight_down');
  l.Add('sim/view/back');
  l.Add('sim/view/left_up');
  l.Add('sim/view/right_up');
  l.Add('sim/view/forward_no_hud');
  l.Add('sim/view/forward_with_hud');
  l.Add('sim/view/tower');
  l.Add('sim/view/runway');
  l.Add('sim/view/chase');
  l.Add('sim/view/circle');
  l.Add('sim/view/circle_with_panel');
  l.Add('sim/view/spot');
  l.Add('sim/view/linear_spot');
  l.Add('sim/view/center');
  l.Add('sim/view/track_weapon');
  l.Add('sim/view/cinema_verite');
  l.Add('sim/view/sunglasses');
  l.Add('sim/view/night_vision_toggle');
  l.Add('sim/view/transparent_panel');
  l.Add('sim/view/3d_cockpit');
  l.Add('sim/view/3d_cockpit_toggle');
  l.Add('sim/view/3d_cockpit_mouse_look');
  l.Add('sim/view/free_view_toggle');
  l.Add('sim/view/show_physics_model');
  l.Add('sim/view/3d_path_toggle');
  l.Add('sim/view/mouse_click_regions_toggle');
  l.Add('sim/view/instrument_descriptions_toggle');
  l.Add('sim/view/hold_3d_angle');
  l.Add('sim/flight_controls/flaps_down');
  l.Add('sim/flight_controls/flaps_up');
  l.Add('sim/flight_controls/cowl_flaps_closed');
  l.Add('sim/flight_controls/cowl_flaps_open');
  l.Add('sim/flight_controls/landing_gear_down');
  l.Add('sim/flight_controls/landing_gear_up');
  l.Add('sim/flight_controls/landing_gear_toggle');
  l.Add('sim/flight_controls/pump_flaps_and_gear');
  l.Add('sim/flight_controls/nwheel_steer_toggle');
  l.Add('sim/flight_controls/speed_brakes_down_one');
  l.Add('sim/flight_controls/speed_brakes_up_one');
  l.Add('sim/flight_controls/speed_brakes_down_all');
  l.Add('sim/flight_controls/speed_brakes_up_all');
  l.Add('sim/flight_controls/speed_brakes_toggle');
  l.Add('sim/flight_controls/brakes_toggle_regular');
  l.Add('sim/flight_controls/brakes_toggle_max');
  l.Add('sim/flight_controls/brakes_regular');
  l.Add('sim/flight_controls/brakes_max');
  l.Add('sim/flight_controls/left_brake');
  l.Add('sim/flight_controls/right_brake');
  l.Add('sim/flight_controls/vector_sweep_aft');
  l.Add('sim/flight_controls/vector_sweep_forward');
  l.Add('sim/flight_controls/blimp_lift_down');
  l.Add('sim/flight_controls/blimp_lift_up');
  l.Add('sim/fuel/indicate_aux');
  l.Add('sim/fuel/fuel_tank_selector_lft_one');
  l.Add('sim/fuel/fuel_tank_selector_rgt_one');
  l.Add('sim/fuel/fuel_selector_none');
  l.Add('sim/fuel/fuel_selector_lft');
  l.Add('sim/fuel/fuel_selector_ctr');
  l.Add('sim/fuel/fuel_selector_rgt');
  l.Add('sim/fuel/fuel_selector_all');
  l.Add('sim/fuel/fuel_transfer_to_lft');
  l.Add('sim/fuel/fuel_transfer_to_ctr');
  l.Add('sim/fuel/fuel_transfer_to_rgt');
  l.Add('sim/fuel/fuel_transfer_to_off');
  l.Add('sim/fuel/fuel_transfer_from_lft');
  l.Add('sim/fuel/fuel_transfer_from_ctr');
  l.Add('sim/fuel/fuel_transfer_from_rgt');
  l.Add('sim/fuel/fuel_transfer_from_off');
  l.Add('sim/fuel/fuel_crossfeed_from_lft_tank');
  l.Add('sim/fuel/fuel_crossfeed_off');
  l.Add('sim/fuel/fuel_crossfeed_from_rgt_tank');
  l.Add('sim/fuel/fuel_firewall_valve_lft_open');
  l.Add('sim/fuel/fuel_firewall_valve_lft_closed');
  l.Add('sim/fuel/fuel_firewall_valve_rgt_open');
  l.Add('sim/fuel/fuel_firewall_valve_rgt_closed');
  l.Add('sim/fuel/fuel_pump_1_on');
  l.Add('sim/fuel/fuel_pump_2_on');
  l.Add('sim/fuel/fuel_pump_3_on');
  l.Add('sim/fuel/fuel_pump_4_on');
  l.Add('sim/fuel/fuel_pump_5_on');
  l.Add('sim/fuel/fuel_pump_6_on');
  l.Add('sim/fuel/fuel_pump_7_on');
  l.Add('sim/fuel/fuel_pump_8_on');
  l.Add('sim/fuel/fuel_pump_1_off');
  l.Add('sim/fuel/fuel_pump_2_off');
  l.Add('sim/fuel/fuel_pump_3_off');
  l.Add('sim/fuel/fuel_pump_4_off');
  l.Add('sim/fuel/fuel_pump_5_off');
  l.Add('sim/fuel/fuel_pump_6_off');
  l.Add('sim/fuel/fuel_pump_7_off');
  l.Add('sim/fuel/fuel_pump_8_off');
  l.Add('sim/fuel/fuel_pump_1_prime');
  l.Add('sim/fuel/fuel_pump_2_prime');
  l.Add('sim/fuel/fuel_pump_3_prime');
  l.Add('sim/fuel/fuel_pump_4_prime');
  l.Add('sim/fuel/fuel_pump_5_prime');
  l.Add('sim/fuel/fuel_pump_6_prime');
  l.Add('sim/fuel/fuel_pump_7_prime');
  l.Add('sim/fuel/fuel_pump_8_prime');
  l.Add('sim/engines/rockets_up');
  l.Add('sim/engines/rockets_down');
  l.Add('sim/engines/rockets_left');
  l.Add('sim/engines/rockets_right');
  l.Add('sim/engines/rockets_forward');
  l.Add('sim/engines/rockets_aft');
  l.Add('sim/lights/landing_lights_on');
  l.Add('sim/lights/landing_lights_off');
  l.Add('sim/lights/landing_lights_toggle');
  l.Add('sim/lights/taxi_lights_on');
  l.Add('sim/lights/taxi_lights_off');
  l.Add('sim/lights/taxi_lights_toggle');
  l.Add('sim/lights/strobe_lights_on');
  l.Add('sim/lights/strobe_lights_off');
  l.Add('sim/lights/strobe_lights_toggle');
  l.Add('sim/lights/nav_lights_on');
  l.Add('sim/lights/nav_lights_off');
  l.Add('sim/lights/nav_lights_toggle');
  l.Add('sim/lights/beacon_lights_on');
  l.Add('sim/lights/beacon_lights_off');
  l.Add('sim/lights/beacon_lights_toggle');
  l.Add('sim/lights/landing_light_left');
  l.Add('sim/lights/landing_light_right');
  l.Add('sim/lights/landing_light_up');
  l.Add('sim/lights/landing_light_down');
  l.Add('sim/lights/landing_light_center');
  l.Add('sim/annunciator/test_all_annunciators');
  l.Add('sim/annunciator/test_stall');
  l.Add('sim/annunciator/test_fire_L_annun');
  l.Add('sim/annunciator/test_fire_R_annun');
  l.Add('sim/annunciator/clear_master_caution');
  l.Add('sim/annunciator/clear_master_warning');
  l.Add('sim/annunciator/clear_master_accept');
  l.Add('sim/annunciator/gear_warning_mute');
  l.Add('sim/electrical/dc_volt_ext');
  l.Add('sim/electrical/dc_volt_ctr');
  l.Add('sim/electrical/dc_volt_lft');
  l.Add('sim/electrical/dc_volt_rgt');
  l.Add('sim/electrical/dc_volt_tpl');
  l.Add('sim/electrical/dc_volt_bat');
  l.Add('sim/electrical/inverters_on');
  l.Add('sim/electrical/inverters_off');
  l.Add('sim/electrical/inverters_toggle');
  l.Add('sim/electrical/inverter_1_on');
  l.Add('sim/electrical/inverter_1_off');
  l.Add('sim/electrical/inverter_1_toggle');
  l.Add('sim/electrical/inverter_2_on');
  l.Add('sim/electrical/inverter_2_off');
  l.Add('sim/electrical/inverter_2_toggle');
  l.Add('sim/electrical/cross_tie_on');
  l.Add('sim/electrical/cross_tie_off');
  l.Add('sim/electrical/cross_tie_toggle');
  l.Add('sim/electrical/battery_1_on');
  l.Add('sim/electrical/battery_1_off');
  l.Add('sim/electrical/battery_2_on');
  l.Add('sim/electrical/battery_2_off');
  l.Add('sim/electrical/batteries_toggle');
  l.Add('sim/electrical/generators_toggle');
  l.Add('sim/electrical/APU_on');
  l.Add('sim/electrical/APU_off');
  l.Add('sim/electrical/APU_start');
  l.Add('sim/electrical/GPU_on');
  l.Add('sim/electrical/GPU_off');
  l.Add('sim/electrical/GPU_toggle');
  l.Add('sim/systems/avionics_on');
  l.Add('sim/systems/avionics_off');
  l.Add('sim/systems/avionics_toggle');
  l.Add('sim/systems/yaw_damper_on');
  l.Add('sim/systems/yaw_damper_off');
  l.Add('sim/systems/yaw_damper_toggle');
  l.Add('sim/systems/prop_sync_on');
  l.Add('sim/systems/prop_sync_off');
  l.Add('sim/systems/prop_sync_toggle');
  l.Add('sim/systems/feather_mode_off');
  l.Add('sim/systems/feather_mode_arm');
  l.Add('sim/systems/feather_mode_test');
  l.Add('sim/systems/overspeed_test');
  l.Add('sim/systems/artificial_stability_toggle');
  l.Add('sim/systems/pre_rotate_toggle');
  l.Add('sim/systems/total_energy_audio_toggle');
  l.Add('sim/bleed_air/bleed_air_left');
  l.Add('sim/bleed_air/bleed_air_auto');
  l.Add('sim/bleed_air/bleed_air_right');
  l.Add('sim/bleed_air/bleed_air_off');
  l.Add('sim/bleed_air/bleed_air_left_on');
  l.Add('sim/bleed_air/bleed_air_left_ins_only');
  l.Add('sim/bleed_air/bleed_air_left_off');
  l.Add('sim/bleed_air/bleed_air_right_on');
  l.Add('sim/bleed_air/bleed_air_right_ins_only');
  l.Add('sim/bleed_air/bleed_air_right_off');
  l.Add('sim/pressurization/dump_on');
  l.Add('sim/pressurization/dump_off');
  l.Add('sim/pressurization/vvi_down');
  l.Add('sim/pressurization/vvi_up');
  l.Add('sim/pressurization/cabin_alt_down');
  l.Add('sim/pressurization/cabin_alt_up');
  l.Add('sim/pressurization/test');
  l.Add('sim/ice/alternate_static_port');
  l.Add('sim/ice/anti_ice_toggle');
  l.Add('sim/ice/inlet_heat0_on');
  l.Add('sim/ice/inlet_heat0_off');
  l.Add('sim/ice/inlet_heat1_on');
  l.Add('sim/ice/inlet_heat1_off');
  l.Add('sim/ice/prop_heat_on');
  l.Add('sim/ice/prop_heat_off');
  l.Add('sim/ice/window_heat_on');
  l.Add('sim/ice/window_heat_off');
  l.Add('sim/ice/pitot_heat_on');
  l.Add('sim/ice/pitot_heat_off');
  l.Add('sim/ice/AOA_heat_on');
  l.Add('sim/ice/AOA_heat_off');
  l.Add('sim/ice/wing_heat0_on');
  l.Add('sim/ice/wing_heat0_off');
  l.Add('sim/ice/wing_heat1_on');
  l.Add('sim/ice/wing_heat1_off');
  l.Add('sim/ice/detect_on');
  l.Add('sim/ice/detect_off');
  l.Add('sim/ice/brake_on');
  l.Add('sim/ice/brake_off');
  l.Add('sim/HUD/power_toggle');
  l.Add('sim/HUD/brightness_toggle');
  l.Add('sim/instruments/map_zoom_in');
  l.Add('sim/instruments/map_zoom_out');
  l.Add('sim/instruments/EFIS_wxr');
  l.Add('sim/instruments/EFIS_tcas');
  l.Add('sim/instruments/EFIS_apt');
  l.Add('sim/instruments/EFIS_fix');
  l.Add('sim/instruments/EFIS_vor');
  l.Add('sim/instruments/EFIS_ndb');
  l.Add('sim/instruments/panel_bright_down');
  l.Add('sim/instruments/panel_bright_up');
  l.Add('sim/instruments/instrument_bright_down');
  l.Add('sim/instruments/instrument_bright_up');
  l.Add('sim/instruments/timer_start_stop');
  l.Add('sim/instruments/timer_reset');
  l.Add('sim/instruments/timer_show_date');
  l.Add('sim/instruments/timer_is_GMT');
  l.Add('sim/instruments/thermo_units_toggle');
  l.Add('sim/instruments/barometer_down');
  l.Add('sim/instruments/barometer_up');
  l.Add('sim/instruments/barometer_2992');
  l.Add('sim/instruments/ah_ref_down');
  l.Add('sim/instruments/ah_ref_up');
  l.Add('sim/instruments/dh_ref_down');
  l.Add('sim/instruments/dh_ref_up');
  l.Add('sim/instruments/asi_bug_down');
  l.Add('sim/instruments/asi_bug_up');
  l.Add('sim/flight_controls/tailhook_down');
  l.Add('sim/flight_controls/tailhook_up');
  l.Add('sim/flight_controls/tailhook_toggle');
  l.Add('sim/flight_controls/canopy_open');
  l.Add('sim/flight_controls/canopy_close');
  l.Add('sim/flight_controls/canopy_toggle');
  l.Add('sim/flight_controls/smoke_toggle');
  l.Add('sim/flight_controls/water_scoop_toggle');
  l.Add('sim/weapons/master_arm_on');
  l.Add('sim/weapons/master_arm_off');
  l.Add('sim/weapons/weapon_select_down');
  l.Add('sim/weapons/weapon_select_up');
  l.Add('sim/weapons/weapon_target_down');
  l.Add('sim/weapons/weapon_target_up');
  l.Add('sim/weapons/fire_guns');
  l.Add('sim/weapons/fire_air_to_air');
  l.Add('sim/weapons/fire_air_to_ground');
  l.Add('sim/weapons/fire_any_armed');
  l.Add('sim/weapons/deploy_chaff');
  l.Add('sim/weapons/deploy_flares');
  l.Add('sim/flight_controls/parachute_flares');
  l.Add('sim/flight_controls/ignite_jato');
  l.Add('sim/flight_controls/jettison_payload');
  l.Add('sim/flight_controls/dump_fuel_toggle');
  l.Add('sim/flight_controls/drop_tank');
  l.Add('sim/flight_controls/deploy_parachute');
  l.Add('sim/flight_controls/eject');
  l.Add('sim/flight_controls/glider_tow_release');
  l.Add('sim/flight_controls/winch_release');
  l.Add('sim/flight_controls/carrier_ILS');
  l.Add('sim/systems/seatbelt_sign_toggle');
  l.Add('sim/systems/no_smoking_toggle');
  l.Add('sim/autopilot/hsi_select_nav_1');
  l.Add('sim/autopilot/hsi_select_nav_2');
  l.Add('sim/autopilot/hsi_select_gps');
  l.Add('sim/autopilot/flight_dir_on_only');
  l.Add('sim/autopilot/servos_and_flight_dir_on');
  l.Add('sim/autopilot/servos_and_flight_dir_off');
  l.Add('sim/autopilot/servos_fdir_yawd_off');
  l.Add('sim/autopilot/servos_fdir_yawd_trim_off');
  l.Add('sim/autopilot/fdir_servos_down_one');
  l.Add('sim/autopilot/fdir_servos_up_one');
  l.Add('sim/autopilot/fdir_servos_toggle');
  l.Add('sim/autopilot/control_wheel_steer');
  l.Add('sim/autopilot/autothrottle_on');
  l.Add('sim/autopilot/autothrottle_off');
  l.Add('sim/autopilot/autothrottle_toggle');
  l.Add('sim/autopilot/wing_leveler');
  l.Add('sim/autopilot/heading');
  l.Add('sim/autopilot/NAV');
  l.Add('sim/autopilot/pitch_sync');
  l.Add('sim/autopilot/level_change');
  l.Add('sim/autopilot/vertical_speed');
  l.Add('sim/autopilot/altitude_hold');
  l.Add('sim/autopilot/altitude_arm');
  l.Add('sim/autopilot/vnav');
  l.Add('sim/autopilot/FMS');
  l.Add('sim/autopilot/glide_slope');
  l.Add('sim/autopilot/back_course');
  l.Add('sim/autopilot/approach');
  l.Add('sim/autopilot/take_off_go_around');
  l.Add('sim/autopilot/reentry');
  l.Add('sim/autopilot/terrain_following');
  l.Add('sim/autopilot/hdg_alt_spd_on');
  l.Add('sim/autopilot/hdg_alt_spd_off');
  l.Add('sim/autopilot/airspeed_down');
  l.Add('sim/autopilot/airspeed_up');
  l.Add('sim/autopilot/airspeed_sync');
  l.Add('sim/autopilot/heading_down');
  l.Add('sim/autopilot/heading_up');
  l.Add('sim/autopilot/heading_sync');
  l.Add('sim/autopilot/vertical_speed_down');
  l.Add('sim/autopilot/vertical_speed_up');
  l.Add('sim/autopilot/vertical_speed_sync');
  l.Add('sim/autopilot/altitude_down');
  l.Add('sim/autopilot/altitude_up');
  l.Add('sim/autopilot/altitude_sync');
  l.Add('sim/autopilot/nose_down');
  l.Add('sim/autopilot/nose_up');
  l.Add('sim/autopilot/nose_down_pitch_mode');
  l.Add('sim/autopilot/nose_up_pitch_mode');
  l.Add('sim/autopilot/override_left');
  l.Add('sim/autopilot/override_right');
  l.Add('sim/autopilot/override_up');
  l.Add('sim/autopilot/override_down');
  l.Add('sim/autopilot/bank_limit_toggle');
  l.Add('sim/autopilot/soft_ride_toggle');
  l.Add('sim/autopilot/listen_g430_1');
  l.Add('sim/autopilot/listen_g430_2');
  l.Add('sim/autopilot/test_auto_annunciators');
  l.Add('sim/radios/obs1_down');
  l.Add('sim/radios/obs1_up');
  l.Add('sim/radios/obs2_down');
  l.Add('sim/radios/obs2_up');
  l.Add('sim/radios/obs_HSI_down');
  l.Add('sim/radios/obs_HSI_up');
  l.Add('sim/radios/RMI_L_tog');
  l.Add('sim/radios/RMI_R_tog');
  l.Add('sim/radios/copilot_obs1_down');
  l.Add('sim/radios/copilot_obs1_up');
  l.Add('sim/radios/copilot_obs2_down');
  l.Add('sim/radios/copilot_obs2_up');
  l.Add('sim/radios/copilot_obs_HSI_down');
  l.Add('sim/radios/copilot_obs_HSI_up');
  l.Add('sim/radios/copilot_RMI_L_tog_cop');
  l.Add('sim/radios/copilot_RMI_R_tog_cop');
  l.Add('sim/radios/nav1_standy_flip');
  l.Add('sim/radios/nav2_standy_flip');
  l.Add('sim/radios/com1_standy_flip');
  l.Add('sim/radios/com2_standy_flip');
  l.Add('sim/radios/adf1_standy_flip');
  l.Add('sim/radios/adf2_standy_flip');
  l.Add('sim/radios/dme_standby_flip');
  l.Add('sim/radios/actv_com1_coarse_down');
  l.Add('sim/radios/actv_com1_coarse_up');
  l.Add('sim/radios/actv_com1_fine_down');
  l.Add('sim/radios/actv_com1_fine_up');
  l.Add('sim/radios/actv_nav1_coarse_down');
  l.Add('sim/radios/actv_nav1_coarse_up');
  l.Add('sim/radios/actv_nav1_fine_down');
  l.Add('sim/radios/actv_nav1_fine_up');
  l.Add('sim/radios/actv_adf1_hundreds_down');
  l.Add('sim/radios/actv_adf1_hundreds_up');
  l.Add('sim/radios/actv_adf1_tens_down');
  l.Add('sim/radios/actv_adf1_tens_up');
  l.Add('sim/radios/actv_adf1_ones_down');
  l.Add('sim/radios/actv_adf1_ones_up');
  l.Add('sim/radios/actv_adf2_hundreds_down');
  l.Add('sim/radios/actv_com2_coarse_down');
  l.Add('sim/radios/actv_com2_coarse_up');
  l.Add('sim/radios/actv_com2_fine_down');
  l.Add('sim/radios/actv_com2_fine_up');
  l.Add('sim/radios/actv_nav2_coarse_down');
  l.Add('sim/radios/actv_nav2_coarse_up');
  l.Add('sim/radios/actv_nav2_fine_down');
  l.Add('sim/radios/actv_nav2_fine_up');
  l.Add('sim/radios/actv_adf2_hundreds_up');
  l.Add('sim/radios/actv_adf2_tens_down');
  l.Add('sim/radios/actv_adf2_tens_up');
  l.Add('sim/radios/actv_adf2_ones_down');
  l.Add('sim/radios/actv_adf2_ones_up');
  l.Add('sim/radios/stby_com1_coarse_down');
  l.Add('sim/radios/stby_com1_coarse_up');
  l.Add('sim/radios/stby_com1_fine_down');
  l.Add('sim/radios/stby_com1_fine_up');
  l.Add('sim/radios/stby_nav1_coarse_down');
  l.Add('sim/radios/stby_nav1_coarse_up');
  l.Add('sim/radios/stby_nav1_fine_down');
  l.Add('sim/radios/stby_nav1_fine_up');
  l.Add('sim/radios/stby_adf1_hundreds_down');
  l.Add('sim/radios/stby_adf1_hundreds_up');
  l.Add('sim/radios/stby_adf1_tens_down');
  l.Add('sim/radios/stby_adf1_tens_up');
  l.Add('sim/radios/stby_adf1_ones_down');
  l.Add('sim/radios/stby_adf1_ones_up');
  l.Add('sim/radios/stby_com2_coarse_down');
  l.Add('sim/radios/stby_com2_coarse_up');
  l.Add('sim/radios/stby_com2_fine_down');
  l.Add('sim/radios/stby_com2_fine_up');
  l.Add('sim/radios/stby_nav2_coarse_down');
  l.Add('sim/radios/stby_nav2_coarse_up');
  l.Add('sim/radios/stby_nav2_fine_down');
  l.Add('sim/radios/stby_nav2_fine_up');
  l.Add('sim/radios/stby_adf2_hundreds_down');
  l.Add('sim/radios/stby_adf2_hundreds_up');
  l.Add('sim/radios/stby_adf2_tens_down');
  l.Add('sim/radios/stby_adf2_tens_up');
  l.Add('sim/radios/stby_adf2_ones_down');
  l.Add('sim/radios/stby_adf2_ones_up');
  l.Add('sim/transponder/transponder_ident');
  l.Add('sim/transponder/transponder_off');
  l.Add('sim/transponder/transponder_standby');
  l.Add('sim/transponder/transponder_on');
  l.Add('sim/transponder/transponder_alt');
  l.Add('sim/transponder/transponder_test');
  l.Add('sim/transponder/transponder_ground');
  l.Add('sim/transponder/transponder_thousands_down');
  l.Add('sim/transponder/transponder_thousands_up');
  l.Add('sim/transponder/transponder_hundreds_down');
  l.Add('sim/transponder/transponder_hundreds_up');
  l.Add('sim/transponder/transponder_tens_down');
  l.Add('sim/transponder/transponder_tens_up');
  l.Add('sim/transponder/transponder_ones_down');
  l.Add('sim/transponder/transponder_ones_up');
  l.Add('sim/audio_panel/select_audio_nav1');
  l.Add('sim/audio_panel/select_audio_nav2');
  l.Add('sim/audio_panel/select_audio_com1');
  l.Add('sim/audio_panel/select_audio_com2');
  l.Add('sim/audio_panel/select_audio_adf1');
  l.Add('sim/audio_panel/select_audio_adf2');
  l.Add('sim/GPS/mode_airport');
  l.Add('sim/GPS/mode_VOR');
  l.Add('sim/GPS/mode_NDB');
  l.Add('sim/GPS/mode_waypoint');
  l.Add('sim/GPS/fine_select_down');
  l.Add('sim/GPS/fine_select_up');
  l.Add('sim/GPS/coarse_select_down');
  l.Add('sim/GPS/coarse_select_up');
  l.Add('sim/GPS/g430n1_zoom_in');
  l.Add('sim/GPS/g430n1_zoom_out');
  l.Add('sim/GPS/g430n1_nav_com_tog');
  l.Add('sim/GPS/g430n1_cdi');
  l.Add('sim/GPS/g430n1_obs');
  l.Add('sim/GPS/g430n1_msg');
  l.Add('sim/GPS/g430n1_fpl');
  l.Add('sim/GPS/g430n1_proc');
  l.Add('sim/GPS/g430n1_direct');
  l.Add('sim/GPS/g430n1_menu');
  l.Add('sim/GPS/g430n1_clr');
  l.Add('sim/GPS/g430n1_ent');
  l.Add('sim/GPS/g430n1_com_ff');
  l.Add('sim/GPS/g430n1_nav_ff');
  l.Add('sim/GPS/g430n1_chapter_up');
  l.Add('sim/GPS/g430n1_chapter_dn');
  l.Add('sim/GPS/g430n1_page_up');
  l.Add('sim/GPS/g430n1_page_dn');
  l.Add('sim/GPS/g430n2_zoom_in');
  l.Add('sim/GPS/g430n2_zoom_out');
  l.Add('sim/GPS/g430n2_nav_com_tog');
  l.Add('sim/GPS/g430n2_cdi');
  l.Add('sim/GPS/g430n2_obs');
  l.Add('sim/GPS/g430n2_msg');
  l.Add('sim/GPS/g430n2_fpl');
  l.Add('sim/GPS/g430n2_proc');
  l.Add('sim/GPS/g430n2_direct');
  l.Add('sim/GPS/g430n2_menu');
  l.Add('sim/GPS/g430n2_clr');
  l.Add('sim/GPS/g430n2_ent');
  l.Add('sim/GPS/g430n2_com_ff');
  l.Add('sim/GPS/g430n2_nav_ff');
  l.Add('sim/GPS/g430n2_chapter_up');
  l.Add('sim/GPS/g430n2_chapter_dn');
  l.Add('sim/GPS/g430n2_page_up');
  l.Add('sim/GPS/g430n2_page_dn');
  l.Add('sim/FMS/ls_1l');
  l.Add('sim/FMS/ls_2l');
  l.Add('sim/FMS/ls_3l');
  l.Add('sim/FMS/ls_4l');
  l.Add('sim/FMS/ls_5l');
  l.Add('sim/FMS/ls_1r');
  l.Add('sim/FMS/ls_2r');
  l.Add('sim/FMS/ls_3r');
  l.Add('sim/FMS/ls_4r');
  l.Add('sim/FMS/ls_5r');
  l.Add('sim/FMS/init');
  l.Add('sim/FMS/prev');
  l.Add('sim/FMS/next');
  l.Add('sim/FMS/clear');
  l.Add('sim/FMS/direct');
  l.Add('sim/FMS/sign');
  l.Add('sim/FMS/type_apt');
  l.Add('sim/FMS/type_vor');
  l.Add('sim/FMS/type_ndb');
  l.Add('sim/FMS/type_fix');
  l.Add('sim/FMS/type_latlon');
  l.Add('sim/FMS/key_0');
  l.Add('sim/FMS/key_1');
  l.Add('sim/FMS/key_2');
  l.Add('sim/FMS/key_3');
  l.Add('sim/FMS/key_4');
  l.Add('sim/FMS/key_5');
  l.Add('sim/FMS/key_6');
  l.Add('sim/FMS/key_7');
  l.Add('sim/FMS/key_8');
  l.Add('sim/FMS/key_9');
  l.Add('sim/FMS/key_A');
  l.Add('sim/FMS/key_B');
  l.Add('sim/FMS/key_C');
  l.Add('sim/FMS/key_D');
  l.Add('sim/FMS/key_E');
  l.Add('sim/FMS/key_F');
  l.Add('sim/FMS/key_G');
  l.Add('sim/FMS/key_H');
  l.Add('sim/FMS/key_I');
  l.Add('sim/FMS/key_J');
  l.Add('sim/FMS/key_K');
  l.Add('sim/FMS/key_L');
  l.Add('sim/FMS/key_M');
  l.Add('sim/FMS/key_N');
  l.Add('sim/FMS/key_O');
  l.Add('sim/FMS/key_P');
  l.Add('sim/FMS/key_Q');
  l.Add('sim/FMS/key_R');
  l.Add('sim/FMS/key_S');
  l.Add('sim/FMS/key_T');
  l.Add('sim/FMS/key_U');
  l.Add('sim/FMS/key_V');
  l.Add('sim/FMS/key_W');
  l.Add('sim/FMS/key_X');
  l.Add('sim/FMS/key_Y');
  l.Add('sim/FMS/key_Z');
  l.Add('sim/FMS/key_back');
  l.Add('sim/FMS/key_space');
  l.Add('sim/FMS/key_load');
  l.Add('sim/FMS/key_save');
  l.Add('sim/FMS/fix_next');
  l.Add('sim/FMS/fix_prev');
  l.Add('sim/operation/pause_toggle');
  l.Add('sim/operation/ground_speed_change');
  l.Add('sim/operation/flightmodel_speed_change');
  l.Add('sim/operation/load_situation_1');
  l.Add('sim/operation/load_situation_2');
  l.Add('sim/operation/load_situation_3');
  l.Add('sim/general/action');
  l.Add('sim/general/left');
  l.Add('sim/general/right');
  l.Add('sim/general/up');
  l.Add('sim/general/down');
  l.Add('sim/general/forward');
  l.Add('sim/general/backward');
  l.Add('sim/general/zoom_in');
  l.Add('sim/general/zoom_out');
  l.Add('sim/general/left_fast');
  l.Add('sim/general/right_fast');
  l.Add('sim/general/up_fast');
  l.Add('sim/general/down_fast');
  l.Add('sim/general/forward_fast');
  l.Add('sim/general/backward_fast');
  l.Add('sim/general/zoom_in_fast');
  l.Add('sim/general/zoom_out_fast');
  l.Add('sim/map/show_low_enroute');
  l.Add('sim/map/show_high_enroute');
  l.Add('sim/map/show_sectional');
  l.Add('sim/map/show_textured');
  l.Add('sim/operation/time_down');
  l.Add('sim/operation/time_up');
  l.Add('sim/operation/date_down');
  l.Add('sim/operation/date_up');
  l.Add('sim/operation/contact_atc');
  l.Add('sim/operation/go_to_default');
  l.Add('sim/operation/reset_to_runway');
  l.Add('sim/operation/reset_3d_path');
  l.Add('sim/operation/fail_system');
  l.Add('sim/operation/fix_all_systems');
  l.Add('sim/operation/show_instructions');
  l.Add('sim/operation/text_file_toggle');
  l.Add('sim/operation/checklist_toggle');
  l.Add('sim/operation/checklist_next');
  l.Add('sim/operation/checklist_previous');
  l.Add('sim/operation/cycle_dump');
  l.Add('sim/operation/screenshot');
  l.Add('sim/operation/quicktime_record_toggle');
  l.Add('sim/operation/make_panel_previews');
  l.Add('sim/operation/stab_deriv_heading');
  l.Add('sim/operation/stab_deriv_pitch');
  l.Add('sim/operation/create_snap_marker');
  l.Add('sim/operation/test_data_ref');
  l.Add('sim/operation/slider_01');
  l.Add('sim/operation/slider_02');
  l.Add('sim/operation/slider_03');
  l.Add('sim/operation/slider_04');
  l.Add('sim/operation/slider_05');
  l.Add('sim/operation/slider_06');
  l.Add('sim/operation/slider_07');
  l.Add('sim/operation/slider_08');
  l.Add('sim/operation/slider_09');
  l.Add('sim/operation/slider_10');
  l.Add('sim/operation/slider_11');
  l.Add('sim/operation/slider_12');
  l.Add('sim/operation/slider_13');
  l.Add('sim/operation/slider_14');
  l.Add('sim/operation/slider_15');
  l.Add('sim/operation/slider_16');
  l.Add('sim/operation/slider_17');
  l.Add('sim/operation/slider_18');
  l.Add('sim/operation/slider_19');
  l.Add('sim/operation/slider_20');
  l.Add('sim/replay/replay_toggle');
  l.Add('sim/replay/rep_begin');
  l.Add('sim/replay/rep_play_fr');
  l.Add('sim/replay/rep_play_rr');
  l.Add('sim/replay/rep_play_sr');
  l.Add('sim/replay/rep_pause');
  l.Add('sim/replay/rep_play_sf');
  l.Add('sim/replay/rep_play_rf');
  l.Add('sim/replay/rep_play_ff');
  l.Add('sim/replay/rep_end');
end;


end.
