unit FSXcontrol;

interface

uses
  Classes, SimConnect, Windows, Messages;

const
  // Define a user message for message driven version of the example
  WM_USER_SIMCONNECT = WM_USER + 2;
  EVENT_SIM_START = 1;
  EVENT_SIM_STOP = 2;

type

  TVarChangeCallbackProcedure = procedure(pValue: Double) of object;

  PAutopilotData = ^TAutopilotData;
  TAutopilotData = record
    vSpeed: Double;
    vElevatorPos: Double;
  end;

  TAutopilotCallbackProcedure = procedure(pData: PAutopilotData) of object;

  TFSXcontrol = class
  private
    fReservedRequestsCount: Integer;
    fReservedDefinitionsCount: Integer;
    { Private declarations }
    fhSimConnect: THandle;               // Handle for the SimConection
    fSCAvailable : boolean;
    fSCConnected : boolean;
    fRegisteredEvents : TStringList;
    fRegisteredVars: TStringList;
    fOnDisconnect: TNotifyEvent;
    fFsxVars: array of variant;
    fFsxInFlight: Boolean;
    fInVarsCount: Integer;
    fAutopilotDataChange: TAutopilotCallbackProcedure;
    fEventValues: TStrings;
    fEventCaptions: TStrings;
    procedure DebugLog(Value: String);
    procedure LogRcvData(pData: PSimConnectRecvSimObjectData);
    function CardinalToStr(pData: Cardinal) : String;
    procedure LogRawSCData(pData: PSimConnectRecv; cbData: DWORD);
    procedure AddDefaultEvents(l : TStrings);
    procedure LoadEvents(pFileName: String; l : TStrings);
    procedure InitEvents;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; Override;
    procedure Init;
    procedure Connect(Handle: THandle);
    function SendEvent(Name: String; Param: Cardinal = 0): boolean;
    function SendEventByTitle(pTitle: String; Param: Cardinal = 0): boolean;
    function SendText(lValue: String): boolean;
    function RegisterFSXVariable(pName, pUnits: String): boolean;
    function GetFSXVariable(pName: String): variant;
    function AddFSXVaribale(pName: String; pUnits: String): Integer;
    procedure SetFSXVariable(pName: String; pUnits: String; pValue: double);
    procedure DispatchHandler(pData: PSimConnectRecv; cbData: DWORD; pContext: Pointer);
    procedure SimConnectMessage(var Message: TMessage);
    function RegisterAutopilotVariables: Boolean;
    property SCAvailable : boolean read fSCAvailable;
    property SCConnected : boolean read fSCConnected;
    property hSimConnect : THandle read fhSimConnect;
    property OnDisconnect: TNotifyEvent read fOnDisconnect write fOnDisconnect;
    property OnAutopiloDataChange: TAutopilotCallbackProcedure read fAutopilotDataChange write fAutopilotDataChange;
    property EventValues: TStrings read fEventValues;
    property EventCaptions: TStrings read fEventCaptions;
  end;

  TFSXVariable = class(TObject)
    public
      Name: String;
      DefinitionId: Integer;
      RequestId: Integer;
      IsString: Boolean;
      constructor Create;
  end;

implementation

uses SysUtils, uGlobals, Forms;

type
  TSimConnectString = packed array[0..255] of char;
  PSimConnectString = ^TSimConnectString;

procedure MyDispatchProc(pData: PSimConnectRecv; cbData: DWORD; pContext:
  Pointer); stdcall;
begin
  Glb.FSX.DispatchHandler(pData, cbData, pContext);
end;

constructor TFSXcontrol.Create;
begin
  //inherited;
  fRegisteredEvents := TstringList.Create;
  fRegisteredEvents.CaseSensitive := False;
  fRegisteredVars := TStringList.Create;
  fRegisteredVars.CaseSensitive := False;
  fReservedRequestsCount := 1;
  fReservedDefinitionsCount := 1;
  fInVarsCount := 0;
  fEventValues := TStringList.Create;
  fEventCaptions := TStringList.Create;
  SetLength(fFsxVars, fInVarsCount);
  fFsxInFlight := False; // this variable won't work if HIDmacros is started
  // already in flight. ould have to ask somehow, but there's no FSX variable...
  fAutopilotDataChange := nil;
end;

destructor TFSXcontrol.Destroy;
var I: Integer;
begin
  fRegisteredEvents.Free;
  for I := 0 to fRegisteredVars.Count - 1 do
    fRegisteredVars.Objects[I].Free;
  fRegisteredVars.Free;
  fEventValues.Free;
  fEventCaptions.Free;
  inherited;
end;

procedure TFSXcontrol.DispatchHandler(pData: PSimConnectRecv; cbData:
  DWORD;
  pContext: Pointer);
var
  OpenData          : PSimConnectRecvOpen;
  pObjData          : PSimConnectRecvSimObjectData;
  Exception : PSimConnectRecvException;
  Evt: PSimconnectRecvEvent;
  lVar: TFSXVariable;
  lVarIndex: Integer;
  lStrValue: String;
  lDoubleValue: Double;
begin
  //LogRawSCData(pData, cbData);
  // Handle the various types of message
  case TSimConnectRecvId(pData^.dwID) of
    SIMCONNECT_RECV_ID_OPEN:
      begin
        //StatusBar.Panels[0].Text := txt.Values['SCConnected'];
        OpenData := PSimConnectRecvOpen(pData);
        with OpenData^ do
        begin
          DebugLog(Format('%s %1d.%1d.%1d.%1d', [szApplicationName,
            dwApplicationVersionMajor, dwApplicationVersionMinor,
              dwApplicationBuildMajor, dwApplicationBuildMinor]));
          DebugLog(Format('%s %1d.%1d.%1d.%1d', ['SimConnect',
            dwSimConnectVersionMajor, dwSimConnectVersionMinor,
              dwSimConnectBuildMajor, dwSimConnectBuildMinor]));
          if (not SUCCEEDED(SimConnect_SubscribeToSystemEvent(fhSimConnect,
              EVENT_SIM_START, 'SimStart'))) then
            DebugLog('FSX:ERROR: Can not subscribe to SimStart');
          if (not SUCCEEDED(SimConnect_SubscribeToSystemEvent(fhSimConnect,
              EVENT_SIM_STOP, 'SimStop'))) then
            DebugLog('FSX:ERROR: Can not subscribe to SimStop');
        end;
      end;
    SIMCONNECT_RECV_ID_EVENT:
      begin
        evt := PSimconnectRecvEvent(pData);
        case evt^.uEventID of
          EVENT_SIM_START: fFsxInFlight := True;
          EVENT_SIM_STOP: fFsxInFlight := False;
          else
            DebugLog('FSX:Unknown SC Event ' + IntToStr(evt^.uEventID) + ', data ' + IntToStr(evt^.dwData));
        end;
        //DebugLog('FSX: FSX in flight:'+BoolToStr(fFsxInFlight));
      end;
    SIMCONNECT_RECV_ID_QUIT:
      begin
        fSCConnected := False;
        if Assigned(fOnDisconnect) then fOnDisconnect(Self);
      end;
    SIMCONNECT_RECV_ID_EXCEPTION:
    begin
      Exception := PSimConnectRecvException(pData);
      DebugLog(Format('***** EXCEPTION=%d  SendID=%d  Index=%d  cbData=%d',
      [Exception^.dwException, Exception^.dwSendID, Exception^.dwIndex, cbData]));
    end;
    SIMCONNECT_RECV_ID_SIMOBJECT_DATA:
    begin
      pObjData := PSimConnectRecvSimObjectData(pData);
      //LogRcvData(pObjData);
      if (pObjData^.dwRequestID = 0) then // autopilot data
      begin
        if Assigned(fAutopilotDataChange) then
          fAutopilotDataChange(PAutopilotData(@pObjData^.dwData));
      end else
      if (pObjData^.dwRequestID < Length(fFsxVars)+fReservedRequestsCount) then
      begin
        // find the variable to find out type
        lVarIndex := pObjData^.dwDefineID - fReservedDefinitionsCount;
        if (lVarIndex >= 0) and (lVarIndex < fRegisteredVars.Count) then
        begin
          lVar := fRegisteredVars.Objects[lVarIndex] as TFSXVariable;
          if lVar.IsString then
          begin
            lStrValue := PSimConnectString(@pObjData^.dwData)^;
            fFsxVars[pObjData^.dwRequestID-fReservedRequestsCount] := lStrValue;
            DebugLog(Format('FSX:Received string var value: index %d = %s',
                [pObjData^.dwRequestID-fReservedRequestsCount, fFsxVars[pObjData^.dwRequestID-fReservedRequestsCount]]));
          end
          else
          begin
            // double
            lDoubleValue := PDouble(@pObjData^.dwData)^;
            fFsxVars[pObjData^.dwRequestID-fReservedRequestsCount] := lDoubleValue;
            DebugLog(Format('FSX:Received var value: index %d = %8.3f',
                [pObjData^.dwRequestID-fReservedRequestsCount,  lDoubleValue]));
          end;
        end;
      end;
    end;
  else
    DebugLog(Format('FSX:Unknown dwID: %d', [pData.dwID]));
  end;
end;

function TFSXcontrol.GetFSXVariable(pName: String): variant;
var
  lVarIndex: Integer;
begin
  Result := 0;
  if not fSCConnected then
  begin
    exit;
  end;
  //DebugLog('FSX:Get variable name ' + pName);
  lVarIndex := fRegisteredVars.IndexOf(pName);
  if lVarIndex > -1 then
    Result := fFsxVars[lVarIndex];
end;

procedure TFSXcontrol.Init;
begin
  fhSimConnect := 0;
  fRegisteredEvents.Clear;
  if InitSimConnect then
  begin
    fSCAvailable := True;
  end
  else
  begin
    fSCAvailable := False;
    fSCConnected := False;
  end;
  InitEvents;
end;

procedure TFSXcontrol.LoadEvents(pFileName: String; l : TStrings);
var
  lFile: TStringList;
  i: Integer;
  lPos: Integer;
  lName, lValue: String;
begin
  lFile := TStringList.Create;
  try
    lFile.LoadFromFile(pFileName);
    for I := 0 to lFile.Count - 1 do
    begin
      if Trim(lFile[i]) = '' then
        Continue;
      lPos := Pos(':', lFile[i]);
      if lPos = 0 then
      begin
        lName := lFile[i];
        lValue := lFile[i];
      end
      else
      begin
        lValue := UpperCase(Trim(Copy(lFile[i], 1, lPos - 1)));
        lName := Trim(Copy(lFile[i], lPos+1, 1024));
      end;
      l.Add(lName + '=' + lValue);
    end;
  finally
    lFile.Free;
  end;
end;

procedure TFSXcontrol.LogRawSCData(pData: PSimConnectRecv; cbData: DWORD);
var I: Integer;
  Res: String;
  TmpP: PByte;
begin
  DebugLog('FSX:SC packet received, size ' + IntToStr(cbData) + ' bytes.');
  Res := '';
  TmpP := PByte(pData);
  for I := 0 to cbData - 1 do
  begin
    Res := Res + IntToHex(TmpP^, 2) + ' ';
    Inc(TmpP);
    if (I+1) mod 8 = 0 then
    begin
      DebugLog(Res);
      Res := '';
    end;
  end;
  DebugLog(Res);
end;

procedure TFSXcontrol.LogRcvData(pData: PSimConnectRecvSimObjectData);
var tmpP: PDouble;
begin
  DebugLog('FSX:dwRequestID: ' + CardinalToStr(pData^.dwRequestID));
  DebugLog('FSX:dwObjectID: ' + CardinalToStr(pData^.dwObjectID));
  DebugLog('FSX:dwDefineID: ' + CardinalToStr(pData^.dwDefineID));
  DebugLog('FSX:dwFlags: ' + CardinalToStr(pData^.dwFlags));
  DebugLog('FSX:dwentrynumber: ' + CardinalToStr(pData^.dwentrynumber));
  DebugLog('FSX:dwoutof: ' + CardinalToStr(pData^.dwoutof));
  DebugLog('FSX:dwDefineCount: ' + CardinalToStr(pData^.dwDefineCount));
  DebugLog('FSX:dwData: ' + CardinalToStr(pData^.dwData));
  tmpP := @pData^.dwData;
  DebugLog('FSX:dwData double: ' + FloatToStr(tmpP^));
end;

function TFSXcontrol.RegisterAutopilotVariables: Boolean;
begin
  Result := True;
  if not fSCConnected then
  begin
    Result := False;
    exit;
  end;
  if not(SUCCEEDED(SimConnect_AddToDataDefinition(fhSimConnect, 0,
      'VERTICAL SPEED', 'ft/min'))) then
    Result := False;
  if not(SUCCEEDED(SimConnect_AddToDataDefinition(fhSimConnect, 0,
      'ELEVATOR POSITION', 'Position'))) then
    Result := False;
  if not(SUCCEEDED(SimConnect_RequestDataOnSimObject(fhSimConnect, 0, 0,
      SIMCONNECT_OBJECT_ID_USER, SIMCONNECT_PERIOD_SECOND, SIMCONNECT_DATA_REQUEST_FLAG_CHANGED))) then
    Result := False;
end;

function TFSXcontrol.RegisterFSXVariable(pName, pUnits: String): boolean;
var
  lVarIndex: Integer;
  lRequestId: Integer;
begin
  Result := True;
  if not fSCConnected then
  begin
    Result := False;
    exit;
  end;
  // if not yet registered, register
  Result := True;
  lVarIndex := AddFSXVaribale(pName, pUnits);
  if (lVarIndex >= 0) and
     (TFSXVariable(fRegisteredVars.Objects[lVarIndex]).RequestId = -1) then
  begin
    lRequestId := fInVarsCount;
    if (SUCCEEDED(SimConnect_RequestDataOnSimObject(fhSimConnect, lRequestId+fReservedRequestsCount, lVarIndex+fReservedDefinitionsCount,
        SIMCONNECT_OBJECT_ID_USER, SIMCONNECT_PERIOD_SECOND, SIMCONNECT_DATA_REQUEST_FLAG_CHANGED))) then
    begin
      TFSXVariable(fRegisteredVars.Objects[lVarIndex]).RequestId := lRequestId;
      Inc(fInVarsCount);
      SetLength(fFsxVars, fInVarsCount);
      DebugLog('FSX: Var ' + pName + ' requested with request id ' + IntToStr(lRequestId));
    end
    else
    begin
      DebugLog('FSX: Error requsting var ' + pName + ' definition ' + IntToStr(lVarIndex));
      Result := False;
    end;
  end;
end;

procedure TFSXcontrol.InitEvents;
var
  lFileName: string;
  lSortedList: TStringList;
  lTmpList: TStrings;
  I: Integer;
begin
  fEventValues.Clear;
  fEventCaptions.Clear;
  lSortedList := TStringList.Create;
  try
    lSortedList.Sorted := True;
    lFileName := ExtractFilePath(Application.ExeName)+'\FSXevents.lst';
    if (FileExists(lFileName)) then
      LoadEvents(lFileName, lSortedList)
    else
    begin
      lTmpList := TStringList.Create;
      try
        AddDefaultEvents(lTmpList);
        for I := 0 to lTmpList.Count - 1 do
          lSortedList.Add(lTmpList[i] + '=' + lTmpList[i]);
      finally
        lTmpList.Free;
      end;
    end;
    for I := 0 to lSortedList.Count - 1 do
    begin
      fEventCaptions.Add(lSortedList.Names[i]);
      fEventValues.Add(lSortedList.ValueFromIndex[i]);
    end;
  finally
    lSortedList.Free;
  end;
end;

function TFSXcontrol.AddFSXVaribale(pName, pUnits: String): Integer;
var
  lVarIndex: Integer;
  lFSXVar: TFSXVariable;
  lRegOK: Boolean;
begin
  // adds SimConnect data definition and returns its id
  Result := -1;
  if not fSCConnected then
    exit;
  // if not yet registered, register
  lVarIndex := fRegisteredVars.IndexOf(pName);
  if lVarIndex = -1 then
  begin
    // add to data definition - there we have in and out vars
    lVarIndex := fRegisteredVars.Count;
    if UpperCase(pUnits) = 'STRING' then
      lRegOK := SUCCEEDED(SimConnect_AddToDataDefinition(fhSimConnect,
          lVarIndex+fReservedDefinitionsCount, pName, '', SIMCONNECT_DATATYPE_STRING256))
    else
      lRegOK := SUCCEEDED(SimConnect_AddToDataDefinition(fhSimConnect,
          lVarIndex+fReservedDefinitionsCount, pName, pUnits));
    if lRegOK then
    begin
      DebugLog('FSX: Var ' + pName + ' added definition id ' + IntToStr(lVarIndex));
      lFSXVar := TFSXVariable.Create;
      lFSXVar.Name := pName;
      lFSXVar.DefinitionId := lVarIndex;
      lFSXVar.IsString := (UpperCase(pUnits) = 'STRING');
      fRegisteredVars.AddObject(pName, lFSXVar);
    end
    else
    begin
      DebugLog('FSX: Error adding definition var ' + pName + ' as ' + IntToStr(lVarIndex));
      lVarIndex := -1;
    end;
  end;
  Result := lVarIndex;
end;

function TFSXcontrol.CardinalToStr(pData: Cardinal): String;
begin
  Result := IntToStr(pData) + '(' + IntToHex(pData, 8) + ')';
end;

procedure TFSXcontrol.Connect(Handle: THandle);
begin
  if (SUCCEEDED(SimConnect_Open(fhSimConnect, 'HID macros', Handle,
    WM_USER_SIMCONNECT, 0, 0))) then
  begin
    fSCConnected := True;
    fRegisteredEvents.Clear;
  end;
end;

procedure TFSXcontrol.SimConnectMessage(var Message: TMessage);
begin
  SimConnect_CallDispatch(hSimConnect, MyDispatchProc, nil);
end;

procedure TFSXcontrol.DebugLog(Value: String);
begin
  Glb.DebugLog(Value, 'FSX');
end;

function TFSXcontrol.SendEvent(Name: String; Param: Cardinal = 0): boolean;
var
  lEventIndex: Integer;
//  tmp: array[0..255] of char;
begin
  if not fSCConnected then
  begin
    Result := False;
    exit;
  end;
  // if not yet registered, register
//  StrPCopy(tmp, 'HIDMACROS: Got event ' + Name + ' par ' + IntToStr(Param));
//  OutputDebugString(tmp);
  Result := True;
  lEventIndex := fRegisteredEvents.IndexOf(Name);
  if lEventIndex = -1 then
  begin
    // we have to register this event first
    lEventIndex := fRegisteredEvents.Count;
    if (SUCCEEDED(SimConnect_MapClientEventToSimEvent(fhSimConnect, lEventIndex+3, Name))) then
    begin
      DebugLog('Event ' + Name + ' registered as ' + IntToStr(lEventINdex+3));
      fRegisteredEvents.Add(Name);
    end
    else
    begin
      DebugLog('Error registering event ' + Name + ' as ' + IntToStr(lEventIndex+3));
      Result := False;
      lEventIndex := -1;
    end;
  end;
  // call with its index
  if Result then
  begin
    Result := SUCCEEDED(SimConnect_TransmitClientEvent(fhSimConnect, 0, lEventIndex+3,
      Param, SIMCONNECT_GROUP_PRIORITY_HIGHEST, SIMCONNECT_EVENT_FLAG_GROUPID_IS_PRIORITY));
    DebugLog(Format('Event %s (%d) sent to FSX with param %d.',
        [Name, lEventIndex+3, Param]));
  end;
end;



function TFSXcontrol.SendEventByTitle(pTitle: String; Param: Cardinal): boolean;
var
  lIndex: Integer;
begin
  lIndex := fEventCaptions.IndexOf(pTitle);
  if (lIndex < 0) then
  begin
    DebugLog('Event with title ' + pTitle + ' not found. Call ignored.');
    Result := False;
    exit;
  end;
  Result := SendEvent(fEventValues[lIndex], Param);
end;

function TFSXcontrol.SendText(lValue: String): boolean;
begin
  Result := True;
  if not fSCConnected then
  begin
    Result := False;
    exit;
  end;
  SimConnect_Text(fhSimConnect, SIMCONNECT_TEXT_TYPE_PRINT_BLACK, 3.0, 0, Length(lValue)+1, PChar(lValue));
end;

procedure TFSXcontrol.SetFSXVariable(pName: String; pUnits: String; pValue: double);
var
  lVarIndex: Integer;
  lVar: TFSXVariable;
begin
  if not fSCConnected then
    exit;
  // if not yet registered, register
  lVarIndex := AddFSXVaribale(pName, pUnits);
  if lVarIndex >= 0 then
  begin
    lVar := fRegisteredVars.Objects[lVarIndex] as TFSXVariable;
    if (lVar <> nil) and (lVar.IsString) then
      exit; // string vars are read only
    if (SUCCEEDED(SimConnect_SetDataOnSimObject(fhSimConnect, lVarIndex+fReservedDefinitionsCount, SIMCONNECT_OBJECT_ID_USER,
          0, 0, sizeof(pValue), @pValue))) then
    begin
      DebugLog('FSX: Var ' + pName + ', definition ' + IntToStr(lVarIndex) +
          ' set to ' + FloatToStr(pValue));
    end
    else
    begin
      DebugLog('FSX: Error setting var ' + pName + ' to ' + FloatToStr(pValue));
    end;
  end;
end;

procedure TFSXcontrol.AddDefaultEvents(l : TStrings);
begin
  l.Add('REPAIR_AND_REFUEL');
  l.Add('DME_SELECT');
  l.Add('FUEL_DUMP_TOGGLE');
  l.Add('VIEW_COCKPIT_FORWARD');
  l.Add('VIEW_VIRTUAL_COCKPIT_FORWARD');
  l.Add('TOW_PLANE_RELEASE');
  l.Add('TOW_PLANE_REQUEST');
  l.Add('REQUEST_FUEL_KEY');
  l.Add('RELEASE_DROPPABLE_OBJECTS');
  l.Add('VIEW_PANEL_ALPHA_SET');
  l.Add('VIEW_PANEL_ALPHA_SELECT');
  l.Add('VIEW_PANEL_ALPHA_INC');
  l.Add('VIEW_PANEL_ALPHA_DEC');
  l.Add('VIEW_LINKING_SET');
  l.Add('VIEW_LINKING_TOGGLE');
  l.Add('RADIO_SELECTED_DME_IDENT_ENABLE');
  l.Add('RADIO_SELECTED_DME_IDENT_DISABLE');
  l.Add('RADIO_SELECTED_DME_IDENT_SET');
  l.Add('RADIO_SELECTED_DME_IDENT_TOGGLE');
  l.Add('GAUGE_KEYSTROKE');
  l.Add('SIMUI_WINDOW_HIDESHOW');
  l.Add('TOGGLE_VARIOMETER_SWITCH');
  l.Add('TOGGLE_TURN_INDICATOR_SWITCH');
  l.Add('VIEW_WINDOW_TITLES_TOGGLE');
  l.Add('VIEW_AXIS_INDICATOR_CYCLE');
  l.Add('VIEW_MAP_ORIENTATION_CYCLE');
  l.Add('TOGGLE_JETWAY');
  l.Add('RETRACT_FLOAT_SWITCH_DEC');
  l.Add('RETRACT_FLOAT_SWITCH_INC');
  l.Add('TOGGLE_WATER_BALLAST_VALVE');
  l.Add('VIEW_CHASE_DISTANCE_ADD');
  l.Add('VIEW_CHASE_DISTANCE_SUB');
  l.Add('APU_STARTER');
  l.Add('APU_OFF_SWITCH');
  l.Add('APU_GENERATOR_SWITCH_TOGGLE');
  l.Add('APU_GENERATOR_SWITCH_SET');
  l.Add('EXTINGUISH_ENGINE_FIRE');
  l.Add('AP_MAX_BANK_INC');
  l.Add('AP_MAX_BANK_DEC');
  l.Add('AP_N1_HOLD');
  l.Add('AP_N1_REF_INC');
  l.Add('AP_N1_REF_DEC');
  l.Add('HYDRAULIC_SWITCH_TOGGLE');
  l.Add('BLEED_AIR_SOURCE_CONTROL_INC');
  l.Add('BLEED_AIR_SOURCE_CONTROL_DEC');
  l.Add('TURBINE_IGNITION_SWITCH_TOGGLE');
  l.Add('CABIN_NO_SMOKING_ALERT_');
  l.Add('SWITCH_TOGGLE');
  l.Add('CABIN_SEATBELTS_ALERT_');
  l.Add('SWITCH_TOGGLE');
  l.Add('ANTISKID_BRAKES_TOGGLE');
  l.Add('GPWS_SWITCH_TOGGLE');
  l.Add('VIDEO_RECORD_TOGGLE');
  l.Add('TOGGLE_AIRPORT_NAME_DISPLAY');
  l.Add('CAPTURE_SCREENSHOT');
  l.Add('MOUSE_LOOK_TOGGLE');
  l.Add('YAXIS_INVERT_TOGGLE');
  l.Add('AUTORUDDER_TOGGLE');
  l.Add('FLY_BY_WIRE_ELAC_TOGGLE');
  l.Add('FLY_BY_WIRE_FAC_TOGGLE');
  l.Add('FLY_BY_WIRE_SEC_TOGGLE');
  l.Add('MANUAL_FUEL_PRESSURE_PUMP');
  l.Add('STEERING_INC');
  l.Add('STEERING_DEC');
  l.Add('STEERING_SET');
  l.Add('FREEZE_LATITUDE_LONGITUDE_TOGGLE');
  l.Add('FREEZE_LATITUDE_LONGITUDE_SET');
  l.Add('FREEZE_ALTITUDE_TOGGLE');
  l.Add('FREEZE_ALTITUDE_SET');
  l.Add('KEY_PRESSURIZATION_PRESSURE_ALT_INC');
  l.Add('KEY_PRESSURIZATION_PRESSURE_ALT_DEC');
  l.Add('PRESSURIZATION_CLIMB_RATE_INC');
  l.Add('PRESSURIZATION_CLIMB_RATE_DEC');
  l.Add('PRESSURIZATION_PRESSURE_DUMP_SWTICH');
  l.Add('FUEL_SELECTOR_LEFT_MAIN');
  l.Add('FUEL_SELECTOR_2_LEFT_MAIN');
  l.Add('FUEL_SELECTOR_3_LEFT_MAIN');
  l.Add('FUEL_SELECTOR_4_LEFT_MAIN');
  l.Add('FUEL_SELECTOR_RIGHT_MAIN');
  l.Add('FUEL_SELECTOR_2_RIGHT_MAIN');
  l.Add('FUEL_SELECTOR_3_RIGHT_MAIN');
  l.Add('FUEL_SELECTOR_4_RIGHT_MAIN');
  l.Add('MP_VOICE_CAPTURE_START');
  l.Add('MP_VOICE_CAPTURE_STOP');
  l.Add('MP_BROADCAST_VOICE_CAPTURE_START');
  l.Add('MP_BROADCAST_VOICE_CAPTURE_STOP');
  l.Add('POINT_OF_INTEREST_TOGGLE_POINTER');
  l.Add('POINT_OF_INTEREST_CYCLE_PREVIOUS');
  l.Add('POINT_OF_INTEREST_CYCLE_NEXT');
  l.Add('G1000_PFD_FLIGHTPLAN_BUTTON');
  l.Add('G1000_PFD_PROCEDURE_BUTTON');
  l.Add('G1000_PFD_ZOOMIN_BUTTON');
  l.Add('G1000_PFD_ZOOMOUT_BUTTON');
  l.Add('G1000_PFD_DIRECTTO_BUTTON');
  l.Add('G1000_PFD_MENU_BUTTON');
  l.Add('G1000_PFD_CLEAR_BUTTON');
  l.Add('G1000_PFD_ENTER_BUTTON');
  l.Add('G1000_PFD_CURSOR_BUTTON');
  l.Add('G1000_PFD_GROUP_KNOB_INC');
  l.Add('G1000_PFD_GROUP_KNOB_DEC');
  l.Add('G1000_PFD_PAGE_KNOB_INC');
  l.Add('G1000_PFD_PAGE_KNOB_DEC');
  l.Add('G1000_PFD_SOFTKEY1');
  l.Add('G1000_PFD_SOFTKEY2');
  l.Add('G1000_PFD_SOFTKEY3');
  l.Add('G1000_PFD_SOFTKEY4');
  l.Add('G1000_PFD_SOFTKEY5');
  l.Add('G1000_PFD_SOFTKEY6');
  l.Add('G1000_PFD_SOFTKEY7');
  l.Add('G1000_PFD_SOFTKEY8');
  l.Add('G1000_PFD_SOFTKEY9');
  l.Add('G1000_PFD_SOFTKEY10');
  l.Add('G1000_PFD_SOFTKEY11');
  l.Add('G1000_PFD_SOFTKEY12');
  l.Add('G1000_MFD_FLIGHTPLAN_BUTTON');
  l.Add('G1000_MFD_PROCEDURE_BUTTON');
  l.Add('G1000_MFD_ZOOMIN_BUTTON');
  l.Add('G1000_MFD_ZOOMOUT_BUTTON');
  l.Add('G1000_MFD_DIRECTTO_BUTTON');
  l.Add('G1000_MFD_MENU_BUTTON');
  l.Add('G1000_MFD_CLEAR_BUTTON');
  l.Add('G1000_MFD_ENTER_BUTTON');
  l.Add('G1000_MFD_CURSOR_BUTTON');
  l.Add('G1000_MFD_GROUP_KNOB_INC');
  l.Add('G1000_MFD_GROUP_KNOB_DEC');
  l.Add('G1000_MFD_PAGE_KNOB_INC');
  l.Add('G1000_MFD_PAGE_KNOB_DEC');
  l.Add('G1000_MFD_SOFTKEY1');
  l.Add('G1000_MFD_SOFTKEY2');
  l.Add('G1000_MFD_SOFTKEY3');
  l.Add('G1000_MFD_SOFTKEY4');
  l.Add('G1000_MFD_SOFTKEY5');
  l.Add('G1000_MFD_SOFTKEY6');
  l.Add('G1000_MFD_SOFTKEY7');
  l.Add('G1000_MFD_SOFTKEY8');
  l.Add('G1000_MFD_SOFTKEY9');
  l.Add('G1000_MFD_SOFTKEY10');
  l.Add('G1000_MFD_SOFTKEY11');
  l.Add('G1000_MFD_SOFTKEY12');
  l.Add('THROTTLE_FULL');
  l.Add('THROTTLE_INCR');
  l.Add('THROTTLE_INCR_SMALL');
  l.Add('THROTTLE_DECR');
  l.Add('THROTTLE_DECR_SMALL');
  l.Add('THROTTLE_CUT');
  l.Add('INCREASE_THROTTLE');
  l.Add('DECREASE_THROTTLE');
  l.Add('THROTTLE_SET');
  l.Add('AXIS_THROTTLE_SET');
  l.Add('THROTTLE1_SET');
  l.Add('THROTTLE2_SET');
  l.Add('THROTTLE3_SET');
  l.Add('THROTTLE4_SET');
  l.Add('THROTTLE1_FULL');
  l.Add('THROTTLE1_INCR');
  l.Add('THROTTLE1_INCR_SMALL');
  l.Add('THROTTLE1_DECR');
  l.Add('THROTTLE1_CUT');
  l.Add('THROTTLE2_FULL');
  l.Add('THROTTLE2_INCR');
  l.Add('THROTTLE2_INCR_SMALL');
  l.Add('THROTTLE2_DECR');
  l.Add('THROTTLE2_CUT');
  l.Add('THROTTLE3_FULL');
  l.Add('THROTTLE3_INCR');
  l.Add('THROTTLE3_INCR_SMALL');
  l.Add('THROTTLE3_DECR');
  l.Add('THROTTLE3_CUT');
  l.Add('THROTTLE4_FULL');
  l.Add('THROTTLE4_INCR');
  l.Add('THROTTLE4_INCR_SMALL');
  l.Add('THROTTLE4_DECR');
  l.Add('THROTTLE4_CUT');
  l.Add('THROTTLE_10');
  l.Add('THROTTLE_20');
  l.Add('THROTTLE_30');
  l.Add('THROTTLE_40');
  l.Add('THROTTLE_50');
  l.Add('THROTTLE_60');
  l.Add('THROTTLE_70');
  l.Add('THROTTLE_80');
  l.Add('THROTTLE_90');
  l.Add('AXIS_THROTTLE1_SET');
  l.Add('AXIS_THROTTLE2_SET');
  l.Add('AXIS_THROTTLE3_SET');
  l.Add('AXIS_THROTTLE4_SET');
  l.Add('THROTTLE1_DECR_SMALL');
  l.Add('THROTTLE2_DECR_SMALL');
  l.Add('THROTTLE3_DECR_SMALL');
  l.Add('THROTTLE4_DECR_SMALL');
  l.Add('PROP_PITCH_DECR_SMALL');
  l.Add('PROP_PITCH1_DECR_SMALL');
  l.Add('PROP_PITCH2_DECR_SMALL');
  l.Add('PROP_PITCH3_DECR_SMALL');
  l.Add('PROP_PITCH4_DECR_SMALL');
  l.Add('MIXTURE1_RICH');
  l.Add('MIXTURE1_INCR');
  l.Add('MIXTURE1_INCR_SMALL');
  l.Add('MIXTURE1_DECR');
  l.Add('MIXTURE1_LEAN');
  l.Add('MIXTURE2_RICH');
  l.Add('MIXTURE2_INCR');
  l.Add('MIXTURE2_INCR_SMALL');
  l.Add('MIXTURE2_DECR');
  l.Add('MIXTURE2_LEAN');
  l.Add('MIXTURE3_RICH');
  l.Add('MIXTURE3_INCR');
  l.Add('MIXTURE3_INCR_SMALL');
  l.Add('MIXTURE3_DECR');
  l.Add('MIXTURE3_LEAN');
  l.Add('MIXTURE4_RICH');
  l.Add('MIXTURE4_INCR');
  l.Add('MIXTURE4_INCR_SMALL');
  l.Add('MIXTURE4_DECR');
  l.Add('MIXTURE4_LEAN');
  l.Add('MIXTURE_SET');
  l.Add('MIXTURE_RICH');
  l.Add('MIXTURE_INCR');
  l.Add('MIXTURE_INCR_SMALL');
  l.Add('MIXTURE_DECR');
  l.Add('MIXTURE_LEAN');
  l.Add('MIXTURE1_SET');
  l.Add('MIXTURE2_SET');
  l.Add('MIXTURE3_SET');
  l.Add('MIXTURE4_SET');
  l.Add('AXIS_MIXTURE_SET');
  l.Add('AXIS_MIXTURE1_SET');
  l.Add('AXIS_MIXTURE2_SET');
  l.Add('AXIS_MIXTURE3_SET');
  l.Add('AXIS_MIXTURE4_SET');
  l.Add('MIXTURE_SET_BEST');
  l.Add('MIXTURE_DECR_SMALL');
  l.Add('MIXTURE1_DECR_SMALL');
  l.Add('MIXTURE2_DECR_SMALL');
  l.Add('MIXTURE3_DECR_SMALL');
  l.Add('MIXTURE4_DECR_SMALL');
  l.Add('PROP_PITCH_SET');
  l.Add('PROP_PITCH_LO');
  l.Add('PROP_PITCH_INCR');
  l.Add('PROP_PITCH_INCR_SMALL');
  l.Add('PROP_PITCH_DECR');
  l.Add('PROP_PITCH_HI');
  l.Add('PROP_PITCH1_SET');
  l.Add('PROP_PITCH2_SET');
  l.Add('PROP_PITCH3_SET');
  l.Add('PROP_PITCH4_SET');
  l.Add('PROP_PITCH1_LO');
  l.Add('PROP_PITCH1_INCR');
  l.Add('PROP_PITCH1_INCR_SMALL');
  l.Add('PROP_PITCH1_DECR');
  l.Add('PROP_PITCH1_HI');
  l.Add('PROP_PITCH2_LO');
  l.Add('PROP_PITCH2_INCR');
  l.Add('PROP_PITCH2_INCR_SMALL');
  l.Add('PROP_PITCH2_DECR');
  l.Add('PROP_PITCH2_HI');
  l.Add('PROP_PITCH3_LO');
  l.Add('PROP_PITCH3_INCR');
  l.Add('PROP_PITCH3_INCR_SMALL');
  l.Add('PROP_PITCH3_DECR');
  l.Add('PROP_PITCH3_HI');
  l.Add('PROP_PITCH4_LO');
  l.Add('PROP_PITCH4_INCR');
  l.Add('PROP_PITCH4_INCR_SMALL');
  l.Add('PROP_PITCH4_DECR');
  l.Add('PROP_PITCH4_HI');
  l.Add('AXIS_PROPELLER_SET');
  l.Add('AXIS_PROPELLER1_SET');
  l.Add('AXIS_PROPELLER2_SET');
  l.Add('AXIS_PROPELLER3_SET');
  l.Add('AXIS_PROPELLER4_SET');
  l.Add('JET_STARTER');
  l.Add('MAGNETO_SET');
  l.Add('TOGGLE_STARTER1');
  l.Add('TOGGLE_STARTER2');
  l.Add('TOGGLE_STARTER3');
  l.Add('TOGGLE_STARTER4');
  l.Add('TOGGLE_ALL_STARTERS');
  l.Add('ENGINE_AUTO_START');
  l.Add('ENGINE_AUTO_SHUTDOWN');
  l.Add('MAGNETO');
  l.Add('MAGNETO_DECR');
  l.Add('MAGNETO_INCR');
  l.Add('MAGNETO1_OFF');
  l.Add('MAGNETO1_RIGHT');
  l.Add('MAGNETO1_LEFT');
  l.Add('MAGNETO1_BOTH');
  l.Add('MAGNETO1_START');
  l.Add('MAGNETO2_OFF');
  l.Add('MAGNETO2_RIGHT');
  l.Add('MAGNETO2_LEFT');
  l.Add('MAGNETO2_BOTH');
  l.Add('MAGNETO2_START');
  l.Add('MAGNETO3_OFF');
  l.Add('MAGNETO3_RIGHT');
  l.Add('MAGNETO3_LEFT');
  l.Add('MAGNETO3_BOTH');
  l.Add('MAGNETO3_START');
  l.Add('MAGNETO4_OFF');
  l.Add('MAGNETO4_RIGHT');
  l.Add('MAGNETO4_LEFT');
  l.Add('MAGNETO4_BOTH');
  l.Add('MAGNETO4_START');
  l.Add('MAGNETO_OFF');
  l.Add('MAGNETO_RIGHT');
  l.Add('MAGNETO_LEFT');
  l.Add('MAGNETO_BOTH');
  l.Add('MAGNETO_START');
  l.Add('MAGNETO1_DECR');
  l.Add('MAGNETO1_INCR');
  l.Add('MAGNETO2_DECR');
  l.Add('MAGNETO2_INCR');
  l.Add('MAGNETO3_DECR');
  l.Add('MAGNETO3_INCR');
  l.Add('MAGNETO4_DECR');
  l.Add('MAGNETO4_INCR');
  l.Add('MAGNETO1_SET');
  l.Add('MAGNETO2_SET');
  l.Add('MAGNETO3_SET');
  l.Add('MAGNETO4_SET');
  l.Add('ANTI_ICE_ON');
  l.Add('ANTI_ICE_OFF');
  l.Add('ANTI_ICE_SET');
  l.Add('ANTI_ICE_TOGGLE');
  l.Add('ANTI_ICE_TOGGLE_ENG1');
  l.Add('ANTI_ICE_TOGGLE_ENG2');
  l.Add('ANTI_ICE_TOGGLE_ENG3');
  l.Add('ANTI_ICE_TOGGLE_ENG4');
  l.Add('ANTI_ICE_SET_ENG1');
  l.Add('ANTI_ICE_SET_ENG2');
  l.Add('ANTI_ICE_SET_ENG3');
  l.Add('ANTI_ICE_SET_ENG4');
  l.Add('TOGGLE_FUEL_VALVE_ALL');
  l.Add('TOGGLE_FUEL_VALVE_ENG1');
  l.Add('TOGGLE_FUEL_VALVE_ENG2');
  l.Add('TOGGLE_FUEL_VALVE_ENG3');
  l.Add('TOGGLE_FUEL_VALVE_ENG4');
  l.Add('COWLFLAP1_SET');
  l.Add('COWLFLAP2_SET');
  l.Add('COWLFLAP3_SET');
  l.Add('COWLFLAP4_SET');
  l.Add('INC_COWL_FLAPS');
  l.Add('DEC_COWL_FLAPS');
  l.Add('INC_COWL_FLAPS1');
  l.Add('DEC_COWL_FLAPS1');
  l.Add('INC_COWL_FLAPS2');
  l.Add('DEC_COWL_FLAPS2');
  l.Add('INC_COWL_FLAPS3');
  l.Add('DEC_COWL_FLAPS3');
  l.Add('INC_COWL_FLAPS4');
  l.Add('DEC_COWL_FLAPS4');
  l.Add('FUEL_PUMP');
  l.Add('TOGGLE_ELECT_FUEL_PUMP');
  l.Add('TOGGLE_ELECT_FUEL_PUMP1');
  l.Add('TOGGLE_ELECT_FUEL_PUMP2');
  l.Add('TOGGLE_ELECT_FUEL_PUMP3');
  l.Add('TOGGLE_ELECT_FUEL_PUMP4');
  l.Add('ENGINE_PRIMER');
  l.Add('TOGGLE_PRIMER');
  l.Add('TOGGLE_PRIMER1');
  l.Add('TOGGLE_PRIMER2');
  l.Add('TOGGLE_PRIMER3');
  l.Add('TOGGLE_PRIMER4');
  l.Add('TOGGLE_FEATHER_SWITCHES');
  l.Add('TOGGLE_FEATHER_SWITCH_1');
  l.Add('TOGGLE_FEATHER_SWITCH_2');
  l.Add('TOGGLE_FEATHER_SWITCH_3');
  l.Add('TOGGLE_FEATHER_SWITCH_4');
  l.Add('TOGGLE_PROPELLER_SYNC');
  l.Add('TOGGLE_AUTOFEATHER_ARM');
  l.Add('TOGGLE_AFTERBURNER');
  l.Add('TOGGLE_AFTERBURNER1');
  l.Add('TOGGLE_AFTERBURNER2');
  l.Add('TOGGLE_AFTERBURNER3');
  l.Add('TOGGLE_AFTERBURNER4');
  l.Add('ENGINE');
  l.Add('SPOILERS_TOGGLE');
  l.Add('FLAPS_UP');
  l.Add('FLAPS_1');
  l.Add('FLAPS_2');
  l.Add('FLAPS_3');
  l.Add('FLAPS_DOWN');
  l.Add('ELEV_TRIM_DN');
  l.Add('ELEV_DOWN');
  l.Add('AILERONS_LEFT');
  l.Add('CENTER_AILER_RUDDER');
  l.Add('AILERONS_RIGHT');
  l.Add('ELEV_TRIM_UP');
  l.Add('ELEV_UP');
  l.Add('RUDDER_LEFT');
  l.Add('RUDDER_CENTER');
  l.Add('RUDDER_RIGHT');
  l.Add('ELEVATOR_SET');
  l.Add('AILERON_SET');
  l.Add('RUDDER_SET');
  l.Add('FLAPS_INCR');
  l.Add('FLAPS_DECR');
  l.Add('AXIS_ELEVATOR_SET');
  l.Add('AXIS_AILERONS_SET');
  l.Add('AXIS_RUDDER_SET');
  l.Add('AXIS_ELEV_TRIM_SET');
  l.Add('SPOILERS_SET');
  l.Add('SPOILERS_ARM_TOGGLE');
  l.Add('SPOILERS_ON');
  l.Add('SPOILERS_OFF');
  l.Add('SPOILERS_ARM_ON');
  l.Add('SPOILERS_ARM_OFF');
  l.Add('SPOILERS_ARM_SET');
  l.Add('AILERON_TRIM_LEFT');
  l.Add('AILERON_TRIM_RIGHT');
  l.Add('RUDDER_TRIM_LEFT');
  l.Add('RUDDER_TRIM_RIGHT');
  l.Add('AXIS_SPOILER_SET');
  l.Add('FLAPS_SET');
  l.Add('ELEVATOR_TRIM_SET');
  l.Add('AXIS_FLAPS_SET');
  l.Add('AP_MASTER');
  l.Add('AUTOPILOT_OFF');
  l.Add('AUTOPILOT_ON');
  l.Add('YAW_DAMPER_TOGGLE');
  l.Add('AP_PANEL_HEADING_HOLD');
  l.Add('AP_PANEL_ALTITUDE_HOLD');
  l.Add('AP_ATT_HOLD_ON');
  l.Add('AP_LOC_HOLD_ON');
  l.Add('AP_APR_HOLD_ON');
  l.Add('AP_HDG_HOLD_ON');
  l.Add('AP_ALT_HOLD_ON');
  l.Add('AP_WING_LEVELER_ON');
  l.Add('AP_BC_HOLD_ON');
  l.Add('AP_NAV1_HOLD_ON');
  l.Add('AP_ATT_HOLD_OFF');
  l.Add('AP_LOC_HOLD_OFF');
  l.Add('AP_APR_HOLD_OFF');
  l.Add('AP_HDG_HOLD_OFF');
  l.Add('AP_ALT_HOLD_OFF');
  l.Add('AP_WING_LEVELER_OFF');
  l.Add('AP_BC_HOLD_OFF');
  l.Add('AP_NAV1_HOLD_OFF');
  l.Add('AP_AIRSPEED_HOLD');
  l.Add('AUTO_THROTTLE_ARM');
  l.Add('AUTO_THROTTLE_TO_GA');
  l.Add('HEADING_BUG_INC');
  l.Add('HEADING_BUG_DEC');
  l.Add('HEADING_BUG_SET');
  l.Add('AP_PANEL_SPEED_HOLD');
  l.Add('AP_ALT_VAR_INC');
  l.Add('AP_ALT_VAR_DEC');
  l.Add('AP_VS_VAR_INC');
  l.Add('AP_VS_VAR_DEC');
  l.Add('AP_SPD_VAR_INC');
  l.Add('AP_SPD_VAR_DEC');
  l.Add('AP_PANEL_MACH_HOLD');
  l.Add('AP_MACH_VAR_INC');
  l.Add('AP_MACH_VAR_DEC');
  l.Add('AP_MACH_HOLD');
  l.Add('AP_ALT_VAR_SET_METRIC');
  l.Add('AP_VS_VAR_SET_ENGLISH');
  l.Add('AP_SPD_VAR_SET');
  l.Add('AP_MACH_VAR_SET');
  l.Add('YAW_DAMPER_ON');
  l.Add('YAW_DAMPER_OFF');
  l.Add('YAW_DAMPER_SET');
  l.Add('AP_AIRSPEED_ON');
  l.Add('AP_AIRSPEED_OFF');
  l.Add('AP_AIRSPEED_SET');
  l.Add('AP_MACH_ON');
  l.Add('AP_MACH_OFF');
  l.Add('AP_MACH_SET');
  l.Add('AP_PANEL_ALTITUDE_ON');
  l.Add('AP_PANEL_ALTITUDE_OFF');
  l.Add('AP_PANEL_ALTITUDE_SET');
  l.Add('AP_PANEL_HEADING_ON');
  l.Add('AP_PANEL_HEADING_OFF');
  l.Add('AP_PANEL_HEADING_SET');
  l.Add('AP_PANEL_MACH_ON');
  l.Add('AP_PANEL_MACH_OFF');
  l.Add('AP_PANEL_MACH_SET');
  l.Add('AP_PANEL_SPEED_ON');
  l.Add('AP_PANEL_SPEED_OFF');
  l.Add('AP_PANEL_SPEED_SET');
  l.Add('AP_ALT_VAR_SET_ENGLISH');
  l.Add('AP_VS_VAR_SET_METRIC');
  l.Add('TOGGLE_FLIGHT_DIRECTOR');
  l.Add('SYNC_FLIGHT_DIRECTOR_PITCH');
  l.Add('INCREASE_AUTOBRAKE_CONTROL');
  l.Add('DECREASE_AUTOBRAKE_CONTROL');
  l.Add('AP_PANEL_SPEED_HOLD_TOGGLE');
  l.Add('AP_PANEL_MACH_HOLD_TOGGLE');
  l.Add('AP_NAV_SELECT_SET');
  l.Add('HEADING_BUG_SELECT');
  l.Add('ALTITUDE_BUG_SELECT');
  l.Add('VSI_BUG_SELECT');
  l.Add('AIRSPEED_BUG_SELECT');
  l.Add('AP_PITCH_REF_INC_UP');
  l.Add('AP_PITCH_REF_INC_DN');
  l.Add('AP_PITCH_REF_SELECT');
  l.Add('AP_ATT_HOLD');
  l.Add('AP_LOC_HOLD');
  l.Add('AP_APR_HOLD');
  l.Add('AP_HDG_HOLD');
  l.Add('AP_ALT_HOLD');
  l.Add('AP_WING_LEVELER');
  l.Add('AP_BC_HOLD');
  l.Add('AP_NAV1_HOLD');
  l.Add('FUEL_SELECTOR_OFF');
  l.Add('FUEL_SELECTOR_ALL');
  l.Add('FUEL_SELECTOR_LEFT');
  l.Add('FUEL_SELECTOR_RIGHT');
  l.Add('FUEL_SELECTOR_LEFT_AUX');
  l.Add('FUEL_SELECTOR_RIGHT_AUX');
  l.Add('FUEL_SELECTOR_CENTER');
  l.Add('FUEL_SELECTOR_SET');
  l.Add('FUEL_SELECTOR_2_OFF');
  l.Add('FUEL_SELECTOR_2_ALL');
  l.Add('FUEL_SELECTOR_2_LEFT');
  l.Add('FUEL_SELECTOR_2_RIGHT');
  l.Add('FUEL_SELECTOR_2_LEFT_AUX');
  l.Add('FUEL_SELECTOR_2_RIGHT_AUX');
  l.Add('FUEL_SELECTOR_2_CENTER');
  l.Add('FUEL_SELECTOR_2_SET');
  l.Add('FUEL_SELECTOR_3_OFF');
  l.Add('FUEL_SELECTOR_3_ALL');
  l.Add('FUEL_SELECTOR_3_LEFT');
  l.Add('FUEL_SELECTOR_3_RIGHT');
  l.Add('FUEL_SELECTOR_3_LEFT_AUX');
  l.Add('FUEL_SELECTOR_3_RIGHT_AUX');
  l.Add('FUEL_SELECTOR_3_CENTER');
  l.Add('FUEL_SELECTOR_3_SET');
  l.Add('FUEL_SELECTOR_4_OFF');
  l.Add('FUEL_SELECTOR_4_ALL');
  l.Add('FUEL_SELECTOR_4_LEFT');
  l.Add('FUEL_SELECTOR_4_RIGHT');
  l.Add('FUEL_SELECTOR_4_LEFT_AUX');
  l.Add('FUEL_SELECTOR_4_RIGHT_AUX');
  l.Add('FUEL_SELECTOR_4_CENTER');
  l.Add('FUEL_SELECTOR_4_SET');
  l.Add('CROSS_FEED_OPEN');
  l.Add('CROSS_FEED_TOGGLE');
  l.Add('CROSS_FEED_OFF');
  l.Add('XPNDR');
  l.Add('ADF');
  l.Add('DME');
  l.Add('COM_RADIO');
  l.Add('VOR_OBS');
  l.Add('NAV_RADIO');
  l.Add('COM_RADIO_WHOLE_DEC');
  l.Add('COM_RADIO_WHOLE_INC');
  l.Add('COM_RADIO_FRACT_DEC');
  l.Add('COM_RADIO_FRACT_INC');
  l.Add('NAV1_RADIO_WHOLE_DEC');
  l.Add('NAV1_RADIO_WHOLE_INC');
  l.Add('NAV1_RADIO_FRACT_DEC');
  l.Add('NAV1_RADIO_FRACT_INC');
  l.Add('NAV2_RADIO_WHOLE_DEC');
  l.Add('NAV2_RADIO_WHOLE_INC');
  l.Add('NAV2_RADIO_FRACT_DEC');
  l.Add('NAV2_RADIO_FRACT_INC');
  l.Add('ADF_100_INC');
  l.Add('ADF_10_INC');
  l.Add('ADF_1_INC');
  l.Add('XPNDR_1000_INC');
  l.Add('XPNDR_100_INC');
  l.Add('XPNDR_10_INC');
  l.Add('XPNDR_1_INC');
  l.Add('VOR1_OBI_DEC');
  l.Add('VOR1_OBI_INC');
  l.Add('VOR2_OBI_DEC');
  l.Add('VOR2_OBI_INC');
  l.Add('ADF_100_DEC');
  l.Add('ADF_10_DEC');
  l.Add('ADF_1_DEC');
  l.Add('COM_RADIO_SET');
  l.Add('NAV1_RADIO_SET');
  l.Add('NAV2_RADIO_SET');
  l.Add('ADF_SET');
  l.Add('XPNDR_SET');
  l.Add('VOR1_SET');
  l.Add('VOR2_SET');
  l.Add('DME1_TOGGLE');
  l.Add('DME2_TOGGLE');
  l.Add('RADIO_VOR1_IDENT_DISABLE');
  l.Add('RADIO_VOR2_IDENT_DISABLE');
  l.Add('RADIO_DME1_IDENT_DISABLE');
  l.Add('RADIO_DME2_IDENT_DISABLE');
  l.Add('RADIO_ADF_IDENT_DISABLE');
  l.Add('RADIO_VOR1_IDENT_ENABLE');
  l.Add('RADIO_VOR2_IDENT_ENABLE');
  l.Add('RADIO_DME1_IDENT_ENABLE');
  l.Add('RADIO_DME2_IDENT_ENABLE');
  l.Add('RADIO_ADF_IDENT_ENABLE');
  l.Add('RADIO_VOR1_IDENT_TOGGLE');
  l.Add('RADIO_VOR2_IDENT_TOGGLE');
  l.Add('RADIO_DME1_IDENT_TOGGLE');
  l.Add('RADIO_DME2_IDENT_TOGGLE');
  l.Add('RADIO_ADF_IDENT_TOGGLE');
  l.Add('RADIO_VOR1_IDENT_SET');
  l.Add('RADIO_VOR2_IDENT_SET');
  l.Add('RADIO_DME1_IDENT_SET');
  l.Add('RADIO_DME2_IDENT_SET');
  l.Add('RADIO_ADF_IDENT_SET');
  l.Add('ADF_CARD_INC');
  l.Add('ADF_CARD_DEC');
  l.Add('ADF_CARD_SET');
  l.Add('TOGGLE_DME');
  l.Add('TOGGLE_AVIONICS_MASTER');
  l.Add('COM_STBY_RADIO_SET');
  l.Add('COM_STBY_RADIO_SWAP');
  l.Add('COM_RADIO_FRACT_DEC_CARRY');
  l.Add('COM_RADIO_FRACT_INC_CARRY');
  l.Add('COM2_RADIO_WHOLE_DEC');
  l.Add('COM2_RADIO_WHOLE_INC');
  l.Add('COM2_RADIO_FRACT_DEC');
  l.Add('COM2_RADIO_FRACT_DEC_CARRY');
  l.Add('COM2_RADIO_FRACT_INC');
  l.Add('COM2_RADIO_FRACT_INC_CARRY');
  l.Add('COM2_RADIO_SET');
  l.Add('COM2_STBY_RADIO_SET');
  l.Add('COM2_RADIO_SWAP');
  l.Add('NAV1_RADIO_FRACT_DEC_CARRY');
  l.Add('NAV1_RADIO_FRACT_INC_CARRY');
  l.Add('NAV1_STBY_SET');
  l.Add('NAV1_RADIO_SWAP');
  l.Add('NAV2_RADIO_FRACT_DEC_CARRY');
  l.Add('NAV2_RADIO_FRACT_INC_CARRY');
  l.Add('NAV2_STBY_SET');
  l.Add('NAV2_RADIO_SWAP');
  l.Add('ADF1_RADIO_TENTHS_DEC');
  l.Add('ADF1_RADIO_TENTHS_INC');
  l.Add('XPNDR_1000_DEC');
  l.Add('XPNDR_100_DEC');
  l.Add('XPNDR_10_DEC');
  l.Add('XPNDR_1_DEC');
  l.Add('XPNDR_DEC_CARRY');
  l.Add('XPNDR_INC_CARRY');
  l.Add('ADF_FRACT_DEC_CARRY');
  l.Add('ADF_FRACT_INC_CARRY');
  l.Add('COM1_TRANSMIT_SELECT');
  l.Add('COM2_TRANSMIT_SELECT');
  l.Add('COM_RECEIVE_ALL_TOGGLE');
  l.Add('COM_RECEIVE_ALL_SET');
  l.Add('MARKER_SOUND_TOGGLE');
  l.Add('ADF_COMPLETE_SET');
  l.Add('ADF1_WHOLE_INC');
  l.Add('ADF1_WHOLE_DEC');
  l.Add('ADF2_100_INC');
  l.Add('ADF2_10_INC');
  l.Add('ADF2_1_INC');
  l.Add('ADF2_RADIO_TENTHS_INC');
  l.Add('ADF2_100_DEC');
  l.Add('ADF2_10_DEC');
  l.Add('ADF2_1_DEC');
  l.Add('ADF2_RADIO_TENTHS_DEC');
  l.Add('ADF2_WHOLE_INC');
  l.Add('ADF2_WHOLE_DEC');
  l.Add('ADF2_FRACT_DEC_CARRY');
  l.Add('ADF2_FRACT_INC_CARRY');
  l.Add('ADF2_COMPLETE_SET');
  l.Add('RADIO_ADF2_IDENT_DISABLE');
  l.Add('RADIO_ADF2_IDENT_ENABLE');
  l.Add('RADIO_ADF2_IDENT_TOGGLE');
  l.Add('RADIO_ADF2_IDENT_SET');
  l.Add('FREQUENCY_SWAP');
  l.Add('TOGGLE_GPS_DRIVES_NAV1');
  l.Add('GPS_POWER_BUTTON');
  l.Add('GPS_NEAREST_BUTTON');
  l.Add('GPS_OBS_BUTTON');
  l.Add('GPS_MSG_BUTTON');
  l.Add('GPS_MSG_BUTTON_DOWN');
  l.Add('GPS_MSG_BUTTON_UP');
  l.Add('GPS_FLIGHTPLAN_BUTTON');
  l.Add('GPS_TERRAIN_BUTTON');
  l.Add('GPS_PROCEDURE_BUTTON');
  l.Add('GPS_ZOOMIN_BUTTON');
  l.Add('GPS_ZOOMOUT_BUTTON');
  l.Add('GPS_DIRECTTO_BUTTON');
  l.Add('GPS_MENU_BUTTON');
  l.Add('GPS_CLEAR_BUTTON');
  l.Add('GPS_CLEAR_ALL_BUTTON');
  l.Add('GPS_CLEAR_BUTTON_DOWN');
  l.Add('GPS_CLEAR_BUTTON_UP');
  l.Add('GPS_ENTER_BUTTON');
  l.Add('GPS_CURSOR_BUTTON');
  l.Add('GPS_GROUP_KNOB_INC');
  l.Add('GPS_GROUP_KNOB_DEC');
  l.Add('GPS_PAGE_KNOB_INC');
  l.Add('GPS_PAGE_KNOB_DEC');
  l.Add('EGT');
  l.Add('EGT_INC');
  l.Add('EGT_DEC');
  l.Add('EGT_SET');
  l.Add('BAROMETRIC');
  l.Add('GYRO_DRIFT_INC');
  l.Add('GYRO_DRIFT_DEC');
  l.Add('KOHLSMAN_INC');
  l.Add('KOHLSMAN_DEC');
  l.Add('KOHLSMAN_SET');
  l.Add('TRUE_AIRSPEED_CAL_INC');
  l.Add('TRUE_AIRSPEED_CAL_DEC');
  l.Add('TRUE_AIRSPEED_CAL_SET');
  l.Add('EGT1_INC');
  l.Add('EGT1_DEC');
  l.Add('EGT1_SET');
  l.Add('EGT2_INC');
  l.Add('EGT2_DEC');
  l.Add('EGT2_SET');
  l.Add('EGT3_INC');
  l.Add('EGT3_DEC');
  l.Add('EGT3_SET');
  l.Add('EGT4_INC');
  l.Add('EGT4_DEC');
  l.Add('EGT4_SET');
  l.Add('ATTITUDE_BARS_POSITION_UP');
  l.Add('ATTITUDE_BARS_POSITION_DOWN');
  l.Add('ATTITUDE_CAGE_BUTTON');
  l.Add('RESET_G_FORCE_INDICATOR');
  l.Add('RESET_MAX_RPM_INDICATOR');
  l.Add('HEADING_GYRO_SET');
  l.Add('GYRO_DRIFT_SET');
  l.Add('STROBES_TOGGLE');
  l.Add('ALL_LIGHTS_TOGGLE');
  l.Add('PANEL_LIGHTS_TOGGLE');
  l.Add('LANDING_LIGHTS_TOGGLE');
  l.Add('LANDING_LIGHT_UP');
  l.Add('LANDING_LIGHT_DOWN');
  l.Add('LANDING_LIGHT_LEFT');
  l.Add('LANDING_LIGHT_RIGHT');
  l.Add('LANDING_LIGHT_HOME');
  l.Add('STROBES_ON');
  l.Add('STROBES_OFF');
  l.Add('STROBES_SET');
  l.Add('PANEL_LIGHTS_ON');
  l.Add('PANEL_LIGHTS_OFF');
  l.Add('PANEL_LIGHTS_SET');
  l.Add('LANDING_LIGHTS_ON');
  l.Add('LANDING_LIGHTS_OFF');
  l.Add('LANDING_LIGHTS_SET');
  l.Add('TOGGLE_BEACON_LIGHTS');
  l.Add('TOGGLE_TAXI_LIGHTS');
  l.Add('TOGGLE_LOGO_LIGHTS');
  l.Add('TOGGLE_RECOGNITION_LIGHTS');
  l.Add('TOGGLE_WING_LIGHTS');
  l.Add('TOGGLE_NAV_LIGHTS');
  l.Add('TOGGLE_CABIN_LIGHTS');
  l.Add('TOGGLE_VACUUM_FAILURE');
  l.Add('TOGGLE_ELECTRICAL_FAILURE');
  l.Add('TOGGLE_PITOT_BLOCKAGE');
  l.Add('TOGGLE_STATIC_PORT_BLOCKAGE');
  l.Add('TOGGLE_HYDRAULIC_FAILURE');
  l.Add('TOGGLE_TOTAL_BRAKE_FAILURE');
  l.Add('TOGGLE_LEFT_BRAKE_FAILURE');
  l.Add('TOGGLE_RIGHT_BRAKE_FAILURE');
  l.Add('TOGGLE_ENGINE1_FAILURE');
  l.Add('TOGGLE_ENGINE2_FAILURE');
  l.Add('TOGGLE_ENGINE3_FAILURE');
  l.Add('TOGGLE_ENGINE4_FAILURE');
  l.Add('SMOKE_TOGGLE');
  l.Add('GEAR_TOGGLE');
  l.Add('BRAKES');
  l.Add('GEAR_SET');
  l.Add('BRAKES_LEFT');
  l.Add('BRAKES_RIGHT');
  l.Add('PARKING_BRAKES');
  l.Add('GEAR_PUMP');
  l.Add('PITOT_HEAT_TOGGLE');
  l.Add('SMOKE_ON');
  l.Add('SMOKE_OFF');
  l.Add('SMOKE_SET');
  l.Add('PITOT_HEAT_ON');
  l.Add('PITOT_HEAT_OFF');
  l.Add('PITOT_HEAT_SET');
  l.Add('GEAR_UP');
  l.Add('GEAR_DOWN');
  l.Add('TOGGLE_MASTER_BATTERY');
  l.Add('TOGGLE_MASTER_ALTERNATOR');
  l.Add('TOGGLE_ELECTRIC_VACUUM_PUMP');
  l.Add('TOGGLE_ALTERNATE_STATIC');
  l.Add('DECREASE_DECISION_HEIGHT');
  l.Add('INCREASE_DECISION_HEIGHT');
  l.Add('TOGGLE_STRUCTURAL_DEICE');
  l.Add('TOGGLE_PROPELLER_DEICE');
  l.Add('TOGGLE_ALTERNATOR1');
  l.Add('TOGGLE_ALTERNATOR2');
  l.Add('TOGGLE_ALTERNATOR3');
  l.Add('TOGGLE_ALTERNATOR4');
  l.Add('TOGGLE_MASTER_BATTERY_ALTERNATOR');
  l.Add('AXIS_LEFT_BRAKE_SET');
  l.Add('AXIS_RIGHT_BRAKE_SET');
  l.Add('TOGGLE_AIRCRAFT_EXIT');
  l.Add('TOGGLE_WING_FOLD');
  l.Add('TOGGLE_TAIL_HOOK_HANDLE');
  l.Add('TOGGLE_WATER_RUDDER');
  l.Add('TOGGLE_PUSHBACK');
  l.Add('TUG_HEADING');
  l.Add('TUG_SPEED');
  l.Add('TUG_DISABLE');
  l.Add('TOGGLE_MASTER_IGNITION_SWITCH');
  l.Add('TOGGLE_TAILWHEEL_LOCK');
  l.Add('ADD_FUEL_QUANTITY');
  l.Add('ROTOR_BRAKE');
  l.Add('ROTOR_CLUTCH_SWITCH_TOGGLE');
  l.Add('ROTOR_CLUTCH_SWITCH_SET');
  l.Add('ROTOR_GOV_SWITCH_TOGGLE');
  l.Add('ROTOR_GOV_SWITCH_SET');
  l.Add('ROTOR_LATERAL_TRIM_INC');
  l.Add('ROTOR_LATERAL_TRIM_DEC');
  l.Add('ROTOR_LATERAL_TRIM_SET');
  l.Add('SLEW_TOGGLE');
  l.Add('SLEW_OFF');
  l.Add('SLEW_ON');
  l.Add('SLEW_SET');
  l.Add('SLEW_ALTIT_UP_FAST');
  l.Add('SLEW_ALTIT_UP_SLOW');
  l.Add('SLEW_ALTIT_FREEZE');
  l.Add('SLEW_ALTIT_DN_SLOW');
  l.Add('SLEW_ALTIT_DN_FAST');
  l.Add('SLEW_ALTIT_PLUS');
  l.Add('SLEW_ALTIT_MINUS');
  l.Add('SLEW_PITCH_DN_FAST');
  l.Add('SLEW_PITCH_DN_SLOW');
  l.Add('SLEW_PITCH_FREEZE');
  l.Add('SLEW_PITCH_UP_SLOW');
  l.Add('SLEW_PITCH_UP_FAST');
  l.Add('SLEW_PITCH_PLUS');
  l.Add('SLEW_PITCH_MINUS');
  l.Add('SLEW_BANK_MINUS');
  l.Add('SLEW_AHEAD_PLUS');
  l.Add('SLEW_BANK_PLUS');
  l.Add('SLEW_LEFT');
  l.Add('SLEW_FREEZE');
  l.Add('SLEW_RIGHT');
  l.Add('SLEW_HEADING_MINUS');
  l.Add('SLEW_AHEAD_MINUS');
  l.Add('SLEW_HEADING_PLUS');
  l.Add('SLEW_RESET');
  l.Add('AXIS_SLEW_AHEAD_SET');
  l.Add('AXIS_SLEW_SIDEWAYS_SET');
  l.Add('AXIS_SLEW_HEADING_SET');
  l.Add('AXIS_SLEW_ALT_SET');
  l.Add('AXIS_SLEW_BANK_SET');
  l.Add('AXIS_SLEW_PITCH_SET');
  l.Add('VIEW_MODE');
  l.Add('VIEW_WINDOW_TO_FRONT');
  l.Add('ZOOM_IN');
  l.Add('ZOOM_OUT');
  l.Add('MAP_ZOOM_FINE_IN');
  l.Add('PAN_LEFT');
  l.Add('PAN_RIGHT');
  l.Add('MAP_ZOOM_FINE_OUT');
  l.Add('VIEW_FORWARD');
  l.Add('VIEW_FORWARD_RIGHT');
  l.Add('VIEW_RIGHT');
  l.Add('VIEW_REAR_RIGHT');
  l.Add('VIEW_REAR');
  l.Add('VIEW_REAR_LEFT');
  l.Add('VIEW_LEFT');
  l.Add('VIEW_FORWARD_LEFT');
  l.Add('VIEW_DOWN');
  l.Add('ZOOM_MINUS');
  l.Add('ZOOM_PLUS');
  l.Add('PAN_UP');
  l.Add('PAN_DOWN');
  l.Add('VIEW_MODE_REV');
  l.Add('ZOOM_IN_FINE');
  l.Add('ZOOM_OUT_FINE');
  l.Add('CLOSE_VIEW');
  l.Add('NEW_VIEW');
  l.Add('NEXT_VIEW');
  l.Add('PREV_VIEW');
  l.Add('PAN_LEFT_UP');
  l.Add('PAN_LEFT_DOWN');
  l.Add('PAN_RIGHT_UP');
  l.Add('PAN_RIGHT_DOWN');
  l.Add('PAN_TILT_LEFT');
  l.Add('PAN_TILT_RIGHT');
  l.Add('PAN_RESET');
  l.Add('VIEW_FORWARD_UP');
  l.Add('VIEW_FORWARD_RIGHT_UP');
  l.Add('VIEW_RIGHT_UP');
  l.Add('VIEW_REAR_RIGHT_UP');
  l.Add('VIEW_REAR_UP');
  l.Add('VIEW_REAR_LEFT_UP');
  l.Add('VIEW_LEFT_UP');
  l.Add('VIEW_FORWARD_LEFT_UP');
  l.Add('VIEW_UP');
  l.Add('VIEW_RESET');
  l.Add('PAN_RESET_COCKPIT');
  l.Add('KEY_CHASE_VIEW_NEXT');
  l.Add('KEY_CHASE_VIEW_PREV');
  l.Add('CHASE_VIEW_TOGGLE');
  l.Add('EYEPOINT_UP');
  l.Add('EYEPOINT_DOWN');
  l.Add('EYEPOINT_RIGHT');
  l.Add('EYEPOINT_LEFT');
  l.Add('EYEPOINT_FORWARD');
  l.Add('EYEPOINT_BACK');
  l.Add('EYEPOINT_RESET');
  l.Add('NEW_MAP');
  l.Add('DEMO_STOP');
  l.Add('SELECT_1');
  l.Add('SELECT_2');
  l.Add('SELECT_3');
  l.Add('SELECT_4');
  l.Add('MINUS');
  l.Add('PLUS');
  l.Add('ZOOM_1X');
  l.Add('SOUND_TOGGLE');
  l.Add('PAUSE_TOGGLE');
  l.Add('SIM_RATE');
  l.Add('JOYSTICK_CALIBRATE');
  l.Add('SITUATION_SAVE');
  l.Add('SITUATION_RESET');
  l.Add('SOUND_SET');
  l.Add('EXIT');
  l.Add('ABORT');
  l.Add('READOUTS_SLEW');
  l.Add('READOUTS_FLIGHT');
  l.Add('MINUS_SHIFT');
  l.Add('PLUS_SHIFT');
  l.Add('SIM_RATE_INCR');
  l.Add('SIM_RATE_DECR');
  l.Add('PAUSE_ON');
  l.Add('PAUSE_OFF');
  l.Add('KNEEBOARD_VIEW');
  l.Add('PANEL_1');
  l.Add('PANEL_2');
  l.Add('PANEL_3');
  l.Add('PANEL_4');
  l.Add('PANEL_5');
  l.Add('PANEL_6');
  l.Add('PANEL_7');
  l.Add('PANEL_8');
  l.Add('PANEL_9');
  l.Add('PAUSE_SET');
  l.Add('SOUND_ON');
  l.Add('SOUND_OFF');
  l.Add('INVOKE_HELP');
  l.Add('TOGGLE_AIRCRAFT_LABELS');
  l.Add('FLIGHT_MAP');
  l.Add('RELOAD_PANELS');
  l.Add('PANEL_ID_TOGGLE');
  l.Add('PANEL_ID_OPEN');
  l.Add('PANEL_ID_CLOSE');
  l.Add('RELOAD_USER_AIRCRAFT');
  l.Add('SIM_RESET');
  l.Add('VIRTUAL_COPILOT_TOGGLE');
  l.Add('VIRTUAL_COPILOT_SET');
  l.Add('VIRTUAL_COPILOT_ACTION');
  l.Add('REFRESH_SCENERY');
  l.Add('CLOCK_HOURS_DEC');
  l.Add('CLOCK_HOURS_INC');
  l.Add('CLOCK_MINUTES_DEC');
  l.Add('CLOCK_MINUTES_INC');
  l.Add('CLOCK_SECONDS_ZERO');
  l.Add('CLOCK_HOURS_SET');
  l.Add('CLOCK_MINUTES_SET');
  l.Add('ZULU_HOURS_SET');
  l.Add('ZULU_MINUTES_SET');
  l.Add('ZULU_DAY_SET');
  l.Add('ZULU_YEAR_SET');
  l.Add('ATC');
  l.Add('ATC_MENU_1');
  l.Add('ATC_MENU_2');
  l.Add('ATC_MENU_3');
  l.Add('ATC_MENU_4');
  l.Add('ATC_MENU_5');
  l.Add('ATC_MENU_6');
  l.Add('ATC_MENU_7');
  l.Add('ATC_MENU_8');
  l.Add('ATC_MENU_9');
  l.Add('ATC_MENU_0');
  l.Add('MP_TRANSFER_CONTROL');
  l.Add('MP_PLAYER_SNAP');
  l.Add('MP_GO_OBSERVER');
  l.Add('MP_CHAT');
  l.Add('MP_ACTIVATE_CHAT');
end;

{ TFSXVariable }

constructor TFSXVariable.Create;
begin
  DefinitionId := -1;
  RequestId := -1;
  IsString := False;
end;

end.
