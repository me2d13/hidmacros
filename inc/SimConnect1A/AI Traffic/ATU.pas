{ 25/05/2007 10:26:30 (GMT+1:00) > [Akadamia] checked in   }
{ 25/05/2007 10:26:12 (GMT+1:00) > [Akadamia] checked out /}
{ 14/02/2007 08:26:50 (GMT+0:00) > [Akadamia] checked in   }
{ 14/02/2007 08:26:39 (GMT+0:00) > [Akadamia] checked out /}
{ 12/02/2007 10:10:28 (GMT+0:00) > [Akadamia] checked in   }
{ 08/02/2007 14:05:57 (GMT+0:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: ATU
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:   Translation of AI Traffic example to Delphi
				    Adds AI aircraft to make the flight from Yakima to Spokane busy.
			      First start the user aircraft at Yakima (or load the Yakima to Spokane
			      flight plan used by the AI aircraft - then drive off the runway to view
    			  the goings on).
		    	  Press the Z key to add six AI aircraft
				    Press the X key to give the parked aircraft the Yakima to Spokane
    				flight plan
		    		Both keys can only work once.
				    The creation of the 747 shguld fail - as Yakima airport is not
				    large enough for this aircraft.
History:
-----------------------------------------------------------------------------}
unit ATU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ToolWin, ActnMan, ActnCtrls, XPStyleActnCtrls,
  ActnList, ComCtrls, SimConnect, StdActns;
const
  // Define a user message for message driven version of the example
  WM_USER_SIMCONNECT = WM_USER + 2;
var
  ParkedBoeingID    : DWORD = SIMCONNECT_OBJECT_ID_USER;
  ParkedMooneyID    : DWORD = SIMCONNECT_OBJECT_ID_USER;
type

  // Use enumerated types to create unique IDs as required
  TEventID = (EventSimStart, EventZ, EventX, EventAddedAircraft,
    EventRemovedAircraft);
  TDataRequestId = (RequestBOEING737, RequestBOEING747, RequestBARON,
    RequestLEARJET, RequestBOEING737_PARKED, RequestMOONEY_PARKED,
    RequestBOEING737_PARKED_FLIGHTPLAN, RequestMOONEY_PARKED_FLIGHTPLAN);
  TGroupId = (GroupZX);
  TInputId = (InputZX);

  // The form
  TAITrafficForm = class(TForm)
    StatusBar: TStatusBar;
    ActionManager: TActionManager;
    ActionToolBar: TActionToolBar;
    Images: TImageList;
    Memo: TMemo;
    StartPoll: TAction;
    FileExit: TFileExit;
    StartEvent: TAction;
    procedure StartPollExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SimConnectMessage(var Message: TMessage); message
      WM_USER_SIMCONNECT;
    procedure StartEventExecute(Sender: TObject);
  private
    { Private declarations }
    RxCount: integer;                   // Count of Rx messages
    Quit: boolean;                      // True when signalled to quit
    hSimConnect: THandle;               // Handle for the SimConection
    PlansSent: boolean;
    AircraftCreated: boolean;
  public
    { Public declarations }
    procedure DispatchHandler(pData: PSimConnectRecv; cbData: DWORD; pContext:
      Pointer);
    procedure SendFlightPlans;
    procedure SetUpAIAircraft;
  end;

var
  AITrafficForm     : TAITrafficForm;

implementation

uses SimConnectSupport;

resourcestring
  StrRx6d           = 'Rx: %6d';

{$R *.dfm}

  {-----------------------------------------------------------------------------
    Procedure: MyDispatchProc
    Wraps the call to the form method in a simple StdCall procedure
    Author:    ken.adam
    Date:      11-Jan-2007
    Arguments:
      pData: PSimConnectRecv
      cbData: DWORD
      pContext: Pointer
  -----------------------------------------------------------------------------}

procedure MyDispatchProc(pData: PSimConnectRecv; cbData: DWORD; pContext:
  Pointer); stdcall;
begin
  AITrafficForm.DispatchHandler(pData, cbData, pContext);
end;

{-----------------------------------------------------------------------------
  Procedure: DispatchHandler
  Handle the Dispatched callbacks in a method of the form. Note that this is
  used by both methods of handling the interface.
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments:
    pData: PSimConnectRecv
    cbData: DWORD
    pContext: Pointer
-----------------------------------------------------------------------------}

procedure TAITrafficForm.DispatchHandler(pData: PSimConnectRecv; cbData:
  DWORD;
  pContext: Pointer);
var
  hr                : HRESULT;
  Evt               : PSimconnectRecvEvent;
  OpenData          : PSimConnectRecvOpen;
  EvtAr             : PSimConnectRecvEventObjectAddRemove;
  pObjData          : PSimConnectRecvAssignedObjectId;
begin
  // Maintain a display of the message count
  Inc(RxCount);
  StatusBar.Panels[1].Text := Format(StrRx6d, [RxCount]);
  // Only keep the last 200 lines in the Memo
  while Memo.Lines.Count > 200 do
    Memo.Lines.Delete(0);
  // Handle the various types of message
  case TSimConnectRecvId(pData^.dwID) of
    SIMCONNECT_RECV_ID_EXCEPTION:
      Memo.Lines.Add(SimConnectException(PSimConnectRecvException(pData)));
    SIMCONNECT_RECV_ID_OPEN:
      begin
        StatusBar.Panels[0].Text := 'Opened';
        OpenData := PSimConnectRecvOpen(pData);
        with OpenData^ do
        begin
          Memo.Lines.Add('');
          Memo.Lines.Add(Format('%s %1d.%1d.%1d.%1d', [szApplicationName,
            dwApplicationVersionMajor, dwApplicationVersionMinor,
              dwApplicationBuildMajor, dwApplicationBuildMinor]));
          Memo.Lines.Add(Format('%s %1d.%1d.%1d.%1d', ['SimConnect',
            dwSimConnectVersionMajor, dwSimConnectVersionMinor,
              dwSimConnectBuildMajor, dwSimConnectBuildMinor]));
          Memo.Lines.Add('');
        end;
      end;
    SIMCONNECT_RECV_ID_EVENT:
      begin
        evt := PSimconnectRecvEvent(pData);
        case TEventId(evt^.uEventID) of
          EventSimStart:
            begin
              // Make the call for data every second, but only when it changes and
              // only that data that has changed
              // Turn the ctrl-shift-A input event on now
              hr := SimConnect_SetInputGroupState(hSimConnect, Ord(InputZX),
                Ord(SIMCONNECT_STATE_ON));
            end;
          EventZ:
            if not aircraftCreated then
            begin
              SetUpAIAircraft;
              AircraftCreated := True;
            end;
          EventX:
            if not PlansSent and AircraftCreated then
            begin
              SendFlightPlans;
              PlansSent := True;
            end;
        end;
      end;
    SIMCONNECT_RECV_ID_EVENT_OBJECT_ADDREMOVE:
      begin
        EvtAr := PSimConnectRecvEventObjectAddRemove(pData);
        case TEventId(EvtAR^.uEventID) of
          EventAddedAircraft:
            Memo.Lines.Add(Format('AI object added: Type=%s, ObjectID=%d',
              [SimConnectSimObjectTypeNames[evtAr^.eObjType], evtAr^.dwData]));
          EventRemovedAircraft:
            Memo.Lines.Add(Format('AI object removed: Type=%s, ObjectID=%d',
              [SimConnectSimObjectTypeNames[evtAr^.eObjType], evtAr^.dwData]));
        end;
      end;
    SIMCONNECT_RECV_ID_ASSIGNED_OBJECT_ID:
      begin
        pObjData := PSimConnectRecvAssignedObjectId(pData);

        case TDataRequestId(pObjData^.dwRequestID) of
          // Do nothing specific in these cases, as the aircraft already have their flight plans
          RequestBOEING737:
            Memo.Lines.Add(Format('Created Boeing 737 id = %d',
              [pObjData^.dwObjectID]));
          RequestBOEING747:
            Memo.Lines.Add(Format('Created Boeing 747 id = %d',
              [pObjData^.dwObjectID]));
          RequestBARON:
            Memo.Lines.Add(Format('Created Beech Baron id = %d',
              [pObjData^.dwObjectID]));
          RequestLEARJET:
            Memo.Lines.Add(Format('Created Learjet id = %d',
              [pObjData^.dwObjectID]));
          RequestBOEING737_PARKED:
            begin
              // Record the object ID, so the flightplan can be sent out later
              ParkedBoeingID := pObjData^.dwObjectID;
              Memo.Lines.Add(Format('Created parked Boeing %d',
                [pObjData^.dwObjectID]));
            end;
          RequestMOONEY_PARKED:
            begin // Record the object ID, so the flightplan can be sent out later
              ParkedMooneyID := pObjData^.dwObjectID;
              Memo.Lines.Add(Format('Created parked Mooney %d',
                [pObjData^.dwObjectID]));
            end
        else
          Memo.Lines.Add(Format('Unknown creation %d',
            [pObjData^.dwRequestID]));
        end;
      end;
    SIMCONNECT_RECV_ID_QUIT:
      begin
        Quit := True;
        StatusBar.Panels[0].Text := 'FS X Quit';
      end
  else
    Memo.Lines.Add(Format('Unknown dwID: %d', [pData.dwID]));
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: FormCloseQuery
  Ensure that we can signal "Quit" to the busy wait loop
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments: Sender: TObject var CanClose: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TAITrafficForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  Quit := True;
  CanClose := True;
end;

{-----------------------------------------------------------------------------
  Procedure: FormCreate
  We are using  run-time dynamic loading of SimConnect.dll, so that the program
  will execute and fail gracefully if the DLL does not exist.
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments:
    Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TAITrafficForm.FormCreate(Sender: TObject);
begin
  if InitSimConnect then
    StatusBar.Panels[2].Text := 'SimConnect.dll Loaded'
  else
  begin
    StatusBar.Panels[2].Text := 'SimConnect.dll NOT FOUND';
    StartPoll.Enabled := False;
    StartEvent.Enabled := False;
  end;
  Quit := False;
  hSimConnect := 0;
    PlansSent:= false;
    AircraftCreated:= false;
  StatusBar.Panels[0].Text := 'Not Connected';
  RxCount := 0;
  StatusBar.Panels[1].Text := Format(StrRx6d, [RxCount]);
end;

{-----------------------------------------------------------------------------
  Procedure: SendFlightPlans
  Author:    ken.adam
  Date:      19-Jan-2007
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TAITrafficForm.SendFlightPlans;
var
  hr                : HRESULT;
begin
  if (ParkedBoeingID <> SIMCONNECT_OBJECT_ID_USER) then
    hr := SimConnect_AISetAircraftFlightPlan(hSimConnect, ParkedBoeingID,
      'IFR Yakima Air Term Mcallister to Spokane Intl',
      Ord(RequestBOEING737_PARKED_FLIGHTPLAN));
  if (ParkedMooneyID <> SIMCONNECT_OBJECT_ID_USER) then
    hr := SimConnect_AISetAircraftFlightPlan(hSimConnect, ParkedMooneyID,
      'IFR Yakima Air Term Mcallister to Spokane Intl',
      Ord(RequestMOONEY_PARKED_FLIGHTPLAN));
end;

procedure TAITrafficForm.SetUpAIAircraft;
var
  hr                : HRESULT;
begin
  // Add some AI controlled aircraft
  hr := SimConnect_AICreateEnrouteATCAircraft(hSimConnect, 'Boeing 737-800',
    'N100', 100, 'IFR Yakima Air Term Mcallister to Spokane Intl', 0.0, false,
    Ord(RequestBOEING737));
  hr := SimConnect_AICreateEnrouteATCAircraft(hSimConnect, 'Boeing 747-400',
    'N101', 101, 'IFR Yakima Air Term Mcallister to Spokane Intl', 0.0, false,
    Ord(RequestBOEING747));
  hr := SimConnect_AICreateEnrouteATCAircraft(hSimConnect, 'Beech Baron 58',
    'N200', 200, 'IFR Yakima Air Term Mcallister to Spokane Intl', 0.0, false,
    Ord(RequestBARON));
  hr := SimConnect_AICreateEnrouteATCAircraft(hSimConnect, 'Learjet 45', 'N201',
    201, 'IFR Yakima Air Term Mcallister to Spokane Intl', 0.0, false,
    Ord(RequestLEARJET));
  // Park a few aircraft
  hr := SimConnect_AICreateParkedATCAircraft(hSimConnect, 'Boeing 737-800',
    'N102', 'KYKM', Ord(RequestBOEING737_PARKED));
  hr := SimConnect_AICreateParkedATCAircraft(hSimConnect, 'Mooney Bravo',
    'N202', 'KYKM', Ord(RequestMOONEY_PARKED));
end;

{-----------------------------------------------------------------------------
  Procedure: SimConnectMessage
  This uses CallDispatch, but could probably avoid the callback and use
  SimConnect_GetNextDispatch instead.
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments:
    var Message: TMessage
-----------------------------------------------------------------------------}

procedure TAITrafficForm.SimConnectMessage(var Message: TMessage);
begin
  SimConnect_CallDispatch(hSimConnect, MyDispatchProc, nil);
end;

{-----------------------------------------------------------------------------
  Procedure: StartEventExecute
  Opens the connection for Event driven handling. If successful sets up the data
  requests and hooks the system event "SimStart".
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments:
    Sender: TObject
-----------------------------------------------------------------------------}

procedure TAITrafficForm.StartEventExecute(Sender: TObject);
var
  hr                : HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', Handle,
    WM_USER_SIMCONNECT, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    // Create some private events
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventZ));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventX));
    // Link the private events to keyboard keys, and ensure input events are off
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(InputZX), 'Z',
      Ord(EventZ));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(InputZX), 'X',
      Ord(EventX));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(InputZX),
      Ord(SIMCONNECT_STATE_OFF));
    // Sign up for notifications
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GroupZX),
      Ord(EventZ));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GroupZX),
      Ord(EventX));
    // Request a simulation start event
    hr := SimConnect_SubscribeToSystemEvent(hSimConnect, Ord(EventSimStart),
      'SimStart');
    // Subscribe to system events notifying the client that objects have been added or removed
    hr := SimConnect_SubscribeToSystemEvent(hSimConnect, Ord(EventAddedAircraft),
      'ObjectAdded');
    hr := SimConnect_SubscribeToSystemEvent(hSimConnect, Ord(EventRemovedAircraft),
      'ObjectRemoved');
    StartEvent.Enabled := False;
    StartPoll.Enabled := False;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: StartPollExecute
  Opens the connection for Polled access. If successful sets up the data
  requests and hooks the system event "SimStart", and then loops indefinitely
  on CallDispatch.
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments:
    Sender: TObject
-----------------------------------------------------------------------------}

procedure TAITrafficForm.StartPollExecute(Sender: TObject);
var
  hr                : HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    // Create some private events
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventZ));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventX));
    // Link the private events to keyboard keys, and ensure input events are off
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(InputZX), 'Z',
      Ord(EventZ));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(InputZX), 'X',
      Ord(EventX));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(InputZX),
      Ord(SIMCONNECT_STATE_OFF));
    // Sign up for notifications
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GroupZX),
      Ord(EventZ));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GroupZX),
      Ord(EventX));
    // Request a simulation start event
    hr := SimConnect_SubscribeToSystemEvent(hSimConnect, Ord(EventSimStart),
      'SimStart');
    // Subscribe to system events notifying the client that objects have been added or removed
    hr := SimConnect_SubscribeToSystemEvent(hSimConnect, Ord(EventAddedAircraft),
      'ObjectAdded');
    hr := SimConnect_SubscribeToSystemEvent(hSimConnect, Ord(EventRemovedAircraft),
      'ObjectRemoved');
    StartEvent.Enabled := False;
    StartPoll.Enabled := False;
    while not Quit do
    begin
      SimConnect_CallDispatch(hSimConnect, MyDispatchProc, nil);
      Application.ProcessMessages;
      Sleep(1);
    end;
    hr := SimConnect_Close(hSimConnect);
  end;
end;

end.

