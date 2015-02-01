{ 25/05/2007 10:24:05 (GMT+1:00) > [Akadamia] checked in   }
{ 25/05/2007 10:22:39 (GMT+1:00) > [Akadamia] checked out /}
{ 25/05/2007 10:20:58 (GMT+1:00) > [Akadamia] checked in Add note to display  }
{ 25/05/2007 09:27:39 (GMT+1:00) > [Akadamia] checked out /Add note to display}
{ 21/05/2007 17:03:07 (GMT+1:00) > [Akadamia] checked in Updates for SP1A  }
{ 21/05/2007 16:56:34 (GMT+1:00) > [Akadamia] checked out /Updates for SP1A}
{ 14/02/2007 08:24:20 (GMT+0:00) > [Akadamia] checked out /}
{ 12/02/2007 10:10:09 (GMT+0:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: AOU
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:   Translation of AI Objects and Waypoints example to Delphi
				    Adds Non ATC controlled simulation objects.
				    With the default flight (Cessna at Seatac) - turn off the engine
				    and watch the antics.
				    Press z to create the objects
				    Press x to load them with their waypoint lists
 History:
-----------------------------------------------------------------------------}
unit AOU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ToolWin, ActnMan, ActnCtrls, XPStyleActnCtrls,
  ActnList, ComCtrls, SimConnect, StdActns;
const
  // Define a user message for message driven version of the example
  WM_USER_SIMCONNECT = WM_USER + 2;
var
  BalloonID         : DWORD = SIMCONNECT_OBJECT_ID_USER;
  BellID            : DWORD = SIMCONNECT_OBJECT_ID_USER;
  MooneyID          : DWORD = SIMCONNECT_OBJECT_ID_USER;
  TruckID           : DWORD = SIMCONNECT_OBJECT_ID_USER;
type
  // Use enumerated types to create unique IDs as required
  TEventID = (EventSimStart, EventZ, EventX, EventC, EventV);
  TDataRequestId = (
    REQUEST_BALLOON1,
    REQUEST_BELL,
    REQUEST_MOONEY,
    REQUEST_DOUGLAS,
    REQUEST_TRUCK,
    REQUEST_WHALE);

  TGroupId = (GroupZX);
  TInputId = (InputZX);
  TDataDefineId = (DefinitionWaypoint, DefinitionThrottle);
  TBallonControl = packed record
    ThrottlePercent: double;
  end;

  // The form
  TAIObjectsForm = class(TForm)
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
    ObjectsCreated: boolean;
    bc: TBallonControl;
  public
    { Public declarations }
    procedure DispatchHandler(pData: PSimConnectRecv; cbData: DWORD; pContext:
      Pointer);
    procedure SendFlightPlans;
    procedure SetUpSimObjects;
  end;

var
  AIObjectsForm     : TAIObjectsForm;

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
  AIObjectsForm.DispatchHandler(pData, cbData, pContext);
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

procedure TAIObjectsForm.DispatchHandler(pData: PSimConnectRecv; cbData:
  DWORD;
  pContext: Pointer);
var
  hr                : HRESULT;
  Evt               : PSimconnectRecvEvent;
  OpenData          : PSimConnectRecvOpen;
  pObjData          : PSimConnectRecvAssignedObjectID;
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
      Memo.Lines.Add(
        SimConnectExceptionNames[TSimConnectException(PSimConnectRecvException(pData)^.dwException)]);
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
              // Sim has started so turn the input events on
              hr := SimConnect_SetInputGroupState(hSimConnect, Ord(InputZX),
                Ord(SIMCONNECT_STATE_ON));
            end;
          EventZ:
            if not ObjectsCreated then
            begin
              SetUpSimObjects;
              objectsCreated := True;
            end;
          EventX:
            if not plansSent and ObjectsCreated then
            begin
              SendFlightPlans;
              PlansSent := True;
            end;
          EventC:
            begin
              Memo.Lines.Add('EventC');
              // Give the balloon some throttle
              // SDK actually uses SIMCONNECT_OBJECT_ID_USER, which is own ship
              // Yet to find a SimObject that will respond to "Throttle"
              if bc.ThrottlePercent < 100.0 then
                bc.throttlePercent := bc.throttlePercent + 5.0;
              hr := SimConnect_SetDataOnSimObject(hSimConnect,
                Ord(DefinitionThrottle), SIMCONNECT_OBJECT_ID_USER, 0, 1,
                sizeof(bc), @bc);
            end;
          EventV:
            begin
              Memo.Lines.Add('EventV');
              // Give the balloon some throttle
              // SDK actually uses SIMCONNECT_OBJECT_ID_USER, which is own ship
              // Yet to find a SimObject that will respond to "Throttle"
              if bc.throttlePercent > 0.0 then
                bc.throttlePercent := bc.throttlePercent - 5.0;
              hr := SimConnect_SetDataOnSimObject(hSimConnect,
                Ord(DefinitionThrottle), SIMCONNECT_OBJECT_ID_USER, 0, 1,
                sizeof(bc), @bc);
            end;
        end;
      end;
    SIMCONNECT_RECV_ID_ASSIGNED_OBJECT_ID:
      begin
        pObjData := PSimConnectRecvAssignedObjectID(pData);
        case TDataRequestId(pObjData^.dwRequestID) of
          REQUEST_BALLOON1:
            begin
              BalloonID := pObjData^.dwObjectID;
              Memo.Lines.Add(Format('Created "Balloon 1" id = %d', [BalloonID]));
            end;
          REQUEST_BELL:
            begin
              BellID := pObjData^.dwObjectID;
              Memo.Lines.Add(Format('Created Bell Helicopter id = %d',
                [BellID]));
            end;
          REQUEST_MOONEY:
            begin
              MooneyID := pObjData^.dwObjectID;
              Memo.Lines.Add(Format('Created Mooney Bravo id = %d',
                [MooneyID]));
            end;
          REQUEST_DOUGLAS:
            Memo.Lines.Add(Format('Created stationary Douglas DC3 id = %d',
              [pObjData^.dwObjectID]));

          REQUEST_TRUCK:
            begin
              TruckID := pObjData^.dwObjectID;
              Memo.Lines.Add(Format('Created truck id = %d', [TruckID]));
            end;
          REQUEST_WHALE:
            Memo.Lines.Add(Format('Created humpback whale id = %d',
              [pObjData^.dwObjectID]));
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

procedure TAIObjectsForm.FormCloseQuery(Sender: TObject;
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

procedure TAIObjectsForm.FormCreate(Sender: TObject);
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
  PlansSent := False;
  ObjectsCreated := False;
  hSimConnect := 0;
  StatusBar.Panels[0].Text := 'Not Connected';
  RxCount := 0;
  StatusBar.Panels[1].Text := Format(StrRx6d, [RxCount]);
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

procedure TAIObjectsForm.SendFlightPlans;
var
  hr                : HRESULT;
  wp                : array[0..2] of TSimConnectDataWaypoint;  // Mooney waypoint list
  ft                : array[0..1] of TSimConnectDataWaypoint;  // Truck waypoint list
begin
  // Mooney aircraft should fly in circles across the North end of the runway
  wp[0].Flags := SIMCONNECT_WAYPOINT_SPEED_REQUESTED;
  wp[0].Altitude := 800;
  wp[0].Latitude := 47 + (27.79 / 60);
  wp[0].Longitude := -122 - (18.46 / 60);
  wp[0].ktsSpeed := 100;

  wp[1].Flags := SIMCONNECT_WAYPOINT_SPEED_REQUESTED;
  wp[1].Altitude := 600;
  wp[1].Latitude := 47 + (27.79 / 60);
  wp[1].Longitude := -122 - (17.37 / 60);
  wp[1].ktsSpeed := 100;

  wp[2].Flags := SIMCONNECT_WAYPOINT_WRAP_TO_FIRST or
    SIMCONNECT_WAYPOINT_SPEED_REQUESTED;
  wp[2].Altitude := 800;
  wp[2].Latitude := 47 + (27.79 / 60);
  wp[2].Longitude := -122 - (19.92 / 60);
  wp[2].ktsSpeed := 100;

  // Send the three waypoints to the Mooney
  // ** Note that this call has changed with SP1A to match the documentation **
  hr := SimConnect_SetDataOnSimObject(hSimConnect, Ord(DefinitionWayPoint),
    MooneyID, 0, Length(wp), SizeOf(wp[0]), @wp[0]);

  // Truck goes down the runway
  ft[0].Flags := SIMCONNECT_WAYPOINT_SPEED_REQUESTED;
  ft[0].Altitude := 433;
  ft[0].Latitude := 47 + (25.93 / 60);
  ft[0].Longitude := -122 - (18.46 / 60);
  ft[0].ktsSpeed := 75;

  ft[1].Flags := SIMCONNECT_WAYPOINT_WRAP_TO_FIRST or
    SIMCONNECT_WAYPOINT_SPEED_REQUESTED;
  ft[1].Altitude := 433;
  ft[1].Latitude := 47 + (26.25 / 60);
  ft[1].Longitude := -122 - (18.46 / 60);
  ft[1].ktsSpeed := 55;

  // Send the two waypoints to the truck
  // ** updated for SP1A **
  hr := SimConnect_SetDataOnSimObject(hSimConnect, Ord(DefinitionWayPoint),
    TruckID, 0, Length(ft), SizeOf(ft[0]), @ft[0]);
end;

procedure TAIObjectsForm.SetUpSimObjects;
var
  Init              : TSimConnectDataInitPosition;
  hr                : HRESULT;
begin

  // Add a parked museum aircraft, just west of the runway

  Init.Altitude := 433.0;               // Altitude of Sea-tac is 433 feet
  Init.Latitude := 47 + (25.97 / 60);   // Convert from 47 25.97 N
  Init.Longitude := -122 - (18.51 / 60); // Convert from 122 18.51 W
  Init.Pitch := 0.0;
  Init.Bank := 0.0;
  Init.Heading := 90.0;
  Init.OnGround := 1;
  Init.Airspeed := 0;
  hr := SimConnect_AICreateSimulatedObject(hSimConnect, 'Douglas DC-3', Init,
    Ord(REQUEST_DOUGLAS));

  // Add a hot air balloon
  // Except Hot Air ballons are scenery, not simobjects, so you can't add one
  // Using an "ANI_elephant_walk01_sm" instead....
  // Need to use (exactly) the name that is in the "sim.cfg" file for the SimObject
  // Not the name of the directory, which may not be the same.
  Init.Altitude := 500.0;               // Altitude of Sea-tac is 433 feet
  Init.Latitude := 47 + (25.97 / 60);   // Convert from 47 26.22 N
  Init.Longitude := -122 - (18.45 / 60); // Convert from 122 18.45 W
  Init.Pitch := 0.0;
  Init.Bank := 0.0;
  Init.Heading := 0.0;
  Init.OnGround := 0;
  Init.Airspeed := 0;
  hr := SimConnect_AICreateSimulatedObject(hSimConnect, 'ANI_elephant_walk01_sm', Init,
    Ord(REQUEST_BALLOON1));

  // Add a helicopter

  Init.Altitude := 433.0;               // Altitude of Sea-tac is 433 feet
  Init.Latitude := 47 + (26.22 / 60);   // Convert from 47 26.22 N
  Init.Longitude := -122 - (18.48 / 60); // Convert from 122 18.48 W
  Init.Pitch := 0.0;
  Init.Bank := 0.0;
  Init.Heading := 0.0;
  Init.OnGround := 1;
  Init.Airspeed := 100;

  //hr = SimConnect_AICreateNonATCAircraft(hSimConnect, 'Bell 206B JetRanger', 'H1000', Init, Ord(REQUEST_BELL));

  // Initialize Mooney aircraft just in front of user aircraft
  // User aircraft is at 47 25.89 N, 122 18.48 W

  Init.Altitude := 433.0;               // Altitude of Sea-tac is 433 feet
  Init.Latitude := 47 + (25.91 / 60);   // Convert from 47 25.90 N
  Init.Longitude := -122 - (18.48 / 60); // Convert from 122 18.48 W
  Init.Pitch := 0.0;
  Init.Bank := 0.0;
  Init.Heading := 360.0;
  Init.OnGround := 1;
  Init.Airspeed := 1;

  hr := SimConnect_AICreateNonATCAircraft(hSimConnect, 'Mooney Bravo', 'N1001',
    Init, Ord(REQUEST_MOONEY));

  // Initialize truck just in front of user aircraft
  // User aircraft is at 47 25.89 N, 122 18.48 W

  Init.Altitude := 433.0;               // Altitude of Sea-tac is 433 feet
  Init.Latitude := 47 + (25.91 / 60);   // Convert from 47 25.90 N
  Init.Longitude := -122 - (18.47 / 60); // Convert from 122 18.48 W
  Init.Pitch := 0.0;
  Init.Bank := 0.0;
  Init.Heading := 360.0;
  Init.OnGround := 1;
  Init.Airspeed := 0;

  hr := SimConnect_AICreateSimulatedObject(hSimConnect, 'VEH_jetTruck', Init,
    Ord(REQUEST_TRUCK));

  // Add a humpback whale
  // This is in the view over your left shoulder, and is actually underground.
  Init.Altitude := 433.0;               // Altitude of Sea-tac is 433 feet
  Init.Latitude := 47 + (25.89 / 60);   // Convert from 47 25.89 N
  Init.Longitude := -122 - (18.51 / 60); // Convert from 122 18.51 W
  Init.Pitch := 0.0;
  Init.Bank := 0.0;
  Init.Heading := 0.0;
  Init.OnGround := 1;
  Init.Airspeed := 0;
  hr := SimConnect_AICreateSimulatedObject(hSimConnect, 'Humpbackwhale', Init,
    Ord(REQUEST_WHALE));
end;

{-----------------------------------------------------------------------------
  Procedure: SimConnectMessage
  Author:    ken.adam
  Date:      19-Jan-2007
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TAIObjectsForm.SimConnectMessage(var Message: TMessage);
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

procedure TAIObjectsForm.StartEventExecute(Sender: TObject);
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
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventC));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventV));

    // Link the private events to keyboard keys, and ensure the input events are off
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(InputZX), 'Z',
      Ord(EventZ));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(InputZX), 'X',
      Ord(EventX));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(InputZX), 'C',
      Ord(EventC));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(InputZX), 'V',
      Ord(EventV));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(InputZX),
      Ord(SIMCONNECT_STATE_OFF));

    // Sign up for notifications
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect,
      Ord(GroupZX), Ord(EventZ));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect,
      Ord(GroupZX), Ord(EventX));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect,
      Ord(GroupZX), Ord(EventC));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect,
      Ord(GroupZX), Ord(EventV));

    // Set up a definition for a waypoint list
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionWaypoint),
      'AI Waypoint List', 'number', SIMCONNECT_DATATYPE_WAYPOINT);

    // Set up a definition for the balloon throttle
    bc.throttlePercent := 0.0;
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionThrottle),
      'GENERAL ENG THROTTLE LEVER POSITION:1', 'percent');

    // Request a simulation start event
    hr := SimConnect_SubscribeToSystemEvent(hSimConnect, Ord(EventSimStart),
      'SimStart');
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

procedure TAIObjectsForm.StartPollExecute(Sender: TObject);
var
  hr                : HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    // Create some private events
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventZ));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventX));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventC));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventV));

    // Link the private events to keyboard keys, and ensure the input events are off
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(InputZX), 'Z',
      Ord(EventZ));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(InputZX), 'X',
      Ord(EventX));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(InputZX), 'C',
      Ord(EventC));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(InputZX), 'V',
      Ord(EventV));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(InputZX),
      Ord(SIMCONNECT_STATE_OFF));

    // Sign up for notifications
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect,
      Ord(GroupZX), Ord(EventZ));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect,
      Ord(GroupZX), Ord(EventX));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect,
      Ord(GroupZX), Ord(EventC));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect,
      Ord(GroupZX), Ord(EventV));

    // Set up a definition for a waypoint list
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionWaypoint),
      'AI Waypoint List', 'number', SIMCONNECT_DATATYPE_WAYPOINT);

    // Set up a definition for the balloon throttle
    bc.throttlePercent := 0.0;
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionThrottle),
      'MASTER IGNITION SWITCH', 'bool', SIMCONNECT_DATATYPE_FLOAT32);
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionThrottle),
      'GENERAL ENG THROTTLE LEVER POSITION:1', 'percent', SIMCONNECT_DATATYPE_FLOAT64);

    // Request a simulation start event
    hr := SimConnect_SubscribeToSystemEvent(hSimConnect, Ord(EventSimStart),
      'SimStart');
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

