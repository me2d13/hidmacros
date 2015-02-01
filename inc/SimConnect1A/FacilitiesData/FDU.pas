{ 25/05/2007 11:15:22 (GMT+1:00) > [Akadamia] checked in   }
{ 25/05/2007 11:14:58 (GMT+1:00) > [Akadamia] checked out /}
{ 25/05/2007 11:14:27 (GMT+1:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: FDU
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:   Translation of Facilities Data example to Delphi
 History:
-----------------------------------------------------------------------------}
unit FDU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ToolWin, ActnMan, ActnCtrls, XPStyleActnCtrls,
  ActnList, ComCtrls, SimConnect, StdActns;
const
  // Define a user message for message driven version of the example
  WM_USER_SIMCONNECT = WM_USER + 2;
type

  // Use enumerated types to create unique IDs as required
  TGroupId = (Group0);
  TInputId = (Input0);
  TEventID = (Event0, Event1, EventMenu1, EventMenu2);
  TDataRequestId = (Request0, Request1);

  // The form
  TFacilitiesDataForm = class(TForm)
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
  public
    { Public declarations }
    procedure DispatchHandler(pData: PSimConnectRecv; cbData: DWORD; pContext:
      Pointer);
    procedure RespondTo(Rcv: PSimConnectRecvFaciltiesList; ListType:
      SIMCONNECT_FACILITY_LIST_TYPE);
  end;

var
  FacilitiesDataForm: TFacilitiesDataForm;

implementation

uses SimConnectSupport;

resourcestring
  StrRx6d           = 'Rx: %6d';
const
  szTitle           = 'Facilities Data' + #0;
  szHelp            = 'Press Ctrl-F1 for Get Facilities, Ctrl-F2 for Subscribe to Facilities'
    + #0;
  GetFacilitiesMenu = 'SimConnect Facilities Test' + #0 +
    'Choose which item:' + #0 +
    'Get airport facilities' + #0 +
    'Get waypoints' + #0 +
    'Get NDB' + #0 +
    'Get VOR' + #0 +
    'Close menu' + #0;
  SubscribeFacilitiesMenu = 'SimConnect Facilities Test' + #0 +
    'Choose which item:' + #0 +
    'Subscribe to airports' + #0 +
    'Subscribe to waypoints' + #0 +
    'Subscribe to NDB' + #0 +
    'Subscribe to VOR' + #0 +
    'Unsubscribe to airports' + #0 +
    'Unsubscribe to waypoints' + #0 +
    'Unsubscribe to NDB' + #0 +
    'Unsubscribe to VOR' + #0 +
    'Close menu' + #0;

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
  FacilitiesDataForm.DispatchHandler(pData, cbData, pContext);
end;

function MenuText(InResult: SIMCONNECT_TEXT_RESULT): string;
begin
  case InResult of
    SIMCONNECT_TEXT_RESULT_DISPLAYED:
      Result := 'Displayed';
    SIMCONNECT_TEXT_RESULT_QUEUED:
      Result := 'Queued';
    SIMCONNECT_TEXT_RESULT_REMOVED:
      Result := 'Removed from Queue';
    SIMCONNECT_TEXT_RESULT_REPLACED:
      Result := 'Replaced in Queue';
    SIMCONNECT_TEXT_RESULT_TIMEOUT:
      Result := 'Timeout';
  else
    begin
      if (SIMCONNECT_TEXT_RESULT_MENU_SELECT_1 <= InResult)
        and (InResult <= SIMCONNECT_TEXT_RESULT_MENU_SELECT_10) then
        Result := Format('%2d Selected', [Ord(InResult) -
          Ord(SIMCONNECT_TEXT_RESULT_MENU_SELECT_1) + 1])
      else
        Result := '<unknown>';
    end;
  end;
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

procedure TFacilitiesDataForm.DispatchHandler(pData: PSimConnectRecv; cbData:
  DWORD;
  pContext: Pointer);
var
  hr                : HRESULT;
  Evt               : PSimconnectRecvEvent;
  OpenData          : PSimConnectRecvOpen;
  Item              : cardinal;
begin
  // Maintain a display of the message count
  Inc(RxCount);
  StatusBar.Panels[1].Text := Format(StrRx6d, [RxCount]);
  // Only keep the last 2000 lines in the Memo
  while Memo.Lines.Count > 2000 do
    Memo.Lines.Delete(0);
  // Handle the various types of message
  case TSimConnectRecvId(pData^.dwID) of
    SIMCONNECT_RECV_ID_EXCEPTION:
      with PSimConnectRecvException(pData)^ do
        Memo.Lines.Add(Format('%s (%8.8x,%8.8x,%8.8x,%8.8x,%8.8x,%8.8x)', [
          SimConnectExceptionNames[TSimConnectException(dwException)],
            dwSize, dwVersion, dwID, dwException, dwSendID, dwIndex]));
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
          Event0:                       // Display menu
            SimConnect_Text(hSimConnect, SIMCONNECT_TEXT_TYPE_MENU, 0,
              Ord(EventMenu1), length(GetFacilitiesMenu),
              @GetFacilitiesMenu[1]);
          Event1:                       // Stop displaying menu
            SimConnect_Text(hSimConnect, SIMCONNECT_TEXT_TYPE_MENU, 0,
              Ord(EventMenu2), length(SubscribeFacilitiesMenu),
              @SubscribeFacilitiesMenu[1]);
          EventMenu1:                   // Respond to the users menu selection
            begin
              Memo.Lines.Add(Format('MENU #1 Event: dwData=%d (%s)',
                [evt^.dwData, MenuText(SIMCONNECT_TEXT_RESULT(evt^.dwData))]));
              Item := evt^.dwData - Ord(SIMCONNECT_TEXT_RESULT_MENU_SELECT_1);
              if Item < Ord(SIMCONNECT_FACILITY_LIST_TYPE_COUNT) then
                // Get the current cached list of airports, waypoints, etc, as the item indicates
                hr := SimConnect_RequestFacilitiesList(hSimConnect,
                  SIMCONNECT_FACILITY_LIST_TYPE(Item), Ord(Request0));
            end;
          EventMenu2:
            begin
              Memo.Lines.Add(Format('MENU #2 Event: dwData=%d (%s)',
                [evt^.dwData, MenuText(SIMCONNECT_TEXT_RESULT(evt^.dwData))]));
              Item := evt^.dwData - Ord(SIMCONNECT_TEXT_RESULT_MENU_SELECT_1);
              if Item < Ord(SIMCONNECT_FACILITY_LIST_TYPE_COUNT) then
                hr := SimConnect_SubscribeToFacilities(hSimConnect,
                  SIMCONNECT_FACILITY_LIST_TYPE(item), Ord(Request1))
              else if (Ord(SIMCONNECT_FACILITY_LIST_TYPE_COUNT) <= Item)
                and (Item < 2 * Ord(SIMCONNECT_FACILITY_LIST_TYPE_COUNT)) then
                hr := SimConnect_UnsubscribeToFacilities(hSimConnect,
                  SIMCONNECT_FACILITY_LIST_TYPE(item -
                  Ord(SIMCONNECT_FACILITY_LIST_TYPE_COUNT)));
            end
        else
          Memo.Lines.Add(Format('SIMCONNECT_RECV_EVENT: 0x%08X 0x%08X 0x%X',
            [evt^.uEventID, evt^.dwData, cbData]));
        end;
      end;
    SIMCONNECT_RECV_ID_AIRPORT_LIST:
      RespondTo(PSimConnectRecvFaciltiesList(pData),
        SIMCONNECT_FACILITY_LIST_TYPE_AIRPORT);
    SIMCONNECT_RECV_ID_WAYPOINT_LIST:
      RespondTo(PSimConnectRecvFaciltiesList(pData),
        SIMCONNECT_FACILITY_LIST_TYPE_WAYPOINT);
    SIMCONNECT_RECV_ID_NDB_LIST:
      RespondTo(PSimConnectRecvFaciltiesList(pData),
        SIMCONNECT_FACILITY_LIST_TYPE_NDB);
    SIMCONNECT_RECV_ID_VOR_LIST:
      RespondTo(PSimConnectRecvFaciltiesList(pData),
        SIMCONNECT_FACILITY_LIST_TYPE_VOR);
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

procedure TFacilitiesDataForm.FormCloseQuery(Sender: TObject;
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

procedure TFacilitiesDataForm.FormCreate(Sender: TObject);
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
  StatusBar.Panels[0].Text := 'Not Connected';
  RxCount := 0;
  StatusBar.Panels[1].Text := Format(StrRx6d, [RxCount]);
end;

procedure TFacilitiesDataForm.RespondTo(Rcv: PSimConnectRecvFaciltiesList;
  ListType: SIMCONNECT_FACILITY_LIST_TYPE);
var
  I                 : integer;

  procedure Dump(pFac: PSIMCONNECT_DATA_FACILITY_AIRPORT); overload;
  begin
    Memo.Lines.Add(Format('  Icao: %s  Latitude: %f  Longitude: %f  Altitude: %f',
      [pFac^.Icao, pFac^.Latitude, pFac^.Longitude, pFac^.Altitude]));
  end;

  procedure Dump(pFac: PSIMCONNECT_DATA_FACILITY_WAYPOINT); overload;
  begin
    Dump(PSIMCONNECT_DATA_FACILITY_AIRPORT(pFac));
    Memo.Lines.Add(Format('  fMagVar: %f', [pFac^.fMagVar]));
  end;

  procedure Dump(pFac: PSIMCONNECT_DATA_FACILITY_NDB); overload;
  begin
    Dump(PSIMCONNECT_DATA_FACILITY_WAYPOINT(pFac));
    Memo.Lines.Add(Format('  fFrequency: %d', [pFac^.fFrequency]));
  end;

  procedure Dump(pFac: PSIMCONNECT_DATA_FACILITY_VOR); overload;
  begin
    Dump(PSIMCONNECT_DATA_FACILITY_NDB(pFac));
    Memo.Lines.Add(Format(
      '  Flags: %x  fLocalizer: %f  GlideLat: %f  GlideLon: %f  GlideAlt: %f  fGlideSlopeAngle: %f',
      [pFac^.Flags, pFac^.fLocalizer, pFac^.GlideLat, pFac^.GlideLon,
      pFac^.GlideAlt, pFac^.fGlideSlopeAngle]));
  end;

begin
  Memo.Lines.Add(Format('RequestID: %d  dwArraySize: %d  dwEntryNumber: %d  dwOutOf: %d',
    [Rcv^.dwRequestID, Rcv^.dwArraySize, Rcv^.dwEntryNumber, Rcv^.dwOutOf]));
  for I := 0 to Rcv^.dwArraySize - 1 do
    case ListType of
      SIMCONNECT_FACILITY_LIST_TYPE_AIRPORT:
        with PSIMCONNECT_RECV_AIRPORT_LIST(Rcv)^ do
          Dump(PSIMCONNECT_DATA_FACILITY_AIRPORT(@rgData[I]));
      SIMCONNECT_FACILITY_LIST_TYPE_WAYPOINT:
        with PSIMCONNECT_RECV_WAYPOINT_LIST(Rcv)^ do
          Dump(PSIMCONNECT_DATA_FACILITY_WAYPOINT(@rgData[I]));
      SIMCONNECT_FACILITY_LIST_TYPE_NDB:
        with PSIMCONNECT_RECV_NDB_LIST(Rcv)^ do
          Dump(PSIMCONNECT_DATA_FACILITY_NDB(@rgData[I]));
      SIMCONNECT_FACILITY_LIST_TYPE_VOR:
        with PSIMCONNECT_RECV_VOR_LIST(Rcv)^ do
          Dump(PSIMCONNECT_DATA_FACILITY_VOR(@rgData[I]));
    end;
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

procedure TFacilitiesDataForm.SimConnectMessage(var Message: TMessage);
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

procedure TFacilitiesDataForm.StartEventExecute(Sender: TObject);
var
  hr                : HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Facilities Data', Handle,
    WM_USER_SIMCONNECT, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(Event0));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(Event1));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(Group0),
      Ord(Event0), TRUE);
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(Group0),
      Ord(Event1), TRUE);
    hr := SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(Group0),
      Ord(SIMCONNECT_GROUP_PRIORITY_HIGHEST));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(Input0),
      'Ctrl+F1', Ord(Event0));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(Input0),
      'Ctrl+F2', Ord(Event1));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(Input0),
      Ord(SIMCONNECT_STATE_ON));
    SimConnect_Text(hSimConnect, SIMCONNECT_TEXT_TYPE_PRINT_RED, 15, 0,
      Length(szTitle), @szTitle[1]);
    SimConnect_Text(hSimConnect, SIMCONNECT_TEXT_TYPE_PRINT_RED, 15, 0,
      Length(szHelp), @szHelp[1]);
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

procedure TFacilitiesDataForm.StartPollExecute(Sender: TObject);
var
  hr                : HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Facilities Data', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(Event0));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(Event1));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(Group0),
      Ord(Event0), TRUE);
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(Group0),
      Ord(Event1), TRUE);
    hr := SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(Group0),
      Ord(SIMCONNECT_GROUP_PRIORITY_HIGHEST));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(Input0),
      'Ctrl+F1', Ord(Event0));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(Input0),
      'Ctrl+F2', Ord(Event1));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(Input0),
      Ord(SIMCONNECT_STATE_ON));
    SimConnect_Text(hSimConnect, SIMCONNECT_TEXT_TYPE_PRINT_RED, 15, 0,
      Length(szTitle), @szTitle[1]);
    SimConnect_Text(hSimConnect, SIMCONNECT_TEXT_TYPE_PRINT_RED, 15, 0,
      Length(szHelp), @szHelp[1]);
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

