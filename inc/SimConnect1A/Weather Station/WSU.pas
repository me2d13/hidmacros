{ 14/02/2007 08:39:25 (GMT+0:00) > [Akadamia] checked in   }
{ 14/02/2007 08:39:10 (GMT+0:00) > [Akadamia] checked out /}
{ 12/02/2007 10:18:00 (GMT+0:00) > [Akadamia] checked in   }
{ 08/02/2007 14:10:47 (GMT+0:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: WSU
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:   Translation of Weather Station example to Delphi
            Requests weather data from the nearest weather station to the
        user aircraft, every 10 seconds
 History:
-----------------------------------------------------------------------------}
unit WSU;

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
  TGroupId = (Group6);
  TInputId = (Input6);
  TEventID = (EventSimStart);
  TDataDefineId = (DefinitionLla);
  TDataRequestId = (RequestLla, RequestWeather);

  PStruct7 = ^TStruct7;
  TStruct7 = record
    Altitude: double;
    Latitude: double;
    Longitude: double;
  end;

  // The form
  TSetDataForm = class(TForm)
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
    RxCount: integer; // Count of Rx messages
    Quit: boolean; // True when signalled to quit
    hSimConnect: THandle; // Handle for the SimConection
  public
    { Public declarations }
    procedure DispatchHandler(pData: PSimConnectRecv; cbData: DWORD; pContext:
      Pointer);
  end;

var
  SetDataForm: TSetDataForm;

implementation

resourcestring
  StrRx6d = 'Rx: %6d';

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
  SetDataForm.DispatchHandler(pData, cbData, pContext);
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

procedure TSetDataForm.DispatchHandler(pData: PSimConnectRecv; cbData:
  DWORD;
  pContext: Pointer);
var
  hr: HRESULT;
  Evt: PSimconnectRecvEvent;
  OpenData: PSimConnectRecvOpen;
  pObjData: PSimConnectRecvSimObjectData;
  ps: PStruct7;
  pszMetar: PChar;
  pWxData: PSimConnectRecvWeatherObservation;
begin
  // Maintain a display of the message count
  Inc(RxCount);
  StatusBar.Panels[1].Text := Format(StrRx6d, [RxCount]);
  // Only keep the last 200 lines in the Memo
  while Memo.Lines.Count > 200 do
    Memo.Lines.Delete(0);
  // Handle the various types of message
  case TSimConnectRecvId(pData^.dwID) of
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
            // Start requesting lat/lon data every 10 seconds
              hr := SimConnect_RequestDataOnSimObject(hSimConnect,
                Ord(RequestLla), Ord(DefinitionLla), SIMCONNECT_OBJECT_ID_USER,
                SIMCONNECT_PERIOD_SECOND, 0, 0, 10, 0);
            end;
        end;
      end;
    SIMCONNECT_RECV_ID_SIMOBJECT_DATA:
      begin
        pObjData := PSimConnectRecvSimObjectData(pData);
        Memo.Lines.Add('Object data received');
        case TDataRequestId(pObjData^.dwRequestID) of
          RequestLla:
            begin
              pS := PStruct7(@pObjData^.dwData);
              Memo.Lines.Add(Format('Lat=%f  Lon=%f  IndAlt=%f',
                [pS^.latitude, pS^.longitude, pS^.altitude]));
                    // Now request the weather data - this will also be requested every 10 seconds
              hr := SimConnect_WeatherRequestObservationAtNearestStation(
                hSimConnect, Ord(RequestWeather), pS^.latitude, pS^.longitude);
             end;
        end;
      end;
    SIMCONNECT_RECV_ID_WEATHER_OBSERVATION:
      begin
        pWxData := PSimConnectRecvWeatherObservation(pData);
        pszMetar := pWxData^.szMetar;
        case TDataRequestId(pWxData^.dwRequestID) of
          RequestWeather:
            Memo.Lines.Add(Format('Weather Observation : %s', [pszMetar]));
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

procedure TSetDataForm.FormCloseQuery(Sender: TObject;
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

procedure TSetDataForm.FormCreate(Sender: TObject);
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

{-----------------------------------------------------------------------------
  Procedure: SimConnectMessage
  This uses CallDispatch, but could probably avoid the callback and use
  SimConnect_GetNextDispatch instead.
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments:
    var Message: TMessage
-----------------------------------------------------------------------------}

procedure TSetDataForm.SimConnectMessage(var Message: TMessage);
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

procedure TSetDataForm.StartEventExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', Handle,
    WM_USER_SIMCONNECT, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    // Set up the data definition, note this matches the order in Struct_7
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionLla), 'Indicated Altitude',   'feet');
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionLla), 'Plane Latitude',       'degrees');
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionLla), 'Plane Longitude',      'degrees');
 		// Request a flight loaded event
    hr := SimConnect_SubscribeToSystemEvent(hSimConnect, Ord(EventSimStart), 'SimStart');
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

procedure TSetDataForm.StartPollExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
     // Set up the data definition, note this matches the order in Struct_7
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionLla), 'Indicated Altitude',   'feet');
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionLla), 'Plane Latitude',       'degrees');
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionLla), 'Plane Longitude',      'degrees');
 		// Request a flight loaded event
    hr := SimConnect_SubscribeToSystemEvent(hSimConnect, Ord(EventSimStart), 'SimStart');
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

