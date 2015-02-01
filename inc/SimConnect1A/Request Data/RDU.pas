{ 14/02/2007 08:33:03 (GMT+0:00) > [Akadamia] checked in   }
{ 14/02/2007 08:32:43 (GMT+0:00) > [Akadamia] checked out /}
{ 12/02/2007 10:16:33 (GMT+0:00) > [Akadamia] checked in   }
{ 08/02/2007 14:08:35 (GMT+0:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: RDU
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:   Translation of Request Data example to Delphi
            After a flight has loaded, request the lat/lon/alt of the user
            aircraft.
 History:
-----------------------------------------------------------------------------}
unit RDU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ToolWin, ActnMan, ActnCtrls, XPStyleActnCtrls,
  ActnList, ComCtrls, SimConnect, StdActns;
const
  // Define a user message for message driven version of the example
  WM_USER_SIMCONNECT = WM_USER + 2;
type
  PStruct1 = ^TStruct1;
  TStruct1 = record
    title: packed array[0..255] of char;
    kohlsmann: double;
    altitude: double;
    latitude: double;
    longitude: double;
  end;

  // Use enumerated types to create unique IDs as required
  TEventPdr = (EventSimStart);
  TDataDefineId = (Definition1);
  TDataRequestId = (Request1);

  // The form
  TRequestDataForm = class(TForm)
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
  end;

var
  RequestDataForm   : TRequestDataForm;

implementation

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
  RequestDataForm.DispatchHandler(pData, cbData, pContext);
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

procedure TRequestDataForm.DispatchHandler(pData: PSimConnectRecv; cbData:
  DWORD;
  pContext: Pointer);
var
  hr                : HRESULT;
  Evt               : PSimconnectRecvEvent;
  pObjData          : PSimConnectRecvSimObjectDataByType;
  pS                : PStruct1;
  ObjectID          : DWORD;
  OpenData          : PSimConnectRecvOpen;
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
            dwApplicationBuildMajor,dwApplicationBuildMinor]));
          Memo.Lines.Add(Format('%s %1d.%1d.%1d.%1d', ['SimConnect',
            dwSimConnectVersionMajor, dwSimConnectVersionMinor,
            dwSimConnectBuildMajor, dwSimConnectBuildMinor]));
          Memo.Lines.Add('');
        end;
      end;
    SIMCONNECT_RECV_ID_EVENT:
      begin
        evt := PSimconnectRecvEvent(pData);
        case TEventPdr(evt^.uEventID) of
          EventSimStart:
            begin
              // Make the call for data every second, but only when it changes and
              // only that data that has changed
              hr := SimConnect_RequestDataOnSimObjectType(hSimConnect,
                Ord(Request1), Ord(Definition1), 0,
                  SIMCONNECT_SIMOBJECT_TYPE_USER);
            end;
        end;
      end;
    SIMCONNECT_RECV_ID_SIMOBJECT_DATA_BYTYPE:
      begin
        pObjData := PSimConnectRecvSimObjectDataByType(pData);
        case TDataRequestId(pObjData^.dwRequestID) of
          Request1:
            begin
              // Note that this maps the structure so that it overlays the memory
              // starting at the location where pObjData^.dwData is stored
              ObjectID := pObjData^.dwObjectID;
              pS := PStruct1(@pObjData^.dwData);
              if Length(pS^.title) <= SizeOf(pS^.title) then // security check
                Memo.Lines.Add(
                  Format('ObjectID=%d Title="%s"' + #13#10 +
                    'Lat=%f  Lon=%f  Alt=%f  Kohlsman=%.2f',
                  [ObjectID, pS^.title, pS^.latitude, pS^.longitude,
                    pS^.altitude, pS^.kohlsmann]));
            end;
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

procedure TRequestDataForm.FormCloseQuery(Sender: TObject;
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

procedure TRequestDataForm.FormCreate(Sender: TObject);
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

procedure TRequestDataForm.SimConnectMessage(var Message: TMessage);
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

procedure TRequestDataForm.StartEventExecute(Sender: TObject);
var
  hr                : HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Request Data', Handle,
    WM_USER_SIMCONNECT, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    // Set up the data definition, but do not yet do anything with it
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DEFINITION1), 'Title',
      '', SIMCONNECT_DATATYPE_STRING256);
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DEFINITION1),
      'Kohlsman setting hg', 'inHg');
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DEFINITION1),
      'Plane Altitude', 'feet');
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DEFINITION1),
      'Plane Latitude', 'degrees');
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DEFINITION1),
      'Plane Longitude', 'degrees');
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

procedure TRequestDataForm.StartPollExecute(Sender: TObject);
var
  hr                : HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Request Data', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    // Set up the data definition, but do not yet do anything with it
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DEFINITION1), 'Title',
      '', SIMCONNECT_DATATYPE_STRING256);
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DEFINITION1),
      'Kohlsman setting hg', 'inHg');
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DEFINITION1),
      'Plane Altitude', 'feet');
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DEFINITION1),
      'Plane Latitude', 'degrees');
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DEFINITION1),
      'Plane Longitude', 'degrees');
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

