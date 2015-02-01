{ 25/05/2007 10:49:50 (GMT+1:00) > [Akadamia] checked in   }
{ 25/05/2007 10:48:58 (GMT+1:00) > [Akadamia] checked out /}
{ 14/02/2007 08:34:19 (GMT+0:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: TDU
 Author:    ken.adam
 Date:      11-Jan-2007
 Purpose:   Translation of Tagged Data example to Delphi
            After a flight has loaded, request the vertical speed and pitot
				    heat switch setting of the user aircraft, but only when the data
				    has changed.
              * adds Latitude and Longitude to requested data
              * supports both polled and message driven data transfers.
 History:
-----------------------------------------------------------------------------}
unit TDU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ToolWin, ActnMan, ActnCtrls, XPStyleActnCtrls,
  ActnList, ComCtrls, SimConnect, StdActns;
const
  // maxReturnedItems is 4 in this case, as the sample only requests
  // vertical speed, pitot heat switch data, latitude and longitude
  MaxReturnedItems  = 4;
  // Define a user message for message driven version of the example
  WM_USER_SIMCONNECT = WM_USER + 2;
type
  // A basic structure for a single item of returned data
  TOneDatum = record
    Id: integer;
    Value: single;
  end;

  // A structure that can be used to receive Tagged data
  TDatum = record
    Datum: array[0..MaxReturnedItems - 1] of TOneDatum;
  end;
  PDatum = ^TDatum;

  // Use enumerated types to create unique IDs as required
  TEventPdr = (EventSimStart);
  TDataDefineId = (DefinitionPdr);
  TDataRequestId = (RequestPdr);
  TDataNames = (DATA_VERTICAL_SPEED, DATA_PITOT_HEAT, DATA_LATITUDE,
    DATA_LONGITUDE);

  // The form
  TTaggedDataForm = class(TForm)
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
    procedure SimConnectMessage(var Message: TMessage); message WM_USER_SIMCONNECT;
    procedure StartEventExecute(Sender: TObject);
  private
    { Private declarations }
    RxCount           : integer;  // Count of Rx messages
    Quit              : boolean;  // True when signalled to quit
    hSimConnect       : THandle;  // Handle for the SimConection
  public
    { Public declarations }
    procedure DispatchHandler(pData: PSimConnectRecv; cbData: DWORD; pContext: Pointer);
  end;

var
  TaggedDataForm    : TTaggedDataForm;

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
  TaggedDataForm.DispatchHandler(pData, cbData, pContext);
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

procedure TTaggedDataForm.DispatchHandler(pData: PSimConnectRecv; cbData: DWORD;
  pContext: Pointer);
var
  hr                : HRESULT;
  Evt               : PSimconnectRecvEvent;
  pObjData          : PSimConnectRecvSimObjectData;
  Count             : DWORD;
  pS                : PDatum;
  OpenData          : PSimConnectRecvOpen;
begin
  // Maintain a display of the message count
  Inc(RxCount);
  StatusBar.Panels[1].Text := Format(StrRx6d,[RxCount]);
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
              hr := SimConnect_RequestDataOnSimObject(hSimConnect,
                Ord(RequestPdr), Ord(DefinitionPdr),
                SIMCONNECT_OBJECT_ID_USER, SIMCONNECT_PERIOD_SECOND,
                SIMCONNECT_DATA_REQUEST_FLAG_CHANGED or
                SIMCONNECT_DATA_REQUEST_FLAG_TAGGED);
            end;
        end;
      end;
    SIMCONNECT_RECV_ID_SIMOBJECT_DATA:
      begin
        pObjData := PSimConnectRecvSimObjectData(pData);
        case TDataRequestId(pObjData^.dwRequestID) of
          RequestPdr:
            begin
              Count := 0;
              // Note that this maps the structure so that it overlays the memory
              // starting at the location where pObjData^.dwData is stored
              pS := PDatum(@pObjData^.dwData);
              // There can be a minimum of 1 and a maximum of maxReturnedItems
              // in the StructDatum structure. The actual number returned will
              // be held in the dwDefineCount parameter.
              while Count < pObjData^.dwDefineCount do
              begin
                case TDataNames(pS.Datum[Count].Id) of
                  DATA_VERTICAL_SPEED:
                    Memo.Lines.Add(Format('Vertical speed = %8.5f',
                      [pS^.datum[count].value]));
                  DATA_PITOT_HEAT:
                    Memo.Lines.Add(Format('Pitot heat = %8.5f',
                      [pS^.datum[count].value]));
                  DATA_LATITUDE:
                    Memo.Lines.Add(Format('Latitude = %8.5f',
                      [pS^.datum[count].value]));
                  DATA_LONGITUDE:
                    Memo.Lines.Add(Format('Longitude = %8.5f',
                      [pS^.datum[count].value]));
                else
                  Memo.Lines.Add(Format('Unknown datum ID: %d',
                    [pS^.datum[count].id]));
                end;
                Inc(Count);
              end;
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

procedure TTaggedDataForm.FormCloseQuery(Sender: TObject;
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

procedure TTaggedDataForm.FormCreate(Sender: TObject);
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
  StatusBar.Panels[1].Text := Format(StrRx6d,[RxCount]);
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

procedure TTaggedDataForm.SimConnectMessage(var Message: TMessage);
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

procedure TTaggedDataForm.StartEventExecute(Sender: TObject);
var
  hr                : HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Tagged Data', Handle, WM_USER_SIMCONNECT, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    // Set up the data definition, ensuring that all the elements are in Float32 units, to
    // match the TDatum structure
    // The number of entries in the DEFINITION_PDR definition should be equal to
    // the maxReturnedItems define
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionPdr),
      'Vertical Speed', 'Feet per second',
      SIMCONNECT_DATATYPE_FLOAT32, 0, Ord(DATA_VERTICAL_SPEED));
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionPdr),
      'Pitot Heat', 'Bool',
      SIMCONNECT_DATATYPE_FLOAT32, 0, Ord(DATA_PITOT_HEAT));
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionPdr),
      'Plane Latitude', 'degrees', SIMCONNECT_DATATYPE_FLOAT32, 0,
      Ord(DATA_LATITUDE));
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionPdr),
      'Plane Longitude', 'degrees', SIMCONNECT_DATATYPE_FLOAT32, 0,
      Ord(DATA_LONGITUDE));
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

procedure TTaggedDataForm.StartPollExecute(Sender: TObject);
var
  hr                : HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Tagged Data', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    // Set up the data definition, ensuring that all the elements are in Float32 units, to
    // match the TDatum structure
    // The number of entries in the DEFINITION_PDR definition should be equal to
    // the maxReturnedItems define
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionPdr),
      'Vertical Speed', 'Feet per second',
      SIMCONNECT_DATATYPE_FLOAT32, 0, Ord(DATA_VERTICAL_SPEED));
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionPdr),
      'Pitot Heat', 'Bool',
      SIMCONNECT_DATATYPE_FLOAT32, 0, Ord(DATA_PITOT_HEAT));
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionPdr),
      'Plane Latitude', 'degrees', SIMCONNECT_DATATYPE_FLOAT32, 0,
      Ord(DATA_LATITUDE));
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(DefinitionPdr),
      'Plane Longitude', 'degrees', SIMCONNECT_DATATYPE_FLOAT32, 0,
      Ord(DATA_LONGITUDE));
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

