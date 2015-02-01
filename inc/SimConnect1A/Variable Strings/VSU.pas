{ 14/02/2007 08:38:55 (GMT+0:00) > [Akadamia] checked in   }
{ 14/02/2007 08:38:40 (GMT+0:00) > [Akadamia] checked out /}
{ 12/02/2007 10:15:12 (GMT+0:00) > [Akadamia] checked in   }
{ 08/02/2007 14:06:18 (GMT+0:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: SDU
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:   Translation of Variable String example to Delphi
            Shows how to extract three variable length strings from a
    				structure
 History:
-----------------------------------------------------------------------------}
unit VSU;

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
  TEventID = (EventSimStart);
  TDataDefineId = (Definition1);
  TDataRequestId = (Request1);
  // Structure which gets mapped onto the received data
  // in practice "Strings" will be longer than 1 character
  StructVS = packed record
    Strings: packed array[0..0] of char; // variable-length strings
  end;
  PStructVS = ^StructVS;

  // The form
  TVariableStringsForm = class(TForm)
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
  VariableStringsForm: TVariableStringsForm;

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
  VariableStringsForm.DispatchHandler(pData, cbData, pContext);
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

procedure TVariableStringsForm.DispatchHandler(pData: PSimConnectRecv; cbData:
  DWORD;
  pContext: Pointer);
var
  hr: HRESULT;
  pszTitle: PChar;
  pszAirline: Pchar;
  pszType: Pchar;
  cbTitle: DWORD;
  cbAirline: DWORD;
  cbType: DWORD;
  ps: PStructVS;
  pObjData: PSimConnectRecvSimObjectDataByType;
  Evt: PSimconnectRecvEvent;
  OpenData: PSimConnectRecvOpen;
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
              hr := SimConnect_RequestDataOnSimObjectType(hSimConnect,
                Ord(Request1), Ord(Definition1), 0,
                SIMCONNECT_SIMOBJECT_TYPE_USER);
            end;
        end;
      end;
    SIMCONNECT_RECV_ID_SIMOBJECT_DATA_BYTYPE:
      begin
        pObjData := PSimConnectRecvSimObjectDataByType(pData);
        Memo.Lines.Add('SimObject received');
        case TDataRequestId(pObjData^.dwRequestID) of
          Request1:
            begin
              pS := pStructVS(@pObjData^.dwData);
              if Succeeded(SimConnect_RetrieveString(pData, cbData,
                @pS^.strings[0], pszTitle, cbTitle))
                and Succeeded(SimConnect_RetrieveString(pData, cbData,
                @pS^.strings[cbTitle], pszAirline, cbAirline))
                and Succeeded(SimConnect_RetrieveString(pData, cbData,
                @pS^.strings[cbTitle + cbAirline], pszType, cbType))
                then
              begin
                Memo.Lines.Add(Format('Title   = "%s" ', [pszTitle]));
                Memo.Lines.Add(Format('Airline = "%s" ', [pszAirline]));
                Memo.Lines.Add(Format('Type    = "%s" ', [pszType]));
              end
              else
                Memo.Lines.Add(Format('Received %s', [pS^.strings]));
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

procedure TVariableStringsForm.FormCloseQuery(Sender: TObject;
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

procedure TVariableStringsForm.FormCreate(Sender: TObject);
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

procedure TVariableStringsForm.SimConnectMessage(var Message: TMessage);
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

procedure TVariableStringsForm.StartEventExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Variable Strings', Handle,
    WM_USER_SIMCONNECT, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    // Set up a data definition contained a number of variable length strings
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(Definition1),
      'TITLE', '', SIMCONNECT_DATATYPE_STRINGV);
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(Definition1),
      'ATC AIRLINE', '', SIMCONNECT_DATATYPE_STRINGV);
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(Definition1),
      'ATC TYPE', '', SIMCONNECT_DATATYPE_STRINGV);
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

procedure TVariableStringsForm.StartPollExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    // Set up a data definition contained a number of variable length strings
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(Definition1),
      'TITLE', '', SIMCONNECT_DATATYPE_STRINGV);
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(Definition1),
      'ATC AIRLINE', '', SIMCONNECT_DATATYPE_STRINGV);
    hr := SimConnect_AddToDataDefinition(hSimConnect, Ord(Definition1),
      'ATC TYPE', '', SIMCONNECT_DATATYPE_STRINGV);
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

