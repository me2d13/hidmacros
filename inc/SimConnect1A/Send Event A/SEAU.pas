{ 25/05/2007 10:56:20 (GMT+1:00) > [Akadamia] checked in   }
{ 25/05/2007 10:56:01 (GMT+1:00) > [Akadamia] checked out /}
{ 21/05/2007 17:51:19 (GMT+1:00) > [Akadamia] checked in Updates for SP1A  }
{ 21/05/2007 17:43:02 (GMT+1:00) > [Akadamia] checked out /Updates for SP1A}
{ 21/05/2007 17:14:57 (GMT+1:00) > [Akadamia] checked in Updates for SP1A  }
{ 21/05/2007 17:05:05 (GMT+1:00) > [Akadamia] checked out /Updates for SP1A}
{ 14/02/2007 08:36:39 (GMT+0:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: SEAU
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:   Translation of Send Event A example to Delphi
            Whenever the brakes are hit, sends two custom client events to
				    all other clients, one of the events is maskable.
				    Send Event B and C should receive these events.
 History:
-----------------------------------------------------------------------------}
unit SEAU;

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
  TGroupId = (GroupA);
  TEventID = (EventBrakes, EventMyevent, EventMaskable);

  // The form
  TSendEventAForm = class(TForm)
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
  SendEventAForm: TSendEventAForm;

implementation

uses
  SimConnectSupport;

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
  SendEventAForm.DispatchHandler(pData, cbData, pContext);
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

procedure TSendEventAForm.DispatchHandler(pData: PSimConnectRecv; cbData:
  DWORD;
  pContext: Pointer);
var
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
          EventBrakes:
            begin
              Memo.Lines.Add(Format('Event brakes: %d', [evt^.dwData]));
              // Original Comment:
              // Send the two events to all other client groups - this is achieved by setting the priority of the
              // message to SIMCONNECT_GROUP_PRIORITY_HIGHEST. This is the priority of the first client group that
              // will be sent the message.
              SimConnect_TransmitClientEvent(hSimConnect, 0, Ord(EventMyevent),
                0, SIMCONNECT_GROUP_PRIORITY_HIGHEST, SIMCONNECT_EVENT_FLAG_GROUPID_IS_PRIORITY);
              SimConnect_TransmitClientEvent(hSimConnect, 0, Ord(EventMaskable),
                0, SIMCONNECT_GROUP_PRIORITY_HIGHEST, SIMCONNECT_EVENT_FLAG_GROUPID_IS_PRIORITY);
            end;
          EventMyevent:
            Memo.Lines.Add('Send Event A received My.event');
          EventMaskable:
            Memo.Lines.Add('Send Event A received My.maskable.event');
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

procedure TSendEventAForm.FormCloseQuery(Sender: TObject;
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

procedure TSendEventAForm.FormCreate(Sender: TObject);
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

procedure TSendEventAForm.SimConnectMessage(var Message: TMessage);
begin
  SimConnect_CallDispatch(hSimConnect, MyDispatchProc, nil);
end;

{-----------------------------------------------------------------------------
  Procedure: StartEventExecute
  Opens the connection for Event driven handling. 
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments:
    Sender: TObject
-----------------------------------------------------------------------------}

procedure TSendEventAForm.StartEventExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', Handle,
    WM_USER_SIMCONNECT, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, ord(EventBrakes),
      'brakes');
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GroupA),
      ord(EventBrakes));
   // Define two custom events, both of which this client will not mask
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventMyevent),
      'My.event');
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GroupA),
      Ord(EventMyevent), false);
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventMaskable),
      'My.maskable.event');
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GroupA),
      Ord(EventMaskable), false);
    hr := SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(GroupA),
      SIMCONNECT_GROUP_PRIORITY_HIGHEST);
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

procedure TSendEventAForm.StartPollExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, ord(EventBrakes),
      'brakes');
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GroupA),
      ord(EventBrakes));
    // Define two custom events, both of which this client will not mask
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventMyevent),
      'My.event');
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GroupA),
      Ord(EventMyevent), false);
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventMaskable),
      'My.maskable.event');
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GroupA),
      Ord(EventMaskable), false);
    hr := SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(GroupA),
      SIMCONNECT_GROUP_PRIORITY_HIGHEST);
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

