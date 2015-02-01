{ 25/05/2007 10:35:31 (GMT+1:00) > [Akadamia] checked in   }
{ 25/05/2007 10:34:36 (GMT+1:00) > [Akadamia] checked out /}
{ 14/02/2007 08:29:56 (GMT+0:00) > [Akadamia] checked in   }
{ 14/02/2007 08:29:41 (GMT+0:00) > [Akadamia] checked out /}
{ 12/02/2007 10:17:14 (GMT+0:00) > [Akadamia] checked in   }
{ 08/02/2007 14:07:44 (GMT+0:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: JIU
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:   Translation of Joystick Input example to Delphi
            Use the "z" key to step through the events sent by the Joystick
    				including X,Y,Z axes, Slider and Hat switch
 History:
-----------------------------------------------------------------------------}
unit JIU;

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
  TInputId = (INPUT_Z,
    INPUT_SLIDER,
    INPUT_XAXIS,
    INPUT_YAXIS,
    INPUT_RZAXIS,
    INPUT_HAT);
  TEventID = (EVENT_Z,
    EVENT_SLIDER,
    EVENT_XAXIS,
    EVENT_YAXIS,
    EVENT_RZAXIS,
    EVENT_HAT);

  // The form
  TJoystickInputForm = class(TForm)
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
    Current: integer;
  public
    { Public declarations }
    procedure DispatchHandler(pData: PSimConnectRecv; cbData: DWORD; pContext:
      Pointer);
  end;

var
  JoystickInputForm: TJoystickInputForm;

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
  JoystickInputForm.DispatchHandler(pData, cbData, pContext);
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

procedure TJoystickInputForm.DispatchHandler(pData: PSimConnectRecv; cbData:
  DWORD;
  pContext: Pointer);
var
  hr: HRESULT;
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
          EVENT_SLIDER:
            Memo.Lines.Add(Format('Slider value:%d', [evt^.dwData]));
          EVENT_XAXIS:
            Memo.Lines.Add(Format('X Axis value:%d', [evt^.dwData]));
          EVENT_YAXIS:
            Memo.Lines.Add(Format('Y Axis value:%d', [evt^.dwData]));
          EVENT_RZAXIS:
            Memo.Lines.Add(Format('Rotate Z axis value:%d', [evt^.dwData]));
          EVENT_HAT:
            Memo.Lines.Add(Format('Hat value:%d', [evt^.dwData]));
          EVENT_Z:
            begin
              Inc(Current);
              if current = 6 then
                current := 1;
              case current of
                1:
                  begin
                    Memo.Lines.Add('SLIDER is active');
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_SLIDER), Ord(SIMCONNECT_STATE_ON));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_XAXIS), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_YAXIS), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_RZAXIS), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_HAT),
                      Ord(SIMCONNECT_STATE_OFF));
                  end;

                2:
                  begin
                    Memo.Lines.Add('X AXIS is active');
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_SLIDER), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_XAXIS), Ord(SIMCONNECT_STATE_ON));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_YAXIS), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_RZAXIS), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_HAT),
                      Ord(SIMCONNECT_STATE_OFF));
                    ;
                  end;
                3:
                  begin
                    Memo.Lines.Add('Y AXIS is active');
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_SLIDER), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_XAXIS), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_YAXIS), Ord(SIMCONNECT_STATE_ON));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_RZAXIS), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_HAT),
                      Ord(SIMCONNECT_STATE_OFF));
                  end;
                4:
                  begin
                    Memo.Lines.Add('Z ROTATION is active');
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_SLIDER), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_XAXIS), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_YAXIS), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_RZAXIS), Ord(SIMCONNECT_STATE_ON));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_HAT),
                      Ord(SIMCONNECT_STATE_OFF));
                  end;
                5:
                  begin
                    Memo.Lines.Add('HAT is active');
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_SLIDER), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_XAXIS), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_YAXIS), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_RZAXIS), Ord(SIMCONNECT_STATE_OFF));
                    hr := SimConnect_SetInputGroupState(hSimConnect,
                      Ord(INPUT_HAT),
                      Ord(SIMCONNECT_STATE_ON));
                  end;
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

procedure TJoystickInputForm.FormCloseQuery(Sender: TObject;
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

procedure TJoystickInputForm.FormCreate(Sender: TObject);
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
  Current := 0;
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

procedure TJoystickInputForm.SimConnectMessage(var Message: TMessage);
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

procedure TJoystickInputForm.StartEventExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', Handle,
    WM_USER_SIMCONNECT, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
  // Set up some private events
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EVENT_Z));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EVENT_SLIDER));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EVENT_XAXIS));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EVENT_YAXIS));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EVENT_RZAXIS));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EVENT_HAT));

  // Add all the private events to a notifcation group
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT_Z));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT_SLIDER));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT_XAXIS));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT_YAXIS));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT_RZAXIS));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT_HAT));

  // Set a high priority for the group
    hr := SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(GROUP0),
      SIMCONNECT_GROUP_PRIORITY_HIGHEST);

  // Map input events to the private client events
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT_Z), 'z',
      Ord(EVENT_Z));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT_SLIDER),
      'joystick:0:slider', Ord(EVENT_SLIDER));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT_XAXIS),
      'joystick:0:XAxis', Ord(EVENT_XAXIS));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT_YAXIS),
      'joystick:0:YAxis', Ord(EVENT_YAXIS));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT_RZAXIS),
      'joystick:0:RzAxis', Ord(EVENT_RZAXIS));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT_HAT),
      'joystick:0:POV', Ord(EVENT_HAT));

  // Turn on the Z key
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(INPUT_Z),
      Ord(SIMCONNECT_STATE_ON));
    hr := SimConnect_SetInputGroupPriority(hSimConnect, Ord(INPUT_Z),
      SIMCONNECT_GROUP_PRIORITY_HIGHEST);

  // Turn all the joystick events off
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(INPUT_SLIDER),
      Ord(SIMCONNECT_STATE_OFF));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(INPUT_XAXIS),
      Ord(SIMCONNECT_STATE_OFF));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(INPUT_YAXIS),
      Ord(SIMCONNECT_STATE_OFF));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(INPUT_RZAXIS),
      Ord(SIMCONNECT_STATE_OFF));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(INPUT_HAT),
      Ord(SIMCONNECT_STATE_OFF));
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

procedure TJoystickInputForm.StartPollExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
   // Set up some private events
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EVENT_Z));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EVENT_SLIDER));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EVENT_XAXIS));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EVENT_YAXIS));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EVENT_RZAXIS));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EVENT_HAT));

  // Add all the private events to a notifcation group
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT_Z));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT_SLIDER));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT_XAXIS));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT_YAXIS));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT_RZAXIS));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT_HAT));

  // Set a high priority for the group
    hr := SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(GROUP0),
      SIMCONNECT_GROUP_PRIORITY_HIGHEST);

  // Map input events to the private client events
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT_Z), 'z',
      Ord(EVENT_Z));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT_SLIDER),
      'joystick:0:slider', Ord(EVENT_SLIDER));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT_XAXIS),
      'joystick:0:XAxis', Ord(EVENT_XAXIS));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT_YAXIS),
      'joystick:0:YAxis', Ord(EVENT_YAXIS));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT_RZAXIS),
      'joystick:0:RzAxis', Ord(EVENT_RZAXIS));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT_HAT),
      'joystick:0:POV', Ord(EVENT_HAT));

  // Turn on the Z key
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(INPUT_Z),
      Ord(SIMCONNECT_STATE_ON));
    hr := SimConnect_SetInputGroupPriority(hSimConnect, Ord(INPUT_Z),
      SIMCONNECT_GROUP_PRIORITY_HIGHEST);

  // Turn all the joystick events off
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(INPUT_SLIDER),
      Ord(SIMCONNECT_STATE_OFF));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(INPUT_XAXIS),
      Ord(SIMCONNECT_STATE_OFF));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(INPUT_YAXIS),
      Ord(SIMCONNECT_STATE_OFF));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(INPUT_RZAXIS),
      Ord(SIMCONNECT_STATE_OFF));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(INPUT_HAT),
      Ord(SIMCONNECT_STATE_OFF));
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

