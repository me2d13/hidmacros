{ 25/05/2007 11:16:27 (GMT+1:00) > [Akadamia] checked in   }
{ 25/05/2007 11:15:54 (GMT+1:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: TMU
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:   Translation of Text Menu example to Delphi
 History:
-----------------------------------------------------------------------------}
unit TMU;

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
  TEventID = (Event1, Event2, EventMenu1);

  // The form
  TTextMenuForm = class(TForm)
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
  TextMenuForm      : TTextMenuForm;

implementation

uses SimConnectSupport;

resourcestring
  StrRx6d           = 'Rx: %6d';
const
  Empty1            = '' + #0;
  Menu1             = 'SimConnect Text Menu' + #0 +
    'Choose which item:' + #0 +
    'Item #1' + #0 +
    'Item #2' + #0 +
    'Item #3' + #0 +
    'Item #4' + #0 +
    'Item #5' + #0;

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
  TextMenuForm.DispatchHandler(pData, cbData, pContext);
end;

function MenuText(InResult: SIMCONNECT_TEXT_RESULT): string;
begin
  case InResult of
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_1:
      Result := 'Item #1 Selected';
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_2:
      Result := 'Item #2 Selected';
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_3:
      Result := 'Item #3 Selected';
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_4:
      Result := 'Item #4 Selected';
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_5:
      Result := 'Item #5 Selected';
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_6:
      Result := 'Item #6 Selected';
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_7:
      Result := 'Item #7 Selected';
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_8:
      Result := 'Item #8 Selected';
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_9:
      Result := 'Item #9 Selected';
    SIMCONNECT_TEXT_RESULT_MENU_SELECT_10:
      Result := 'Item #10 Selected';
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
    Result := '<unknown>';
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

procedure TTextMenuForm.DispatchHandler(pData: PSimConnectRecv; cbData:
  DWORD;
  pContext: Pointer);
var
  hr                : HRESULT;
  Evt               : PSimconnectRecvEvent;
  Init              : TSimConnectDataInitPosition;
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
          Event1:                       // Display menu
            SimConnect_Text(hSimConnect, SIMCONNECT_TEXT_TYPE_MENU, 0,
              Ord(EventMenu1), length(Menu1), @Menu1[1]);
          Event2:                       // Stop displaying menu
            SimConnect_Text(hSimConnect, SIMCONNECT_TEXT_TYPE_MENU, 0,
              Ord(EventMenu1), length(Empty1), @Empty1[1]);
          EventMenu1:                   // Respond to the users menu selection
            Memo.Lines.Add(MenuText(SIMCONNECT_TEXT_RESULT(evt^.dwData)));
        else
          Memo.Lines.Add(Format('SIMCONNECT_RECV_EVENT: 0x%08X 0x%08X 0x%X',
            [evt^.uEventID, evt^.dwData, cbData]));
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

procedure TTextMenuForm.FormCloseQuery(Sender: TObject;
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

procedure TTextMenuForm.FormCreate(Sender: TObject);
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

procedure TTextMenuForm.SimConnectMessage(var Message: TMessage);
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

procedure TTextMenuForm.StartEventExecute(Sender: TObject);
var
  hr                : HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Text Menu', Handle,
    WM_USER_SIMCONNECT, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(Event1));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(Event2));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT1), True);
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT2), True);
    hr := SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(GROUP0), SIMCONNECT_GROUP_PRIORITY_HIGHEST);
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT0), 'Ctrl+F1', Ord(EVENT1));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT0), 'Ctrl+F2', Ord(EVENT2));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(Input0),
      Ord(SIMCONNECT_STATE_ON));
    // Sign up for notifications for EVENT6
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

procedure TTextMenuForm.StartPollExecute(Sender: TObject);
var
  hr                : HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Text Menu', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(Event1));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(Event2));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT1), True);
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(EVENT2), True);
    hr := SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(GROUP0), SIMCONNECT_GROUP_PRIORITY_HIGHEST);
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT0), 'Ctrl+F1', Ord(EVENT1));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT0), 'Ctrl+F2', Ord(EVENT2));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(Input0),
      Ord(SIMCONNECT_STATE_ON));
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

