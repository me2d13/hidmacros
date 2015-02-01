{ 25/05/2007 10:50:23 (GMT+1:00) > [Akadamia] checked in   }
{ 25/05/2007 10:30:11 (GMT+1:00) > [Akadamia] checked out /}
{ 14/02/2007 08:28:48 (GMT+0:00) > [Akadamia] checked in   }
{ 14/02/2007 08:28:29 (GMT+0:00) > [Akadamia] checked out /}
{ 12/02/2007 10:12:26 (GMT+0:00) > [Akadamia] checked in   }
{ 12/02/2007 10:12:19 (GMT+0:00) > [Akadamia] checked out /}
{ 12/02/2007 10:11:32 (GMT+0:00) > [Akadamia] checked in   }
{ 08/02/2007 14:07:15 (GMT+0:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: DBMU
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:   Translation of DialogBoxMode example to Delphi
       If the key combination U+Q is typed, a request is sent to set
       Dialog Mode, and if it is successful, a message box is rendered, and then
       Dialog Mode is turned off.
History:
-----------------------------------------------------------------------------}
unit DBMU;

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
  TEventID = (Event0);
  TInputId = (Input0);
  TDataRequestId = (Request0);

  // The form
  TDialogBoxModeForm = class(TForm)
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
  DialogBoxModeForm: TDialogBoxModeForm;

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
  DialogBoxModeForm.DispatchHandler(pData, cbData, pContext);
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

procedure TDialogBoxModeForm.DispatchHandler(pData: PSimConnectRecv; cbData:
  DWORD;
  pContext: Pointer);
var
  hr: HRESULT;
  Evt: PSimconnectRecvEvent;
  OpenData: PSimConnectRecvOpen;
  pState: PSimConnectRecvSystemState;
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
          Event0:
            begin
              Memo.Lines.Add(Format('EVENT0: %d', [evt^.dwData]));
              // Send a request to turn Dialog Mode on
              hr := SimConnect_SetSystemState(hSimConnect, 'DialogMode', 1, 0,
                nil);
              // Send a request to ask whether Dialog Mode is on
              hr := SimConnect_RequestSystemState(hSimConnect, Ord(REQUEST0),
                'DialogMode');
            end;
        else
          Memo.Lines.Add(Format('SIMCONNECT_RECV_EVENT: 0x%08X 0x%08X 0x%X',
            [evt^.uEventID, evt^.dwData, cbData]));
        end;
      end;
    SIMCONNECT_RECV_ID_SYSTEM_STATE:
      begin
        pState := PSimConnectRecvSystemState(pData);
        case TDataRequestId(pState^.dwRequestID) of
          REQUEST0:
            begin
              // If Dialog Mode is on, show a Message box
              if pState^.dwInteger <> 0 then
              begin
                MessageBox(0, 'Test!', 'Dialog Mode is on', MB_OK);
                // Send a request to turn Dialog Mode off
                hr := SimConnect_SetSystemState(hSimConnect, 'DialogMode', 0, 0,
                  nil);
              end;
            end;
        end;
        Memo.Lines.Add(Format('SIMCONNECT_RECV_SYSTEM_STATE RequestID=%d  dwInteger=%d  fFloat=%f  szString="%s"',
          [pState^.dwRequestID, pState^.dwInteger, pState^.fFloat,
          pState^.szString]));
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

procedure TDialogBoxModeForm.FormCloseQuery(Sender: TObject;
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

procedure TDialogBoxModeForm.FormCreate(Sender: TObject);
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

procedure TDialogBoxModeForm.SimConnectMessage(var Message: TMessage);
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

procedure TDialogBoxModeForm.StartEventExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', Handle,
    WM_USER_SIMCONNECT, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(Event0));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(Event0));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT0), 'U+Q',
      Ord(Event0));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(INPUT0),
      Ord(SIMCONNECT_STATE_ON));
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

procedure TDialogBoxModeForm.StartPollExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(Event0));
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(GROUP0),
      Ord(Event0));
    hr := SimConnect_MapInputEventToClientEvent(hSimConnect, Ord(INPUT0), 'U+Q',
      Ord(Event0));
    hr := SimConnect_SetInputGroupState(hSimConnect, Ord(INPUT0),
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

