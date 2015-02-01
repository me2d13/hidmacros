{ 25/05/2007 11:16:46 (GMT+1:00) > [Akadamia] checked in   }
{ 14/02/2007 08:32:06 (GMT+0:00) > [Akadamia] checked in   }
{ 14/02/2007 08:31:52 (GMT+0:00) > [Akadamia] checked out /}
{ 12/02/2007 10:16:54 (GMT+0:00) > [Akadamia] checked in   }
{ 08/02/2007 14:08:09 (GMT+0:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: NCU
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:   Translation of No Callback example to Delphi
            Responds to the user aircraft brakes, without a callback function
 History:
-----------------------------------------------------------------------------}
unit NCU;

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
  TGroupId = (Group10);
  TEventID = (EventBrakes10);

  // The form
  TNoCallbackForm = class(TForm)
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
  end;

var
  NoCallbackForm: TNoCallbackForm;

implementation

resourcestring
  StrRx6d = 'Rx: %6d';

{$R *.dfm}

{-----------------------------------------------------------------------------
  Procedure: FormCloseQuery
  Ensure that we can signal "Quit" to the busy wait loop
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments: Sender: TObject var CanClose: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TNoCallbackForm.FormCloseQuery(Sender: TObject;
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

procedure TNoCallbackForm.FormCreate(Sender: TObject);
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
  This uses SimConnect_GetNextDispatch instead.
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments:
    var Message: TMessage
-----------------------------------------------------------------------------}

procedure TNoCallbackForm.SimConnectMessage(var Message: TMessage);
var
  pData: PSimConnectRecv;
  cbData: DWORD;
  hr: HRESULT;
  Evt: PSimconnectRecvEvent;
begin
  hr := SimConnect_GetNextDispatch(hSimConnect, pData, cbData);
  if SUCCEEDED(hr) then
  begin
    case TSimConnectRecvId(pData^.dwID) of
      SIMCONNECT_RECV_ID_EVENT:
        begin
          evt := PSimconnectRecvEvent(pData);
          case TEventId(evt^.uEventID) of
            EventBrakes10:
              Memo.Lines.Add(Format('Event brakes: %d', [evt^.dwData]));
          end;
        end;
      SIMCONNECT_RECV_ID_QUIT: // enter code to handle exiting the application
        quit := True;
    end;
  end;
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

procedure TNoCallbackForm.StartEventExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'No Callback', Handle,
    WM_USER_SIMCONNECT, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventBrakes10),
      'brakes');
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect,
      Ord(Group10), Ord(EventBrakes10));
    hr := SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(Group10),
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

procedure TNoCallbackForm.StartPollExecute(Sender: TObject);
var
  hr: HRESULT;
  pData: PSimConnectRecv;
  cbData: DWORD;
  Evt: PSimconnectRecvEvent;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventBrakes10),
      'brakes');
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect,
      Ord(Group10), Ord(EventBrakes10));
    hr := SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(Group10),
      SIMCONNECT_GROUP_PRIORITY_HIGHEST);
    StartEvent.Enabled := False;
    StartPoll.Enabled := False;
    while not Quit do
    begin
      hr := SimConnect_GetNextDispatch(hSimConnect, &pData, &cbData);
      if SUCCEEDED(hr) then
      begin
        case TSimConnectRecvId(pData^.dwID) of
          SIMCONNECT_RECV_ID_EVENT:
            begin
              evt := PSimconnectRecvEvent(pData);
              case TEventId(evt^.uEventID) of
                EventBrakes10:
                  Memo.Lines.Add(Format('Event brakes: %d', [evt^.dwData]));
              end;
            end;
          SIMCONNECT_RECV_ID_QUIT: // enter code to handle exiting the application
            quit := True;
        end;
      end;
    end;
    hr := SimConnect_Close(hSimConnect);
  end;
end;

end.

