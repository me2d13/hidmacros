{ 14/02/2007 08:52:42 (GMT+0:00) > [Akadamia] checked in   }
{ 12/02/2007 10:17:05 (GMT+0:00) > [Akadamia] checked in   }
{ 08/02/2007 14:07:56 (GMT+0:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: MIU
 Author:    ken.adam
 Date:      05-Feb-2007
 Purpose:   Translation of Menu Items example to Delphi
            Add one menu item, after it has been selected four times
			      replace it with another menu item
 History:
-----------------------------------------------------------------------------}
unit MIU;

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
  TGroupId = (GroupMenu);
  TEventID = (EventMenuOne, EventMenuTwo);

  // The form
  TMenuItemsForm = class(TForm)
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
    MenuUseCount: integer;
  public
    { Public declarations }
    procedure DispatchHandler(pData: PSimConnectRecv; cbData: DWORD; pContext:
      Pointer);
  end;

var
  MenuItemsForm: TMenuItemsForm;

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
  MenuItemsForm.DispatchHandler(pData, cbData, pContext);
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

procedure TMenuItemsForm.DispatchHandler(pData: PSimConnectRecv; cbData:
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
          EventMenuOne:
            begin
              Memo.Lines.Add(Format('Menu item one selected %d',
                [evt^.dwData]));
              Inc(MenuUseCount);
              // Selected four times, so replace item one with item two
              if MenuUseCount = 4 then
              begin
                hr := SimConnect_MenuDeleteItem(hSimConnect, Ord(EventMenuOne));
                hr := SimConnect_RemoveClientEvent(hSimConnect, Ord(GroupMenu),
                  Ord(EventMenuOne));

                hr := SimConnect_MenuAddItem(hSimConnect, 'Menu Item Two',
                  Ord(EventMenuTwo), 54321);
                hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect,
                  Ord(GroupMenu), Ord(EventMenuTwo));
              end;
            end;
          EventMenuTwo:
            begin
              Inc(MenuUseCount);
              Memo.Lines.Add(Format('Menu item two selected %d',
                [evt^.dwData]));
              if menuUseCount = 6 then
                quit := True;
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

procedure TMenuItemsForm.FormCloseQuery(Sender: TObject;
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

procedure TMenuItemsForm.FormCreate(Sender: TObject);
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
  MenuUseCount := 0;
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

procedure TMenuItemsForm.SimConnectMessage(var Message: TMessage);
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

procedure TMenuItemsForm.StartEventExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', Handle,
    WM_USER_SIMCONNECT, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    // Create some private events
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventMenuOne));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventMenuTwo));
    // Add one menu item
    hr := SimConnect_MenuAddItem(hSimConnect, 'Menu Item One',
      Ord(EventMenuOne), 12345);
    // Sign up for the notifications
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect,
      Ord(GroupMenu), Ord(EventMenuOne));
    hr := SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(GroupMenu),
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

procedure TMenuItemsForm.StartPollExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    // Create some private events
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventMenuOne));
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventMenuTwo));
    // Add one menu item
    hr := SimConnect_MenuAddItem(hSimConnect, 'Menu Item One',
      Ord(EventMenuOne), 12345);
    // Sign up for the notifications
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect,
      Ord(GroupMenu), Ord(EventMenuOne));
    hr := SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(GroupMenu),
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

