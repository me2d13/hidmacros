{ 25/05/2007 11:10:25 (GMT+1:00) > [Akadamia] checked in   }
{ 25/05/2007 11:08:25 (GMT+1:00) > [Akadamia] checked out /}
{ 14/02/2007 08:43:02 (GMT+0:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: MAU
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:   Translation of Set Data example to Delphi
 History:
-----------------------------------------------------------------------------}
unit MAU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ToolWin, ActnMan, ActnCtrls, XPStyleActnCtrls,
  ActnList, ComCtrls, SimConnect, StdActns;
const
  // Define a user message for message driven version of the example
  WM_USER_SIMCONNECT = WM_USER + 2;
  //These must copy the GUIDS from the mission file
  GuidCustomAction1: TGuid = '{CDA37D47-3645-4149-8CF6-F11553829B55}';
  GuidCustomAction2: TGuid = '{C342D8DE-1FC6-4027-9E18-EE9581BEC7E4}';
  GuidMissionAction1: TGuid = '{E47162A7-3FED-46B1-B16C-9B331B91A825}';
  GuidMissionAction2: TGuid = '{3D2D8194-A5F8-43D5-8CB8-89081BD2C0CC}';
type

  // Use enumerated types to create unique IDs as required
  TEventID = (EventMissionAction, EventMissionCompleted);

  // The form
  TMissionActionForm = class(TForm)
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
  MissionActionForm: TMissionActionForm;

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
  MissionActionForm.DispatchHandler(pData, cbData, pContext);
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

procedure TMissionActionForm.DispatchHandler(pData: PSimConnectRecv; cbData:
  DWORD;
  pContext: Pointer);
var
  hr: HRESULT;
  Evt: PSimconnectRecvEvent;
  OpenData: PSimConnectRecvOpen;
  pCustomAction: PSimConnectRecvCustomAction;
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
          EventMissionAction:
            Memo.Lines.Add(Format('Mission Action : %d', [Evt^.dwData]));
          EventMissionCompleted:
            Memo.Lines.Add(Format('Mission Completed : %d', [Evt^.dwData]));
        end;
      end;
    SIMCONNECT_RECV_ID_CUSTOM_ACTION:
      begin
        pCustomAction := PSimConnectRecvCustomAction(pData);
        if GUIDToString(pCustomAction^.guidInstanceId) = GUIDToString(GuidCustomAction1) then
        begin
          Memo.Lines.Add(Format('Custom Action 1, payload: %s',
            [pCustomAction^.szPayLoad]));
          // Custom actions can include calls to actions within the mission xml file, though
          // if this is done we cannot know if the actions have been completed within this
          // section of code (the actions may initiate triggers and it may be some time
          // before the sequence is ended).
          hr := SimConnect_ExecuteMissionAction(hSimConnect, GuidMissionAction1);
          hr := SimConnect_ExecuteMissionAction(hSimConnect, GuidMissionAction2);
        end
        else
          if GUIDToString(pCustomAction^.guidInstanceId) = GUIDToString(GuidCustomAction2) then
          begin
            Memo.Lines.Add(Format('Custom Action 2, payload: %s',
              [pCustomAction^.szPayLoad]));
            // This action simply notifies the Mission system that the first action
            // is complete
            hr := SimConnect_CompleteCustomMissionAction(hSimConnect,
              GuidCustomAction1);
          end else
            Memo.Lines.Add(Format('Unknown custom action: %s',
              [GUIDToString(pCustomAction^.guidInstanceId)]));
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

procedure TMissionActionForm.FormCloseQuery(Sender: TObject;
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

procedure TMissionActionForm.FormCreate(Sender: TObject);
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

procedure TMissionActionForm.SimConnectMessage(var Message: TMessage);
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

procedure TMissionActionForm.StartEventExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', Handle,
    WM_USER_SIMCONNECT, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
  // Subscribe to the mission completed event
    hr := SimConnect_SubscribeToSystemEvent(hSimConnect,
      Ord(EventMissionCompleted), 'MissionCompleted');
  // Subscribe to a notification when a custom action executes
    hr := SimConnect_SubscribeToSystemEvent(hSimConnect, Ord(EventMissionAction),
      'CustomMissionActionExecuted');
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

procedure TMissionActionForm.StartPollExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Mission Action', 0, 0, 0, 0)))
    then
  begin
    StatusBar.Panels[0].Text := 'Connected';
  // Subscribe to the mission completed event
    hr := SimConnect_SubscribeToSystemEvent(hSimConnect,
      Ord(EventMissionCompleted), 'MissionCompleted');
  // Subscribe to a notification when a custom action executes
    hr := SimConnect_SubscribeToSystemEvent(hSimConnect, Ord(EventMissionAction),
      'CustomMissionActionExecuted');
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

