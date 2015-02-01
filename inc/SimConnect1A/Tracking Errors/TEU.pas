{ 25/05/2007 11:18:00 (GMT+1:00) > [Akadamia] checked in   }
{ 25/05/2007 11:06:12 (GMT+1:00) > [Akadamia] checked out /}
{ 14/02/2007 08:41:12 (GMT+0:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: SDU
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:   Translation of Tracking Errors example to Delphi
            Shows how to use GetLastSendID to record the ID of a request, along
        with an identification string, in order to match the IDs of errors
        returned to identify which call caused the error
 History:
-----------------------------------------------------------------------------}
unit TEU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ToolWin, ActnMan, ActnCtrls, XPStyleActnCtrls,
  ActnList, ComCtrls, SimConnect, StdActns;
const
  // Define a user message for message driven version of the example
  WM_USER_SIMCONNECT = WM_USER + 2;
  MaxSendRecords = 10;
type
  // Use enumerated types to create unique IDs as required
  TGroupId = (Group11);
  TEventID = (EventBrakes11, EventBAD);
  TRecordStruct = record
    Call: string;
    SendId: DWORD;
  end;

  // The form
  TTrackingErrorsForm = class(TForm)
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
    SendRecord: array[0..MaxSendRecords - 1] of TRecordStruct;
    RecordCount: integer;
  public
    { Public declarations }
    procedure DispatchHandler(pData: PSimConnectRecv; cbData: DWORD; pContext:
      Pointer);
    procedure AddSendRecord(s: string);
    function FindSendRecord(Id: DWORD): string;
  end;

var
  TrackingErrorsForm: TTrackingErrorsForm;

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
  TrackingErrorsForm.DispatchHandler(pData, cbData, pContext);
end;

{-----------------------------------------------------------------------------
  Procedure: AddSendRecord
  Record the ID along with the identification string in the send_record structure
  Author:    Ken
  Date:      12-Feb-2007
  Arguments: s: string
  Result:    None
-----------------------------------------------------------------------------}

procedure TTrackingErrorsForm.AddSendRecord(s: string);
var
  Id: DWORD;
  Hr: HRESULT;
begin
  if RecordCount < MaxSendRecords then
  begin
    hr := SimConnect_GetLastSentPacketID(hSimConnect, id);
    SendRecord[RecordCount].call := s;
    SendRecord[RecordCount].sendid := id;
    Inc(RecordCount);
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

procedure TTrackingErrorsForm.DispatchHandler(pData: PSimConnectRecv; cbData:
  DWORD;
  pContext: Pointer);
var
  Evt: PSimconnectRecvEvent;
  OpenData: PSimConnectRecvOpen;
  s : string;
  Exception : PSimConnectRecvException;
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
          EventBrakes11:
              Memo.Lines.Add(Format('Event Brakes: %d',[Evt^.dwData]));
        end;
      end;
    SIMCONNECT_RECV_ID_EXCEPTION:
    begin
      Exception := PSimConnectRecvException(pData);
      Memo.Lines.Add(Format('***** EXCEPTION=%d  SendID=%d  Index=%d  cbData=%d',
      [Exception^.dwException, Exception^.dwSendID, Exception^.dwIndex, cbData]));
			// Locate the bad call and print it out
      s := findSendRecord(Exception^.dwSendID);
      Memo.Lines.Add(s);
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
  Procedure: FindSendRecord
  Given the ID of an erroneous packet, find the identification string of the call
  Author:    Ken
  Date:      12-Feb-2007
  Arguments: Id: DWORD
  Result:    string
-----------------------------------------------------------------------------}

function TTrackingErrorsForm.FindSendRecord(Id: DWORD): string;
var
  Found : boolean;
  Count : integer;
begin
    Found  := false;
    Count   := 0;
    while (not Found) and (Count < RecordCount) do
    begin
        if Id = SendRecord[Count].SendId then
        begin
          Result := SendRecord[Count].Call;
          Exit;
        end;
        Inc(Count);
    end;
    Result := 'Send Record not found';
end;

{-----------------------------------------------------------------------------
  Procedure: FormCloseQuery
  Ensure that we can signal "Quit" to the busy wait loop
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments: Sender: TObject var CanClose: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TTrackingErrorsForm.FormCloseQuery(Sender: TObject;
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

procedure TTrackingErrorsForm.FormCreate(Sender: TObject);
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
  RecordCount := 0;
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

procedure TTrackingErrorsForm.SimConnectMessage(var Message: TMessage);
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

procedure TTrackingErrorsForm.StartEventExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', Handle,
    WM_USER_SIMCONNECT, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventBrakes11), 'brakes');
    AddSendRecord('SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventBrakes11), ''brakes'');');
    // To force an error, use the wrong event
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(Group11), Ord(EventBAD));
    AddSendRecord('SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(Group11), Ord(EventBAD));');
    hr := SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(Group11), SIMCONNECT_GROUP_PRIORITY_HIGHEST);
    AddSendRecord('SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(Group11), SIMCONNECT_GROUP_PRIORITY_HIGHEST);');
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

procedure TTrackingErrorsForm.StartPollExecute(Sender: TObject);
var
  hr: HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Set Data', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    StatusBar.Panels[0].Text := 'Connected';
    hr := SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventBrakes11), 'brakes');
    AddSendRecord('SimConnect_MapClientEventToSimEvent(hSimConnect, Ord(EventBrakes11), ''brakes'');');
    // To force an error, use the wrong event
    hr := SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(Group11), Ord(EventBAD));
    AddSendRecord('SimConnect_AddClientEventToNotificationGroup(hSimConnect, Ord(Group11), Ord(EventBAD));');
    hr := SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(Group11), SIMCONNECT_GROUP_PRIORITY_HIGHEST);
    AddSendRecord('SimConnect_SetNotificationGroupPriority(hSimConnect, Ord(Group11), SIMCONNECT_GROUP_PRIORITY_HIGHEST);');
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

