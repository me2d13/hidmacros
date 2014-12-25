unit WHookInt;

interface

uses
  Windows, Messages, SysUtils, uHookCommon;

function SetHook: Boolean; stdcall; export;
function FreeHook: Boolean; stdcall; export;
function MsgFilterFuncKbd(Code: Integer; wParam, lParam: Longint): Longint stdcall; export;
function MsgFilterFuncMou(Code: Integer; wParam, lParam: Longint): Longint stdcall; export;

implementation

uses MemMap;

const
  WH_KEYBOARD_LL = 13;
  WH_MOUSE_LL = 14;

type
 { Structure used by WH_KEYBOARD_LL }
 PKBDLLHookStruct = ^TKBDLLHookStruct;
 {$EXTERNALSYM tagKBDLLHOOKSTRUCT}
 tagKBDLLHOOKSTRUCT = packed record
   vkCode: DWORD;
   scanCode: DWORD;
   flags: DWORD;
   time: DWORD;
   dwExtraInfo: PULONG;
 end;
 TKBDLLHookStruct = tagKBDLLHOOKSTRUCT;
 {$EXTERNALSYM KBDLLHOOKSTRUCT}
 KBDLLHOOKSTRUCT = tagKBDLLHOOKSTRUCT;

 ULONG_PTR = ^DWORD;

  POINT = packed record
    x,y: longint;
  end;

  // Low Level Mouse Hook Info Struct
  // http://msdn.microsoft.com/en-us/ms644970.aspx
  MSLLHOOKSTRUCT = packed record
    pt: POINT;
    mouseData: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;
  PMSLLHOOKSTRUCT = ^MSLLHOOKSTRUCT;

// Actual hook stuff

type
  TPMsg = ^TMsg;

const
  VK_D = $44;
  VK_E = $45;
  VK_F = $46;
  VK_M = $4D;
  VK_R = $52;

  // global variables, only valid in the process which installs the hook.
var
  lMemMap: TMemMap;
  lSharedPtr: PMMFData;
  lPid: DWORD;
  lHookKbd: HHOOK;
  lHookMou: HHOOK;

{
  The SetWindowsHookEx function installs an application-defined
  hook procedure into a hook chain.

  WH_GETMESSAGE Installs a hook procedure that monitors messages
  posted to a message queue.
  For more information, see the GetMsgProc hook procedure.
}

function SetHook: Boolean; stdcall;
begin
  Result := False;
  if lSharedPtr = nil then
    exit;

  lHookKbd := SetWindowsHookEx(WH_KEYBOARD, MsgFilterFuncKbd, HInstance, 0);
  lHookMou := SetWindowsHookEx(WH_MOUSE, MsgFilterFuncMou, HInstance, 0);
  if (lHookKbd = 0) or (lHookMou = 0) then
    FreeHook  // free is something was ok
  else
    Result := True;
end;


{
  The UnhookWindowsHookEx function removes the hook procedure installed
  in a hook chain by the SetWindowsHookEx function.
}

function FreeHook: Boolean; stdcall;
var b1, b2: Boolean;
begin
  Result := False;
  b1 := True;
  b2 := True;
  if (lHookKbd <> 0) then
    b1 := UnHookWindowsHookEx(lHookKbd);
  if (lHookMou <> 0) then
    b2 := UnHookWindowsHookEx(lHookMou);
  Result := b1 and b2;
end;



(*
    GetMsgProc(
    nCode: Integer;  {the hook code}
    wParam: WPARAM;  {message removal flag}
    lParam: LPARAM  {a pointer to a TMsg structure}
    ): LRESULT;  {this function should always return zero}

    { See help on ==> GetMsgProc}
*)

function MsgFilterFuncKbd(Code: Integer; wParam, lParam: Longint): Longint;
var
  Kill: boolean;
  what2do : Integer;
  MessageId: Word;
//  ext_code: Cardinal;
begin
  if (Code < 0) or (Code <> HC_ACTION) or (lSharedPtr = nil) then
  begin
    Result := CallNextHookEx(lHookKbd {ignored in API}, Code, wParam, lParam);
    exit;
  end;
  Result := 0;
  Kill := False;
  // ask HDM form only when we're not inside. If we are in HDM, pass all without
  // any interaction
  //if (lSharedPtr^.HdmPID <> lPID) then - can't ignore, we need to synchronzie list of events
  begin
    //OutputDebugString('DLL: Would ask HIDmacros what to do for KEYBOARD.');
    if (lParam and $80000000 > 0) then
      MessageId := WM_KEYUP
    else
      MessageId := WM_KEYDOWN;
    what2do := SendMessage(lSharedPtr^.MainWinHandle, WM_ASKHDMFORM, MessageId , wParam);
    if (what2do = -1) then
      Kill := True;
  end;
  if Kill then
    Result := 1
  else
    Result := CallNextHookEx(lHookKbd {ignored in API}, Code, wParam, lParam);
end;

function MsgFilterFuncMou(Code: Integer; wParam, lParam: Longint): Longint;
var
  Kill: boolean;
  locData: TMMFData;
  what2do: Integer;
begin
  Result := 0;
  if (Code < 0) or (Code <> HC_ACTION) or (lSharedPtr = nil) then
  begin
    Result := CallNextHookEx(lHookMou {ignored in API}, Code, wParam, lParam);
    exit;
  end;
  Kill := False;
  // ask HDM form only when we're not inside. If we are in HDM, pass all without
  // any interaction
  if  //(lSharedPtr^.HdmPID <> lPID) and 
     (
      (wParam = WM_LBUTTONDOWN) or
      (wParam = WM_LBUTTONUP) or
      (wParam = WM_MBUTTONDOWN) or
      (wParam = WM_MBUTTONUP) or
      (wParam = WM_RBUTTONDOWN) or
      (wParam = WM_RBUTTONUP) or
      (wParam = WM_MOUSEWHEEL) or
      (wParam = WM_NCLBUTTONDOWN) or
      (wParam = WM_NCLBUTTONUP) or
      (wParam = WM_NCMBUTTONDOWN) or
      (wParam = WM_NCMBUTTONUP) or
      (wParam = WM_NCRBUTTONDOWN) or
      (wParam = WM_NCRBUTTONUP)
      )
      then
  begin
    //if GetAncestor(PMSLLHOOKSTRUCT(lParam)^.hwnd, GA_ROOT) = locData.WinHandle then
    // this would be for LL to have Wheel direction, but I wouldn't have
    // target window handle then. Would have to calculate it from mouse pos
    // so rather ignore direction
    if (lSharedPtr^.HdmPID = lPID) then
    begin
      //OutputDebugString('DLL: Just info message to HIDMacros window.');
      SendMessage(lSharedPtr^.MainWinHandle, WM_ASKHDMFORM, wParam , 1);
    end
    else
    begin
      //OutputDebugString('DLL: Would ask HIDmacros what to do for MOUSE.');
      what2do := SendMessage(lSharedPtr^.MainWinHandle, WM_ASKHDMFORM, wParam , 0);
      if (what2do = -1) then
        Kill := True;
    end;
  end;
  if Kill then
    Result := 1
  else
    Result := CallNextHookEx(lHookMou {ignored in API}, Code, wParam, lParam);
end;

procedure DebugLog(Value: String);
var
  tmp: PChar;
  lVal: String;
begin
  lVal := 'DLL:'+ Value;
  GetMem(tmp, Length(lVal) + 1);
  try
    StrPCopy(tmp, lVal);
    OutputDebugString(tmp);
  finally
    FreeMem(tmp);
  end;
end;


initialization
begin
  lPID := GetCurrentProcessId;
  lSharedPtr := nil;
  //DebugLog('Attached to PID ' + IntToStr(lPID));
  try
    lMemMap := TMemMap.Create(MMFName, SizeOf(TMMFData));
    lSharedPtr := lMemMap.Memory;
  except
    on EMemMapException do
    begin
      DebugLog(IntToStr(lPID)+': Can''t create MMF.');
      lMemMap := nil;
    end;
  end;
end;

finalization
  FreeHook;
  if lMemMap <> nil then
    try
      lMemMap.Free;
    except
      on EMemMapException do
        DebugLog(IntToStr(lPID)+': Can''t release MMF.');
    end;
end.
