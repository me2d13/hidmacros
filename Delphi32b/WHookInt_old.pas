unit WHookInt;

interface

uses
  Windows, Messages, SysUtils;

function SetHook(WinHandle: HWND; MsgToSend: Integer): Boolean; stdcall; export;
function FreeHook: Boolean; stdcall; export;
function MsgFilterFuncKbd(Code: Integer; wParam, lParam: Longint): Longint stdcall; export;
function MsgFilterFuncMou(Code: Integer; wParam, lParam: Longint): Longint stdcall; export;
function MsgFilterFuncMouLL(Code: Integer; wParam, lParam: Longint): Longint stdcall; export;
function SetDialogHandle(Value: HWND): Boolean; stdcall; export;
function SetMapWindowHandle(Value: HWND): Boolean; stdcall; export;

implementation

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

// Memory map file stuff

{
  The CreateFileMapping function creates unnamed file-mapping object
  for the specified file.
}

function CreateMMF(Name: string; Size: Integer): THandle;
begin
  Result := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, Size, PChar(Name));
  if Result <> 0 then
  begin
    if GetLastError = ERROR_ALREADY_EXISTS then
    begin
      CloseHandle(Result);
      Result := 0;
    end;
  end;
end;

{ The OpenFileMapping function opens a named file-mapping object. }

function OpenMMF(Name: string): THandle;
begin
  Result := OpenFileMapping(FILE_MAP_ALL_ACCESS, False, PChar(Name));
  // The return value is an open handle to the specified file-mapping object.
end;

{
 The MapViewOfFile function maps a view of a file into
 the address space of the calling process.
}

function MapMMF(MMFHandle: THandle): Pointer;
begin
  Result := MapViewOfFile(MMFHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
end;

{
  The UnmapViewOfFile function unmaps a mapped view of a file
  from the calling process's address space.
}

function UnMapMMF(P: Pointer): Boolean;
begin
  Result := UnmapViewOfFile(P);
end;

function CloseMMF(MMFHandle: THandle): Boolean;
begin
  Result := CloseHandle(MMFHandle);
end;


// Actual hook stuff

type
  TPMsg = ^TMsg;

const
  VK_D = $44;
  VK_E = $45;
  VK_F = $46;
  VK_M = $4D;
  VK_R = $52;

  MMFName = 'HIDMacrosSharedMem';

type
  PMMFData = ^TMMFData;
  TMMFData = record
    WinHandle: HWND;
    MsgToSend: Integer;
    DialogHandle: HWND;
    MapWindowHandle: HWND;
  end;

  // global variables, only valid in the process which installs the hook.
var
  MMFHandle: THandle;
  MMFData: PMMFData;
  lHookKbd: HHOOK;
  lHookMou: HHOOK;

function UnMapAndCloseMMF: Boolean;
begin
  Result := False;
  if UnMapMMF(MMFData) then
  begin
    MMFData := nil;
    if CloseMMF(MMFHandle) then
    begin
      MMFHandle := 0;
      Result := True;
    end;
  end;
end;

{
  The SetWindowsHookEx function installs an application-defined
  hook procedure into a hook chain.

  WH_GETMESSAGE Installs a hook procedure that monitors messages
  posted to a message queue.
  For more information, see the GetMsgProc hook procedure.
}

function SetHook(WinHandle: HWND; MsgToSend: Integer): Boolean; stdcall;
begin
  Result := False;
  if (MMFData = nil) and (MMFHandle = 0) then
  begin
    MMFHandle := CreateMMF(MMFName, SizeOf(TMMFData));
    if MMFHandle <> 0 then
    begin
      MMFData := MapMMF(MMFHandle);
      if MMFData <> nil then
      begin
        MMFData.WinHandle := WinHandle;
        MMFData.MsgToSend := MsgToSend;
        MMFData.DialogHandle := 0;
        MMFData.MapWindowHandle := 0;
        lHookKbd := SetWindowsHookEx(WH_KEYBOARD, MsgFilterFuncKbd, HInstance, 0);
        lHookMou := SetWindowsHookEx(WH_MOUSE, MsgFilterFuncMou, HInstance, 0);

        // low level hooking, not used now
        //lHookKbd := SetWindowsHookEx(WH_KEYBOARD_LL, MsgFilterFuncKbdLL, HInstance, 0);
        //lHookMou := SetWindowsHookEx(WH_MOUSE_LL, MsgFilterFuncMouLL, HInstance, 0);

        if (lHookKbd = 0) or (lHookMou = 0) then
        begin
          FreeHook;  // free is something was ok
        end
        else
          Result := True;
      end
      else
      begin
        CloseMMF(MMFHandle);
        MMFHandle := 0;
      end;
    end;
  end;
end;


{
  The UnhookWindowsHookEx function removes the hook procedure installed
  in a hook chain by the SetWindowsHookEx function.
}

function FreeHook: Boolean; stdcall;
var b1, b2, b3: Boolean;
begin
  Result := False;
  b1 := True;
  b2 := True;
  if (lHookKbd <> 0) then
    b1 := UnHookWindowsHookEx(lHookKbd);
  if (lHookMou <> 0) then
    b2 := UnHookWindowsHookEx(lHookMou);
  if (MMFData <> nil) and (MMFHandle <> 0) then
  begin
    b3 := UnMapAndCloseMMF;
    Result := b1 and b2 and b3;
  end;
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
  MMFHandle: THandle;
  MMFData: PMMFData;
  Kill: boolean;
  locData: TMMFData;
  dataFetched : boolean;
  what2do : Integer;
  MessageId: Word;
//  ext_code: Cardinal;
begin
  if (Code < 0) or (Code <> HC_ACTION) then
  begin
    Result := CallNextHookEx(lHookKbd {ignored in API}, Code, wParam, lParam);
    exit;
  end;
  Result := 0;
  dataFetched := False;
  MMFHandle := OpenMMF(MMFName);
  if MMFHandle <> 0 then
  begin
    MMFData := MapMMF(MMFHandle);
    if MMFData <> nil then
    begin
      locData := MMFData^;
      dataFetched := True;
      UnMapMMF(MMFData);
    end;
    CloseMMF(MMFHandle);
  end;
  if (dataFetched) then
  begin
    Kill := False;
    if (locData.WinHandle <> 0) and (locData.MsgToSend > 0) then
    begin
      //OutputDebugString('DLL: Would ask HIDmacros what to do for KEYBOARD.');
      if (lParam and $80000000 > 0) then
        MessageId := WM_KEYUP
      else
        MessageId := WM_KEYDOWN;
      what2do := SendMessage(locData.WinHandle, locData.MsgToSend, MessageId , wParam);
      if (what2do = -1) then
        Kill := True;
    end;
    if Kill then
      Result := 1
    else
      Result := CallNextHookEx(lHookKbd {ignored in API}, Code, wParam, lParam);
  end;
end;

function MsgFilterFuncKbdLL(Code: Integer; wParam, lParam: Longint): Longint;
var
  MMFHandle: THandle;
  MMFData: PMMFData;
  Kill: boolean;
  locData: TMMFData;
  dataFetched : boolean;
  what2do : Integer;
//  ext_code: Cardinal;
begin
  Result := 0;
  dataFetched := False;
  MMFHandle := OpenMMF(MMFName);
  if MMFHandle <> 0 then
  begin
    MMFData := MapMMF(MMFHandle);
    if MMFData <> nil then
    begin
      locData := MMFData^;
      dataFetched := True;
      UnMapMMF(MMFData);
    end;
    CloseMMF(MMFHandle);
  end;
  if (dataFetched) then
  begin
    if (Code < 0) or (Code <> HC_ACTION) then
      {
        The CallNextHookEx function passes the hook information to the
        next hook procedure in the current hook chain.
      }
      Result := CallNextHookEx(lHookKbd {ignored in API}, Code, wParam, lParam)
    else
    begin
      Kill := False;
      if (locData.WinHandle <> 0) and (locData.MsgToSend > 0) then
      begin
        //OutputDebugString('DLL: Would ask HIDmacros what to do for KEYBOARD.');
        what2do := SendMessage(locData.WinHandle, locData.MsgToSend, wParam , PKBDLLHookStruct(lParam)^.vkCode);
        if (what2do = -1) then
          Kill := True;
      end;
      if Kill then
        Result := 1
      else
        Result := CallNextHookEx(lHookKbd {ignored in API}, Code, wParam, lParam);
    end;
  end;
end;

function MsgFilterFuncMou(Code: Integer; wParam, lParam: Longint): Longint;
var
  MMFHandle: THandle;
  MMFData: PMMFData;
  Kill: boolean;
  locData: TMMFData;
  dataFetched : boolean;
  what2do: Integer;

//  ext_code: Cardinal;
begin
  Result := 0;
  dataFetched := False;
  MMFHandle := OpenMMF(MMFName);
  if MMFHandle <> 0 then
  begin
    MMFData := MapMMF(MMFHandle);
    if MMFData <> nil then
    begin
      locData := MMFData^;
      dataFetched := True;
      UnMapMMF(MMFData);
    end;
    CloseMMF(MMFHandle);
  end;
  if (dataFetched) then
  begin
    if (Code < 0) or (Code <> HC_ACTION) then
      Result := CallNextHookEx(lHookMou {ignored in API}, Code, wParam, lParam)
    else
    begin
      Kill := False;
      if (locData.WinHandle <> 0) and (locData.MsgToSend > 0)
          and (
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
        if (GetAncestor(PMouseHookStruct(lParam)^.hwnd, GA_ROOT) = locData.WinHandle) or
           ((locData.DialogHandle > 0) and (GetAncestor(PMouseHookStruct(lParam)^.hwnd, GA_ROOT) = locData.DialogHandle)) or
           ((locData.MapWindowHandle > 0) and (GetAncestor(PMouseHookStruct(lParam)^.hwnd, GA_ROOT) = locData.MapWindowHandle))
         then
        begin
          //OutputDebugString('DLL: Just info message to HIDMacros window.');
          SendMessage(locData.WinHandle, locData.MsgToSend, wParam , 1);
        end
        else
        begin
          //OutputDebugString('DLL: Would ask HIDmacros what to do for MOUSE.');
          what2do := SendMessage(locData.WinHandle, locData.MsgToSend, wParam , 0);
          if (what2do = -1) then
            Kill := True;
        end;
      end;
      if Kill then
        Result := 1
      else
        Result := CallNextHookEx(lHookMou {ignored in API}, Code, wParam, lParam);
    end;
  end;
end;

function MsgFilterFuncMouLL(Code: Integer; wParam, lParam: Longint): Longint;
var
  MMFHandle: THandle;
  MMFData: PMMFData;
  Kill: boolean;
  locData: TMMFData;
  dataFetched : boolean;
  what2do: Integer;

//  ext_code: Cardinal;
begin
  Result := 0;
  dataFetched := False;
  MMFHandle := OpenMMF(MMFName);
  if MMFHandle <> 0 then
  begin
    MMFData := MapMMF(MMFHandle);
    if MMFData <> nil then
    begin
      locData := MMFData^;
      dataFetched := True;
      UnMapMMF(MMFData);
    end;
    CloseMMF(MMFHandle);
  end;
  if (dataFetched) then
  begin
    if (Code < 0) or (Code <> HC_ACTION) then
      Result := CallNextHookEx(lHookMou {ignored in API}, Code, wParam, lParam)
    else
    begin
      Kill := False;
      if (locData.WinHandle <> 0) and (locData.MsgToSend > 0)
          and (
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
        //if GetAncestor(PMouseHookStruct(lParam)^.hwnd, GA_ROOT) = locData.WinHandle then
        //  OutputDebugString('DLL: No user message when message goes to HIDMacros window.')
        //else
        begin
          //OutputDebugString('DLL: Would ask HIDmacros what to do for MOUSE.');
          what2do := SendMessage(locData.WinHandle, locData.MsgToSend, wParam , 0);
          if (what2do = -1) then
            Kill := True;
        end;
      end;
      if Kill then
        Result := 1
      else
        Result := CallNextHookEx(lHookMou {ignored in API}, Code, wParam, lParam);
    end;
  end;
end;


function SetDialogHandle(Value: HWND): Boolean;
var
  MMFHandle: THandle;
  MMFData: PMMFData;
begin
  Result := False;
  MMFHandle := OpenMMF(MMFName);
  if MMFHandle <> 0 then
  begin
    MMFData := MapMMF(MMFHandle);
    if MMFData <> nil then
    begin
      MMFData^.DialogHandle := Value;
      UnMapMMF(MMFData);
      Result := True;
    end;
    CloseMMF(MMFHandle);
  end;
end;

function SetMapWindowHandle(Value: HWND): Boolean;
var
  MMFHandle: THandle;
  MMFData: PMMFData;
begin
  Result := False;
  MMFHandle := OpenMMF(MMFName);
  if MMFHandle <> 0 then
  begin
    MMFData := MapMMF(MMFHandle);
    if MMFData <> nil then
    begin
      MMFData^.MapWindowHandle := Value;
      UnMapMMF(MMFData);
      Result := True;
    end;
    CloseMMF(MMFHandle);
  end;
end;

initialization
  begin
    MMFHandle := 0;
    MMFData   := nil;
  end;

finalization
  FreeHook;
end.
