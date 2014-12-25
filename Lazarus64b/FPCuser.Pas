//   Delphi <-> FSUIPC/WideFS Communication                       Version 1.004
//   Copyright © 2000 by Pelle F. S. Liljendal               pelle@liljendal.dk
//   All Rights Reserved
//
//   FPCuser.pas
//
//   This unit is based on C-Source by Peter Dowson (and Adam Szofran). It makes
//   it possible for Delphi programmers to interact with (MS) FlightSimulator
//   through FSUIPC and/or WideFS (both by Peter Dowson).
//
//-----------------------------------------------------------------------------
Unit FPCuser;

//=============================================================================
Interface
//=============================================================================


Uses
  Windows, SysUtils;

{$I FPCuserIntf.INC}

Var
  //--- IPC Client Stuff ---
  m_hWnd             : hwnd;     // FS6 window handle
  m_msg              : uint;     // id of registered window message
  m_atom             : atom;     // global atom containing name of file-mapping object
  m_hMap             : thandle;  // handle of file-mapping object
  m_pView            : pointer;  // pointer to view of file-mapping object
  m_pNext            : pointer;


Procedure FSUIPC_Close;
Function  FSUIPC_Open(dwFSReq : DWORD; var dwResult : DWORD) : Boolean;
Function  FSUIPC_Process(var dwResult : DWORD) : Boolean;
Function  FSUIPC_Read(dwOffset : DWORD; dwSize : DWORD; pDest : Pointer; var dwResult : DWORD) : Boolean;
Function  FSUIPC_Write(dwOffset : DWORD; dwSize : DWORD; pSrce : Pointer; var dwResult : DWORD) : Boolean;


//=============================================================================
Implementation
//=============================================================================

{$I FPCuserImpl.INC}


//--- Stop the Client ---------------------------------------------------------
Procedure FSUIPC_Close;
begin
  m_hWnd  := 0;
  m_msg   := 0;
  m_pNext := Nil;
  if (m_atom <> 0) then begin
    GlobalDeleteAtom(m_atom);
    m_atom := 0;
  end;
  if (m_pView <> Nil) then begin
    UnmapViewOfFile(m_pView);
    m_pView := Nil;
  end;
  if (m_hMap <> 0) then begin
    CloseHandle(m_hMap);
    m_hMap := 0;
  end;
end;


//--- Start the Client --------------------------------------------------------
// returns TRUE if successful, FALSE otherwise,
// if FALSE dwResult contains the "error-code"
Function FSUIPC_Open(dwFSReq : DWORD; var dwResult : DWORD) : Boolean;
var
  szName  : AnsiString;
  fWideFS : Boolean;  
  nTry    : Integer;
  i       : Integer;
begin
  nTry    := 0;
  fWideFS := False;
  i       := 0;

  // abort if already started
  if (m_pView <> Nil) then
  begin
    dwResult := FSUIPC_ERR_OPEN;
    Result   := False;
    Exit;
  end;
  //  Clear version information, so know when connected
  FSUIPC_Version    := 0;
  FSUIPC_FS_Version := 0;

  // Connect via FSUIPC, wich is known to be FSUIPC's own
  // and isn't subject to user modification
  m_hWnd := FindWindowEx(0, 0, PChar('UIPCMAIN'), Nil);
  if (m_hWnd = 0) then begin
    // If there's no UIPCMAIN, we may be using WideClient
    // which only simulates FS98
    m_hWnd := FindWindowEx(0, 0, PChar('FS98MAIN'), Nil);
    fWideFS := TRUE;
    if (m_hWnd = 0) then begin
      dwResult := FSUIPC_ERR_NOFS;
      Result   := FALSE;
      Exit;
    end;
  end;
  // register the window message
  m_msg := RegisterWindowMessage(FS6IPC_MSGNAME1);
  if (m_msg = 0) then begin
    dwResult := FSUIPC_ERR_REGMSG;
    Result   := FALSE;
    Exit;
  end;

  // create the name of our file-mapping object
  Inc(nTry); // Ensures a unique string is used in case user closes and reopens
  szName := Format('%s:%X:%X', [FS6IPC_MSGNAME1, GetCurrentProcessId, nTry]);

  // stuff the name into a global atom
  m_atom := GlobalAddAtom(PChar(szName));
  if (m_atom = 0) then begin
    dwResult := FSUIPC_ERR_ATOM;
    FSUIPC_Close;
    Result := FALSE;
    Exit;
  end;

  // create the file-mapping object
  m_hMap := CreateFileMapping(THANDLE($FFFFFFFF), // use system paging file
                              Nil,                // security
                              PAGE_READWRITE,     // protection
                              0, MAX_SIZE+256,    // size
                              PChar(szName));     // name

  if ((m_hMap = 0) or (GetLastError = ERROR_ALREADY_EXISTS)) then begin
    dwResult := FSUIPC_ERR_MAP;
    FSUIPC_Close;
    Result := FALSE;
    Exit;
  end;

  // get a view of the file-mapping object
  m_pView := MapViewOfFile(m_hMap, FILE_MAP_WRITE, 0, 0, 0);
  if (m_pView = Nil) then begin
    dwResult := FSUIPC_ERR_VIEW;
    FSUIPC_Close;
    Result := FALSE;
    Exit;
  end;

  // Okay, now determine FSUIPC version AND FS type
  m_pNext := m_pView;

  // Try up to 5 times with a 100msec rest between each
  // Note that WideClient returns zeros initially, whilst waiting
  // for the Server to get the data
  while ((i < 5) and ((FSUIPC_Version = 0) or (FSUIPC_FS_Version = 0))) do begin
    Inc(i);
    // Read FSUIPC version
    if (not FSUIPC_Read($3304, 4, @FSUIPC_Version, dwResult)) then begin
      FSUIPC_Close;
      Result := FALSE;
      Exit;
    end;

    // and FS version and validity check pattern
    if (not FSUIPC_Read($3308, 4, @FSUIPC_FS_Version, dwResult)) then begin
      FSUIPC_Close;
      Result := FALSE;
      Exit;
    end;

    // write our Library version number to a special read-only offset
    // This is to assist diagnosis from FSUIPC logging
    // But only do this on first try
    if (i<2) and (not FSUIPC_Write($330A, 2, @FSUIPC_Lib_Version, dwResult)) then begin
      FSUIPC_Close;
      Result := FALSE;
      Exit;
    end;

    // Actually send the request ang get the responses ("process")
    if not(FSUIPC_Process(dwResult)) then begin
      FSUIPC_Close;
      Result := FALSE;
      Exit;
    end;

    // Maybe running on WideClient, and need another try
    Sleep(100); // Give it a chance
  end;

  // Only allow running on FSUIPC 1.998e or later
  // with correct check pattern $FADE
  if ((FSUIPC_Version < $19980005) or ((FSUIPC_FS_Version and $FFFF0000) <> $FADE0000)) then begin
    if fWideFS then dwResult := FSUIPC_ERR_RUNNING
               else dwResult := FSUIPC_ERR_VERSION;
    FSUIPC_Close();
    Result := FALSE;
    Exit;
  end;

  FSUIPC_FS_Version := (FSUIPC_FS_Version and $ffff); // Isolates the FS version number
  // Optional user specific FS request
  if (dwFSReq <> 0) and (dwFSReq <> FSUIPC_FS_Version) then begin
    dwResult := FSUIPC_ERR_WRONGFS;
    FSUIPC_Close;
    Result := FALSE;
    Exit;
  end;

  dwResult := FSUIPC_ERR_OK;
  Result   := TRUE;
end;


//--- Process read/write ------------------------------------------------------
Function  FSUIPC_Process(var dwResult : DWORD) : Boolean;
var
  dwError : DWORD;
  pdw     : ^DWORD;  //Pointer;
  pHdrR   : ^FS6IPC_READSTATEDATA_HDR;
  pHdrW   : ^FS6IPC_WRITESTATEDATA_HDR;
  i       : Integer;
begin
  i := 0;

  if (m_pView = Nil) then
  begin
    dwResult := FSUIPC_ERR_NOTOPEN;
    Result   := FALSE;
    Exit;
  end;

  if (m_pView = m_pNext) then
  begin
    dwResult := FSUIPC_ERR_NODATA;
    Result   := FALSE;
    Exit;
  end;

  ZeroMemory(m_pNext, 4); // Terminator
  m_pNext := m_pView;

  // send the request (allow up to 9 tries)
  while (i < 10) and ((SendMessageTimeout(
                          m_hWnd,              // FS6 window handle
                          m_msg,               // our registered message id
                          m_atom,              // wParam: name of file-mapping object
                          0,                   // lParam: offset of request into file-mapping obj
                          SMTO_BLOCK,          // halt this thread until we get a response
                          2000,	               // time out interval
                          dwError)) = 0) do
  begin  // return value
    Inc(i);
    Sleep(100); // Allow for things to happen
  end;

  if (i >= 10) then
  begin  // Failed all tries
    if GetLastError = 0 then dwResult := FSUIPC_ERR_TIMEOUT
                        else dwResult := FSUIPC_ERR_SENDMSG;
    Result := FALSE;
    Exit;
  end;

  if (dwError <> FS6IPC_MESSAGE_SUCCESS) then
  begin // FSUIPC didn't like something in the data!
    dwResult := FSUIPC_ERR_DATA;
    Result   := FALSE;
    Exit;
  end;

  // Decode and store results of Read requests
  pdw := m_pView;
  while (pdw <> Nil) and Boolean(pdw^) do
  begin
    case pdw^ of
      FS6IPC_READSTATEDATA_ID :
        begin
          pHdrR := Pointer(pdw);
          DWORD(m_pNext) := DWORD(m_pNext) + sizeof(FS6IPC_READSTATEDATA_HDR);
          if (pHdrR^.pDest <> Nil) and (pHdrR^.nBytes <> 0) then
          begin
            CopyMemory(pHdrR^.pDest, m_pNext, pHdrR^.nBytes);
          end;
          DWORD(m_pNext) := DWORD(m_pNext) + pHdrR^.nBytes;
        end;
    FS6IPC_WRITESTATEDATA_ID :
      begin
        // This is a write, so there's no returned data to store
        pHdrW := Pointer(pdw);
        DWORD(m_pNext) := DWORD(m_pNext) + sizeof(FS6IPC_WRITESTATEDATA_HDR) +
                          pHdrW^.nBytes;
      end;
    else
      // Error! So terminate the scan
      pdw := Nil;
    end;
    pdw := m_pNext;
  end;
  m_pNext  := m_pView;
  dwResult := FSUIPC_ERR_OK;
  Result   := TRUE;
end;


//--- Read Data ---------------------------------------------------------------
Function  FSUIPC_Read(dwOffset : DWORD; dwSize : DWORD; pDest : Pointer; var dwResult : DWORD) : Boolean;
var
  pHdr : ^FS6IPC_READSTATEDATA_HDR;
begin
  pHdr := m_pNext;

  // Check link is open
  if (m_pView = Nil) then
  begin
    dwResult := FSUIPC_ERR_NOTOPEN;
    Result   := FALSE;
    Exit;
  end;

  // Check have space for this request (including terminator)
  if (((DWORD(m_pNext) - DWORD(m_pView)) + 4 + (dwSize + sizeof(FS6IPC_READSTATEDATA_HDR))) > MAX_SIZE) then
  begin
    dwResult := FSUIPC_ERR_SIZE;
    Result   := FALSE;
    Exit;
  end;

  // Initialise header for read request
  pHdr^.dwId     := FS6IPC_READSTATEDATA_ID;
  pHdr^.dwOffset := dwOffset;
  pHdr^.nBytes   := dwSize;
  pHdr^.pDest    := pDest;

  // Move pointer past the Record
  DWORD(m_pNext) := DWORD(m_pNext) + sizeof(FS6IPC_READSTATEDATA_HDR);
  if (dwSize <> 0) then
  begin
    // Zero the reception area, so rubbish won't be returned
    ZeroMemory(m_pNext, dwSize);
    // Update the pointer ready for more data
    DWORD(m_pNext) := DWORD(m_pNext) + dwSize;
  end;
  dwResult := FSUIPC_ERR_OK;
  Result   :=  TRUE;
end;


//--- Write Data --------------------------------------------------------------
Function  FSUIPC_Write(dwOffset : DWORD; dwSize : DWORD; pSrce : Pointer; var dwResult : DWORD) : Boolean;
var
  pHdr : ^FS6IPC_WRITESTATEDATA_HDR;
begin
  pHdr := m_pNext;

  if (m_pView = Nil) then
  begin
    dwResult := FSUIPC_ERR_NOTOPEN;
    Result   := FALSE;
    Exit;
  end;

  // Check have space for this request (including terminator)
  if (((DWORD(m_pNext) - DWORD(m_pView)) + 4 +(dwSize + sizeof(FS6IPC_WRITESTATEDATA_HDR))) > MAX_SIZE) then
  begin
    dwResult := FSUIPC_ERR_SIZE;
    Result   := FALSE;
    Exit;
  end;
  // Initialise header for write request
  pHdr^.dwId     := FS6IPC_WRITESTATEDATA_ID;
  pHdr^.dwOffset := dwOffset;
  pHdr^.nBytes   := dwSize;

  // Move pointer past the Record
  DWORD(m_pNext) := DWORD(m_pNext) + sizeof(FS6IPC_WRITESTATEDATA_HDR);
  if (dwSize <> 0) then
  begin    // Copy in the data to be written
    CopyMemory(m_pNext, pSrce, dwSize);    // Update the pointer ready for more data
    DWORD(m_pNext) := DWORD(m_pNext) + dwSize;
  end;
  dwResult := FSUIPC_ERR_OK;
  Result   := TRUE;
end;


Initialization
  //--- Initialize global variables ---
  FSUIPC_Version       := 0;
  FSUIPC_FS_Version    := 0;
  FSUIPC_Lib_Version   := LIB_VERSION;
  FSUIPC_Class_Version := 0;
  //--- IPC Client Stuff ---
  m_hWnd    := 0;      // FS6 window handle
  m_msg     := 0;      // id of registered window message
  m_atom    := 0;      // global atom containing name of file-mapping object
  m_hMap    := 0;      // handle of file-mapping object
  m_pView   := Nil;    // pointer to view of file-mapping object
  m_pNext   := Nil;
finalization
  //--- Automatic "close" if programmer "forgets" ---
  FSUIPC_Close;
end.
