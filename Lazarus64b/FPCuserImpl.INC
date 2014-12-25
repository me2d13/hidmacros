//   Delphi <-> FSUIPC/WideFS Communication                       Version 1.004
//   Copyright © 2000 by Pelle F. S. Liljendal               pelle@liljendal.dk
//   All Rights Reserved
//
//   FPCuserImpl.INC
//
//   This include-file contains the "implementation-section" of global
//   definitions used by "FPCuser.PAS" or "FPCuserClass.PAS". These definitions
//   are therefore NOT accessible outside "FPCuser.PAS" or "FPCuserClass.PAS".
//
//-----------------------------------------------------------------------------

Const
  LIB_VERSION              = 2005;   // 2.005  (Peter Dowson's source)
  CLASS_VERSION            = 0900;   // 0.900  (Pelle F. S. Liljendal's Source)
  MAX_SIZE                 = $7F00;  // Largest data (kept below 32k to avoid
                                     // any possible 16-bit sign problems
  FS6IPC_MSGNAME1          = 'FsasmLib:IPC';
  FS6IPC_MESSAGE_SUCCESS   = 1;
  FS6IPC_MESSAGE_FAILURE   = 0;
  // IPC message types
  FS6IPC_READSTATEDATA_ID  = 1;
  FS6IPC_WRITESTATEDATA_ID = 2;


Type
  FS6IPC_READSTATEDATA_HDR = packed record
    dwId     : dword;
    dwOffset : dword;
    nBytes   : dword;
    pDest    : pointer;
  end;
  FS6IPC_WRITESTATEDATA_HDR = packed record
    dwId     : dword;
    dwOffset : dword;
    nBytes   : dword;
  end;

