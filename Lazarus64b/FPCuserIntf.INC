//   Delphi <-> FSUIPC/WideFS Communication                       Version 1.005
//   Copyright © 2000 by Pelle F. S. Liljendal               pelle@liljendal.dk
//   All Rights Reserved
//
//   FPCuserIntf.INC
//
//   This include-file contains the "interface-section" of global definitions
//   used by "FPCuser.PAS" or "FPCuserClass.PAS". These definitions are
//   therefore accessible to units using either "FPCuser.PAS" or
//   "FPCuserClass.PAS"
//
//   (Modified by Chris Brett to add support for FS2002 UIPC compatibility.)
//   (chris@formulate.clara.net)
//
//-----------------------------------------------------------------------------

Const
  // Supported Sims
  SIM_ANY                  =  0;
  SIM_FS98                 =  1;
  SIM_FS2K                 =  2;
  SIM_CFS2                 =  3;
  SIM_CFS1                 =  4;
  SIM_FS2K2	           =  6;
  SIM_FS2K4	           =  7;
  SIM_FSX	           =  8;
  SIM_ESP	           =  9;

  // Error numbers
  FSUIPC_ERR_OK	           =  0;
  FSUIPC_ERR_OPEN          =  1;  // Attempt to Open when already Open
  FSUIPC_ERR_NOFS          =  2;  // Cannot link to FSUIPC or WideClient
  FSUIPC_ERR_REGMSG        =  3;  // Failed to Register common message with Windows
  FSUIPC_ERR_ATOM          =  4;  // Failed to create Atom for mapping filename
  FSUIPC_ERR_MAP           =  5;  // Failed to create a file mapping object
  FSUIPC_ERR_VIEW          =  6;  // Failed to open a view to the file map
  FSUIPC_ERR_VERSION       =  7;  // Incorrect version of FSUIPC, or not FSUIPC
  FSUIPC_ERR_WRONGFS       =  8;  // Sim is not version requested
  FSUIPC_ERR_NOTOPEN       =  9;  // Call cannot execute, link not Open
  FSUIPC_ERR_NODATA        = 10;  // Call cannot execute: no requests accumulated
  FSUIPC_ERR_TIMEOUT       = 11;  // IPC timed out all retries
  FSUIPC_ERR_SENDMSG       = 12;  // IPC sendmessage failed all retries
  FSUIPC_ERR_DATA          = 13;  // IPC request contains bad data
  FSUIPC_ERR_RUNNING       = 14;  // Maybe running on WideClient, but FS not running on Server, or wrong FSUIPC
  FSUIPC_ERR_SIZE          = 15;  // Read or Write request cannot be added, memory for Process is full

Var
  //--- IPC Client Stuff ---
  FSUIPC_Version       : DWORD;    // Version of FSUIPC/Wide-FS
  FSUIPC_FS_Version    : DWORD;    // Version of FS
  FSUIPC_Lib_Version   : DWORD;    // Version of Lib (according to Peter Dowson's source)
  FSUIPC_Class_Version : DWORD;    // Version of Class implementation (Pelle F. S. Liljendal)


